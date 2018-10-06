;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build node-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            node-build))

;; Commentary:
;;
;; Builder-side code of the standard Node/npm package build procedure.
;;
;; Code:

(define* (read-package-data #:key (filename "package.json"))
  (call-with-input-file filename
    (lambda (port)
      (read-json port))))

(define* (build #:key inputs #:allow-other-keys)
  (define (build-from-package-json? package-file)
    (let* ((package-data (read-package-data #:filename package-file))
           (scripts (assoc-ref package-data "scripts")))
      (assoc-ref scripts "build")))

  "Build a new node module using the appropriate build system."
  ;; XXX: Develop a more robust heuristic, allow override
  (cond ((file-exists? "gulpfile.js")
         (invoke "gulp"))
        ((file-exists? "gruntfile.js")
         (invoke "grunt"))
        ((file-exists? "Makefile")
         (invoke "make"))
        ((and (file-exists? "package.json")
              (build-from-package-json? "package.json"))
         (invoke "npm" "run" "build"))
        (else
         #t)))

(define* (link-npm-dependencies #:key inputs #:allow-other-keys)
  (define (inputs->node-inputs inputs)
    "Filter the directory part from INPUTS."
    (filter (lambda (input)
              (match input
                ((name . _) (node-package? name))))
            inputs))
  (define (inputs->directories inputs)
    "Extract the directory part from INPUTS."
    (match inputs
      (((names . directories) ...)
       directories)))
  (define (make-node-path root)
    (string-append root "/lib/node_modules/"))
  
  (let ((input-node-directories (inputs->directories
                                 (inputs->node-inputs inputs))))
    (union-build "node_modules"
                 (map make-node-path input-node-directories))))

(define configure link-npm-dependencies)

(define* (check #:key tests? #:allow-other-keys)
  "Run 'npm test' if TESTS?"
  (if tests?
      ;; Should only be enabled once we know that there are tests
      (invoke "npm" "test")))

(define* (install #:key outputs inputs global? #:allow-other-keys)
  "Install the node module to the output store item. MODULENAME defines
under which name the module will be installed, GLOBAL? determines whether this
is an npm global install."
  (let* ((out         (assoc-ref outputs "out"))
         (src-dir     (getcwd))
         (tgt-dir     (string-append out "/lib"))
         (bin-dir     (string-append out "/bin"))
         (home        (string-append (getenv "NIX_BUILD_TOP") "/npm-home"))
         (modulename  (string-append  (assoc-ref (read-package-data) "name"))))
    (setenv "HOME" home)
    (mkdir-p tgt-dir)
    (copy-recursively "." (string-append tgt-dir "/node_modules/" modulename))
    (when global? ;;XXX: Should actually wrap inputs as well
      (symlink (string-append tgt-dir "/node_modules/" modulename "/bin") bin-dir))
    #t))

(define (node-package? name)
  "Check if NAME correspond to the name of an Node package."
  (string-prefix? "node-" name))


(define %standard-phases
  (modify-phases gnu:%standard-phases
    ;;(add-after 'unpack 'delete-minified-files delete-minified-files) ;;TODO: Use existing solution for this.
    (replace 'configure configure)
    (replace 'build build)
    (replace 'install install)
    (delete 'check)
    (add-after 'install 'check check)
    (delete 'strip) ;;
    ))

(define* (node-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
