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
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            node-build
            npm-home))

;; Commentary:
;;
;; Builder-side code of the standard Node/npm package build procedure.
;;
;; Code:

(define (delete-minified-files . _)
  "Delete minified files and their associated source mappings on the grounds
   that they are build artifacts that Guix should generate from source code."
  (for-each (lambda (file)
              (format #t "deleting minified JavaScript file '~a'~%" file)
              (delete-file file))
            (find-files "." "(min\\.js|min\\.js\\.map|min\\.map)$"))
  #t)

(define* (build #:key outputs inputs #:allow-other-keys)
  "Build a new node module using the appropriate build system."
  ;; XXX: Develop a more robust heuristic, allow override
  (cond ((file-exists? "gulpfile.js")
         (zero? (system* "gulp")))
        ((file-exists? "gruntfile.js")
         (zero? (system* "grunt")))
        ((file-exists? "Makefile")
         (zero? (system* "make")))
        (else
         #t)))

(define* (check #:key tests? #:allow-other-keys)
  "Run 'npm test' if TESTS?"
  (if tests?
      ;; Should only be enabled once we know that there are tests
      (zero? (system* "npm" "test"))
      #t))

(define* (install #:key outputs inputs modulename global? #:allow-other-keys)
  "Install the node module to the output store item. MODULENAME defines how
under which name the module will be installed, GLOBAL? determines whether this
is an npm global install."
  (let* ((out         (assoc-ref outputs "out"))
         (src-dir     (getcwd))
         (tgt-dir     (string-append out "/lib"))
         (bin-dir     (string-append out "/bin"))
         (home        (string-append (getenv "NIX_BUILD_TOP") "/npm-home")))
    (setenv "HOME" home)
    (mkdir-p tgt-dir)
    (copy-recursively "." (string-append tgt-dir "/node_modules/" modulename))
    (when global? ;;XXX: Should actually wrap inputs as well
      (symlink (string-append tgt-dir "/node_modules/" modulename "/bin") bin-dir))
    #t))


(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'delete-minified-files delete-minified-files)
    (delete 'configure)
    ;(replace 'build build)
    (delete 'build)
    (replace 'install install)
    (delete 'check)
    (delete 'strip) ;;
    ;;(replace 'check check)
    ))

(define* (node-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
