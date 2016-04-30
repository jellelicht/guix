;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nils Gillmann <ng0@libertad.pw>
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

(define-module (gnu packages rust)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  ;; #:use-module (gnu packages gcc)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages jemalloc))


;; XXX: 
;; patch-shebang: ./src/llvm/utils/llvm-compilers-check: warning: no binary for interpreter `python3' found in $PATH
;; patch-shebang: ./src/llvm/utils/makellvm: warning: no binary for interpreter `csh' found in $PATH
;; x86_64-unknown-linux-gnu/stage0/bin/rustc:
;; /gnu/store/b1yqjimbdh5bf9jnizd4h7yf110744j2-bash-4.3.42/bin/bash: x86_64-unknown-linux-gnu/stage0/bin/rustc: No such file or directory
;; /gnu/store/b...-bash-4.3.42/bin/bash: some/dir/rustc: No such file or directory



;;(list
;; (string-append
;; "https://github.com/rust-lang/rust/archive/"
;; version ".tar.gz")
;;gh "1m2d1dc243s7ym8fq2yag3fr5jvki0q9c39llfwgcpq1gc8jvcn8"))))

;; -> take/recreate patch from https://github.com/rust-lang/rust/pull/32168/files

;; PAtch rust-disable-codegen-tests.patch

;; Trivial build system demo

;; (define* (wrap-python3 python
;;                        #:optional
;;                        (name (string-append (package-name python) "-wrapper")))
;;   (package (inherit python)
;;     (name name)
;;     (source #f)
;;     (build-system trivial-build-system)
;;     (outputs '("out"))
;;     (propagated-inputs `(("python" ,python)))
;;     (arguments
;;      `(#:modules ((guix build utils))
;;        #:builder
;;          (begin
;;            (use-modules (guix build utils))
;;            (let ((bin (string-append (assoc-ref %outputs "out") "/bin"))
;;                  (python (string-append (assoc-ref %build-inputs "python") "/bin/")))
;;                 (mkdir-p bin)
;;                 (for-each
;;                   (lambda (old new)
;;                     (symlink (string-append python old)
;;                              (string-append bin "/" new)))
;;                   `("python3" ,"pydoc3" ,"idle3")
;;                   `("python"  ,"pydoc"  ,"idle"))))))
;;     (synopsis "Wrapper for the Python 3 commands")
;;     (description
;;      "This package provides wrappers for the commands of Python@tie{}3.x such
;; that they can be invoked under their usual name---e.g., @command{python}
;; instead of @command{python3}.")))



;; (define-public rust-stage0
;;   ;; XXX: Untrusted binary, woohoo
;;   (package
;;     ;;(name "rustc")
;;     (name "rust-stage0")
;;     ;; XXX: Use a 'stable' stage0 compiler, once Rust has a working workflow
;;     (version "2016-02-17-4d3eebf-linux-i386-5f194aa7628c0703f0fd48adc4ec7f3cc64b98c7")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append
;;                     "http://static.rust-lang.org/stage0-snapshots/" name
;;                     "-" version ".tar.bz2"))
;;               (file-name (string-append name "-" version ".tar.bz2"))
;;               (sha256
;;                (base32
;;                 "16fd2hmli86g1q3fyicdhh2l4aqryzxcij7sk1pljig8dr2m8hg5"))))
;;     (build-system trivial-build-system)
;;     (arguments
;;      `(#:modules ((guix build utils))
;;                  #:builder
;;                  (begin
;;                    (use-modules (guix build utils))
;;                    (let ((tar  (assoc-ref %build-inputs "tar"))
;;                          (bzip (assoc-ref %build-inputs "bzip2"))
;;                          (out  (assoc-ref %outputs "out")))
;;                      (setenv "PATH" (string-append tar "/bin:" bzip "/bin"))
;;                      (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
;;                      (chdir ,name)
;;                      (copy-recursively "bin" (string-append out "/bin"))))))
;;     (native-inputs
;;      `(("tar" ,tar)
;;        ("bzip2" ,bzip2)))
;;     (home-page "https://www.rust-lang.org/")
;;     (synopsis
;;      "Rustc stage0 bootstrap")
;;     (description
;;      "LOREM IPSUM BLA")
;;     (license license:gpl3+)))

;; ./configure --local-rust-root=rustc --enable-rust-root
;;(define-public rustc
                                        ; rust-stage0-2016-02-17-4d3eebf-linux-x86_64-d29b7607d13d64078b6324aec82926fb493f59ba.tar.bz2jj
(define-public rust
  (let* ((bootstrap-name "rust-stage0")
         (bootstrap-version "2016-02-17-4d3eebf-linux-x86_64-d29b7607d13d64078b6324aec82926fb493f59ba")
         (rust-bootstrap (origin
                          (method url-fetch)
                          (uri (string-append
                                "http://static.rust-lang.org/stage0-snapshots/" bootstrap-name "-"
                                bootstrap-version ".tar.bz2"))
                          (sha256
                           (base32
                            "0gk87rknijyirlhw3h34bjxzq98j0v0icp3l8flrxn5pgil8pswd")))))
    ;
    (package
      (name "rust")
      (version "1.8.0")
      (source (origin
                (method url-fetch)
                (uri "https://static.rust-lang.org/dist/rustc-1.8.0-src.tar.gz")

                                        ;(string-append
                      ;;"https://static.rust-lang.org/dist/" name "-"
                      ;;version "-src.tar.gz"))
                      ;"https://github.com/rust-lang/" name "/archive/"
                      ;version ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  ;;"0jblby81qlbz4csdldsv7hqrcbfwsaa79q265bgb8kcdgqa6ci5g"))))
                  ;"1p45ik0mr818ywy5gpxi68h6md9ndmcpw7g4rx7l6frvfn5xd4sy"
                  "0jblby81qlbz4csdldsv7hqrcbfwsaa79q265bgb8kcdgqa6ci5g"
                  ))
                (patches
                 (search-patches "rust-disable-codegen-tests.patch"
                                 "rustc-env-workaround.patch"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         ;; XXX: commit a3fdde7
         ;;#:configure-flags "--disable-codegen-tests"
         (modify-phases (alist-replace
                         'configure
                         (lambda* (#:key outputs #:allow-other-keys)
                           ;; This old `configure' script doesn't support
                           ;; variables passed as arguments.
                           (let ((out (assoc-ref outputs "out"))
                                 (llvm (assoc-ref %build-inputs "llvm"))
                                 (jemalloc (assoc-ref %build-inputs "jemalloc")))
                             (setenv "CONFIG_SHELL" (which "bash"))
                             (zero?
                              (system* "./configure"
                                       "--disable-codegen-tests"
                                        ;(string-append "--local-rust-root=" rust-stage0)
                                       (string-append "--prefix=" out)
                                       (string-append "--llvm-root=" llvm)
                                       (string-append "--jemalloc-root=" jemalloc "/lib")))))
                         %standard-phases)
           (add-after 'configure 'use-static-stage0
             (lambda _
               (system* "cp" (assoc-ref %build-inputs "rust-bootstrap")
                        (string-append  "dl/"
                                        ,bootstrap-name "-" ,bootstrap-version ".tar.bz2"))
               )))))
      ;(system* "cp" (assoc-ref %build-inputs "rust-bootstrap") "dl")
      ;; #:configure-flags
      ;; (list
      ;;  (string-append "--llvm-root="
      ;;                 (assoc-ref %build-inputs "llvm")))))
      (inputs
       `(("python-2" ,python-2)
         ("curl" ,curl)
         ("git" ,git)
         ("valgrind" ,valgrind)
         ("libffi" ,libffi)
         ("perl" ,perl)
         ("llvm" ,llvm)
         ("jemalloc" ,jemalloc)))
      (native-inputs
       `(
         ("rust-bootstrap" ,rust-bootstrap)
         ))
      (home-page "https://www.rust-lang.org/")
      (synopsis
       "The Rust Programming Language")
      (description
       "LOREM IPSUM BLA")
      (license license:gpl3+)))
  )
