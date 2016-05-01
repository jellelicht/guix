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
  #:use-module (ice-9 regex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages jemalloc))

;; For a true bootstrap:
;; https://github.com/rust-lang/rust/tree/ef75860a0a72f79f97216f8aaa5b388d98da6480/src/boot
;; (But that would take about 319 iterative bootstrapped compiler builds)
;; It could eventually work once the rust team has a more structured approach for bootstrapping


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

(define (archive-name version platform checksum)
  (string-append "rust-stage0-" version "-" platform "-" checksum ".tar.bz2"))

(define rust-stage0-bootstrap-x86_64-archive
  (archive-name "2016-02-17-4d3eebf" "linux-x86_64" "d29b7607d13d64078b6324aec82926fb493f59ba"))

(define rust-stage0-bootstrap-x86_64
  (origin
    (method url-fetch)
    (uri
     (string-append "https://static.rust-lang.org/" rust-stage0-bootstrap-x86_64-archive))
    (sha256 
     (base32
      "0gk87rknijyirlhw3h34bjxzq98j0v0icp3l8flrxn5pgil8pswd"))))

(define rust-stage0-bootstrap-i386-archive
  (archive-name "2016-02-17-4d3eebf" "linux-i386" "5f194aa7628c0703f0fd48adc4ec7f3cc64b98c7"))

(define rust-stage0-bootstrap-i386
  (origin
    (method url-fetch)
    (uri
     (string-append "https://static.rust-lang.org/" rust-stage0-bootstrap-i386-archive))
    (sha256
     (base32
      "16fd2hmli86g1q3fyicdhh2l4aqryzxcij7sk1pljig8dr2m8hg5"))))

(define-public rust-stage0
  (package
    (name "rust-stage0")
    (version "2016-02-17-4d3eebf")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((out (assoc-ref %outputs "out"))
             (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
             (bzip2 (string-append (assoc-ref %build-inputs "bzip2") "/bin/bzip2"))
             (patchelf (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf"))
             (tarball (assoc-ref %build-inputs "rust-bootstrap"))
             (ld-so (string-append (assoc-ref %build-inputs "libc")
                                   ,(glibc-dynamic-linker)))
             (gcc:lib (assoc-ref %build-inputs "gcc:lib")))
         (use-modules (guix build utils))
         (mkdir out)
         (copy-file tarball "bootstrap.tar.bz2")
         (let ((builddir (getcwd)))
           (with-directory-excursion out
             (and (zero? (system* bzip2 "-d"
                                  (string-append builddir "/bootstrap.tar.bz2")))
                  (zero? (system* tar "xvf"
                                  (string-append builddir "/bootstrap.tar")))
                  (zero? (system* patchelf "--set-interpreter" ld-so "rust-stage0/bin/rustc"))
                  (zero? (system* patchelf "--set-rpath" (string-append gcc:lib "/lib") "rust-stage0/bin/rustc")))
              (copy-recursively "rust-stage0" "." )
              (delete-file-recursively "rust-stage0"))))))
    (inputs
     `(("tar" ,tar)
       ("bzip2" ,bzip2)
       ("libc" ,glibc)
       ("gcc:lib" ,gcc "lib")
       ("rust-bootstrap"
        ,(if (string-match "x86_64" (or (%current-target-system) (%current-system)))
             rust-stage0-bootstrap-x86_64
             rust-stage0-bootstrap-i386))
       ("patchelf" ,patchelf))
     )
    (source #f)
    (synopsis "todo")
    (description "todo")
    (license license:gpl3+)
    (home-page "none")
    ));; XXX: rewrite/fix. Maybe inherit?

(define-public rust
  (package
    (name "rust")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri "https://static.rust-lang.org/dist/rustc-1.8.0-src.tar.gz")
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jblby81qlbz4csdldsv7hqrcbfwsaa79q265bgb8kcdgqa6ci5g"))
              (patches
               (search-patches "rust-disable-codegen-tests.patch"
                               "rustc-env-workaround.patch"))))
    (build-system gnu-build-system)
    ;; XXX: rust should probably run on everything
    (supported-systems '("i686-linux" "x86_64-linux")) 
    (arguments
     `(#:make-flags (list "CC=gcc") ;; XXX: stil not enough

       #:phases
       ;; XXX: commit a3fdde7
       ;;#:configure-flags "--disable-codegen-tests"

       (alist-cons-before
        'configure 'patch-sources
        (lambda _
          ;; XXX Cannot use snippet because zip files are not supported
          (substitute* "mk/cfg/x86_64-unknown-linux-gnu.mk" ; <- check this
            (("^CC_x86_64-unknown-linux-gnu=.*(CC)$") "CC_x86_64-unknown-linux-gnu=gcc"))
          (substitute* "mk/cfg/i686-unknown-linux-gnu.mk" ; <- check this
            (("^CC_i686-unknown-linux-gnu=.*(CC)$") "CC_i686-unknown-linux-gnu=gcc"))
          (substitute* "src/librustc_back/target/mod.rs" ; <- check this
            (("^.*linker: option_env!(\"CFG_DEFAULT_LINKER\").unwrap_or(\"cc\").to_string(),$")
             "            linker: option_env!(\"CFG_DEFAULT_LINKER\").unwrap_or(\"gcc\").to_string(),")))

        (modify-phases (alist-replace
                        'configure
                        (lambda* (#:key outputs #:allow-other-keys)
                          ;; This old `configure' script doesn't support
                          ;; variables passed as arguments.
                          (let ((out (assoc-ref outputs "out"))
                                (llvm (assoc-ref %build-inputs "llvm"))
                                (jemalloc (assoc-ref %build-inputs "jemalloc"))
                                (rust-stage0 (assoc-ref %build-inputs "rust-stage0")))
                            (setenv "CONFIG_SHELL" (which "bash"))
                            (setenv "CC" "gcc") ;; XXX; still not enough
                                        ;(setenv "CC" (string-append (assoc-ref %build-inputs "gcc") "/bin/gcc"))
                            (zero?
                             (system* "./configure"
                                      "--disable-codegen-tests"
                                      "--enable-local-rust"
                                      "--default-linker=gcc" ;; XXX; why oh why rust do you not heed this argument XD
                                      (string-append "--prefix=" out)
                                      (string-append "--llvm-root=" llvm)
                                      (string-append "--jemalloc-root=" jemalloc "/lib")
                                      (string-append "--local-rust-root=" rust-stage0)))))
                        %standard-phases)))))
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
     `(("patchelf" ,patchelf)
       ("which" ,which)
       ("rust-stage0" ,rust-stage0)))
    (home-page "https://www.rust-lang.org/")
    (synopsis
     "The Rust Programming Language")
    (description
     "LOREM IPSUM BLA")
    (license license:gpl3+)))
