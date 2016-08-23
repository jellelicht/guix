;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages node)
  #:use-module ((guix licenses) #:select (expat))
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages compression) #:prefix compression:)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages tls) #:prefix tls:)
  #:use-module (gnu packages valgrind))

(define-public http-parser
  (package
    (name "http-parser")
    (version "2.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/nodejs/http-parser/archive/v"
                                  version ".tar.gz"))
              (sha256 (base32 "1cw6nf8xy4jhib1w0jd2y0gpqjbdasg8b7pkl2k2vpp54k9rlh3h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc" (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:test-target "test-valgrind"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-makefile
           (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* '("Makefile")
                (("/usr/local") ""))))
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (zero? (apply system* "make" "library" make-flags))))
         )))
    (inputs '())
    (native-inputs `(("valgrind" ,valgrind)))
    (home-page "https://github.com/nodejs/http-parser")
    (synopsis "HTTP request/response parser for C")
    (description "HTTP parser is a parser for HTTP messages written in C.  It
parses both requests and responses.  The parser is designed to be used in
performance HTTP applications.  It does not make any syscalls nor allocations,
it does not buffer data, it can be interrupted at anytime.")
    (license expat)))

(define-public node
  (package
    (name "node")
    (version "6.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1l4p2zgld68c061njx6drxm06685hmp656ijm9i0hnyg30397355"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO: Package http_parser and add --shared-http-parser.
     '(#:configure-flags '("--shared-openssl"
                           "--shared-zlib"
                           "--shared-libuv"
                           "--shared-cares"
                           "--shared-http-parser"
                           "--without-snapshot")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-files
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Fix hardcoded /bin/sh references.
             (substitute* '("lib/child_process.js"
                            "lib/internal/v8_prof_polyfill.js"
                            "test/parallel/test-stdio-closed.js")
               (("'/bin/sh'")
                (string-append "'" (which "bash") "'")))

             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (which "env") "'")))

             ;; Having the build fail because of linter errors is insane!
             (substitute* '("Makefile")
               (("	\\$\\(MAKE\\) jslint") "")
               (("	\\$\\(MAKE\\) cpplint\n") ""))

             ;; FIXME: These tests fail in the build container, but they don't
             ;; seem to be indicative of real problems in practice.
             (for-each delete-file
                       '("test/parallel/test-dgram-membership.js"
                         "test/parallel/test-cluster-master-error.js"
                         "test/parallel/test-cluster-master-kill.js"
                         "test/parallel/test-npm-install.js"
                         "test/sequential/test-child-process-emfile.js"))
             #t))
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (flags (cons (string-append "--prefix=" prefix)
                                 configure-flags)))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               ;; Node's configure script expects the CC environment variable to
               ;; be set.
               (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
               (zero? (apply system*
                             (string-append (assoc-ref inputs "python")
                                            "/bin/python")
                             "configure" flags)))))
         (add-after 'patch-shebangs 'patch-npm-shebang
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bindir (string-append (assoc-ref outputs "out")
                                           "/bin"))
                    (npm    (string-append bindir "/npm"))
                    (target (readlink npm)))
               (with-directory-excursion bindir
                 (patch-shebang target (list bindir))
                 #t)))))))
    (native-inputs
     `(("python" ,python-2)
       ("perl" ,perl)
       ("procps" ,procps)
       ("util-linux" ,util-linux)
       ("which" ,which)))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     `(("libuv" ,libuv)
       ("openssl" ,tls:openssl)
       ("zlib" ,compression:zlib)
       ("http-parser" ,http-parser)
       ("c-ares" ,c-ares)))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "http://nodejs.org/")
    (license expat)))
