;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages coffee-script)
  ;;#:use-module ((guix licenses) #:select (expat))
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  ;#:use-module (gnu packages gcc)
  ;#:use-module (gnu packages libevent)
  ;#:use-module (gnu packages linux)
  ;#:use-module (gnu packages perl)
  )

(define-public node-ancient
  (package
    (name "node-ancient")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1d972im6zfpl2isbvqqh5yi2wvkyj7cxj4yhi8805anf6v8w6lxa"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags `("--without-snapshot"
                           "--shared-cares")
       #:make-flags (list (string-append "CXXFLAGS=-I"
                                         (assoc-ref %build-inputs "linux-headers")
                                         "/include")
                          (string-append "CFLAGS=-I"
                                         (assoc-ref %build-inputs "linux-headers")
                                         "/include")
                          )
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-files
           (lambda* (#:key inputs #:allow-other-keys)
             ;; FIXME: These tests fail in the build container, but they don't
             ;; seem to be indicative of real problems in practice.
             (for-each delete-file
                       '("test/simple/test-dgram-multicast.js"
                         "test/simple/test-http-304.js"
                         "test/simple/test-c-ares.js"
                         "test/simple/test-stdout-to-file.js"
                         "test/simple/test-error-reporting.js"
                         "test/simple/test-readline.js"
                         "test/simple/test-child-process-deprecated-api.js"
                         "test/simple/test-stdin-from-file.js"
                         "test/simple/test-pipe-head.js"
                         "test/simple/test-child-process-exec-env.js"
                         "test/simple/test-child-process-exec-cwd.js"
                         "test/simple/test-fs-realpath.js"
                         "test/simple/test-child-process-cwd.js"
                         "test/simple/test-exec.js"
                         "test/simple/test-child-process-custom-fds.js"
                         "test/simple/test-child-process-env.js"
                         ;; The following test depends on an apache http server
                         "test/simple/test-http-full-response.js"))))
          ;; https://github.com/nodejs/node-v0.x-archive/issues/3286
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (flags
                     (cons (string-append "--prefix=" prefix)
                           configure-flags)))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
               (zero? (apply system*
                             "./configure" flags))))))))
    (native-inputs
     `(("coreutils" ,coreutils) ;; for running dd with make test
       ("curl" ,curl) ;; for running dd with make test
       ("python" ,python-2)
       ("linux-headers" ,linux-libre-headers)
       ("util-linux" ,util-linux)
       ("pkg-config" ,pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     `(
       ("openssl" ,openssl)
       ("c-ares" ,c-ares)
       ))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "http://nodejs.org/")
    (license expat)))

(define-public node-legacy
  (package
    (name "node-legacy")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1fbq56w40h71l304bq8ggf5z80g0bsldbqciy3gm8dild5pphzmc"
                ))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags `("--without-snapshot"
                           "--shared-cares")
       #:make-flags (list (string-append "CXXFLAGS=-I"
                                         (assoc-ref %build-inputs "linux-headers")
                                         "/include"))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-files
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("test/simple/test-child-process-deprecated-api.js"
                            "test/simple/test-child-process-exec-env.js"
                            "test/simple/test-child-process-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (which "env") "'")))

             (for-each delete-file
                       '("test/simple/test-net-connect-timeout.js"
                         "test/simple/test-http-dns-fail.js"
                         "test/simple/test-regress-GH-819.js"
                         "test/simple/test-child-process-exec-cwd.js"
                         "test/simple/test-dgram-multicast.js"
                         "test/simple/test-c-ares.js"
                         "test/simple/test-process-env.js"))))
         ;; https://github.com/nodejs/node-v0.x-archive/issues/3286
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (flags
                     (cons (string-append "--prefix=" prefix)
                           configure-flags)))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
               (zero? (apply system*
                             "./configure" flags))))))
       ))
    (native-inputs
     `(
       ("coreutils" ,coreutils) ;; for running dd with make test
       ("curl" ,curl) ;; for running dd with make test
       ("python" ,python-2)
       ("linux-headers" ,linux-libre-headers)
       ("util-linux" ,util-linux)
       ("pkg-config" ,pkg-config)
       ;;("perl" ,perl)
       ;;("procps" ,procps)
       ;;("util-linux" ,util-linux)
       ;;("which" ,which)
       ))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     `(
       ("openssl" ,openssl)
       ("c-ares" ,c-ares)
       ))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "http://nodejs.org/")
    (license expat)))


(define-public coffee-script-legacy
  (package
    (name "coffee-script-legacy")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/jashkenas/coffeescript/archive/0.3.2.tar.gz")
       (sha256
        (base32
         "0109bb3s3wcwpxb1ar2krsvx9l1v4acl4f5jamhwf49qmz8yr6b1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-gemspec-date
           (lambda _
             (substitute* "coffee-script.gemspec"
               (("2010-2-8") "2010-02-08")))))
       ;; Werkt nog op fa63288f524353bcb037252d57612e240cb5489c

       ))
    (native-inputs
     `())
    (synopsis "CoffeeScript is a little language that compiles into
JavaScript.")
    (description "CoffeeScript is a little language that compiles into
JavaScript. Think of it as JavaScript's less ostentatious kid brother -- the
same genes, roughly the same height, but a different sense of style. Apart
from a handful of bonus goodies, statements in CoffeeScript correspond
one-to-one with their equivalent in JavaScript, it's just another way of
saying it. ")
    (home-page "http://jashkenas.github.com/coffee-script/")
    (license expat)))

(define-public coffee-script-js
  (package
    (name "coffee-script-js")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/jashkenas/coffeescript/archive/1.0.0.tar.gz")
       (sha256
        (base32
         "1ssg6lgyr36q7v0p25xpj7wgvz8qxq1cvnlvn5p3r1q8mj93048f"))))
    (build-system node-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'delete-minified-files 'delete-generated-files
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file)
                         (format #t "deleting generated JavaScript file '~a'~%" file)
                         (delete-file file))
                       (find-files "." "(\\.js)$"))
             #t
             ))
         (replace 'build
           (lambda* (#:key outputs intputs #:allow-other-keys)
             #t
             ;; compile all files:

             ;; First 0.5

             ;; We need jison 0.1
             ;; pseudo code:
             ;;  extend global, require('util')
             ;;  require 'jison'
             ;;  parser = require('./lib/grammar').parser
             ;;  fs.writeFile 'lib/parser.js', parser.generate()



             )))))
    (native-inputs
     `())
    (synopsis "CoffeeScript is a little language that compiles into
JavaScript.")
    (description "CoffeeScript is a little language that compiles into
JavaScript.")
    (home-page "http://jashkenas.github.com/coffee-script/")
    (license expat)))

(define-public node-jison@0.3.10
  (package
    (name "node-jison")
    (version "0.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/zaach/jison/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "17w04zqfj1333qiplq084jcvnzfs06bwknmbwa7yiy0cpmn51677"))))
    (build-system node-build-system)
    (arguments
     `(#:global? #t))
    (propagated-inputs
     `(("node-nomnom" ,node-nomnom)))
    (native-inputs
     `(("node-test" ,node-test)))
    (synopsis "API for creating parsers in JavaScript.")
    (description "Jison generates bottom-up parsers in JavaScript.  Its API is
similar to Bison's, hence the name.  It supports many of Bison's major
features, plus some of its own.")
    (home-page
     "https://github.com/zaach/jison")
    (license expat)))

;; imported
(define-public node-ansi-font
  (package
    (name "node-ansi-font")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/Gozala/ansi-font/archive/v0.0.2/ansi-font-v0.0.2.tar.gz")
       (sha256
        (base32
         "1sp8kf4wps9spwspi64x7fsir6lkgb4r6sl811y4v8wnkz3hdf31"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "ANSI font styling utils")
    (description "ANSI font styling utils")
    (home-page "https://github.com/Gozala/ansi-font")
    (license expat)))
(define-public node-has-flag
  (package
    (name "node-has-flag")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/sindresorhus/has-flag/archive/v2.0.0/has-flag-v2.0.0.tar.gz")
       (sha256
        (base32
         "1wsgw71fmsb09vr0mxx44l52qjqp1vdxg4dwv8j72wbaffc79xw1"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Check if argv has a specific flag")
    (description "Check if argv has a specific flag")
    (home-page
     "https://github.com/sindresorhus/has-flag#readme")
    (license expat)))
(define-public node-supports-color
  (package
    (name "node-supports-color")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/chalk/supports-color/archive/v3.1.2/supports-color-v3.1.2.tar.gz")
       (sha256
        (base32
         "1xj4z60f8n0wxn4zgjhms867n5s9c9r5ia9qv0faa2v42slf1479"))))
    (build-system node-build-system)
    (propagated-inputs
     `(("node-has-flag" ,node-has-flag)))
    (native-inputs `())
    (synopsis
     "Detect whether a terminal supports color")
    (description
     "Detect whether a terminal supports color")
    (home-page
     "https://github.com/chalk/supports-color")
    (license expat)))
(define-public node-has-ansi
  (package
    (name "node-has-ansi")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/sindresorhus/has-ansi/archive/2.0.0/has-ansi-2.0.0.tar.gz")
       (sha256
        (base32
         "1jal36jp1k41ryzpp4hz2271xnlaw8sqz5ybarlbd4r6w04mrz2m"))))
    (build-system node-build-system)
    (propagated-inputs
     `(("node-ansi-regex" ,node-ansi-regex)))
    (native-inputs `())
    (synopsis
     "Check if a string has ANSI escape codes")
    (description
     "Check if a string has ANSI escape codes")
    (home-page
     "https://github.com/sindresorhus/has-ansi")
    (license expat)))
(define-public node-escape-string-regexp
  (package
    (name "node-escape-string-regexp")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/sindresorhus/escape-string-regexp/archive/v1.0.5/escape-string-regexp-v1.0.5.tar.gz")
       (sha256
        (base32
         "0ygf86hs0lrhd6hmnbgz720p6nh4b6prhcmrdy7mdx6ad05bxj6d"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Escape RegExp special characters")
    (description "Escape RegExp special characters")
    (home-page
     "https://github.com/sindresorhus/escape-string-regexp")
    (license expat)))
(define-public node-ansi-styles
  (package
    (name "node-ansi-styles")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/chalk/ansi-styles/archive/v2.2.1/ansi-styles-v2.2.1.tar.gz")
       (sha256
        (base32
         "1xz3wmrhcbfxjjygb66qjqll257crgrpqx53ynipiawvzkw07rsa"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
     "ANSI escape codes for styling strings in the terminal")
    (description
     "ANSI escape codes for styling strings in the terminal")
    (home-page
     "https://github.com/chalk/ansi-styles#readme")
    (license expat)))
(define-public node-ansi-regex
  (package
    (name "node-ansi-regex")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/sindresorhus/ansi-regex/archive/2.0.0/ansi-regex-2.0.0.tar.gz")
       (sha256
        (base32
         "17bq99qp4gic157pdnnhl5hxrc4qmlgk38yq0pycylc7cxas40cx"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
     "Regular expression for matching ANSI escape codes")
    (description
     "Regular expression for matching ANSI escape codes")
    (home-page
     "https://github.com/sindresorhus/ansi-regex")
    (license expat)))

(define-public node-strip-ansi
  (package
    (name "node-strip-ansi")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/chalk/strip-ansi/archive/v3.0.1/strip-ansi-v3.0.1.tar.gz")
       (sha256
        (base32
         "03kvhmnk7dyi2z1app80pkr6501gq3w64fd837nh2hlg0n4qadsa"))))
    (build-system node-build-system)
    (propagated-inputs
     `(("node-ansi-regex" ,node-ansi-regex)))
    (native-inputs `())
    (synopsis "Strip ANSI escape codes")
    (description "Strip ANSI escape codes")
    (home-page "https://github.com/chalk/strip-ansi")
    (license expat)))

(define-public node-chalk
  (package
    (name "node-chalk")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/chalk/chalk/archive/v1.1.3/chalk-v1.1.3.tar.gz")
       (sha256
        (base32
         "1rkawhaywsswig9x432j7zk16qrall5avpqkzr1njzia4c5wbrad"))))
    (build-system node-build-system)
    (arguments
     `(#:tests? #f)) ;;XXX: Need major test framework bootstrapped
    (propagated-inputs
     `(("node-strip-ansi" ,node-strip-ansi)
       ("node-ansi-styles" ,node-ansi-styles)
       ("node-escape-string-regexp"
        ,node-escape-string-regexp)
       ("node-has-ansi" ,node-has-ansi)
       ("node-supports-color" ,node-supports-color)))
    (native-inputs `())
    (synopsis
     "Terminal string styling done right. Much color.")
    (description
     "Terminal string styling done right. Much color.")
    (home-page
     "https://github.com/chalk/chalk#readme")
    (license expat)))


(define-public node-test
  (package
    (name "node-test")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/Gozala/test-commonjs/archive/v0.6.0/test-commonjs-v0.6.0.tar.gz")
       (sha256
        (base32
         "166ja3j1hk760xj4y6ghlakq6gnm1n2j3jd26x04xs82x8pbm3qy"))))
    (build-system node-build-system)
    (propagated-inputs
     `(("node-ansi-font" ,node-ansi-font)))
    (native-inputs `())
    (synopsis "(Un)CommonJS test runner.")
    (description "(Un)CommonJS test runner.")
    (home-page
     "https://github.com/Gozala/test-commonjs/")
    (license expat)))

(define-public node-underscore
  (package
    (name "node-underscore")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/jashkenas/underscore/archive/1.8.3/underscore-1.8.3.tar.gz")
       (sha256
        (base32
         "0h67p9p9l0y6w7qbwd6z40pw18zlhjb6whpz16pabwa204lvs05v"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
     "JavaScript's functional programming helper library.")
    (description
     "JavaScript's functional programming helper library.")
    (home-page "http://underscorejs.org")
    (license expat)))

(define-public node-nomnom
  (let ((commit "6110ac36f9d94b7b7a0799e70ace729872fb26eb"))
    (package
      (name "node-nomnom")
      (version (string-append "1.8.1." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/harthur/nomnom.git")
               (commit commit)))
         (sha256
          (base32
           "0fn374i4n3xhcxjn2hxmiq071ranvvrxiyw5n46aj7b5i512ybzd"))))
      (build-system node-build-system)
      (arguments
       `(#:tests? #f)) ;;XXX: can't package nodeunit yet
      (propagated-inputs
       `(("node-chalk" ,node-chalk)
         ("node-underscore" ,node-underscore)))
      (native-inputs `())
      (synopsis "Option parser with generated usage and commands")
      (description "Option parser with generated usage and commands")
      (home-page
       "https://github.com/harthur/nomnom")
      (license expat))))

(define-public node-jsonselect
  (let ((commit "91ca3a0e7a570dbf17374e9b7b450cfbedd1734b"))
    (package
      (name "node-jsonselect")
      (version (string-append "0.4.0." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lloyd/JSONSelect.git")
               (commit commit)))
         (sha256
          (base32
           "092a9bn708cd0lxiz65dwc9hgwnipfm3lilx7s5zsnw5zygwz9rb"))))
      (build-system node-build-system)
      (propagated-inputs
       `())
      (native-inputs `())
      (synopsis "CSS-like selectors for JSON")
      (description "CSS-like selectors for JSON")
      (home-page
       "https://github.com/lloyd/JSONSelect")
      (license expat))))
