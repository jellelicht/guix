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
  ;;#:use-module ((guix licenses) #:select (expat))
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system node)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages compression) #:prefix compression:)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages tls) #:prefix tls:))

(define-public node
  (package
    (name "node")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1xh883fbhyhgna1vi8xmd6klg4r186lb1h1xr08hn89wy7f48q9z"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO: Package http_parser and add --shared-http-parser.
     '(#:configure-flags '("--shared-openssl"
                           "--shared-zlib"
                           "--shared-libuv"
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
                       '("test/parallel/test-https-connect-address-family.js"
                         "test/parallel/test-tls-connect-address-family.js"
                         "test/parallel/test-dgram-membership.js"
                         "test/parallel/test-cluster-master-error.js"
                         "test/parallel/test-cluster-master-kill.js"
                         "test/parallel/test-npm-install.js"
                         "test/parallel/test-stdout-close-unref.js"
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
       ("zlib" ,compression:zlib)))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "http://nodejs.org/")
    (license expat)))

(define-public node-minimist
  (package
    (name "node-minimist")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/substack/minimist/archive/1.2.0/minimist-1.2.0.tar.gz")
       (sha256
        (base32
         "138s1vnc86w5wr453ri9dzy89nswzyp5sylm7d9b9f53d1bcqhwb"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (synopsis "parse argument options")
    (description "parse argument options")
    (home-page
     "https://github.com/substack/minimist")
    (license #f)))

;;; XXX: Imported code page

;; (define-public node-aws4
;;   (package
;;     (name "node-aws4")
;;     (version "1.4.1")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri "https://github.com/mhart/aws4/archive/v1.4.1/aws4-v1.4.1.tar.gz")
;;         (sha256
;;           (base32
;;             "0nxy07mg3dafvvrfirmpk79y980g7sn3qxay9p7z51ypjnaljlj5"))))
;;     (build-system node-build-system)
;;     (propagated-inputs `())
;;     (native-inputs `())
;;     (synopsis
;;       "Signs and prepares requests using AWS Signature Version 4")
;;     (description
;;       "Signs and prepares requests using AWS Signature Version 4")
;;     (home-page
;;       "https://github.com/mhart/aws4#readme")
;;     (license expat)))
;; (define-public node-aws-sign2
;;   (package
;;     (name "node-aws-sign2")
;;     (version "0.6.0")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri "https://github.com/mikeal/aws-sign/archive/v0.6.0/aws-sign-v0.6.0.tar.gz")
;;         (sha256
;;           (base32
;;             "0680mvafg7y3vmnqnl8b4xfqcp8flmxfciawzg82fw1hpc7bqhqc"))))
;;     (build-system node-build-system)
;;     (propagated-inputs `())
;;     (native-inputs `())
;;     (synopsis
;;       "AWS signing. Originally pulled from LearnBoost/knox, maintained as vendor in request, now a standalone module.")
;;     (description
;;       "AWS signing. Originally pulled from LearnBoost/knox, maintained as vendor in request, now a standalone module.")
;;     (home-page


;; (define-public node-coveralls
;;   ;;XXX: DUMMY VALUE
;;   #t)
;; (define-public node-tap #t)
;; (define-public node-tape #t)
;; (define-public node-zuul #t)
;; (define-public node-mocha #t)
;; (define-public node-chai #t)
;; (define-public node-eslint #t)
;; (define-public node-handlebars #t)
;; (define-public node-esprima #t)
;; (define-public node-requirejs #t)
;; (define-public node-resolve #t)
;; (define-public node-fileset #t)
;; (define-public node-standard-version #t)
;; (define-public node-standard #t)

;; (define-public node-gulp
;;   (package
;;     (name "node-gulp")
;;     (version "3.9.1")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/gulpjs/gulp/archive/v3.9.1/gulp-v3.9.1.tar.gz")
;;        (sha256
;;         (base32
;;          "05adn02pmg289l52xvzd759rsdii14k9dh5x69c3frd1ijycc5ln"))))
;;     (build-system node-build-system)
;;     (propagated-inputs
;;      `(("node-minimist" ,node-minimist)
;;        ("node-v8flags" ,node-v8flags)
;;        ("node-gulp-util" ,node-gulp-util)
;;        ("node-interpret" ,node-interpret)
;;        ("node-semver" ,node-semver)
;;        ("node-archy" ,node-archy)
;;        ("node-chalk" ,node-chalk)
;;        ("node-deprecated" ,node-deprecated)
;;        ("node-orchestrator" ,node-orchestrator)
;;        ("node-vinyl-fs" ,node-vinyl-fs)
;;        ("node-tildify" ,node-tildify)
;;        ("node-pretty-hrtime" ,node-pretty-hrtime)
;;        ("node-liftoff" ,node-liftoff)))
;;     (native-inputs
;;      `(("node-eslint" ,node-eslint)
;;        ("node-istanbul" ,node-istanbul)
;;        ("node-mocha-lcov-reporter"
;;         ,node-mocha-lcov-reporter)
;;        ("node-coveralls" ,node-coveralls)
;;        ("node-should" ,node-should)
;;        ("node-q" ,node-q)
;;        ("node-eslint-config-gulp"
;;         ,node-eslint-config-gulp)
;;        ("node-graceful-fs" ,node-graceful-fs)
;;        ("node-mocha" ,node-mocha)
;;        ("node-jscs-preset-gulp" ,node-jscs-preset-gulp)
;;        ("node-marked-man" ,node-marked-man)
;;        ("node-rimraf" ,node-rimraf)
;;        ("node-mkdirp" ,node-mkdirp)
;;        ("node-jscs" ,node-jscs)))
;;     (synopsis "The streaming build system")
;;     (description "The streaming build system")
;;     (home-page "http://gulpjs.com")
;;     (license expat)))

;; (define-public node-v8flags
;;   (package
;;     (name "node-v8flags")
;;     (version "2.0.11")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/tkellen/node-v8flags/archive/2.0.11/node-v8flags-2.0.11.tar.gz")
;;        (sha256
;;         (base32
;;          "0hn6w4admpmq0vkf81abh7flfcq808q26d7m0r2q52aj8q7zbr0y"))))
;;     (build-system node-build-system)
;;     (propagated-inputs
;;      `(("node-user-home" ,node-user-home)))
;;     (native-inputs
;;      `(("node-chai" ,node-chai)
;;        ("node-mocha" ,node-mocha)
;;        ("node-async" ,node-async)))
;;     (synopsis "Get available v8 flags.")
;;     (description "Get available v8 flags.")
;;     (home-page
;;      "https://github.com/tkellen/node-v8flags")
;;     (license (list expat))))

;; (define-public node-user-home
;;   (package
;;     (name "node-user-home")
;;     (version "2.0.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/sindresorhus/user-home/archive/2.0.0/user-home-2.0.0.tar.gz")
;;        (sha256
;;         (base32
;;          "0hl33jppwnsql2zbscyr2vmjgafd1l4mwkl1j41q4si996mglmwx"))))
;;     (build-system node-build-system)
;;     (propagated-inputs
;;      `(("node-os-homedir" ,node-os-homedir)))
;;     (native-inputs
;;      `(("node-ava" ,node-ava)
;;        ("node-path-exists" ,node-path-exists)))
;;     (synopsis
;;      "Get the path to the user home directory")
;;     (description
;;      "Get the path to the user home directory")
;;     (home-page
;;      "https://github.com/sindresorhus/user-home")
;;     (license expat)))

;; (define-public node-os-homedir
;;   (package
;;     (name "node-os-homedir")
;;     (version "1.0.1")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/sindresorhus/os-homedir/archive/v1.0.1/os-homedir-v1.0.1.tar.gz")
;;        (sha256
;;         (base32
;;          "0qh3p0j0cbk00vgk7d0l02cs5y2rd7d9pd5m6q7nblhprih1c5yj"))))
;;     (build-system node-build-system)
;;     (propagated-inputs `())
;;     (native-inputs
;;      `(("node-ava" ,node-ava)
;;        ("node-path-exists" ,node-path-exists)))
;;     (synopsis "io.js 2.3.0 os.homedir() ponyfill")
;;     (description "io.js 2.3.0 os.homedir() ponyfill")
;;     (home-page
;;      "https://github.com/sindresorhus/os-homedir")
;;     (license expat)))

;; (define-public node-path-exists
;;   (package
;;     (name "node-path-exists")
;;     (version "3.0.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/sindresorhus/path-exists/archive/v3.0.0/path-exists-v3.0.0.tar.gz")
;;        (sha256
;;         (base32
;;          "0hiyv9n3w4w751klwckdfia8h7am5anyqkqwm5pvpwy328hlf1wk"))))
;;     (build-system node-build-system)
;;     (propagated-inputs `())
;;     (native-inputs
;;      `(("node-xo" ,node-xo) ("node-ava" ,node-ava)))
;;     (synopsis "Check if a path exists")
;;     (description "Check if a path exists")
;;     (home-page
;;      "https://github.com/sindresorhus/path-exists#readme")
;;     (license expat)))

;; (define-public node-xo
;;   (package
;;     (name "node-xo")
;;     (version "0.16.0")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri "https://github.com/sindresorhus/xo/archive/v0.16.0/xo-v0.16.0.tar.gz")
;;         (sha256
;;           (base32
;;             "16g9yqhbqjv34f9pkz8pqh4vjnv52aqqfpdzc6942vyaadd85b92"))))
;;     (build-system node-build-system)
;;     (propagated-inputs
;;       `(("node-eslint-config-xo" ,node-eslint-config-xo)
;;         ("node-eslint" ,node-eslint)
;;         ("node-home-or-tmp" ,node-home-or-tmp)
;;         ("node-babel-eslint" ,node-babel-eslint)
;;         ("node-eslint-plugin-no-use-extend-native"
;;          ,node-eslint-plugin-no-use-extend-native)
;;         ("node-xo-init" ,node-xo-init)
;;         ("node-get-stdin" ,node-get-stdin)
;;         ("node-deep-assign" ,node-deep-assign)
;;         ("node-eslint-plugin-ava"
;;          ,node-eslint-plugin-ava)
;;         ("node-eslint-plugin-promise"
;;          ,node-eslint-plugin-promise)
;;         ("node-globby" ,node-globby)
;;         ("node-resolve-from" ,node-resolve-from)
;;         ("node-update-notifier" ,node-update-notifier)
;;         ("node-eslint-formatter-pretty"
;;          ,node-eslint-formatter-pretty)
;;         ("node-resolve-cwd" ,node-resolve-cwd)
;;         ("node-eslint-plugin-import"
;;          ,node-eslint-plugin-import)
;;         ("node-eslint-plugin-xo" ,node-eslint-plugin-xo)
;;         ("node-path-exists" ,node-path-exists)
;;         ("node-has-flag" ,node-has-flag)
;;         ("node-arrify" ,node-arrify)
;;         ("node-debug" ,node-debug)
;;         ("node-meow" ,node-meow)
;;         ("node-pkg-conf" ,node-pkg-conf)
;;         ("node-multimatch" ,node-multimatch)
;;         ("node-eslint-plugin-babel"
;;          ,node-eslint-plugin-babel)
;;         ("node-object-assign" ,node-object-assign)))
;;     (native-inputs
;;       `(("node-temp-write" ,node-temp-write)
;;         ("node-coveralls" ,node-coveralls)
;;         ("node-xo" ,node-xo)
;;         ("node-eslint-config-xo-react"
;;          ,node-eslint-config-xo-react)
;;         ("node-ava" ,node-ava)
;;         ("node-eslint-plugin-react"
;;          ,node-eslint-plugin-react)
;;         ("node-nyc" ,node-nyc)
;;         ("node-proxyquire" ,node-proxyquire)
;;         ("node-execa" ,node-execa)))
;;     (synopsis "JavaScript happiness style linter ❤️")
;;     (description
;;       "JavaScript happiness style linter ❤️")
;;     (home-page
;;       "https://github.com/sindresorhus/xo#readme")
;;     (license expat)))

;; (define-public node-ava
;;   (package
;;     (name "node-ava")
;;     (version "0.15.2")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri "https://github.com/avajs/ava/archive/v0.15.2/ava-v0.15.2.tar.gz")
;;         (sha256
;;           (base32
;;             "133ibqk7w2n55pkz5qpppdf4j310wy6q94hcqhah2zz4w24l802w"))))
;;     (build-system node-build-system)
;;     (propagated-inputs
;;       `(("node-lodash.debounce" ,node-lodash.debounce)
;;         ("node-meow" ,node-meow)
;;         ("node-debug" ,node-debug)
;;         ("node-babel-plugin-ava-throws-helper"
;;          ,node-babel-plugin-ava-throws-helper)
;;         ("node-arrify" ,node-arrify)
;;         ("node-ignore-by-default"
;;          ,node-ignore-by-default)
;;         ("node-object-assign" ,node-object-assign)
;;         ("node-require-precompiled"
;;          ,node-require-precompiled)
;;         ("node-resolve-cwd" ,node-resolve-cwd)
;;         ("node-babel-runtime" ,node-babel-runtime)
;;         ("node-strip-ansi" ,node-strip-ansi)
;;         ("node-has-flag" ,node-has-flag)
;;         ("node-matcher" ,node-matcher)
;;         ("node-array-uniq" ,node-array-uniq)
;;         ("node-currently-unhandled"
;;          ,node-currently-unhandled)
;;         ("node-power-assert-renderers"
;;          ,node-power-assert-renderers)
;;         ("node-is-generator-fn" ,node-is-generator-fn)
;;         ("node-md5-hex" ,node-md5-hex)
;;         ("node-source-map-support"
;;          ,node-source-map-support)
;;         ("node-cli-truncate" ,node-cli-truncate)
;;         ("node-babel-plugin-detective"
;;          ,node-babel-plugin-detective)
;;         ("node-pretty-ms" ,node-pretty-ms)
;;         ("node-core-assert" ,node-core-assert)
;;         ("node-figures" ,node-figures)
;;         ("node-last-line-stream" ,node-last-line-stream)
;;         ("node-ms" ,node-ms)
;;         ("node-babel-preset-es2015"
;;          ,node-babel-preset-es2015)
;;         ("node-repeating" ,node-repeating)
;;         ("node-cli-cursor" ,node-cli-cursor)
;;         ("node-is-obj" ,node-is-obj)
;;         ("node-package-hash" ,node-package-hash)
;;         ("node-babel-core" ,node-babel-core)
;;         ("node-set-immediate-shim"
;;          ,node-set-immediate-shim)
;;         ("node-loud-rejection" ,node-loud-rejection)
;;         ("node-find-cache-dir" ,node-find-cache-dir)
;;         ("node-arr-flatten" ,node-arr-flatten)
;;         ("node-babel-plugin-espower"
;;          ,node-babel-plugin-espower)
;;         ("node-stack-utils" ,node-stack-utils)
;;         ("node-co-with-promise" ,node-co-with-promise)
;;         ("node-plur" ,node-plur)
;;         ("node-not-so-shallow" ,node-not-so-shallow)
;;         ("node-empower-core" ,node-empower-core)
;;         ("node-array-union" ,node-array-union)
;;         ("node-slash" ,node-slash)
;;         ("node-convert-source-map"
;;          ,node-convert-source-map)
;;         ("node-observable-to-promise"
;;          ,node-observable-to-promise)
;;         ("node-fn-name" ,node-fn-name)
;;         ("node-babel-plugin-transform-runtime"
;;          ,node-babel-plugin-transform-runtime)
;;         ("node-chokidar" ,node-chokidar)
;;         ("node-clean-yaml-object"
;;          ,node-clean-yaml-object)
;;         ("node-update-notifier" ,node-update-notifier)
;;         ("node-is-observable" ,node-is-observable)
;;         ("node-common-path-prefix"
;;          ,node-common-path-prefix)
;;         ("node-babel-preset-stage-2"
;;          ,node-babel-preset-stage-2)
;;         ("node-unique-temp-dir" ,node-unique-temp-dir)
;;         ("node-power-assert-formatter"
;;          ,node-power-assert-formatter)
;;         ("node-max-timeout" ,node-max-timeout)
;;         ("node-chalk" ,node-chalk)
;;         ("node-cli-spinners" ,node-cli-spinners)
;;         ("node-ava-init" ,node-ava-init)
;;         ("node-arr-diff" ,node-arr-diff)
;;         ("node-option-chain" ,node-option-chain)
;;         ("node-caching-transform"
;;          ,node-caching-transform)
;;         ("node-is-promise" ,node-is-promise)
;;         ("node-globby" ,node-globby)
;;         ("node-is-ci" ,node-is-ci)
;;         ("node-bluebird" ,node-bluebird)
;;         ("node-multimatch" ,node-multimatch)
;;         ("node-time-require" ,node-time-require)
;;         ("node-babel-code-frame" ,node-babel-code-frame)
;;         ("node-pkg-conf" ,node-pkg-conf)
;;         ("node-strip-bom" ,node-strip-bom)))
;;     (native-inputs
;;       `(("node-pify" ,node-pify)
;;         ("node-cli-table2" ,node-cli-table2)
;;         ("node-coveralls" ,node-coveralls)
;;         ("node-xo" ,node-xo)
;;         ("node-get-stream" ,node-get-stream)
;;         ("node-git-branch" ,node-git-branch)
;;         ("node-zen-observable" ,node-zen-observable)
;;         ("node-delay" ,node-delay)
;;         ("node-source-map-fixtures"
;;          ,node-source-map-fixtures)
;;         ("node-signal-exit" ,node-signal-exit)
;;         ("node-lolex" ,node-lolex)
;;         ("node-nyc" ,node-nyc)
;;         ("node-touch" ,node-touch)
;;         ("node-has-ansi" ,node-has-ansi)
;;         ("node-inquirer" ,node-inquirer)
;;         ("node-proxyquire" ,node-proxyquire)
;;         ("node-rimraf" ,node-rimraf)
;;         ("node-execa" ,node-execa)
;;         ("node-sinon" ,node-sinon)
;;         ("node-mkdirp" ,node-mkdirp)
;;         ("node-tap" ,node-tap)))
;;     (synopsis "Futuristic test runner 🚀")
;;     (description "Futuristic test runner 🚀")
;;     (home-page "https://ava.li")
;;     (license expat)))


(define-public node-escape-html
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/component/escape-html/archive/v1.0.3/escape-html-v1.0.3.tar.gz")
        (sha256
          (base32
            "1dv59cz0r5aajf3n2j7ysv5cq9zq2424n1w6k1544zbvr25dda2n"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Escape string for use in HTML")
    (description "Escape string for use in HTML")
    (home-page
      "https://github.com/component/escape-html")
    (license expat)))
(define-public node-send
  (package
    (name "node-send")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/send/archive/0.14.1/send-0.14.1.tar.gz")
        (sha256
          (base32
            "01by9fq2g1cg9p0w42yg2ijb99m6nv5pwmg6wrp0152xbkv67qfs"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-depd" ,node-depd)
        ("node-statuses" ,node-statuses)
        ("node-ms" ,node-ms)
        ("node-mime" ,node-mime)
        ("node-destroy" ,node-destroy)
        ("node-encodeurl" ,node-encodeurl)
        ("node-range-parser" ,node-range-parser)
        ("node-http-errors" ,node-http-errors)
        ("node-etag" ,node-etag)
        ("node-fresh" ,node-fresh)
        ("node-debug" ,node-debug)
        ("node-on-finished" ,node-on-finished)
        ("node-escape-html" ,node-escape-html)))
    (native-inputs `())
    (synopsis
      "Better streaming static file server with Range and conditional-GET support")
    (description
      "Better streaming static file server with Range and conditional-GET support")
    (home-page
      "https://github.com/pillarjs/send#readme")
    (license expat)))
(define-public node-content-type
  (package
    (name "node-content-type")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/content-type/archive/v1.0.2/content-type-v1.0.2.tar.gz")
        (sha256
          (base32
            "0spbbv1df40dd0cs57c4l8hbd6isqjs8nl4jx2k8l0yjf9g27mpi"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Create and parse HTTP Content-Type header")
    (description
      "Create and parse HTTP Content-Type header")
    (home-page
      "https://github.com/jshttp/content-type#readme")
    (license expat)))
(define-public node-utils-merge
  (package
    (name "node-utils-merge")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jaredhanson/utils-merge/archive/v1.0.0/utils-merge-v1.0.0.tar.gz")
        (sha256
          (base32
            "02i4wrm9rbrrf4i5hzgpcnvjxxslk9rqm7p1azv3x5k3v473wc0m"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "merge() utility function")
    (description "merge() utility function")
    (home-page #f)
    (license (list expat))))
(define-public node-on-finished
  (package
    (name "node-on-finished")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/on-finished/archive/v2.3.0/on-finished-v2.3.0.tar.gz")
        (sha256
          (base32
            "13qdg0n455rbchx1qzpsd5gkfcldilv8jaqgrmqf657qnqjfrlbs"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-ee-first" ,node-ee-first)))
    (native-inputs `())
    (synopsis
      "Execute a callback when a request closes, finishes, or errors")
    (description
      "Execute a callback when a request closes, finishes, or errors")
    (home-page
      "https://github.com/jshttp/on-finished")
    (license expat)))
(define-public node-merge-descriptors
  (package
    (name "node-merge-descriptors")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/component/merge-descriptors/archive/1.0.1/merge-descriptors-1.0.1.tar.gz")
        (sha256
          (base32
            "1776k6yqmdhppgsbfzcwq4ps61fnwgpa4dkd37br4q3jx0h5qdzz"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Merge objects using descriptors")
    (description "Merge objects using descriptors")
    (home-page
      "https://github.com/component/merge-descriptors")
    (license expat)))
(define-public node-debug
  (package
    (name "node-debug")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/visionmedia/debug/archive/2.2.0/debug-2.2.0.tar.gz")
        (sha256
          (base32
            "0b3gklgd4jsk26yi3bkj3jkrsig00br77a9ldyah930lmwsmldva"))))
    (build-system node-build-system)
    (propagated-inputs `(("node-ms" ,node-ms)))
    (native-inputs `())
    (synopsis "small debugging utility")
    (description "small debugging utility")
    (home-page
      "https://github.com/visionmedia/debug")
    (license expat)))
(define-public node-parseurl
  (package
    (name "node-parseurl")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/parseurl/archive/1.3.1/parseurl-1.3.1.tar.gz")
        (sha256
          (base32
            "1b8xxz6528s6g99v3ga0hns8aa74yajs59v0i121q31c6qz4src2"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "parse a url with memoization")
    (description "parse a url with memoization")
    (home-page
      "https://github.com/pillarjs/parseurl")
    (license expat)))
(define-public node-methods
  (package
    (name "node-methods")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/methods/archive/v1.1.2/methods-v1.1.2.tar.gz")
        (sha256
          (base32
            "1glb0kn1li1x8qvs33fylk8i7md69wxi2rfsd7vv23nznmbydx3j"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "HTTP methods that node supports")
    (description "HTTP methods that node supports")
    (home-page "https://github.com/jshttp/methods")
    (license expat)))
(define-public node-fresh
  (package
    (name "node-fresh")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/fresh/archive/v0.3.0/fresh-v0.3.0.tar.gz")
        (sha256
          (base32
            "1asz018i6zdw9vx6fix3hdd4a33a89r1aakprkb1r963ni236yd3"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "HTTP response freshness testing")
    (description "HTTP response freshness testing")
    (home-page "https://github.com/jshttp/fresh")
    (license expat)))
(define-public node-unpipe
  (package
    (name "node-unpipe")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/stream-utils/unpipe/archive/v1.0.0/unpipe-v1.0.0.tar.gz")
        (sha256
          (base32
            "0fnaizpmxhiwk862i1n44wyavc40i26ggxx370rczdhj42dc1dz9"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Unpipe a stream from all destinations")
    (description
      "Unpipe a stream from all destinations")
    (home-page
      "https://github.com/stream-utils/unpipe")
    (license expat)))
(define-public node-finalhandler
  (package
    (name "node-finalhandler")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/finalhandler/archive/v0.5.0/finalhandler-v0.5.0.tar.gz")
        (sha256
          (base32
            "13nsjirfsxa0id096w0vnvpmkllygl87yszd45icskvcsjyzfhrn"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-statuses" ,node-statuses)
        ("node-debug" ,node-debug)
        ("node-on-finished" ,node-on-finished)
        ("node-unpipe" ,node-unpipe)
        ("node-escape-html" ,node-escape-html)))
    (native-inputs `())
    (synopsis "Node.js final http responder")
    (description "Node.js final http responder")
    (home-page
      "https://github.com/pillarjs/finalhandler")
    (license expat)))
(define-public node-etag
  (package
    (name "node-etag")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/etag/archive/v1.7.0/etag-v1.7.0.tar.gz")
        (sha256
          (base32
            "1fgw7dral17x57jk4jh1gi0x0br8r4nizj1q6by51426dsjkf397"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Create simple ETags")
    (description "Create simple ETags")
    (home-page "https://github.com/jshttp/etag")
    (license expat)))
(define-public node-media-typer
  (package
    (name "node-media-typer")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/media-typer/archive/v0.3.0/media-typer-v0.3.0.tar.gz")
        (sha256
          (base32
            "172axx0jdrqz1iblhkzvgpxcvrqqc297i1b0i7qn45sgqlr5v37r"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Simple RFC 6838 media type parser and formatter")
    (description
      "Simple RFC 6838 media type parser and formatter")
    (home-page
      "https://github.com/jshttp/media-typer")
    (license expat)))
(define-public node-type-is
  (package
    (name "node-type-is")
    (version "1.6.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/type-is/archive/1.6.13/type-is-1.6.13.tar.gz")
        (sha256
          (base32
            "0spb44ga0sym5w74gxxxh2i9znrd20gni3rg49f6n1w79rra1ygg"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-media-typer" ,node-media-typer)
        ("node-mime-types" ,node-mime-types)))
    (native-inputs `())
    (synopsis "Infer the content-type of a request.")
    (description
      "Infer the content-type of a request.")
    (home-page
      "https://github.com/jshttp/type-is#readme")
    (license expat)))
(define-public node-qs
  (package
    (name "node-qs")
    (version "6.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/ljharb/qs/archive/v6.2.0/qs-v6.2.0.tar.gz")
        (sha256
          (base32
            "1hqwidzl23vskmp1lrghddnvl6jzqf3m4asby70g4vms77sh2drh"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
      "A querystring parser that supports nesting and arrays, with a depth limit")
    (home-page "https://github.com/ljharb/qs")
    (license bsd-3)))
(define-public node-isarray
  (package
    (name "node-isarray")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/juliangruber/isarray/archive/v1.0.0/isarray-v1.0.0.tar.gz")
        (sha256
          (base32
            "12yg0v6hb0987445xw2hsw2g97wb1spfdrlqw2pv1rk2hgz3z78r"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Array#isArray for older browsers")
    (description "Array#isArray for older browsers")
    (home-page
      "https://github.com/juliangruber/isarray")
    (license expat)))
(define-public node-path-to-regexp
  (package
    (name "node-path-to-regexp")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/path-to-regexp/archive/v1.5.3/path-to-regexp-v1.5.3.tar.gz")
        (sha256
          (base32
            "1b1zfrvr3zm55iy45wnbr7y0l8p2knjas7wsin6r7p8js2pncjlz"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-isarray" ,node-isarray)))
    (native-inputs `())
    (synopsis "Express style path to RegExp utility")
    (description
      "Express style path to RegExp utility")
    (home-page
      "https://github.com/pillarjs/path-to-regexp#readme")
    (license expat)))
(define-public node-range-parser
  (package
    (name "node-range-parser")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/range-parser/archive/v1.2.0/range-parser-v1.2.0.tar.gz")
        (sha256
          (base32
            "1h306svf0i8v0f5c5ylp5rd4q50c87frgl7j0n39ivbx1rl0lvba"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Range header field string parser")
    (description "Range header field string parser")
    (home-page
      "https://github.com/jshttp/range-parser")
    (license expat)))
(define-public node-content-disposition
  (package
    (name "node-content-disposition")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/content-disposition/archive/v0.5.1/content-disposition-v0.5.1.tar.gz")
        (sha256
          (base32
            "0jwgp77n64jk9mw2724dimky3xw04p6vsc3vpq74y99y7qv74d0x"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Create and parse Content-Disposition header")
    (description
      "Create and parse Content-Disposition header")
    (home-page
      "https://github.com/jshttp/content-disposition")
    (license expat)))
(define-public node-encodeurl
  (package
    (name "node-encodeurl")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/encodeurl/archive/v1.0.1/encodeurl-v1.0.1.tar.gz")
        (sha256
          (base32
            "1jf3336hczi1vbi233qz15q4jsd3z7ly6hkw32zgnmir2ik5acl5"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (description
      "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (home-page
      "https://github.com/pillarjs/encodeurl#readme")
    (license expat)))
(define-public node-vary
  (package
    (name "node-vary")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/vary/archive/v1.1.0/vary-v1.1.0.tar.gz")
        (sha256
          (base32
            "099xis1bjpphwwsm963lk8sibsb3ayncs5md5q8kp2rfkg8vw2x0"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Manipulate the HTTP Vary header")
    (description "Manipulate the HTTP Vary header")
    (home-page "https://github.com/jshttp/vary")
    (license expat)))
(define-public node-cookie
  (package
    (name "node-cookie")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/cookie/archive/v0.3.1/cookie-v0.3.1.tar.gz")
        (sha256
          (base32
            "1g34qgcg7lvai6xi2ikhbqghb890qih7cls4nfzx0534hzzn0npb"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "HTTP server cookie parsing and serialization")
    (description
      "HTTP server cookie parsing and serialization")
    (home-page "https://github.com/jshttp/cookie")
    (license expat)))
(define-public node-escape-html
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/component/escape-html/archive/v1.0.3/escape-html-v1.0.3.tar.gz")
        (sha256
          (base32
            "1dv59cz0r5aajf3n2j7ysv5cq9zq2424n1w6k1544zbvr25dda2n"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Escape string for use in HTML")
    (description "Escape string for use in HTML")
    (home-page
      "https://github.com/component/escape-html")
    (license expat)))
(define-public node-escape-html
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/component/escape-html/archive/v1.0.3/escape-html-v1.0.3.tar.gz")
        (sha256
          (base32
            "1dv59cz0r5aajf3n2j7ysv5cq9zq2424n1w6k1544zbvr25dda2n"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Escape string for use in HTML")
    (description "Escape string for use in HTML")
    (home-page
      "https://github.com/component/escape-html")
    (license expat)))
(define-public node-ee-first
  (package
    (name "node-ee-first")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jonathanong/ee-first/archive/1.1.1/ee-first-1.1.1.tar.gz")
        (sha256
          (base32
            "1q4vjjjjq52iml17gw5w5dfjdz7x9ggi7j2md72wr2ksw9dmigp8"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "return the first event in a set of ee/event pairs")
    (description
      "return the first event in a set of ee/event pairs")
    (home-page
      "https://github.com/jonathanong/ee-first")
    (license expat)))
(define-public node-on-finished
  (package
    (name "node-on-finished")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/on-finished/archive/v2.3.0/on-finished-v2.3.0.tar.gz")
        (sha256
          (base32
            "13qdg0n455rbchx1qzpsd5gkfcldilv8jaqgrmqf657qnqjfrlbs"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-ee-first" ,node-ee-first)))
    (native-inputs `())
    (synopsis
      "Execute a callback when a request closes, finishes, or errors")
    (description
      "Execute a callback when a request closes, finishes, or errors")
    (home-page
      "https://github.com/jshttp/on-finished")
    (license expat)))
(define-public node-debug
  (package
    (name "node-debug")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/visionmedia/debug/archive/2.2.0/debug-2.2.0.tar.gz")
        (sha256
          (base32
            "0b3gklgd4jsk26yi3bkj3jkrsig00br77a9ldyah930lmwsmldva"))))
    (build-system node-build-system)
    (propagated-inputs `(("node-ms" ,node-ms)))
    (native-inputs `())
    (synopsis "small debugging utility")
    (description "small debugging utility")
    (home-page
      "https://github.com/visionmedia/debug")
    (license expat)))
(define-public node-fresh
  (package
    (name "node-fresh")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/fresh/archive/v0.3.0/fresh-v0.3.0.tar.gz")
        (sha256
          (base32
            "1asz018i6zdw9vx6fix3hdd4a33a89r1aakprkb1r963ni236yd3"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "HTTP response freshness testing")
    (description "HTTP response freshness testing")
    (home-page "https://github.com/jshttp/fresh")
    (license expat)))
(define-public node-etag
  (package
    (name "node-etag")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/etag/archive/v1.7.0/etag-v1.7.0.tar.gz")
        (sha256
          (base32
            "1fgw7dral17x57jk4jh1gi0x0br8r4nizj1q6by51426dsjkf397"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Create simple ETags")
    (description "Create simple ETags")
    (home-page "https://github.com/jshttp/etag")
    (license expat)))
#f
(define-public node-inherits
  (package
    (name "node-inherits")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/isaacs/inherits/archive/v2.0.1/inherits-v2.0.1.tar.gz")
        (sha256
          (base32
            "0515p79q7ps7jsldgxfw43wf5197fyaigjs4q0p06ghvmdnxiiy3"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (description
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (home-page #f)
    (license isc)))
(define-public node-http-errors
  (package
    (name "node-http-errors")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/http-errors/archive/1.5.0/http-errors-1.5.0.tar.gz")
        (sha256
          (base32
            "14cjlv9rky70k79fbkgpdj0qc0d0092asbfwf4qnk926r2bwlpk6"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-inherits" ,node-inherits)
        ("node-statuses" ,node-statuses)
        ("node-setprototypeof" ,node-setprototypeof)))
    (native-inputs `())
    (synopsis "Create HTTP error objects")
    (description "Create HTTP error objects")
    (home-page
      "https://github.com/jshttp/http-errors#readme")
    (license expat)))
(define-public node-range-parser
  (package
    (name "node-range-parser")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/range-parser/archive/v1.2.0/range-parser-v1.2.0.tar.gz")
        (sha256
          (base32
            "1h306svf0i8v0f5c5ylp5rd4q50c87frgl7j0n39ivbx1rl0lvba"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Range header field string parser")
    (description "Range header field string parser")
    (home-page
      "https://github.com/jshttp/range-parser")
    (license expat)))
(define-public node-destroy
  (package
    (name "node-destroy")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/stream-utils/destroy/archive/1.0.4/destroy-1.0.4.tar.gz")
        (sha256
          (base32
            "1wgll1nbqrbvm72vsg0yrc7ib059wwdhfbf4903pfc0srp36d0cp"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "destroy a stream if possible")
    (description "destroy a stream if possible")
    (home-page
      "https://github.com/stream-utils/destroy")
    (license expat)))
(define-public node-mime
  (package
    (name "node-mime")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/broofa/node-mime/archive/v1.3.4/node-mime-v1.3.4.tar.gz")
        (sha256
          (base32
            "07a69dq55kxpwlhn8hx80xcdq5i3r3l1fhmrwrgxzy9cj2kjcpas"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "A comprehensive library for mime-type mapping")
    (description
      "A comprehensive library for mime-type mapping")
    (home-page "https://github.com/broofa/node-mime")
    (license (list expat))))
(define-public node-ms
  (package
    (name "node-ms")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/guille/ms.js/archive/0.7.1/ms.js-0.7.1.tar.gz")
        (sha256
          (base32
            "1lzy1x6h38q2ccl33p87nayw4c40bikzhsrx4khxn7356j40lsnd"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Tiny ms conversion utility")
    (description "Tiny ms conversion utility")
    (home-page "https://github.com/guille/ms.js")
    (license #f)))
(define-public node-statuses
  (package
    (name "node-statuses")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/statuses/archive/v1.3.0/statuses-v1.3.0.tar.gz")
        (sha256
          (base32
            "16xdm0vqm2kfy195b3hiwpw5ff3v8f1q1yqdp443i5jq3a1j3zxg"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "HTTP status utility")
    (description "HTTP status utility")
    (home-page "https://github.com/jshttp/statuses")
    (license expat)))
(define-public node-send
  (package
    (name "node-send")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/send/archive/0.14.1/send-0.14.1.tar.gz")
        (sha256
          (base32
            "01by9fq2g1cg9p0w42yg2ijb99m6nv5pwmg6wrp0152xbkv67qfs"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-depd" ,node-depd)
        ("node-statuses" ,node-statuses)
        ("node-ms" ,node-ms)
        ("node-mime" ,node-mime)
        ("node-destroy" ,node-destroy)
        ("node-encodeurl" ,node-encodeurl)
        ("node-range-parser" ,node-range-parser)
        ("node-http-errors" ,node-http-errors)
        ("node-etag" ,node-etag)
        ("node-fresh" ,node-fresh)
        ("node-debug" ,node-debug)
        ("node-on-finished" ,node-on-finished)
        ("node-escape-html" ,node-escape-html)))
    (native-inputs `())
    (synopsis
      "Better streaming static file server with Range and conditional-GET support")
    (description
      "Better streaming static file server with Range and conditional-GET support")
    (home-page
      "https://github.com/pillarjs/send#readme")
    (license expat)))
(define-public node-parseurl
  (package
    (name "node-parseurl")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/parseurl/archive/1.3.1/parseurl-1.3.1.tar.gz")
        (sha256
          (base32
            "1b8xxz6528s6g99v3ga0hns8aa74yajs59v0i121q31c6qz4src2"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "parse a url with memoization")
    (description "parse a url with memoization")
    (home-page
      "https://github.com/pillarjs/parseurl")
    (license expat)))
(define-public node-encodeurl
  (package
    (name "node-encodeurl")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/pillarjs/encodeurl/archive/v1.0.1/encodeurl-v1.0.1.tar.gz")
        (sha256
          (base32
            "1jf3336hczi1vbi233qz15q4jsd3z7ly6hkw32zgnmir2ik5acl5"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis
      "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (description
      "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (home-page
      "https://github.com/pillarjs/encodeurl#readme")
    (license expat)))
(define-public node-serve-static
  (package
    (name "node-serve-static")
    (version "1.11.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/expressjs/serve-static/archive/v1.11.1/serve-static-v1.11.1.tar.gz")
        (sha256
          (base32
            "03ma1gpv222rrmhcaf5dv65ahjkf9mf3cmcb8m58w1pcvvfavrbp"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-encodeurl" ,node-encodeurl)
        ("node-parseurl" ,node-parseurl)
        ("node-send" ,node-send)
        ("node-escape-html" ,node-escape-html)))
    (native-inputs `())
    (synopsis "Serve static files")
    (description "Serve static files")
    (home-page
      "https://github.com/expressjs/serve-static#readme")
    (license expat)))
(define-public node-cookie-signature
  (package
    (name "node-cookie-signature")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/visionmedia/node-cookie-signature/archive/1.0.6/node-cookie-signature-1.0.6.tar.gz")
        (sha256
          (base32
            "1wahybh5ys9bch3p42d6r3xysq3l602k9k7h1x546ladi2b6rz2w"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Sign and unsign cookies")
    (description "Sign and unsign cookies")
    (home-page
      "https://github.com/visionmedia/node-cookie-signature")
    (license expat)))
(define-public node-array-flatten
  (package
    (name "node-array-flatten")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/blakeembrey/array-flatten/archive/v2.1.0/array-flatten-v2.1.0.tar.gz")
        (sha256
          (base32
            "0xxkfshw26sm885q3yn9hljqw8wav0m04gp5xnynx3fdb4i7ig7m"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Flatten nested arrays")
    (description "Flatten nested arrays")
    (home-page
      "https://github.com/blakeembrey/array-flatten")
    (license expat)))
#f
(define-public node-forwarded
  (package
    (name "node-forwarded")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/forwarded/archive/v0.1.0/forwarded-v0.1.0.tar.gz")
        (sha256
          (base32
            "10gz7hjbfk7csfw869rb45q91xd15awdzrwzjqhyvx1xasl6yhz6"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Parse HTTP X-Forwarded-For header")
    (description "Parse HTTP X-Forwarded-For header")
    (home-page "https://github.com/jshttp/forwarded")
    (license expat)))
(define-public node-proxy-addr
  (package
    (name "node-proxy-addr")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/proxy-addr/archive/v1.1.2/proxy-addr-v1.1.2.tar.gz")
        (sha256
          (base32
            "0jjg27vxihmb9b0cz75kws6yxpkp3xrs3wr6ssy4yrdpj1qppd0z"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-forwarded" ,node-forwarded)
        ("node-ipaddr.js" ,node-ipaddr.js)))
    (native-inputs `())
    (synopsis "Determine address of proxied request")
    (description
      "Determine address of proxied request")
    (home-page
      "https://github.com/jshttp/proxy-addr#readme")
    (license expat)))
(define-public node-depd
  (package
    (name "node-depd")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/dougwilson/nodejs-depd/archive/v1.1.0/nodejs-depd-v1.1.0.tar.gz")
        (sha256
          (base32
            "0cy1acpl8i819bh4mqly4v5ndryd21nyb2vcn17nn8k4m2wbfzi2"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Deprecate all the things")
    (description "Deprecate all the things")
    (home-page
      "https://github.com/dougwilson/nodejs-depd")
    (license expat)))
(define-public node-mime-db
  (package
    (name "node-mime-db")
    (version "1.23.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/mime-db/archive/v1.23.0/mime-db-v1.23.0.tar.gz")
        (sha256
          (base32
            "0vvzgkna98x468wh1kb2ycla0f6gdcmpfqp3ri86fgbnr47ds192"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "Media Type Database")
    (description "Media Type Database")
    (home-page
      "https://github.com/jshttp/mime-db#readme")
    (license expat)))
(define-public node-mime-types
  (package
    (name "node-mime-types")
    (version "2.1.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/mime-types/archive/2.1.11/mime-types-2.1.11.tar.gz")
        (sha256
          (base32
            "1rbjgaq9mnv521z0l2ia4x5k8z8zvq8im6fdagxydwwpjjmmfz6x"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-mime-db" ,node-mime-db)))
    (native-inputs `())
    (synopsis
      "The ultimate javascript content-type utility.")
    (description
      "The ultimate javascript content-type utility.")
    (home-page
      "https://github.com/jshttp/mime-types#readme")
    (license expat)))
(define-public node-negotiator
  (package
    (name "node-negotiator")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/negotiator/archive/0.6.1/negotiator-0.6.1.tar.gz")
        (sha256
          (base32
            "1j7y0k19npl2fbfxcs321w1hhhf0qcahq4v2qgnijvbh5ls37snl"))))
    (build-system node-build-system)
    (propagated-inputs `())
    (native-inputs `())
    (synopsis "HTTP content negotiation")
    (description "HTTP content negotiation")
    (home-page
      "https://github.com/jshttp/negotiator#readme")
    (license expat)))
(define-public node-accepts
  (package
    (name "node-accepts")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/jshttp/accepts/archive/1.3.3/accepts-1.3.3.tar.gz")
        (sha256
          (base32
            "0v3r7jin04dskzalv5s8q8icnb4c7i8nzzzbdk0q0zwr9skczjy3"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-negotiator" ,node-negotiator)
        ("node-mime-types" ,node-mime-types)))
    (native-inputs `())
    (synopsis "Higher-level content negotiation")
    (description "Higher-level content negotiation")
    (home-page
      "https://github.com/jshttp/accepts#readme")
    (license expat)))
(define-public node-express
  (package
    (name "node-express")
    (version "4.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/expressjs/express/archive/4.14.0/express-4.14.0.tar.gz")
        (sha256
          (base32
            "1r30ddanrblqcfqah5qk3alkxy9kfdbiwi82s6704dvf4mxgfm2x"))))
    (build-system node-build-system)
    (propagated-inputs
      `(("node-accepts" ,node-accepts)
        ("node-depd" ,node-depd)
        ("node-proxy-addr" ,node-proxy-addr)
        ("node-array-flatten" ,node-array-flatten)
        ("node-cookie-signature" ,node-cookie-signature)
        ("node-serve-static" ,node-serve-static)
        ("node-cookie" ,node-cookie)
        ("node-vary" ,node-vary)
        ("node-encodeurl" ,node-encodeurl)
        ("node-content-disposition"
         ,node-content-disposition)
        ("node-range-parser" ,node-range-parser)
        ("node-path-to-regexp" ,node-path-to-regexp)
        ("node-qs" ,node-qs)
        ("node-type-is" ,node-type-is)
        ("node-etag" ,node-etag)
        ("node-finalhandler" ,node-finalhandler)
        ("node-fresh" ,node-fresh)
        ("node-methods" ,node-methods)
        ("node-parseurl" ,node-parseurl)
        ("node-debug" ,node-debug)
        ("node-merge-descriptors"
         ,node-merge-descriptors)
        ("node-on-finished" ,node-on-finished)
        ("node-utils-merge" ,node-utils-merge)
        ("node-content-type" ,node-content-type)
        ("node-send" ,node-send)
        ("node-escape-html" ,node-escape-html)))
    (native-inputs `())
    (synopsis
      "Fast, unopinionated, minimalist web framework")
    (description
      "Fast, unopinionated, minimalist web framework")
    (home-page "http://expressjs.com/")
    (license expat)))

(define-public node-setprototypeof
  (let ((commit "1e3d0cde6b7f4a9fba10cd28e62b200c9d8f899f")) 
    (package
      (name "node-setprototypeof")
      (version (string-append "1.0.1." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wesleytodd/setprototypeof.git")
               (commit commit)))
         (sha256
          (base32
           "13yj4bfqkq6wywd3bcm7gz631hp38lmwpb3wgwnrxjn1giq4k01l"))))
      (build-system node-build-system)
      (propagated-inputs `())
      (native-inputs `())
      (synopsis "A small polyfill for Object.setprototypeof")
      (description "A small polyfill for Object.setprototypeof")
      (home-page
       "https://github.com/wesleytodd/setprototypeof")
      (license isc))))
