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
  #:use-module ((guix licenses) #:select (expat))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define (coffee-script-build-helper-wrap file lib-directory)
  `(add-after 'install 'wrap-binary
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/" ,file))
                       (dir (string-append out "/lib/node_modules/coffee-script/" ,lib-directory)))
                  (display "  HELLO>   ")
                  (display ,lib-directory)
                  (display "  HELLO2>   ")
                  (display dir)
                  (display "  HELLO3>   ")
                  (display (getenv "NODE_PATH"))
                  (wrap-program bin
                    `("NODE_PATH" ":" prefix (,dir ;,(getenv "NODE_PATH")
                                                   )))
                  #t))))


(define (coffee-script-build-helper-backport backport-files)
  `(add-after 'unpack 'backport-node
              (lambda _
                (substitute* ',backport-files
                    ;'("bin/node_coffee" ;"bin/cake"
                    ;           "bin/coffee")
                  (("#!/usr/bin/env node --") "#!/usr/bin/env node")
                  (("require\\.paths.*$") "")
                  ))))

(define (coffee-script-build-helper-generated-files dir)
  `(add-after 'unpack 'delete-generated-files
              (lambda _
                ;;TODO: Regenerate parser, instead of keeping it
                (copy-file (string-append ,dir "/parser.js") (string-append ,dir "/parser.js.keep")) 
                (for-each (lambda (file)
                            (format #t "deleting generated JavaScript file '~a'~%" file)
                            (delete-file file))
                          (find-files ,dir ".*\\.js$"))
                (copy-file (string-append ,dir "/parser.js.keep") (string-append ,dir "/parser.js")))))

(define (coffee-script-build-helper-build cs-file target-dir)
  `(replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cs (assoc-ref inputs "coffee-script"))
                   (coffee (string-append cs "/" ,cs-file)))
                                        ;(setenv "NODE_PATH"
                                        ;(string-append
                                        ;(getenv "NODE_PATH")
                                        ;":" (assoc-ref outputs "out")
                                        ;"/lib/python"
                                        ;(string-take (string-take-right
                                        ;(assoc-ref inputs "python") 5) 3)
                                        ;"/site-packages"))
               (zero? (apply system* coffee "-o" ,target-dir (find-files "src" ".*\\.coffee$")))))))


(define-public coffee-script-boot0
  (package
    (name "coffee-script-boot0")
    (version "0.3.2")
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
               (("2010-2-8") "2010-02-08")))))))
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

(define-public coffee-script-yolo
  (let ((commit "bedc005d67c1dc52a0a4909c439a6f6347738213")) ;;last ruby commit
    (package (inherit coffee-script-boot0)
             (name "coffee-script-yolo")
             (version (string-append "0.3.2." (string-take commit 7)))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jashkenas/coffeescript.git")
                             (commit commit)))
                       (sha256
                        (base32
                         "01yy0l5f4a51jxg2bxc6c69b08c72jz41ldgksvzsk0jh3cg0s8f"))))
             )

    )

  )

;;require('vm').runInThisContext

;; last working
;; 1 parent 561a02c commit e4bb6c91e70cd8386d76040d65e0669dd6d9a255 @jashkenas jashkenas committed on 16 Feb 2010 

;; works:
;; a3c8c0b commit 79fa4723abe45f68dab90ff4fa86b2149c0219e9 @jashkenas jashkenas committed on 16 Feb 2010 
;;  561a02c commit e4bb6c91e70cd8386d76040d65e0669dd6d9a255 @jashkenas jashkenas committed on 16 Feb 2010 

;; does not work:
;; f6a1f16 commit 0f2cf552e9e0b26dd568c89be189212f3d940983 @jashkenas jashkenas committed on 17 Feb 2010 

;; WORKS!
;;src/command_line.coffee
;src/grammar.coffee


(define-public coffee-script-boot1
  (let ((commit
         "7667e16732268944232d31cea6a201698ce0661a"
         ;"e4bb6c91e70cd8386d76040d65e0669dd6d9a255"
         ;;"48c501a7a28b9da3681151fd30eb139ea475f853"
         ;"2d0ad73af815b2a833277741c690aea0c2ca877c"
         ;;"fa63288f524353bcb037252d57612e240cb5489c"
         ))
    (package (inherit coffee-script-boot0)
             (name "coffee-script-boot1")
             (version (string-append "0.5.0." (string-take commit 7)))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jashkenas/coffeescript.git")
                             (commit commit)))
                       (sha256
                        (base32
                         ;;"swag"
                                        ;"1cr7x5mvjjl7pl7cs96khxijbww4fk15v287sx3mazqx5glbzx64"
                         "0qq5p8k9yb8dx66lffjnirfb6hkv8hch82myidxcaxavi9dgl4kq"
                         ;;"0qq10i2znsa6xjp6p465qhyr3d2g4dr2zkph5jagnvgpnbgajpfm"
                         ;;"16ykn9i4zb143ikdricyzhiinjd2jk7sjwm46l2jhv8pn7xjz2g1"
                         ;;"0rj2gx3amw2abyzd83bgyy4983k6gz13dakz5pkic3q3wkjh9iw6"
                         ))
                       (modules '((guix build utils)))
                       (snippet
                        '(begin
                           (delete-file "src/narwhal/coffee-script.coffee")
                           ;; (substitute* '("src/command_line.coffee" "src/grammar.coffee")
                           ;;   (("require 'posix'")
                           ;;    "require 'fs'")
                           ;;   (("posix.cat\\(")
                           ;;    "posix.readFile(")
                           ;;   (("\\).addCallback \\(")
                           ;;    " , (err, ")
                           ;;   (("    compile_scripts\\(\\)")
                           ;;    "    compile_scripts())")
                           ;;   (("posix.write\\(fd, js\\)")
                           ;;    "posix.write(fd, js))")
                           ;;   )
                           )
                        )))
             (build-system node-build-system)
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.try
                #:phases
                (modify-phases %standard-phases
                  ;;(add-after 'unpack 'remove-narwhal-support
                    ;;(lambda _
                      ;;(delete-file "src/narwhal/coffee-script.coffee")
                      ;;#t))
                  ,(coffee-script-build-helper-backport '("bin/node_coffee"))
                  ,(coffee-script-build-helper-generated-files "lib/coffee_script")
                  ,(coffee-script-build-helper-wrap  "bin/node_coffee" "lib/coffee_script") 
                  ,(coffee-script-build-helper-build "bin/coffee" "lib/coffee_script"))))
             (inputs
              `(("node" ,node-0.try)))
             (native-inputs
              `(("coffee-script" ,coffee-script-yolo)
                ("ruby" ,ruby))))))


;; at least further than fbfa12c
(define-public coffee-script-boot2
  (let ((commit
         "4ea8be8e0be5521c0b94a7e6031c2c3a60725d5a"
         ;"e4bb6c91e70cd8386d76040d65e0669dd6d9a255"
         ;;"48c501a7a28b9da3681151fd30eb139ea475f853"
         ;"2d0ad73af815b2a833277741c690aea0c2ca877c"
         ;;"fa63288f524353bcb037252d57612e240cb5489c"
         ))
    (package (inherit coffee-script-boot1)
             (name "coffee-script-boot2")
             (version (string-append "0.5.0." (string-take commit 7)))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jashkenas/coffeescript.git")
                             (commit commit)))
                       (sha256
                        (base32
                         ;;"swag"
                                        ;"1cr7x5mvjjl7pl7cs96khxijbww4fk15v287sx3mazqx5glbzx64"
                         ;;"0qq10i2znsa6xjp6p465qhyr3d2g4dr2zkph5jagnvgpnbgajpfm"
                         "0qq5p8k9yb8dx66lffjnirfb6hkv8hch82myidxcaxavi9dgl4kq"
                         ;;"16ykn9i4zb143ikdricyzhiinjd2jk7sjwm46l2jhv8pn7xjz2g1"
                         ;;"0rj2gx3amw2abyzd83bgyy4983k6gz13dakz5pkic3q3wkjh9iw6"
                         ))
                       (modules '((guix build utils)))
                       (snippet
                        '(begin
                           (delete-file "src/narwhal/coffee-script.coffee")
                           (substitute* '("src/command_line.coffee" "src/grammar.coffee")
                             (("require 'posix'")
                              "require 'fs'")
                             (("posix.cat\\(")
                              "posix.readFile(")
                             (("\\).addCallback \\(")
                              " , (err, ")
                             (("    compile_scripts\\(\\)")
                              "    compile_scripts())")
                             (("posix.write\\(fd, js\\)")
                              "posix.write(fd, js))")
                             ))
                        )))
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.1
                #:phases
                (modify-phases %standard-phases
                  ;;(add-after 'unpack 'remove-narwhal-support
                  ;;(lambda _
                  ;;(delete-file "src/narwhal/coffee-script.coffee")
                  ;;#t))
                  ,(coffee-script-build-helper-backport '("bin/node_coffee"))
                  ,(coffee-script-build-helper-generated-files "lib/coffee_script")
                  ,(coffee-script-build-helper-wrap  "bin/node_coffee" "lib/coffee_script")
                  ,(coffee-script-build-helper-build "bin/node_coffee" "lib/coffee_script"))))
             ;; (build-system node-build-system)
             ;; (arguments
             ;;  `(#:tests? #f
             ;;    #:global? #t
             ;;    #:phases
             ;;    (modify-phases %standard-phases
             ;;      (add-after 'unpack 'remove-narwhal-support
             ;;        (lambda _
             ;;          (delete-file "src/narwhal/coffee-script.coffee")
             ;;          #t))
             ;;      ,(coffee-script-build-helper-backport '("bin/node_coffee"))
             ;;      ,(coffee-script-build-helper-generated-files "lib/coffee_script")
             ;;      ,(coffee-script-build-helper-wrap  "bin/node_coffee" "lib/coffee_script") 
             ;;      ,(coffee-script-build-helper-build "lib/coffee_script"))))
             ;; (inputs
             ;;  `(("node" ,node-0.1)))
             (native-inputs
              `(("coffee-script" ,coffee-script-boot1)
                ("ruby" ,ruby)))
             ))
  )



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
