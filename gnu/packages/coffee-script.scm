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
  #:use-module ((guix licenses) #:select (expat))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux))

(define (coffee-script-build-helper-wrap file lib-directory)
  "Generates a post-`install' phase named `wrap-binary' that extends the
NODE_PATH environment variable with LIB-DIRECTORY for executable <out>/FILE."
  `(add-after 'install 'wrap-binary
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/" ,file))
                       (dir (string-append out
                                           "/lib/node_modules/coffee-script/"
                                           ,lib-directory))
                       ;; FIXME: Use unbundled jison
                       (jison-vendor
                        (string-append
                         out
                         "/lib/node_modules/coffee-script/vendor/jison/lib")))
                  (wrap-program bin
                    `("NODE_PATH" ":" prefix (,dir ,jison-vendor)))
                  #t))))


(define (coffee-script-build-helper-backport backport-files)
  "Generates a post-`unpack' phase named `backport-node' that disables
deprecated Node-js concepts used in the list of files BACKPORT-FILES."
  `(add-after 'unpack 'backport-node
              (lambda _
                (substitute* ',backport-files
                  (("#!/usr/bin/env node --") "#!/usr/bin/env node")
                  (("require\\.paths.*$") ""))
                #t))) ;;to prevent NODE_PATH shenanigans

(define (delete-generated-js-phase dir)
  "Generates a post-`unpack' phase named `delete-generated-files' that removes
any (pre-compiled) JavaScript files in DIR."
  `(add-after 'unpack 'delete-generated-files
              (lambda _
                ;;FIXME: Regenerate parser, instead of keeping it
                (copy-file (string-append ,dir "/parser.js")
                           (string-append ,dir "/parser.js.keep"))
                (for-each
                 (lambda (file)
                   (format #t "deleting generated JavaScript file '~a'~%" file)
                   (delete-file file))
                 (find-files ,dir ".*\\.js$"))
                (copy-file (string-append ,dir "/parser.js.keep")
                           (string-append ,dir "/parser.js"))
                (delete-file (string-append ,dir  "/parser.js.keep"))
                #t)))

(define* (coffee-script-build-phase coffeescript-compiler target-dir #:optional (flags #f))
  "Generates a replacement `build' phase that uses a COFFEESCRIPT-COMPILER to
compile coffeescript sources in TARGET-DIR.  Compiler FLAGS are passed on to
COFFEESCRIPT-COMPILER if available."
  `(replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((cs (assoc-ref inputs "coffee-script"))
                     (coffee (string-append cs "/" ,coffeescript-compiler)))
                ,(if flags
                     `(zero? (apply system* coffee ,flags "-o" ,target-dir (find-files "src" ".*\\.coffee$")))
                     `(zero? (apply system* coffee "-o" ,target-dir (find-files "src" ".*\\.coffee$"))))))))

(define-public coffee-script-boot0
  (let ((commit "bedc005d67c1dc52a0a4909c439a6f6347738213")) ;;last ruby commit
    (package ;(inherit coffee-script-boot0)
      (name "coffee-script-boot0")
      (version (string-append "0.3.2." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jashkenas/coffeescript.git")
               (commit commit)))
         (sha256
          (base32
           "01yy0l5f4a51jxg2bxc6c69b08c72jz41ldgksvzsk0jh3cg0s8f"))))
      (build-system ruby-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-gemspec-date
             (lambda _
               ;;XXX: Fix incorrect date in gemspec
               (substitute* "coffee-script.gemspec"
                 (("2010-2-8") "2010-02-08")))))))
      (synopsis "CoffeeScript is a little language that compiles into
JavaScript.")
      (description "CoffeeScript is a little language that compiles
into JavaScript.  Think of it as JavaScript's less ostentatious kid brother --
the same genes, roughly the same height, but a different sense of style.  Apart
from a handful of bonus goodies, statements in CoffeeScript correspond
one-to-one with their equivalent in JavaScript, it's just another way of
saying it.")
      (home-page "http://jashkenas.github.com/coffee-script/")
      (license expat))))

(define-public coffee-script-boot1
  (let ((commit
         "e4bb6c91e70cd8386d76040d65e0669dd6d9a255"))
    (package (inherit coffee-script-boot0)
             (name "coffee-script-boot1")
             (version (string-append "0.3.2." (string-take commit 7)))
             (source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jashkenas/coffeescript.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0qq10i2znsa6xjp6p465qhyr3d2g4dr2zkph5jagnvgpnbgajpfm"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Remove narwhal support
                    (delete-file "src/narwhal/coffee-script.coffee")
                    ;; Not easy to determine what node version was used, so
                    ;; translate the only incompatible function
                    (substitute* "src/command_line.coffee"
                      (("fs\\.cat")
                       "fs.readFile"))))))
             (build-system node-build-system)
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.1.29
                #:phases
                (modify-phases %standard-phases
                  ,(delete-generated-js-phase "lib/coffee_script")
                  ,(coffee-script-build-helper-wrap "bin/node_coffee"
                                                    "lib/coffee_script")
                  ,(coffee-script-build-phase "bin/coffee"
                                              "lib/coffee_script"))))
             (inputs
              `(("node" ,node-0.1.29)))
             (native-inputs
              `(("coffee-script" ,coffee-script-boot0)
                ("ruby" ,ruby))))))

(define-public coffee-script-boot2
  (let ((commit "4ec7514d1056ff05b56ada79ba66ab2876a948e0"))
    (package (inherit coffee-script-boot1)
             (name "coffee-script-boot2")
             (version (string-append "0.5.2." (string-take commit 7)))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jashkenas/coffeescript.git")
                             (commit commit)))
                       (sha256
                        (base32
                         "08cnyj62qr7vn08kqayn3krpp224vs5856g2r556mbn7mg9018kk"))
                       (modules '((guix build utils)))
                       (snippet
                        '(begin
                           ;; Remove narwhal support
                           (delete-file "src/narwhal.coffee")))))
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.1.30
                #:phases
                (modify-phases %standard-phases
                  ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                  ,(delete-generated-js-phase "lib")
                  ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                  ,(coffee-script-build-phase "bin/node_coffee" "lib"))))
             (inputs
              `(("node" ,node-0.1.30)))
             (native-inputs
              `(("coffee-script" ,coffee-script-boot1))))))

(define-public coffee-script-0.5.3
  (package (inherit coffee-script-boot2)
           (name "coffee-script")
           (version "0.5.3")
           (source (origin
                     (method url-fetch)
                     (uri (string-append "https://github.com/jashkenas/coffeescript/archive/"
                                         version ".tar.gz"))
                     (sha256
                      (base32
                       "0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"))
                     (modules '((guix build utils)))
                     (snippet
                      '(begin
                         ;; Remove narwhal support
                         (delete-file "src/narwhal.coffee")))))
           (arguments
            `(#:tests? #f
              #:global? #t
              #:node ,node-0.1.30
              #:phases
              (modify-phases %standard-phases
                ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                ,(delete-generated-js-phase "lib")
                ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                ,(coffee-script-build-phase "bin/coffee" "lib" "-c"))))
           (native-inputs
            `(("coffee-script" ,coffee-script-boot2)))))

(define-public coffee-script-0.5.4
  (package (inherit coffee-script-boot2)
           (name "coffee-script")
           (version (string-append "0.5.4"))
           (source (origin
                     (method url-fetch)
                     (uri (string-append
                           "https://github.com/jashkenas/coffeescript/archive/"
                           version ".tar.gz"))
                     (sha256
                      (base32
                       "05n7yq1qs8h4934mkln1k577yg6f5dn77sdbznva47gf4srsggna"))))
           (arguments
            `(#:tests? #f
              #:global? #t
              #:node ,node-0.1.31
              #:phases
              (modify-phases %standard-phases
                ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                ,(delete-generated-js-phase "lib")
                ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                ,(coffee-script-build-phase "bin/coffee" "lib" "-c"))))
           (inputs
            `(("node" ,node-0.1.31)))
           (native-inputs
            `(("coffee-script" ,coffee-script-0.5.3)))))

(define-public coffee-script-boot3
  (let ((commit "08341286a3f75a7be9faa0e942340e717a391fb8"))
    (package (inherit coffee-script-0.5.4)
             (name "coffee-script-boot3")
             (version (string-append "0.5.4." (string-take commit 7)))
             (source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jashkenas/coffeescript.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1cc0qd2qs46yrkwmlccy8byslzb9xkjx2knxwmjp2fq8rv06flrk"))))
             (native-inputs
              `(("coffee-script" ,coffee-script-0.5.4))))))

(define (coffee-replicate package-object key name)
  (let ((ni (package-native-inputs package-object)))
    (package (inherit package-object)
             (name name)
             (native-inputs
              (cons `(,key ,package-object) ni)))))

(define* (coffee-script-bootstrap previous nname #:optional (node #f))
  (lambda (commit version hash)
    (let ((node (or node
                    (cadr (assoc "node" (package-inputs previous)))))
          (revision "1"))
      (package
        (inherit previous)
        (name nname)
        (version (string-append version "-" revision "."
                                (string-take commit 7)))
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/jashkenas/coffeescript.git")
                        (commit commit)))
                  (sha256 (base32 hash))
                  (file-name
                   (string-append "coffeescript-" version "-checkout"))))
        (arguments
         `(#:tests? #f
           #:global? #t
           #:node ,node
           #:phases
           (modify-phases %standard-phases
             ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
             ,(delete-generated-js-phase "lib")
             ,(coffee-script-build-helper-wrap "bin/coffee" "lib")
             ,(coffee-script-build-phase "bin/coffee" "lib" "-c"))))
        (inputs
         `(("node" ,node)))
        (native-inputs
         `(("coffee-script" ,previous)))))))

(define* (patch-sources #:key previous (snippet #f) (patch #f))
  (let ((base-origin (package-source previous))
        (patches (if patch (search-patches patch) '())))
    (package (inherit previous)
             (source
              (origin (inherit base-origin)
                      (modules '((guix build utils)))
                      (snippet snippet)
                      (patches patches))))))

(define boot4 (coffee-script-bootstrap coffee-script-boot3
                                       "coffee-script-boot4"))
(define-public coffee-script-boot4
  (boot4 "47682de0f02d5d444fbce34e3af02500fe8b9aa8"
         "0.5.4"
         "01r35xfd5mvv66zlm2lmhdj0dfjib58w8nlacyavq4cs6ycskc56"))

(define boot5
  (coffee-script-bootstrap coffee-script-boot4 "coffee-script-boot5"))
(define-public coffee-script-boot5
  (boot5
   "90f2e0dbb9aeafb9ad4c37b886b924d09dff896f"
   "0.5.4"
   "1j9g35kgcwk6a0p17rxg9p3gw49ch5qx6wj8ypwf0wj419i4msd9"))

(define boot6
  (coffee-script-bootstrap coffee-script-boot5 "coffee-script-boot6"))
(define-public coffee-script-boot6
  (boot6
   "b72641693df90e4ffee37c2c98f7d8675d68ce0e"
   "0.5.5"
   "00aws51vp3kak29kwbjzzbf5n1hwrymsljfjc8qh4dhvndcz61rl"))

(define boot7
  (coffee-script-bootstrap coffee-script-boot6 "coffee-script-boot7"))
(define-public coffee-script-boot7
  (boot7
   "cbfe7f5822607e67563c98fe73e97b1475b702ac"
   "0.5.5"
   "1w9ml532nj3i9xklm9klfrwllapck84j5j8v4ymiblh0cg4lbbfm"))

(define boot8
  (coffee-script-bootstrap coffee-script-boot7 "coffee-script-boot8"))
(define-public coffee-script-boot8
  (boot8
   "177ec92c399504fdff0bbea5e30fa474c7579d8f"
   "0.5.6"
   "02d8xj557nygvax6sv9gmhq8p2kfw7y5w1gfz2wc0imbxlw148dy"))

(define boot9
  (coffee-script-bootstrap coffee-script-boot8 "coffee-script-boot9"))
(define-public coffee-script-boot9
  (boot9
   "572aa4e98f6b983b5775b03884d89a2f5c17fac9"
   "0.5.6"
   "15aw9lbrldwwpyq9fb6s48h0p91w73q67zqlw034i85dr0bxah43"))

(define boot10
  (coffee-script-bootstrap coffee-script-boot9 "coffee-script-boot10"))
(define-public coffee-script-boot10
  (boot10
   "c067808b54e8f7d022156959e3aa47217cd96f88"
   "0.6.0"
   "1six1n162xhsjk2h49m4ccw0wma32z2fl4y8pib72s4lk060zcx3"))

(define boot11
  (coffee-script-bootstrap coffee-script-boot10
                           "coffee-script-boot11"
                           node-0.1.90))
(define-public coffee-script-boot11
  (boot11
   "8317960f813e2eefd1271748730c5b621975571c"
   "0.6.0"
   "124wqh0w2i7p8q75qyjylxb4pdhw1z06z5wh6bvjsa47jvlh8d3i"))

(define boot12
  (coffee-script-bootstrap coffee-script-boot11 "coffee-script-boot12"))
(define-public coffee-script-boot12
  (boot12
   "a894db35fd609b944df56375c3f0702fa70fe3ca"
   "0.6.1"
   "19cbqp8hjcrdlzxrb9lxzx9lz72y8pydz5v8m6731pdvgrr1b75p"))

(define boot13
  (coffee-script-bootstrap coffee-script-boot12 "coffee-script-boot13"))
(define-public coffee-script-boot13
  (boot13
   "b746c9018e6c4c7d1c1028816f0b96c6b90c04ce"
   "0.6.1"
   "1vwnqi1m2h31m03xn7flp949x5wn7dqp3rp31dn05z54i1p3gxpi"))

(define boot14
  (coffee-script-bootstrap coffee-script-boot13 "coffee-script-boot14" node-0.1.95))
(define-public coffee-script-boot14
  (boot14
   "a133e018cc4d27eeec19a8296a7a0ce6f5d6552a"
   "0.6.2"
   "08dbl57b2l9pzfdmsdal7l33mg09ps3i1xb6qbg0s6j3r6ym51yd"))

(define boot15
  (coffee-script-bootstrap coffee-script-boot14
                           "coffee-script-boot15"
                           node-0.1.98))
(define coffee-script-boot15-helper
  (boot15
   "0222d90fa3c21c3f4c33f64e8e964d5366744cfc"
   "0.6.2"
   "18rn6s7xxjmsd3696c0gpvm1m028lcmd2kyladybw5las4d3cp0z"))

(define-public coffee-script-boot15
  (patch-sources #:previous coffee-script-boot15-helper
                 #:snippet
                 '(begin
                    (substitute* '("src/lexer.coffee" "src/rewriter.coffee")
                      (("loop\n")
                       "while true\n")))))

(define boot17
  (coffee-script-bootstrap coffee-script-boot15 "coffee-script-boot17"))
(define-public coffee-script-boot17
  (boot17
   "4b284f6687920734c42792233b03ea0402e7cd5a"
   "0.6.2"
   "0bgqmhxw0vyhibdgi1pkljdx8qd548jcap9vd1fhrhm4r5kj625w"))

(define-public coffee-script-boot18
  (patch-sources
   #:previous coffee-script-boot17
   #:snippet '(begin
                (substitute* "src/lexer.coffee"
                  (("^([ ]+)@tokens\\.pop\\(\\)\n" all spaces)
                   (string-append spaces
                                  "@tokens.pop() if @tag() isnt '@'\n"))))))

(define boot19
  (coffee-script-bootstrap coffee-script-boot18 "coffee-script-boot19"))
(define-public coffee-script-boot19
  (boot19
   "0fcfb80be47d42703270a77cb311daf276e4d70a"
   "0.6.2"
   "075xqmyivhfnv4v53jqz6migrxygx4ri4cikgaykvi9gy3dhli78"))


(define boot20
  (coffee-script-bootstrap coffee-script-boot19 "coffee-script-boot20"))
(define-public coffee-script-boot20
  (boot20
   "7c426db36a1e15d61b56a87739571909033c7d62"
   "0.7.0"
   "0ibsy7lcygvkg0pf1a4g19jh70f5hj8x0jyqybnxc1ix403in1rz"))

(define boot21
  (coffee-script-bootstrap coffee-script-boot20 "coffee-script-boot21"))
(define coffee-script-boot21-helper
  (boot21
   "7d79d73b58576658c88ff59df8fc45cab5cff6dc"
   "0.7.0"
   "1cppvxnvqip5yy7ms4p98xyydycg0iczshl75rbk1bm58n9cpca4"))

(define-public coffee-script-boot21
  (patch-sources #:previous coffee-script-boot21-helper
                 #:snippet
                 '(begin
                    (substitute* '("src/grammar.coffee"
                                   "src/coffee-script.coffee"
                                   "src/lexer.coffee"
                                   "src/nodes.coffee")
                      (("(new [a-zA-Z]+)\n" all identifier)
                       (string-append identifier "()\n"))
                      (("\\((new [a-zA-Z]+)\\)" all identifier)
                       (string-append "(" identifier "())"))))))

(define boot22
  (coffee-script-bootstrap coffee-script-boot21 "coffee-script-boot22"))
(define-public coffee-script-boot22
  (boot22
   "2a932597e4d9f1adc8b8788f2ea2885728c78db1"
   "0.7.2"
   "0ynprqg3frhm6lw1aflmjzv7gsxy6g357j8ydi0rm3hczc40pgv3"))

(define boot23
  (coffee-script-bootstrap coffee-script-boot22 "coffee-script-boot23"))
(define coffee-script-boot23-helper
  (boot23
   "72c4efbc3966a9717eb2a79a6eeded64f30a4206"
   "0.7.2"
   "1iafv758mrh1b78qaciny87mk35qcd5r2m08nr19jmxi1bpxk0bf"))
(define-public coffee-script-boot23
  (patch-sources #:previous coffee-script-boot23-helper
                 #:snippet
                 '(begin
                    (substitute* "src/cake.coffee"
                      (("([ ]+)for all name, task of tasks" all spaces)
                       (string-append spaces "for name, task of tasks")))
                    (substitute* "src/helpers.coffee"
                      (("for all key, val") "for key, val")))))

(define boot24
  (coffee-script-bootstrap coffee-script-boot23
                           "coffee-script-boot24"
                           node-0.1.101))
(define-public coffee-script-boot24
  (boot24
   "88847df70b5b21854616c0a14dad38ba4f426b91"
   "0.7.2"
   "17kb30sp85cddxbv20vc2nj2i2rk1j5fb7b6d704dpb9v7pa05py"))

(define boot25
  (coffee-script-bootstrap coffee-script-boot24 "coffee-script-boot25"))
(define coffee-script-boot25-helper
  (boot25
   "f9dff6ffc48d32dee6ff1f7748ce028d1ff88252"
   "0.7.2"
   "0smpnx42bx4bkvaa5km32klsxpwrad2viz990c7g19v10igcpg6c"))
(define-public coffee-script-boot25
  (patch-sources #:previous coffee-script-boot25-helper
                 #:snippet
                 '(begin
                    (substitute* '("src/coffee-script.coffee"
                                   "src/command.coffee"
                                   "src/lexer.coffee"
                                   "src/nodes.coffee"
                                   "src/rewriter.coffee")
                      (("or=") "||=")
                      (("and=") "&&=")))
                 #:patch "coffeescript-backport-optional-braces.patch"))

(define boot26
  (coffee-script-bootstrap coffee-script-boot25 "coffee-script-boot26"))
(define coffee-script-boot26-helper
  (boot26
   "9c8a22832cfa589a2f4bf7c340505c2754d09cdc"
   "0.7.2"
   "1ndfclfbdpqqpf18ixmx8h5v2ssh87hndhchxg5zwk0np3rydz3c"))
(define-public coffee-script-boot26
  (patch-sources #:previous coffee-script-boot26-helper
                 #:patch
                 "coffeescript-backport-dollar-string-interpolation.patch"))

(define boot27
  (coffee-script-bootstrap coffee-script-boot26 "coffee-script-boot27"))
(define-public coffee-script-boot27
  (boot27
   "9be1453886beefe2fd1096568eb28d6942810a96"
   "0.7.2"
   "1586pfw4nn3sfkcqkbyisna2cwympd8cyqp4dz7s6ggi773nvqm8"))

(define boot28
  (coffee-script-bootstrap coffee-script-boot27 "coffee-script-boot28"))
(define coffee-script-boot28-helper
  (boot28
   "c9421cbfcdecad656e4be4002bc851303c83ca84"
   "0.7.2"
   "17v1c2p3yf2caplyn5xq7sk667yrh1f5g5m8wb6z6zclmahnfy52"))
(define-public coffee-script-boot28
  (patch-sources #:previous coffee-script-boot28-helper
                 #:patch "coffeescript-backport-prefixed-splats.patch"))

(define boot29
  (coffee-script-bootstrap coffee-script-boot28 "coffee-script-boot29"))
(define coffee-script-boot29-helper
  (boot29
   "129e950c59bde7fa529baf510f93e619a033a767"
   "0.9.1"
   "11r6mbnjvcyfmhz5xi9w9d9h0xhl12b6jh7w1i960jlzv8xrfb5v"))
(define-public coffee-script-boot29
  (patch-sources #:previous coffee-script-boot29-helper
                 #:patch
                 "coffeescript-backport-implicit-object-function-call.patch"))

(define boot30
  (coffee-script-bootstrap coffee-script-boot29 "coffee-script-boot30"))
(define-public coffee-script-boot30
  (boot30
   "4ddd65a4c4df5baa4911bf095246a7829e9acca6"
   "0.9.1"
   "13zw49b373rkz1ynhwgb41q0n6rdazwbc23crmzgypdsm7cp7lw5"))

(define boot31
  (coffee-script-bootstrap coffee-script-boot30 "coffee-script-boot31"))
(define coffee-script-boot31-helper
  (boot31
   "87693d84cb9f0286503d36093c9e9e8ec6450a51"
   "0.9.1"
   "156rs10q5pg53aqz6xf97vmcz6cjsg02m3m5lgxmlivsrmrfx93x"))
(define-public coffee-script-boot31
  (patch-sources #:previous coffee-script-boot31-helper
                 #:patch "coffeescript-backport-not-instanceof.patch"))

(define boot32
  (coffee-script-bootstrap coffee-script-boot31 "coffee-script-boot32"))
(define-public coffee-script-boot32
  (boot32
   "b2313beaf4a77649506371eb1ece1f4aea1ad23b"
   "0.9.4"
   "02kp2584ixxvwy1afsxrnpfwds66yqrcc6gbgvw4f03ygcx321hn"))

(define boot33
  (coffee-script-bootstrap coffee-script-boot32 "coffee-script-boot33"))
(define-public coffee-script-boot33
  (boot33
   "493fa7d8fe3bdc604e2b7420c084b0873564a0a6"
   "0.9.4"
   "047qnx6qvrb3rypdl9ksn3pvr2hp9wgj99mw1six7gqhyc7ybf0l"))

(define boot34
  (coffee-script-bootstrap coffee-script-boot33 "coffee-script-boot34"))
(define coffee-script-boot34-helper
  (boot34
   "380bee97ddbab7677d83dc1018bf46b6f27fa3d9"
   "0.9.4"
   "0mn0gkp5hgmnv130vgr8affiidhk1ybr9xr2r4ah241v61563lis"))
(define-public coffee-script-boot34
  (patch-sources #:previous coffee-script-boot34-helper
                 #:patch "coffeescript-backport-lexer-inof-forinof.patch"))

(define boot35
  (coffee-script-bootstrap coffee-script-boot34 "coffee-script-boot35"))
(define coffee-script-boot35-helper
  (boot35
   "acafb1b53a9197555a6e5d38a08b884efd78c643"
   "0.9.4"
   "1ml8haprj0akifvnd26lsijp78s42j80bq77wh8rpamiv50bkqmf"))
(define-public coffee-script-boot35
  (patch-sources #:previous coffee-script-boot35-helper
                 #:patch "coffeescript-backport-if-else-chain.patch"))

(define boot36
  (coffee-script-bootstrap coffee-script-boot35 "coffee-script-boot36"))
(define-public coffee-script-boot36
  (boot36
   "2f7c076a503ca00c19a0db599512010a89dde951"
   "0.9.4"
   "1gccmx8wi4s06vdr157h6lmqdqnjajs7brcri2f80pkbkx258frf"))

(define boot37
  (coffee-script-bootstrap coffee-script-boot36 "coffee-script-boot37"))
(define-public coffee-script-boot37
  (boot37
   "a8da3218838c9ad23703430fe15334903bb9fd5e"
   "0.9.4"
   "1rngdd4y4f7mghbh7mm221d8ss6qaz9ksbd2hj7qk2gjvxm53y4v"))

(define boot38
  (coffee-script-bootstrap coffee-script-boot37 "coffee-script-boot38"))
(define-public coffee-script-boot38
  (boot38
   "bfc236fca386a3cc438cfeec9ebc6ed2032c4abf"
   "0.9.4"
   "0nf4p2mzdbh847f88nxk6i4ynvjk35bz7x3zjq3gvzsilmkq0l4v"))

(define boot39
  (coffee-script-bootstrap coffee-script-boot38
                           "coffee-script-boot39"
                           node-0.3.0))
(define coffee-script-boot39-helper
  (boot39
   "85c8a6780a84dab00c98d14eaf03c86ca4acf2c8"
   "0.9.4"
   "1w08a6xc29xx7147ad65f7zf5d72ijj8rv75pz9avbnhy65afp15"))
(define-public coffee-script-boot39
  (patch-sources #:previous coffee-script-boot39-helper
                 #:patch "coffeescript-backport-destructuring.patch"))

(define boot40
  (coffee-script-bootstrap coffee-script-boot39 "coffee-script-boot40"))
;; has a broken repl, fixed in later commit
(define-public coffee-script-boot40
  (boot40
   "b32a75858a469aa8971de0ad271fbaba3967e5d3"
   "0.9.4"
   "0ki3cp8yj1jy5sihyfzc09i7ll1w34pwkrjmmdjkmhhnf2wp6mw2"))

(define boot41
  (coffee-script-bootstrap coffee-script-boot40 "coffee-script-boot41"))
(define coffee-script-boot41-helper
  (boot41
   "b2be475f93f7ac940bc3591626f2db598b5e4cd1"
   "0.9.4"
   "1kj9wslyxddnb32i72y9jjx4701vjihyg2nk3fh7vj17qh23hd0z"))
(define-public coffee-script-boot41
  (patch-sources #:previous coffee-script-boot41-helper
                 #:patch "coffeescript-backport-leading-then-expression.patch"))

(define boot42
  (coffee-script-bootstrap coffee-script-boot41 "coffee-script-boot42"))
(define coffee-script-boot42-helper
  (boot42
   "ecd4722b7ce64fbef41d0240d972c76a07d5d127"
   "0.9.4"
   "0wal456lzsndknca347g8z1va10k5999dd788wc7lrib2zl2yawb"))
(define-public coffee-script-boot42
  (patch-sources #:previous coffee-script-boot42-helper
                 #:patch "coffeescript-backport-default-arguments.patch"))

(define boot43
  (coffee-script-bootstrap coffee-script-boot42 "coffee-script-boot43"))
(define coffee-script-boot43-helper
  (boot43
   "a2d33112b8cf0d4428b9d423a8edc02a8112e126"
   "0.9.4"
   "0qqj2jjr9lys9mvzk49hck588yh1qijlv80wab3gyjxpfj8n9anj"))
(define-public coffee-script-boot43
  (patch-sources #:previous coffee-script-boot43-helper
                 #:patch "coffeescript-backport-executable-class-body.patch"))

(define boot44
  (coffee-script-bootstrap coffee-script-boot43 "coffee-script-boot44"))
(define-public coffee-script-boot44
  (boot44
   "3059db85156db5556119b3dd4fe72838bbe4798d"
   "0.9.4"
   "0cpqrnk2c6rpkip407xs4ly3rw0lv47gq6lgf34vsm5bv8spsrh9"))

(define boot45
  (coffee-script-bootstrap coffee-script-boot44 "coffee-script-boot45"))
(define coffee-script-boot45-helper
  (boot45
   "2aedbc2e42cc782e3e6f6b396dc04819ef790ae6"
   "0.9.4"
   "100qs8x25dbvf4mwj7kqz4j7p1py4917ykqndwmaagg47nyanrxv")) 
(define-public coffee-script-boot45
  (patch-sources #:previous coffee-script-boot45-helper
                 #:patch "coffeescript-backport-static-value-syntax.patch"))


(define boot46
  (coffee-script-bootstrap coffee-script-boot45 "coffee-script-boot46"))
(define coffee-script-boot46-helper
  (boot46
   "15bdcf79e6bdc90e062de49e5358911d70021a89"
   "0.9.4"
   "0jmlk3v9nhm1r0npx6dydaw8qsv2sndx0cfqbcpvybrsxhrbxr7q"))
(define-public coffee-script-boot46
  (patch-sources #:previous coffee-script-boot46-helper
                 #:patch
                 "coffeescript-backport-explicit-constructor-syntax.patch"))

(define boot47
  (coffee-script-bootstrap coffee-script-boot46 "coffee-script-boot47"))
(define-public coffee-script-boot47
  (boot47
   "a1aaa4495cc2635db2f3ef12c43d6cfcfcf123a0"
   "0.9.4"
   "18f05apxvqqz3rdh3sxczdi3vw5nm9q29qs20idifgmf37j8dcr3"))

(define boot48
  (coffee-script-bootstrap coffee-script-boot47 "coffee-script-boot48"))
(define coffee-script-boot48-helper
  (boot48
   "9f708ad0c83628d2e2ffb9c9055816aff44f3fb6"
   "0.9.4"
   "17bkbxhf5gar8g5ciaqwmxswhz7p0dcp07vycw79wk0v2iq2q974"))
(define-public coffee-script-boot48
  (patch-sources #:previous coffee-script-boot48-helper
                 #:patch "coffeescript-backport-range-literals.patch"))

(define boot49
  (coffee-script-bootstrap coffee-script-boot48 "coffee-script-boot49"))
(define-public coffee-script-boot49
  (boot49
   "fc64fa49ac0a0a1f6e51195b5eda377ccde232fc"
   "0.9.5"
   "00visn0j8zq11i9a5pc9f3ww06zwp8gn9y23linvyvxwh9fmmfyc"))

(define boot50
  (coffee-script-bootstrap coffee-script-boot49 "coffee-script-boot50"))
(define coffee-script-boot50-helper
  (boot50
   "4afa6a2887fe6ef6d1de5bb2a13a2edddc82649b"
   "0.9.5"
   "1sdl8w63l36hs3v1dj8xcg0kal6i5kmmg33j1rv6wpphn77mwf1j"))
(define-public coffee-script-boot50
  (patch-sources #:previous coffee-script-boot50-helper
                 #:patch
                 "coffeescript-backport-forall-loop-syntax-change.patch"))

(define-public coffee-script
  (package
    (name "coffee-script")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jashkenas/coffeescript/archive/"
             version ".tar.gz"))
       (sha256
        (base32
         "1ssg6lgyr36q7v0p25xpj7wgvz8qxq1cvnlvn5p3r1q8mj93048f"))))
    (build-system node-build-system)
    (arguments
     `(#:global? #t
       #:node ,node-0.3.0
       #:phases
       (modify-phases %standard-phases
         ,(delete-generated-js-phase "lib")
         ,(coffee-script-build-phase "bin/coffee" "lib" "-c")
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin/"))
                    (libdir
                     (string-append out
                                    "/lib/node_modules/coffee-script/lib")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("NODE_PATH" ":" prefix (,libdir))))
                         (find-files bindir))
               #t)))
         (replace 'check
           (lambda* _ 
             (zero? (system* "cake" "test")))))))
    (inputs
     `(("node" ,node-0.3.0)))
    (native-inputs
     `(("coffee-script" ,coffee-script-boot50)))
    (synopsis "CoffeeScript is a little language that compiles into
JavaScript.")
    (description "CoffeeScript is a little language that compiles into
JavaScript.  Think of it as JavaScript's less ostentatious kid brother -- the
same genes, roughly the same height, but a different sense of style.  Apart
from a handful of bonus goodies, statements in CoffeeScript correspond
one-to-one with their equivalent in JavaScript, it's just another way of
saying it.")
    (home-page "http://jashkenas.github.com/coffee-script/")
    (license expat)))
