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
                       (dir (string-append out "/lib/node_modules/coffee-script/" ,lib-directory))
                       ;; FIXME: Use (bootstrapped) jison
                       (jison-vendor (string-append
                                      out
                                      "/lib/node_modules/coffee-script/vendor/jison/lib")))
                  (wrap-program bin
                    `("NODE_PATH" ":" prefix (,dir ,jison-vendor)))
                  #t))))


(define (coffee-script-build-helper-backport backport-files)
  `(add-after 'unpack 'backport-node
              (lambda _
                (substitute* ',backport-files
                  (("#!/usr/bin/env node --") "#!/usr/bin/env node")
                  (("require\\.paths.*$") "") ;;to prevent unshift funny business
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

(define* (coffee-script-build-helper-build cs-file target-dir #:optional (flags #f))
  `(replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cs (assoc-ref inputs "coffee-script"))
                   (coffee (string-append cs "/" ,cs-file)))
               ,(if flags
                    `(zero? (apply system* coffee ,flags "-o" ,target-dir (find-files "src" ".*\\.coffee$")))
                    `(zero? (apply system* coffee "-o" ,target-dir (find-files "src" ".*\\.coffee$")))
                    )))))


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
                         "01yy0l5f4a51jxg2bxc6c69b08c72jz41ldgksvzsk0jh3cg0s8f")))))))

(define-public coffee-script-boot1
  (let ((commit
         "e4bb6c91e70cd8386d76040d65e0669dd6d9a255"))
    (package (inherit coffee-script-boot0)
             (name "coffee-script-boot1")
             (version (string-append "0.3.2." (string-take commit 7)))
             (source (origin
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
                           (delete-file "src/narwhal/coffee-script.coffee")
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
                  ,(coffee-script-build-helper-generated-files "lib/coffee_script")
                  ,(coffee-script-build-helper-wrap  "bin/node_coffee" "lib/coffee_script") 
                  ,(coffee-script-build-helper-build "bin/coffee" "lib/coffee_script"))))
             (inputs
              `(("node" ,node-0.1.29)))
             (native-inputs
              `(("coffee-script" ,coffee-script-yolo)
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
                           (delete-file "src/narwhal.coffee")))))
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.1.30
                #:phases
                (modify-phases %standard-phases
                  ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                  ,(coffee-script-build-helper-generated-files "lib")
                  ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                  ,(coffee-script-build-helper-build "bin/node_coffee" "lib"))))
             (inputs
              `(("node" ,node-0.1.30)))
             (native-inputs
              `(("coffee-script" ,coffee-script-boot1))))))

(define-public coffee-script-0.5.2
  (package (inherit coffee-script-boot2)
           (name "coffee-script")
           (version (string-append "0.5.2"))
           (source (origin
                     (method url-fetch)
                     (uri (string-append "https://github.com/jashkenas/coffeescript/archive/"
                                         version ".tar.gz"))
                     (sha256
                      (base32
                       "0sv3sq3wf0c79hqb3nvx2xwf1fkpnj6c3861qyqwc31i84fza4c3"))))
           (native-inputs
            `(("coffee-script" ,coffee-script-boot1)))))

(define-public coffee-script-between
  (let ((commit
         "1c7e4c4203fef1ae8f6ae58ddaefdd9e5801226a"))
    (package (inherit coffee-script-0.5.2)
             (name "coffee-script-between")
             (version (string-append "0.5.2." (string-take commit 7)))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jashkenas/coffeescript.git")
                             (commit commit)))
                       (sha256
                        (base32
                         "00aws51vp3kak29kwbjzzbf5n1hwrymsljfjc8qh4dhvndcz61rl"))))
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.1.30
                #:phases
                (modify-phases %standard-phases
                  ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                  ,(coffee-script-build-helper-generated-files "lib")
                  ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                  ,(coffee-script-build-helper-build "bin/coffee" "lib")
                  )))
             (native-inputs
              `(("coffee-script" ,coffee-script-0.5.2))))))


(define-public coffee-script-0.5.3
  (package (inherit coffee-script-0.5.2)
           (name "coffee-script")
           (version (string-append "0.5.3"))
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
                         (delete-file "src/narwhal.coffee")))))
           (arguments
            `(#:tests? #f
              #:global? #t
              #:node ,node-0.1.30
              #:phases
              (modify-phases %standard-phases
                ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                ,(coffee-script-build-helper-generated-files "lib")
                ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c"))))
           (native-inputs
            `(("coffee-script" ,coffee-script-boot2)))))


(define-public coffee-script-test
  (package (inherit coffee-script-0.5.2)
           (name "coffee-script-test")
           (version (string-append "0.5.3"))
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
                         (delete-file "src/narwhal.coffee")))))
           (arguments
            `(#:tests? #f
              #:global? #t
              #:node ,node-0.1.30
              #:phases
              (modify-phases %standard-phases
                ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                ,(coffee-script-build-helper-generated-files "lib")
                ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c"))))
           (native-inputs
            `(("coffee-script" ,coffee-script-0.5.3)))
           ))

(define-public coffee-script-0.5.4
  (package (inherit coffee-script-boot2)
           (name "coffee-script")
           (version (string-append "0.5.4"))
           (source (origin
                     (method url-fetch)
                     (uri (string-append "https://github.com/jashkenas/coffeescript/archive/"
                                         version ".tar.gz"))
                     (sha256
                      (base32
                       "05n7yq1qs8h4934mkln1k577yg6f5dn77sdbznva47gf4srsggna"))
                     ))
           (arguments
            `(#:tests? #f
              #:global? #t
              #:node ,node-0.1.31
              #:phases
              (modify-phases %standard-phases
                ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                ,(coffee-script-build-helper-generated-files "lib")
                ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c")
                )))
           (inputs
            `(("node" ,node-0.1.31)))
           (native-inputs
            `(("coffee-script" ,coffee-script-0.5.3)))))

(define-public coffee-script-0.5.5
  (package (inherit coffee-script-0.5.4)
           (name "coffee-script")
           (version (string-append "0.5.5"))
           (source (origin
                     (method url-fetch)
                     (uri (string-append "https://github.com/jashkenas/coffeescript/archive/"
                                         version ".tar.gz"))
                     (sha256
                      (base32
                       "0ddghl4ax704hpzqd94q4s7lyngjmhvqzbki0lz4gc61k3320df3"))
                     ))
           (arguments
            `(#:tests? #f
              #:global? #t
              #:node ,node-0.1.31
              #:phases
              (modify-phases %standard-phases
                ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                ,(coffee-script-build-helper-generated-files "lib")
                ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c")
                )))
           (inputs
            `(("node" ,node-0.1.31)))
           (native-inputs
            `(("coffee-script" ,coffee-script-0.5.4)))))

(define-public coffee-script-boot3
  (let ((commit "0834128"))
    (package (inherit coffee-script-0.5.4)
             (name "coffee-script-boot3")
             (version (string-append "0.5.4." (string-take commit 7)))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jashkenas/coffeescript.git")
                             (commit commit)))
                       (sha256
                        (base32
                         "1cc0qd2qs46yrkwmlccy8byslzb9xkjx2knxwmjp2fq8rv06flrk"
                         ))
                       ))
             (native-inputs
              `(("coffee-script" ,coffee-script-0.5.4))))))

(define (coffee-replicate pkg-def key name)
  (let ((ni (package-native-inputs pkg-def))) 
    (package (inherit pkg-def)
             (name name)
             (native-inputs
              (cons `(,key ,pkg-def) ni)))))

(define* (bootstrap prev nname #:optional (node #f))
  (lambda (commit hash) 
    (let ((node (or node
                    (cadr (assoc "node" (package-inputs prev))))))
      (package (inherit prev)
               (name nname)
               (version (string-append "0.5.4." (string-take commit 7)))
               (source (origin
                         (method git-fetch)
                         (uri (git-reference
                               (url "https://github.com/jashkenas/coffeescript.git")
                               (commit commit)))
                         (sha256 (base32 hash))
                         (file-name (string-append "coffeescript-" version "-checkout"))
                         ))
               (arguments
                `(#:tests? #f
                  #:global? #t
                  #:node ,node
                  #:phases
                  (modify-phases %standard-phases
                    ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
                    ,(coffee-script-build-helper-generated-files "lib")
                    ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
                    ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c")
                    )))
               (inputs
                `(("node" ,node)))
               (native-inputs
                `(("coffee-script" ,prev)
                  ))))))

(define* (patch-sources #:key prev (snippet #f) (patch #f))
  (let ((base-origin (package-source prev))
        (patches (if patch (search-patches patch) '())))
    (package (inherit prev)
             (source
              (origin (inherit base-origin)
                      (modules '((guix build utils)))
                      (snippet snippet)
                      (patches patches))))))

(define boot4 (bootstrap coffee-script-boot3 "coffee-script-boot4"))

(define-public coffee-script-boot4 (boot4
                                    "47682de"
                                    "01r35xfd5mvv66zlm2lmhdj0dfjib58w8nlacyavq4cs6ycskc56"))

(define-public boot4-check (coffee-replicate coffee-script-boot4 "coffee-script" "coffee-script-boot4r" ))

(define boot5 (bootstrap coffee-script-boot4 "coffee-script-boot5"))
(define-public coffee-script-boot5 (boot5
                                    "90f2e0d"
                                    "1j9g35kgcwk6a0p17rxg9p3gw49ch5qx6wj8ypwf0wj419i4msd9"))

(define-public boot5-check (coffee-replicate coffee-script-boot5 "coffee-script" "coffee-script-boot5r" ))

(define boot6 (bootstrap coffee-script-boot5 "coffee-script-boot6"))
(define-public coffee-script-boot6 (boot6
                                    "b726416"
                                    "00aws51vp3kak29kwbjzzbf5n1hwrymsljfjc8qh4dhvndcz61rl"
                                    ))

(define-public boot6-check (coffee-replicate coffee-script-boot6 "coffee-script" "coffee-script-boot6r" ))

(define boot7 (bootstrap coffee-script-boot6 "coffee-script-boot7"))
(define-public coffee-script-boot7 (boot7
                                    "cbfe7f5"
                                    "1w9ml532nj3i9xklm9klfrwllapck84j5j8v4ymiblh0cg4lbbfm"
                                    ))

(define-public boot7-check (coffee-replicate coffee-script-boot7 "coffee-script" "coffee-script-boot7r" ))


(define boot8 (bootstrap coffee-script-boot7 "coffee-script-boot8"))
(define-public coffee-script-boot8 (boot8
                                    "177ec92"
                                    "02d8xj557nygvax6sv9gmhq8p2kfw7y5w1gfz2wc0imbxlw148dy"
                                    ))

(define-public boot8-check (coffee-replicate coffee-script-boot8 "coffee-script" "coffee-script-boot8r" ))


(define boot9 (bootstrap coffee-script-boot8 "coffee-script-boot9"))
(define-public coffee-script-boot9 (boot9
                                    "572aa4e"
                                    "15aw9lbrldwwpyq9fb6s48h0p91w73q67zqlw034i85dr0bxah43"
                                    ))

(define-public boot9-check (coffee-replicate coffee-script-boot9 "coffee-script" "coffee-script-boot9r" ))

(define boot10 (bootstrap coffee-script-boot9 "coffee-script-boot10"))

(define-public coffee-script-boot10 (boot10
                                     "c067808"
                                     "1six1n162xhsjk2h49m4ccw0wma32z2fl4y8pib72s4lk060zcx3"
                                    ))

(define-public boot10-check (coffee-replicate coffee-script-boot10 "coffee-script" "coffee-script-boot10r" ))
(define-public boot10-checkd (coffee-replicate boot10-check "coffee-script" "coffee-script-boot10rr" ))


(define boot11 (bootstrap coffee-script-boot10 "coffee-script-boot11" node-0.1.90))
(define-public coffee-script-boot11 (boot11
                                     "8317960"
                                     "124wqh0w2i7p8q75qyjylxb4pdhw1z06z5wh6bvjsa47jvlh8d3i"
                                     ))

(define-public boot11-check (coffee-replicate coffee-script-boot11 "coffee-script" "coffee-script-boot11r" ))


(define boot12 (bootstrap coffee-script-boot11 "coffee-script-boot12"))
(define-public coffee-script-boot12 (boot12
                                     "a894db3"
                                     "19cbqp8hjcrdlzxrb9lxzx9lz72y8pydz5v8m6731pdvgrr1b75p"
                                     ))

(define-public boot12-check (coffee-replicate coffee-script-boot12 "coffee-script" "coffee-script-boot12r" ))


(define boot13 (bootstrap coffee-script-boot12 "coffee-script-boot13"))
(define-public coffee-script-boot13 (boot13
                                     "b746c90"
                                     "1vwnqi1m2h31m03xn7flp949x5wn7dqp3rp31dn05z54i1p3gxpi"
                                     ))

(define-public boot13-check (coffee-replicate coffee-script-boot13 "coffee-script" "coffee-script-boot13r" ))


(define boot14 (bootstrap coffee-script-boot13 "coffee-script-boot14" node-0.1.95))
(define-public coffee-script-boot14 (boot14
                                     "a133e01"
                                     "08dbl57b2l9pzfdmsdal7l33mg09ps3i1xb6qbg0s6j3r6ym51yd"
                                     ))

(define-public boot14-check (coffee-replicate coffee-script-boot14 "coffee-script" "coffee-script-boot14r" ))


;; 15 broken: have to split commit
(define boot15 (bootstrap coffee-script-boot14 "coffee-script-boot15-temp" node-0.1.98))
(define coffee-script-boot15-temp (boot15
                                     "0222d90"
                                     "18rn6s7xxjmsd3696c0gpvm1m028lcmd2kyladybw5las4d3cp0z"
                                     ))

(define-public coffee-script-boot15
  (let ((support-origin (package-source coffee-script-boot15-temp)))
    (package (inherit coffee-script-boot15-temp)
             (name "coffee-script-boot15")
             (source (origin (inherit support-origin)
                             (modules '((guix build utils)))
                             (snippet
                              '(begin
                                 (substitute* '("src/lexer.coffee" "src/rewriter.coffee")
                                   (("loop\n")
                                    "while true\n")))))))))

(define-public boot15-check (coffee-replicate coffee-script-boot15 "coffee-script" "coffee-script-boot15r" ))

(define-public coffee-script-boot16
  (package (inherit coffee-script-boot15-temp)
           (name "coffee-script-boot16")
           (native-inputs
            `(("coffee-script" ,coffee-script-boot15))
            )))

(define-public boot16-check (coffee-replicate coffee-script-boot16 "coffee-script" "coffee-script-boot16r" ))


(define boot17 (bootstrap coffee-script-boot16 "coffee-script-boot17"))
(define-public coffee-script-boot17 (boot17
                                   "4b284f6"
                                   "0bgqmhxw0vyhibdgi1pkljdx8qd548jcap9vd1fhrhm4r5kj625w"
                                   ))

(define-public boot17-check (coffee-replicate coffee-script-boot17 "coffee-script" "coffee-script-boot17r" ))


(define-public coffee-script-boot18
  (let ((support-origin (package-source coffee-script-boot17)))
    (package (inherit coffee-script-boot17)
             (name "coffee-script-boot18")
             (source (origin (inherit support-origin)
                             (modules '((guix build utils)))
                             (snippet
                              '(begin
                                 (substitute* '("src/lexer.coffee")
                                   (("^([ ]+)@tokens\\.pop\\(\\)\n" all spaces)
                                    (string-append spaces "@tokens.pop() if @tag() isnt '@'\n")
                                    )))))))))


(define boot19 (bootstrap coffee-script-boot18 "coffee-script-boot19"))
(define-public coffee-script-boot19 (boot19
                                     "0fcfb80"
                                     "075xqmyivhfnv4v53jqz6migrxygx4ri4cikgaykvi9gy3dhli78"
                                     ))

(define-public boot19-check (coffee-replicate coffee-script-boot19 "coffee-script" "coffee-script-boot19r" ))


(define boot20 (bootstrap coffee-script-boot19 "coffee-script-boot20"))
(define-public coffee-script-boot20 (boot20
                                     "7c426db"
                                     "0ibsy7lcygvkg0pf1a4g19jh70f5hj8x0jyqybnxc1ix403in1rz"
                                     ))

(define-public boot20-check (coffee-replicate coffee-script-boot20 "coffee-script" "coffee-script-boot20r" ))

;;
;;o "NEW Invocation",                         -> $2.newInstance()
;;o "NEW Value",                              -> (new CallNode($2, [])).newInstance()

(define boot21 (bootstrap coffee-script-boot20 "coffee-script-boot21"))
(define coffee-script-boot21-helper (boot21 "7d79d73" 
                                            "1cppvxnvqip5yy7ms4p98xyydycg0iczshl75rbk1bm58n9cpca4"))

(define-public coffee-script-boot21
  (patch-sources #:prev coffee-script-boot21-helper
                 #:snippet
                 '(begin
                    (substitute* '("src/grammar.coffee" "src/coffee-script.coffee"
                                   "src/lexer.coffee" "src/nodes.coffee")
                      (("(new [a-zA-Z]+)\n" all identifier)
                       (string-append identifier "()\n"))
                      (("\\((new [a-zA-Z]+)\\)" all identifier)
                       (string-append "(" identifier "())"))))))

;; (define-public coffee-script-boot21
;;   (let ((so (package-source coffee-script-boot21-helper))) 
;;     (package (inherit coffee-script-boot21-helper)
;;              (source
;;               (origin (inherit so)
;;                       (modules '((guix build utils)))
;;                       (snippet
;;                        '(begin
;;                           (substitute* '("src/grammar.coffee" "src/coffee-script.coffee"
;;                                          "src/lexer.coffee" "src/nodes.coffee")
;;                             (("(new [a-zA-Z]+)\n" all identifier)
;;                              (string-append identifier "()\n"))
;;                             (("\\((new [a-zA-Z]+)\\)" all identifier)
;;                              (string-append "(" identifier "())"))))))))))

(define-public boot21-check (coffee-replicate coffee-script-boot21 "coffee-script" "coffee-script-boot21r" ))


(define boot22 (bootstrap coffee-script-boot21 "coffee-script-boot22"))
(define-public coffee-script-boot22 (boot22
                                     "2a93259"
                                     "0ynprqg3frhm6lw1aflmjzv7gsxy6g357j8ydi0rm3hczc40pgv3"
                                     ))

(define-public boot22-check (coffee-replicate coffee-script-boot22 "coffee-script" "coffee-script-boot22r" ))


;;-  for name, task of tasks
;;+  for all name, task of tasks


(define boot23 (bootstrap coffee-script-boot22 "coffee-script-boot23"))
(define coffee-script-boot23-helper (boot23
                                     "72c4efb"
                                     "1iafv758mrh1b78qaciny87mk35qcd5r2m08nr19jmxi1bpxk0bf"
                                     ))

(define-public coffee-script-boot23
  (patch-sources #:prev coffee-script-boot23-helper
                 #:snippet
                 '(begin
                    (substitute* "src/cake.coffee"
                      (("([ ]+)for all name, task of tasks" all spaces)
                       (string-append spaces "for name, task of tasks")))
                    (substitute* "src/helpers.coffee"
                      (("for all key, val") "for key, val")))))

(define-public boot23-check (coffee-replicate coffee-script-boot23 "coffee-script" "coffee-script-boot23r" ))

(define boot24 (bootstrap coffee-script-boot23 "coffee-script-boot24" node-0.1.101))
(define-public coffee-script-boot24 (boot24
                                     "88847df"
                                     "17kb30sp85cddxbv20vc2nj2i2rk1j5fb7b6d704dpb9v7pa05py"
                                     ))

(define-public boot24-check (coffee-replicate coffee-script-boot24 "coffee-script" "coffee-script-boot24r" ))


(define boot25 (bootstrap coffee-script-boot24 "coffee-script-boot25"))
(define coffee-script-boot25-helper (boot25
                                     "f9dff6f"
                                      "0smpnx42bx4bkvaa5km32klsxpwrad2viz990c7g19v10igcpg6c"
                                      ))

(define-public coffee-script-boot25
  (patch-sources #:prev coffee-script-boot25-helper
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

(define-public boot25-check (coffee-replicate coffee-script-boot25 "coffee-script" "coffee-script-boot25r" ))


(define boot26 (bootstrap coffee-script-boot25 "coffee-script-boot26"))
(define coffee-script-boot26-helper (boot26 "9c8a228"
                                     "1ndfclfbdpqqpf18ixmx8h5v2ssh87hndhchxg5zwk0np3rydz3c"))

(define-public coffee-script-boot26
  (patch-sources #:prev coffee-script-boot26-helper
                 #:patch "coffeescript-backport-dollar-string-interpolation.patch"))

(define-public boot26-check (coffee-replicate coffee-script-boot26 "coffee-script" "coffee-script-boot26r" ))

(define boot27 (bootstrap coffee-script-boot26 "coffee-script-boot27"))
(define-public coffee-script-boot27 (boot27
                                     "9be1453"
                                     "1586pfw4nn3sfkcqkbyisna2cwympd8cyqp4dz7s6ggi773nvqm8"
                                            ))
(define-public boot27-check (coffee-replicate coffee-script-boot27 "coffee-script" "coffee-script-boot27r" ))


(define boot28 (bootstrap coffee-script-boot27 "coffee-script-boot28"))
(define coffee-script-boot28-helper (boot28
                                     "c9421cb"
                                     "17v1c2p3yf2caplyn5xq7sk667yrh1f5g5m8wb6z6zclmahnfy52"))

(define-public coffee-script-boot28
  (patch-sources #:prev coffee-script-boot28-helper
                 #:patch "coffeescript-backport-prefixed-splats.patch"))
(define-public boot28-check (coffee-replicate coffee-script-boot28 "coffee-script" "coffee-script-boot28r" ))


(define boot29 (bootstrap coffee-script-boot28 "coffee-script-boot29"))
(define-public coffee-script-boot29 (boot29
                                     "129e950"
                                     "11r6mbnjvcyfmhz5xi9w9d9h0xhl12b6jh7w1i960jlzv8xrfb5v"))
(define-public boot29-check (coffee-replicate coffee-script-boot29 "coffee-script" "coffee-script-boot29r" ))


(define boot30 (bootstrap coffee-script-boot29 "coffee-script-boot30"))
(define-public coffee-script-boot30 (boot30
                                     "4ddd65a"
                                     "13zw49b373rkz1ynhwgb41q0n6rdazwbc23crmzgypdsm7cp7lw5"))
(define-public boot30-check (coffee-replicate coffee-script-boot30 "coffee-script" "coffee-script-boot30r" ))

(define boot31 (bootstrap coffee-script-boot30 "coffee-script-boot31"))
(define coffee-script-boot31-helper (boot31
                                     "87693d8"
                                     "156rs10q5pg53aqz6xf97vmcz6cjsg02m3m5lgxmlivsrmrfx93x"))

(define-public coffee-script-boot31
  (patch-sources #:prev coffee-script-boot31-helper
                 #:patch "coffeescript-backport-not-instanceof.patch"))
(define-public boot31-check (coffee-replicate coffee-script-boot31 "coffee-script" "coffee-script-boot31r" ))


(define boot32 (bootstrap coffee-script-boot31 "coffee-script-boot32"))
(define-public coffee-script-boot32 (boot32
                                     "b2313be"
                                     "02kp2584ixxvwy1afsxrnpfwds66yqrcc6gbgvw4f03ygcx321hn"
                                     ))

(define-public boot32-check (coffee-replicate coffee-script-boot32 "coffee-script" "coffee-script-boot32r" ))


(define boot33 (bootstrap coffee-script-boot32 "coffee-script-boot33"))
(define-public coffee-script-boot33 (boot33
                                     "493fa7d"
                                     "047qnx6qvrb3rypdl9ksn3pvr2hp9wgj99mw1six7gqhyc7ybf0l"))

(define-public boot33-check (coffee-replicate coffee-script-boot33 "coffee-script" "coffee-script-boot33r" ))


(define boot34 (bootstrap coffee-script-boot33 "coffee-script-boot34"))
(define coffee-script-boot34-helper (boot34
                                     "380bee9"
                                     "0mn0gkp5hgmnv130vgr8affiidhk1ybr9xr2r4ah241v61563lis"))


(define-public coffee-script-boot34
  (patch-sources #:prev coffee-script-boot34-helper
                 #:patch "coffeescript-backport-lexer-inof-forinof.patch"))

(define-public boot34-check (coffee-replicate coffee-script-boot34 "coffee-script" "coffee-script-boot34r" ))


(define boot35 (bootstrap coffee-script-boot34 "coffee-script-boot35"))
(define coffee-script-boot35-helper (boot35
                                     "acafb1b"
                                     "1ml8haprj0akifvnd26lsijp78s42j80bq77wh8rpamiv50bkqmf"
                                     ))

;;coffeescript-backport-if-else-chain.patch
(define-public coffee-script-boot35
  (patch-sources #:prev coffee-script-boot35-helper
                 #:patch "coffeescript-backport-if-else-chain.patch"))

(define-public boot35-check (coffee-replicate coffee-script-boot35 "coffee-script" "coffee-script-boot35r" ))
;;~/Projects/guix/gnu/packages/patches/coffeescript-backport-lexer-inof-forinof.patch

(define boot36 (bootstrap coffee-script-boot35 "coffee-script-boot36"))
(define-public coffee-script-boot36 (boot36
                                     "2f7c076"
                                     "1gccmx8wi4s06vdr157h6lmqdqnjajs7brcri2f80pkbkx258frf"
                                     ))

(define-public boot36-check (coffee-replicate coffee-script-boot36 "coffee-script" "coffee-script-boot36r" ))

(define boot37 (bootstrap coffee-script-boot36 "coffee-script-boot37"))
(define-public coffee-script-boot37 (boot37
                                     "a8da321"
                                     "1rngdd4y4f7mghbh7mm221d8ss6qaz9ksbd2hj7qk2gjvxm53y4v"
                                     ))

(define-public boot37-check (coffee-replicate coffee-script-boot37 "coffee-script" "coffee-script-boot37r" ))


(define boot38 (bootstrap coffee-script-boot37 "coffee-script-boot38" ;;node-0.3.0
                          ))
(define-public coffee-script-boot38 (boot38
                                     "bfc236f"
                                     "0nf4p2mzdbh847f88nxk6i4ynvjk35bz7x3zjq3gvzsilmkq0l4v"
                                     ))

(define-public boot38-check (coffee-replicate coffee-script-boot38 "coffee-script" "coffee-script-boot38r" ))


(define boot39 (bootstrap coffee-script-boot38 "coffee-script-boot39" node-0.3.0))
(define coffee-script-boot39-helper (boot39
                                     "85c8a67"
                                     "1w08a6xc29xx7147ad65f7zf5d72ijj8rv75pz9avbnhy65afp15"))

(define-public coffee-script-boot39
  (patch-sources #:prev coffee-script-boot39-helper
                 #:patch "coffeescript-backport-destructuring.patch"))

(define-public boot39-check (coffee-replicate coffee-script-boot39 "coffee-script" "coffee-script-boot39r" ))

(define boot40 (bootstrap coffee-script-boot39 "coffee-script-boot40"))
(define-public coffee-script-boot40 (boot40 ;;has a broken repl, fixed in later commit
                                     "b32a758"
                                     "0ki3cp8yj1jy5sihyfzc09i7ll1w34pwkrjmmdjkmhhnf2wp6mw2"
                                     ))

(define-public boot40-check (coffee-replicate coffee-script-boot40 "coffee-script" "coffee-script-boot40r" ))


(define boot41 (bootstrap coffee-script-boot40 "coffee-script-boot41"))
(define coffee-script-boot41-helper (boot41
                                     "b2be475"
                                     "1kj9wslyxddnb32i72y9jjx4701vjihyg2nk3fh7vj17qh23hd0z"))


(define-public coffee-script-boot41
  (patch-sources #:prev coffee-script-boot41-helper
                 #:patch "coffeescript-backport-leading-then-expression.patch"))
;;coffeescript-backport-leading-then-expression.patch
(define-public boot41-check (coffee-replicate coffee-script-boot41 "coffee-script" "coffee-script-boot41r" ))


(define boot42 (bootstrap coffee-script-boot41 "coffee-script-boot42"))
(define coffee-script-boot42-helper (boot42
                                     "ecd4722"
                                     "0wal456lzsndknca347g8z1va10k5999dd788wc7lrib2zl2yawb"
                                     ))
(define-public coffee-script-boot42
  (patch-sources #:prev coffee-script-boot42-helper
                                        #:patch "coffeescript-backport-default-arguments.patch"
                 ))
;coffeescript-backport-default-arguments.patch


(define-public boot42-check (coffee-replicate coffee-script-boot42 "coffee-script" "coffee-script-boot42r" ))


(define boot43 (bootstrap coffee-script-boot42 "coffee-script-boot43"))
(define coffee-script-boot43-helper (boot43
                                     "a2d3311"
                                     "0qqj2jjr9lys9mvzk49hck588yh1qijlv80wab3gyjxpfj8n9anj"
                                     ))

(define-public coffee-script-boot43
  (patch-sources #:prev coffee-script-boot43-helper
                 #:patch "coffeescript-backport-executable-class-body.patch"
                 ))
;;might not be able to replicate
;;(define-public boot43-check (coffee-replicate coffee-script-boot43 "coffee-script" "coffee-script-boot43r" ))

(define boot44 (bootstrap coffee-script-boot43 "coffee-script-boot44"))
(define-public coffee-script-boot44 (boot44
                                     "3059db8"
                                     "0cpqrnk2c6rpkip407xs4ly3rw0lv47gq6lgf34vsm5bv8spsrh9"))

(define-public boot44-check (coffee-replicate coffee-script-boot44 "coffee-script" "coffee-script-boot44r" ))


(define boot45 (bootstrap coffee-script-boot44 "coffee-script-boot45"))
(define coffee-script-boot45-helper (boot45
                                     "2aedbc2"
                                     "100qs8x25dbvf4mwj7kqz4j7p1py4917ykqndwmaagg47nyanrxv")) 

(define-public coffee-script-boot45
  (patch-sources #:prev coffee-script-boot45-helper
                 #:patch "coffeescript-backport-static-value-syntax.patch"
                 ))

(define-public boot45-check (coffee-replicate coffee-script-boot45 "coffee-script" "coffee-script-boot45r" ))

(define boot46 (bootstrap coffee-script-boot45 "coffee-script-boot46"))
(define coffee-script-boot46-helper (boot46
                                     "15bdcf7"
                                     "0jmlk3v9nhm1r0npx6dydaw8qsv2sndx0cfqbcpvybrsxhrbxr7q"))
(define-public coffee-script-boot46
  (patch-sources #:prev coffee-script-boot46-helper
                 #:patch "coffeescript-backport-explicit-constructor-syntax.patch"
                 ))
;;might not be able to replicate
;;(define-public boot46-check (coffee-replicate coffee-script-boot46 "coffee-script" "coffee-script-boot46r" ))

(define boot47 (bootstrap coffee-script-boot46 "coffee-script-boot47"))
(define-public coffee-script-boot47 (boot47
                                     "a1aaa44"
                                     "18f05apxvqqz3rdh3sxczdi3vw5nm9q29qs20idifgmf37j8dcr3"))

(define-public boot47-check (coffee-replicate coffee-script-boot47 "coffee-script" "coffee-script-boot47r" ))

(define boot48 (bootstrap coffee-script-boot47 "coffee-script-boot48"))
(define coffee-script-boot48-helper (boot48
                                     "9f708ad"
                                     "17bkbxhf5gar8g5ciaqwmxswhz7p0dcp07vycw79wk0v2iq2q974"))
(define-public coffee-script-boot48
  (patch-sources #:prev coffee-script-boot48-helper
                 #:patch "coffeescript-backport-range-literals.patch"
                 ))

;;might not be able to replicate
;;(define-public boot48-check (coffee-replicate coffee-script-boot48 "coffee-script" "coffee-script-boot48r" ))


(define boot49 (bootstrap coffee-script-boot48 "coffee-script-boot49"))
(define-public coffee-script-boot49 (boot49
                                     "fc64fa4"
                                     "00visn0j8zq11i9a5pc9f3ww06zwp8gn9y23linvyvxwh9fmmfyc"))

(define-public boot49-check (coffee-replicate coffee-script-boot49 "coffee-script" "coffee-script-boot49r" ))

(define boot50 (bootstrap coffee-script-boot49 "coffee-script-boot50"))
(define coffee-script-boot50-helper (boot50
                                     "4afa6a2"
                                     "1sdl8w63l36hs3v1dj8xcg0kal6i5kmmg33j1rv6wpphn77mwf1j"))

(define-public coffee-script-boot50
  (patch-sources #:prev coffee-script-boot50-helper
                 #:patch "coffeescript-backport-forall-loop-syntax-change.patch"
                 ))
;;might not be able to replicate
;;(define-public boot50-check (coffee-replicate coffee-script-boot50 "coffee-script" "coffee-script-boot50r" ))


;;;CS 1.0.0, we did it :-)
(define boot51 (bootstrap coffee-script-boot50 "coffee-script-boot51"))
(define-public coffee-script-boot51 (boot51
                                     "33d2577"
                                     "16dg3z2885qzzdzkldlvfcgd97lxrim6zs2bi5yyal2a9y3bfs8c"))

(define-public boot51-check (coffee-replicate coffee-script-boot51 "coffee-script" "coffee-script-boot51r" ))
