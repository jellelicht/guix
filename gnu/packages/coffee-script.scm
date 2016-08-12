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
                    ;'("bin/node_coffee" ;"bin/cake"
                    ;           "bin/coffee")
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
                ;(rename-file "vendor" "node_modules")
                ;;(for-each (lambda (file)
                            ;;(format #t "deleting vendored  file '~a'~%" file)
                            ;;(delete-file file))
                          ;;(find-files "./vendor" ".*"))
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
                              "fs.readFile")))
                        )))
             (build-system node-build-system)
             (arguments
              `(#:tests? #f
                #:global? #t
                #:node ,node-0.1.29
                #:phases
                (modify-phases %standard-phases
                  ;;,(coffee-script-build-helper-backport '("bin/node_coffee"))
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
                           (delete-file "src/narwhal.coffee")))
                       ))
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
              `(("coffee-script" ,coffee-script-boot1)
                ;;("ruby" ,ruby)
                )))))

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
                       ;;"05n7yq1qs8h4934mkln1k577yg6f5dn77sdbznva47gf4srsggna"
                       "0sv3sq3wf0c79hqb3nvx2xwf1fkpnj6c3861qyqwc31i84fza4c3"
                       ;;"0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"
                       ))
                     ))
           ;; (arguments
           ;;  `(#:tests? #f
           ;;    #:global? #t
           ;;    #:node ,node-0.1.31
           ;;    #:phases
           ;;    (modify-phases %standard-phases
           ;;      ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
           ;;      ,(coffee-script-build-helper-generated-files "lib")
           ;;      ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
           ;;      ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c")
           ;;      )))
           ;; (inputs
           ;;  `(("node" ,node-0.1.31)))
           (native-inputs
            `(("coffee-script" ,coffee-script-boot1)
             ;; ("ruby" ,ruby)
              ))))

(define-public coffee-script-between
  (let ((commit
         ;"44398d044ffae2465be54715da59625f1748b673"
         "1c7e4c4203fef1ae8f6ae58ddaefdd9e5801226a"
         ;;"fe7d5dfd1951bceaa440ab1ba27704759c078124"
         ;;"965034e16ec43b3462f76f6046e424aa869516d2"
         ;;"b4ea43cbd059ababdc8351f8a3d3eec3993fb453"
        ;; "4906cf1aff9dfe1e273ed2922ae5b9f43c01a17e"
         ))
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
                         "00aws51vp3kak29kwbjzzbf5n1hwrymsljfjc8qh4dhvndcz61rl"
                         ))
                       ))
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
             ;; (inputs
             ;;  `(("node" ,node-0.1.31)))
             (native-inputs
              `(("coffee-script" ,coffee-script-0.5.2)
               ;; ("ruby" ,ruby)
                )))))


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
                       ;;"05n7yq1qs8h4934mkln1k577yg6f5dn77sdbznva47gf4srsggna"
                       "0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"
                       ;;"0sv3sq3wf0c79hqb3nvx2xwf1fkpnj6c3861qyqwc31i84fza4c3"
                       ;;"0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"
                       ))
                     (modules '((guix build utils)))
                     (snippet
                      '(begin
                         (delete-file "src/narwhal.coffee")))
                     ))
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
            `(("coffee-script" ,coffee-script-boot2)
              ;; ("ruby" ,ruby)
              ))
           ))


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
                       ;;"05n7yq1qs8h4934mkln1k577yg6f5dn77sdbznva47gf4srsggna"
                       "0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"
                       ;;"0sv3sq3wf0c79hqb3nvx2xwf1fkpnj6c3861qyqwc31i84fza4c3"
                       ;;"0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"
                       ))
                     (modules '((guix build utils)))
                     (snippet
                      '(begin
                         (delete-file "src/narwhal.coffee")))
                     ))
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
            `(("coffee-script" ,coffee-script-0.5.3)
              ;; ("ruby" ,ruby)
              ))
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
                       "05n7yq1qs8h4934mkln1k577yg6f5dn77sdbznva47gf4srsggna"
                       ;;"0zaflcffgb2l1linp1z9bk9ch6dq47kcj9lpm3iis9f1na1iknar"
                       ))
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
            `(("coffee-script" ,coffee-script-0.5.3)
              ;;("ruby" ,ruby)
              ))))

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
                       "0ddghl4ax704hpzqd94q4s7lyngjmhvqzbki0lz4gc61k3320df3"
                       ;;"00aws51vp3kak29kwbjzzbf5n1hwrymsljfjc8qh4dhvndcz61rl"
                       ))
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
            `(("coffee-script" ,coffee-script-0.5.4)
              ;;("ruby" ,ruby)
              )
            )))

(define-public coffee-script-boot3
  (let ((commit
         ;"44398d044ffae2465be54715da59625f1748b673"
         ;;"62b2ab29cd1c47c4b8d287c2c08846a0aadb5828"
         ;"965034e16ec43b3462f76f6046e424aa869516d2"
         ;;"fe7d5dfd1951bceaa440ab1ba27704759c078124"
         ;;"c6d7a27"
         ;"5b3ef78"
         ;;kj"15b00cb"
         ;;"4c3b0b9"
         ;;"b4ea43c"
         "0834128"
         ;;"fe7d5dfd1951bceaa440ab1ba27704759c078124"
         ;;"965034e16ec43b3462f76f6046e424aa869516d2"
         ;;"b4ea43cbd059ababdc8351f8a3d3eec3993fb453"
        ;; "4906cf1aff9dfe1e273ed2922ae5b9f43c01a17e"
         ))
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
                         ;;"0cg7piw4awfqpalzhrg8crj8gmwmn0qxqiizvn5i49m60sy0pwiy"
                         ;;"0411ildnadhbh7fgri48mprs3p5dzp61x33i8jia32qlg6lixfzc"
                         ;;"1168p4mxa3sk3b6sgyqfy5pdc0q8va0vcpsq6742ffbsjscsjsf4"
                         ;;"1pix34ssfclihzgff992jiv80pil0ag8b2gyqvgga33h22hf1wb0";;
                         ;;"1w1kklan1am9lvlpj0ysjmshhr854rgjnlh4fascfy0qbjy28g9a"
                         ;;"1ajdl2f0d8a7k0jh7qqrrj9b762isrmyrdi6ygxwck9dk14rpbc7"
                         ;;"0bawlb577jbck003p3z1bpwsibwk58bkdlssaxdams9binjr0rjd"
                         "1cc0qd2qs46yrkwmlccy8byslzb9xkjx2knxwmjp2fq8rv06flrk"
                         ;; "00aws51vp3kak29kwbjzzbf5n1hwrymsljfjc8qh4dhvndcz61rl"
                         ))
                       ))
             ;; (arguments
             ;;  `(#:tests? #f
             ;;    #:global? #t
             ;;    #:node ,node-0.1.31
             ;;    #:phases
             ;;    (modify-phases %standard-phases
             ;;      ,(coffee-script-build-helper-backport '("bin/coffee" "bin/cake"))
             ;;      ,(coffee-script-build-helper-generated-files "lib")
             ;;      ,(coffee-script-build-helper-wrap  "bin/coffee" "lib")
             ;;      ,(coffee-script-build-helper-build "bin/coffee" "lib" "-c")
             ;;      )))
             ;; (inputs
             ;;  `(("node" ,node-0.1.31)))
             (native-inputs
              `(("coffee-script" ,coffee-script-0.5.4)
                ;;("ruby" ,ruby)
                )))))

(define (coffee-replicate pkg-def key name)
  (let ((ni (package-native-inputs pkg-def))) 
    (package (inherit pkg-def)
             (name name)
             (native-inputs
              (cons `(,key ,pkg-def) ni)))))

;; (define (boot4 commit hash)
;;   (package (inherit coffee-script-boot3)
;;              (name "coffee-script-boot4")
;;              (version (string-append "0.5.4." (string-take commit 7)))
;;              (source (origin
;;                        (method git-fetch)
;;                        (uri (git-reference
;;                              (url "https://github.com/jashkenas/coffeescript.git")
;;                              (commit commit)))
;;                        (sha256 (base32 hash))))
;;              (native-inputs
;;               `(("coffee-script" ,coffee-script-boot3)
;;                 ))))

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
                                     ;;"065bf54"
                                     ;;"0hh6ds6qqf1jgigbkfl876hav7sysg12dxi9nlyk609vr9fff6af"
                                     "8317960"
                                     "124wqh0w2i7p8q75qyjylxb4pdhw1z06z5wh6bvjsa47jvlh8d3i"

                                     ))

(define-public boot11-check (coffee-replicate coffee-script-boot11 "coffee-script" "coffee-script-boot11r" ))


(define boot12 (bootstrap coffee-script-boot11 "coffee-script-boot12"))
(define-public coffee-script-boot12 (boot12
                                     ;;"065bf54"
                                     ;;"0hh6ds6qqf1jgigbkfl876hav7sysg12dxi9nlyk609vr9fff6af"
                                     "a894db3"
                                     "19cbqp8hjcrdlzxrb9lxzx9lz72y8pydz5v8m6731pdvgrr1b75p"
                                     ))

(define-public boot12-check (coffee-replicate coffee-script-boot12 "coffee-script" "coffee-script-boot12r" ))


(define boot13 (bootstrap coffee-script-boot12 "coffee-script-boot13"))
(define-public coffee-script-boot13 (boot13
                                     ;;"065bf54"
                                     ;;"0hh6ds6qqf1jgigbkfl876hav7sysg12dxi9nlyk609vr9fff6af"
                                     "b746c90"
                                     "1vwnqi1m2h31m03xn7flp949x5wn7dqp3rp31dn05z54i1p3gxpi"
                                     ))

(define-public boot13-check (coffee-replicate coffee-script-boot13 "coffee-script" "coffee-script-boot13r" ))


(define boot14 (bootstrap coffee-script-boot13 "coffee-script-boot14" node-0.1.95))
(define-public coffee-script-boot14 (boot14
                                     ;;"065bf54"
                                     ;;"0hh6ds6qqf1jgigbkfl876hav7sysg12dxi9nlyk609vr9fff6af"
                                     "a133e01"
                                     "08dbl57b2l9pzfdmsdal7l33mg09ps3i1xb6qbg0s6j3r6ym51yd"
                                     ))

(define-public boot14-check (coffee-replicate coffee-script-boot14 "coffee-script" "coffee-script-boot14r" ))


;; 15 broken: have to split commit
(define boot15 (bootstrap boot14-check "coffee-script-boot15"))
(define-public coffee-script-boot15 (boot15
                                     ;;"065bf54"
                                     ;;"0hh6ds6qqf1jgigbkfl876hav7sysg12dxi9nlyk609vr9fff6af"
                                     ;;"a133e01"
                                     ;;"08dbl57b2l9pzfdmsdal7l33mg09ps3i1xb6qbg0s6j3r6ym51yd"
                                     "0222d90"
                                     "18rn6s7xxjmsd3696c0gpvm1m028lcmd2kyladybw5las4d3cp0z"

                                     ))

(define-public boot15-check (coffee-replicate coffee-script-boot15 "coffee-script" "coffee-script-boot15r" ))

;; "8317960" <after 11, but only with node 0.1.90
;; "124wqh0w2i7p8q75qyjylxb4pdhw1z06z5wh6bvjsa47jvlh8d3i"



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
             (for-each (lambda (file)
                         (format #t "deleting generated JavaScript file '~a'~%" file)
                         (delete-file file))
                       (find-files "./vendor" ".*"))
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
