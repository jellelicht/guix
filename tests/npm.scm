;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (test-gem)
  #:use-module (guix import npm)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module (guix tests)
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define test-json
"{
  \"name\": \"foo\",
  \"description\": \"General purpose utilities to foo your bars\",
  \"version\": \"1.2.3\",
  \"author\": \"Jelle Licht <jlicht@fsfe.org>\",
  \"dependencies\": {
    \"bar-utils\": \"^1.0.0\",
  },
  \"devDependencies\": {
    \"node-megabuilder\": \"^0.0.2\",
  },
  \"repository\": \"quartz/foo\",
  \"homepage\": \"https://github.com/quartz/foo\",
  \"license\": \"MIT\"
} ")

(define archive-url
  "https://github.com/quartz/foo/archive/1.2.3/foo-v1.2.3.tar.gz")

(test-begin "npm")

(test-assert "npm->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix import utils) url-fetch
         (lambda (url file-name)
           (match url
             ("https://registry.npmjs.org/foo"
              (with-output-to-file file-name
                (lambda ()
                  (display test-json))))
             (archive-url
              (with-output-to-file file-name
                (lambda ()
                  (display "supposed_tar_gz_data")))) ;;
             ("https://api.github.com/repos/quartz/foo/tags"
              (with-output-to-file file-name
                (lambda ()
                  (display "[
 {
  \"name\": \"v1.2.3\",
  \"zipball_url\": \"https://I_Do_Not_Matter.net/file-1.2.3.zip\",
  \"tarball_url\": \"https://I_Do_Not_Matter.net/file-1.2.3.tar.gz\",
  \"commit\": {
               \"sha\": \"f26d33d41\",
               \"url\":
               \"https://api.github.com/repos/quartz/foo/commits/f26d33d41\"
               }
  },
 {
  \"name\": \"v1.0.1\",
  \"zipball_url\": \"https://I_Do_Not_Matter.net/file-1.0.1.zip\",
  \"tarball_url\": \"https://I_Do_Not_Matter.net/file-1.0.1.tar.gz\",
  \"commit\": {
               \"sha\": \"c37f284771\",
               \"url\":
               \"https://api.github.com/repos/quartz/foo/commits/c37f284771\"
               }
  }
]"))))
             (_ (error "Unexpected URL: " url)))))
    (match (npm->guix-package "foo")
      (('package
         ('name "node-foo")
         ('version "1.2.3")
         ('source ('origin
                    ('method 'url-fetch)
                    ('uri archive-url))
                    ('sha256
                     ('base32
                      "06dp45b8ag287hqz907g26ndcvnp6gpdm5i306wa7r9b9r9r7mxr")))
         ('build-system 'node-build-system)
         ('propagated-inputs
          ('quasiquote
           (("node-bar-utils" ('unquote 'node-bar-utils)))))
         ('native-inputs
          ('quasiquote
           (("node-megabuilder" ('unquote 'node-megabuilder)))))
         ('synopsis "General purpose utilities to foo your bars")
         ('description "General purpose utilities to foo your bars")
         ('home-page "https://github.com/quartz/foo")
         ('license ('list 'expat)))
       #t)
      (x
       (pk 'fail x #f)))))

(test-end "npm")
