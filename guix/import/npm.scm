;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (guix import npm)
  #:use-module (json)
  #:use-module ((guix licenses) #:select (expat))
  #:use-module (guix utils)
  #:use-module (web uri)
  #:use-module (guix build git)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix serialization)
  #:use-module (guix import json)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (guix import utils)
  #:export (npm->guix-package))

(define (normalise-git-url url)
  "Return a simple canonical url for the git repository at URL."
  ;;XXX: Weird npm urls need to be normalised
  ;;Not very robust right now
  ;; git://github.com/user/project.git
  ;; git+ssh://user@hostname:project.git
  ;; git+ssh://user@hostname/project.git
  ;; git+http://user@hostname/project/blah.git
  ;; git+https://user@hostname/project/blah.git
  ;; TODO: should support the user/repo shorthand for github
  (let ((uri (string->uri url)))
    (if (uri? uri)
        (case (uri-scheme uri)
          ((git+ssh)
           (if (equal? (uri-host uri) "github.com")
               (normalise-git-url (uri->string (set-fields uri
                                                           ((uri-scheme) 'git))))
               url))
          ((git+https git+http)
           (substring url 4)) ; ignore git+ part of uri scheme
          ((https)
           (uri->string (set-fields uri
                                    ((uri-userinfo) #f)))) ;dont use userinfo when using the https interface
          ((git)
           (if (equal? (uri-host uri) "github.com")
               ; Try the https github interface first, because it works with both git-fetch and url-fetch
               (normalise-git-url (uri->string (set-fields uri
                                                           ((uri-scheme) 'https))))
               uri))
          (else
           url))
        url)))

;; Taken and tweaked from (guix import github)
(define %github-token
  ;; Token to be passed to Github.com to avoid the 60-request per hour
  ;; limit, or #f.
  (make-parameter (getenv "GUIX_GITHUB_TOKEN")))

(define (json-fetch* url)
  "Return a list/hash representation of the JSON resource URL, or #f on
failure."
  (call-with-output-file "/dev/null"
    (lambda (null)
      (with-error-to-port null
        (lambda ()
          (call-with-temporary-output-file
           (lambda (temp port)
             (and (url-fetch url temp)
                  (call-with-input-file temp json->scm)))))))))

(define (fuzzy-tag-match github-repo version)
  "Return a likely match for the git tag VERSION for the repository at
GITHUB-REPO"
  (let* ((fuzzy-tags (list version
                           (string-append "v" version)))
         (token (%github-token))
         (api-url (string-append
                   "https://api.github.com/repos/"
                   (github-user-slash-repository github-repo)
                   "/tags"))
         (json (json-fetch*
                (if token
                    (string-append api-url "?access_token=" token)
                    api-url))))
    (if (eq? json #f)
        (if token
            (error "Error downloading release information through the GitHub
API when using a GitHub token")
            (error "Error downloading release information through the GitHub
API. This may be fixed by using an access token and setting the environment
variable GUIX_GITHUB_TOKEN, for instance one procured from
https://github.com/settings/tokens"))
        (let ((proper-release
               (filter
                (lambda (x)
                  (let ((name (hash-ref x "name")))
                    (member name fuzzy-tags))) 
                json)))
          (match proper-release
            (()                       ;empty release list
             #f)
            ((release . rest)         ;one or more releases
             ;;XXX: Just pick the first release
             (let ((tag (hash-ref release "name")))
               tag)))))))

(define (github-user-slash-repository github-url)
  "Return a string e.g. arq5x/bedtools2 of the owner and the name of the
repository separated by a forward slash, from a string URL of the form
'https://github.com/arq5x/bedtools2.git'"
  (match (string-split (uri-path (string->uri github-url)) #\/)
    ((_ owner project . rest)
     (string-append owner "/" (string-drop-right project 4)))))

(define (github-repository github-url)
  "Return a string e.g. bedtools2 of the name of the repository, from a string
URL of the form 'https://github.com/arq5x/bedtools2.git'"
  (match (string-split (uri-path (string->uri github-url)) #\/)
    ((_ owner project . rest)
     (string-drop-right project 4))))

(define (github-release-url github-url version)
  "Return the url for the tagged release VERSION on the github repo found at
GITHUB-URL."
  (if (equal? (uri-host (string->uri github-url)) "github.com")
      (let ((ext     ".tar.gz")
            (version version)
            (prefix  (string-append "https://github.com/"
                                    (github-user-slash-repository github-url)))
            (repo    (github-repository github-url)))
        (string-append prefix "/archive/" version "/"
                       repo "-" version ext))))

;; NPM specific code
(define *REGISTRY*  "https://registry.npmjs.org/")

;; First try with released tarball, (github only?)
;; then heuristics for git repo

(define (npm-fetch name)
  "Return metadata from the npm registry for package NAME."
  (json-fetch (string-append *REGISTRY* name)))

(define (latest-source-release npm-meta)
  "Return the latest source release for NPM-META."
  (assoc-ref* npm-meta "dist-tags" "latest"))

(define (source-uri npm-meta version)
  "Return the repository url for version VERSION of NPM-META"
  (let* ((v    (assoc-ref* npm-meta "versions" version)))
    (normalise-git-url (assoc-ref* v "repository" "url"))))

(define (guix-hash-url path)
  "Return the hash of PATH in nix-base32 format. PATH can be either a file or
directory."
  (let-values (((port get-hash) (open-sha256-port)))
    (write-file path port)
    (flush-output-port port)
    (bytevector->nix-base32-string (get-hash))))

(define (node-package-name name)
  "Given the NAME of a package on npmjs, return a Guix-compliant name for the
package."
  (if (string-prefix? "node-" name)
      (snake-case name)
      (string-append "node-" (snake-case name))))

(define (spdx-string->license str)
  "Convert STR, a SPDX formatted license identifier, to a license object.
   Return #f if STR does not match any known identifiers."
  (match str
    ("MIT" 'expat)
    (_ #f)))

(define (node-git-fetch url commit directory)
  "Fetch the git repo located at URL, clone into DIRECTORY and check out
revision COMMIT. URL can be any url supported by the 'git clone' command.
COMMIT can be identifier of a commit that works with the 'git checkout'
command."
  ;;XXX: Assumes 'commit' is actually a tag
  (let ((url (normalise-git-url url))
        (v (string-append "v" commit)))
    (git-fetch url v directory)))

(define (package-origin repo-url version)
  "Return a complete package origin for version VERSION of the software
located at REPO-URL. Tries to locate a released tarball before falling back to
a git checkout."
  (let ((uri (string->uri repo-url)))
    (if (equal? (uri-host uri) "github.com")
        (call-with-temporary-output-file
         (lambda (temp port)
           (let* ((gh-version (fuzzy-tag-match repo-url version))
                  (tb (github-release-url repo-url gh-version))
                  (result (url-fetch tb temp))
                  (hash (bytevector->nix-base32-string (port-sha256 port))))
             (close-port port)
             `(origin
                (method url-fetch)
                (uri ,tb)
                (sha256
                 (base32
                  ,hash))))))
        (call-with-temporary-directory
         (lambda (temp-dir)
           (and (node-git-fetch repo-url version temp-dir)
                `(origin
                   (method git-fetch)
                   (uri (git-reference
                         (url ,repo-url)
                         (commit (string-append "v" version))))
                   (sha256
                    (base32
                     ,(guix-hash-url temp-dir))))))))))

(define (make-npm-sexp name version home-page description
                       dependencies dev-dependencies license source-url)
  "Return the `package' s-expression for a Node package with the given NAME,
VERSION, HOME-PAGE, DESCRIPTION, DEPENDENCIES, DEV-DEPENDENCIES, LICENSES and
SOURCE-URL."
  (let ((origin (package-origin source-url version)))
    `(package
       (name ,(node-package-name name))
       (version ,version)
       (source ,origin)
       (build-system node-build-system)
       ,@`((arguments
          (,'quasiquote (#:modulename ,name))))
       ,@(if (not dependencies)
             '()
             `((propagated-inputs
                (,'quasiquote
                 ,(map (lambda (name)
                         `(,name
                           (,'unquote
                            ,(string->symbol name))))
                       dependencies)))))
       ,@(if (not dev-dependencies)
             '()
             `((native-inputs
                (,'quasiquote
                 ,(map (lambda (name)
                         `(,name
                           (,'unquote
                            ,(string->symbol name))))
                       dev-dependencies)))))
       (native-search-paths
        (list (search-path-specification
               (variable "NODE_PATH")
               (files '("lib/node_modules")))))
       (synopsis ,description) ; no synopsis field in package.json files
       (description ,description)
       (home-page ,home-page)
       (license ,(license->symbol license)))))

(define (extract-dependencies dependencies)
  "Returns a list of dependencies according to the guix naming scheme, from
the npm list of dependencies DEPENDENCIES."
  (if (not dependencies)
      '()
      (map (compose node-package-name car) dependencies)))


(define (npm->guix-package package-name)
  "Fetch the metadata for PACKAGE-NAME from registry.npmjs.com and return the
 `package' s-expression corresponding to that package, or #f on failure."
  (let ((package (npm-fetch package-name)))
    (and package
         (let* ((name (assoc-ref package "name"))
                (version (latest-source-release package))
                (curr (assoc-ref* package "versions" version))
                (dependencies (extract-dependencies (assoc-ref curr "dependencies")))
                (dev-dependencies (extract-dependencies (assoc-ref curr "dev-dependencies")))
                (description (assoc-ref package "description"))
                (home-page (assoc-ref package "homepage"))
                (license (spdx-string->license (assoc-ref package "license")))
                (source-url (source-uri package version)))
           (make-npm-sexp name version home-page description
                          dependencies dev-dependencies license source-url)))))

