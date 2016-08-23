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

(define-module (guix scripts import npm)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix import npm)
  #:use-module (guix scripts import)
  #:use-module (guix scripts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-npm))

;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (_ "Usage: guix import npm PACKAGE-NAME
   Import and convert the npm package for PACKAGE-NAME.\n"))
  (display (_ "
     -h, --help             display this help and exit"))
  (display (_ "
     -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import npm")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))

;;;
;;; Entry point.
;;;

(define (guix-import-npm . args)
  (define (parse-options)
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))
  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                             (('argument . value)
                              value)
                             (_ #f))
                           (reverse opts))))
    (match args
      ((package-name)
       (if (assoc-ref opts 'recursive)
           ;; Recursive import
           (map (match-lambda
                  ((and (label . (('package ('name name) . rest)))
                        (label . (pkg)))
                   `(define-public ,(string->symbol name)
                      ,pkg))
                  (_ #f))
                (recursive-import package-name))
           ;; Single import
           (let ((sexp (npm->guix-package package-name)))
             (unless sexp
               (leave (_ "failed to download meta-data for package '~a'~%")
                      package-name))
             sexp)))
      (()
       (leave (_ "too few arguments~%")))
      ((many ...)
       (leave (_ "too many arguments~%"))))))
