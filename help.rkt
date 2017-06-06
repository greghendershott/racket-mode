#lang at-exp racket/base

(require (only-in help/help-utils find-help)
         (only-in help/search perform-search)
         json
         net/url
         racket/contract
         racket/file
         racket/format
         racket/match
         racket/port
         racket/promise
         racket/system
         "scribble.rkt")

(provide
 (contract-out [rename -find-help find-help (-> syntax? boolean?)]
               [perform-search (-> string? any)]))

;; It is 2017 therefore it is hard to activate a web browser and show
;; an anchor link within a local HTML file.
;;
;; 1. On macOS `find-help` suffers from the fact that `send-url/file`
;;    doesn't supply a `browser` arg to `send-url/mac`. This causes it
;;    to give an "open location" command to osascript. This causes
;;    macOS to ignore #anchor fragments in the URL. Although the
;;    correct page will open, it won't be scrolled to the item of
;;    interest.
;;
;; 2. Furthermore, `send-url/mac` doesn't use an "activate" command to
;;    show the browser window (it might be hidden behind Emacs).
;;
;; Let's pretend it's 2020. If we we're on mac and can determine the
;; default browser (from plist files^1), do the equivalent of
;; `send-url/mac` but with both desired behaviors.
;;
;; ^1: This is kludgy because the plist has "bundle IDs" like
;; "com.google.chrome" but osascript wants strings like "chrome".

(define mac-browser ;; (promise/c (or/c string? #f))
   (delay/sync (mac-default-browser)))

(define (-find-help stx)
  ((if (force mac-browser)
       find-help/mac
       find-help/boolean)
   stx))

(define (find-help/boolean stx)
  ;; Like `find-help` but returns whether help was found and shown.
  ;; That way, if this returns #f caller knows it could next call
  ;; `perform-search` as Plan B.
  (with-handlers ([exn:fail? (λ _ #f)])
    (match (with-output-to-string (λ () (find-help stx)))
      [(pregexp "Sending to web browser") #t]
      [_ #f])))

(define (find-help/mac stx)
  (let-values ([(path anchor) (binding->path+anchor stx)])
    (and path anchor
         (let ([path-url (path->url (path->complete-path path))])
           (browse-file-url/mac
            (url->string (struct-copy url path-url [fragment anchor]))
            (force mac-browser))))))
(define osascript (delay/sync (find-executable-path "osascript" #f)))
(define (browse-file-url/mac file-url browser)
  ;; Note: Unlike `send-url/mac`, we also do an "activate" to show the
  ;; browser window.
  (system*
   (force osascript)
   "-e"
   @~a{tell application "@browser" to open location "@file-url" activate}))

;;; Discover default browser on macOS

(define launch-plists
  '("Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist"
    "Library/Preferences/com.apple.LaunchServices.plist"))
(define (mac-default-browser)
  (and (equal? (system-type) 'macosx)
       (for/or ([plist launch-plists])
         (match (mac-http-handler (build-path (find-system-path 'home-dir) plist))
           [#f #f]
           [(pregexp "^.+\\.(.+?)$" ;after final dot
                     (list _ s)) s]))))

(define (mac-http-handler plist-path) ;; path? -> (or/c string? #f)
  (for/or ([h (in-list (hash-ref (read-bplist plist-path) 'LSHandlers '()))])
    (and (equal? (hash-ref h 'LSHandlerURLScheme #f) "http")
         (hash-ref h 'LSHandlerRoleAll #f))))

(define plutil (delay/sync (find-executable-path "plutil" #f)))
(define (read-bplist plist-path) ;path? -> json?
  (define out-path (make-temporary-file))
  (begin0
      (if (system* (force plutil)
                   "-convert" "json"
                   "-o" out-path
                   plist-path)
          (with-input-from-file out-path read-json)
          (make-hash))
    (delete-file out-path)))
