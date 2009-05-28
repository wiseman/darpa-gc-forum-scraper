(cl:defpackage #:trivial-browser
  (:use :common-lisp)
  (:export
   #:browser
   #:browser-delay
   #:browser-user-agent
   #:browser-download-callback
   #:browser-read-size-hint
   #:browser-debug-log-stream
   
   #:contents-of-stream
   #:get-url
   #:unthrottled-get-url
   #:report))

