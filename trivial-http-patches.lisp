;; Patch trivial-http:http-get so we can specify custom headers (e.g.,
;; User-Agent,

(in-package :trivial-http)

(defun http-get (url &key headers)
  (let* ((host (url-host url))
         (port (url-port url))
         (stream (open-stream host port)))
    ;; Allow caller to override Host header, if, for some reason, they
    ;; want to.
    (unless (assoc "Host" headers :test #'string=)
      (setf headers (cons (cons "Host" host) headers)))
    (format stream "GET ~A HTTP/1.0~A" url +crlf+)
    (stream-headers headers stream)
    (format stream "~A" +crlf+)
    (force-output stream)
    (list
     (response-read-code stream)
     (response-read-headers stream)
     stream)))

(defun stream-headers (headers stream)
  (dolist (header headers)
    (format stream "~A: ~A~A" (car header) (cdr header) +crlf+)))
