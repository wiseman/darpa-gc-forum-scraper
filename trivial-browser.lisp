(in-package :trivial-browser)

(defclass browser ()
  ((delay                 :initform nil               :initarg :delay             :accessor browser-delay)
   (user-agent            :initform "Trivial Browser" :initarg :user-agent        :accessor browser-user-agent)
   (download-callback     :initform nil               :initarg :download-callback :accessor browser-download-callback)
   (read-size-hint        :initform 4096              :initarg :read-size-hint    :accessor browser-read-size-hint)
   (debug-log-stream      :initform nil               :initarg :debug-log-stream  :accessor browser-debug-log-stream)
   
   (first-get-time        :initform nil                                           :accessor browser-first-get-time)
   (last-get-time         :initform 0                                             :accessor browser-last-get-time)
   (total-bytes-retrieved :initform 0                                             :accessor browser-total-bytes-retrieved)
   (url-get-count         :initform 0                                             :accessor browser-url-get-count)))

(defgeneric get-url (browser url &key headers))
(defgeneric unthrottled-get-url (browser url &key headers))
(defgeneric report (browser &key stream))

(defgeneric download-stream (browser stream))
(defgeneric debug-log (browser &rest args))


(defmethod get-url ((self browser) url &key headers)
  (let* ((now (get-universal-time))
	 (elapsed (- now (browser-last-get-time self)))
	 (delay (browser-delay self)))
    (when (and delay (< elapsed delay))
      (debug-log self "Sleeping for ~S seconds before getting url." (- delay elapsed))
      (sleep (- delay elapsed)))
    (unthrottled-get-url self url :headers headers)))

(defmethod unthrottled-get-url ((self browser) url &key headers)
  (debug-log self "Getting url ~S" url)
  (when (browser-user-agent self)
    (setf headers (cons (cons "User-Agent" (browser-user-agent self))
			headers)))
  (incf (browser-url-get-count self))
  (unless (browser-first-get-time self)
    (setf (browser-first-get-time self) (get-universal-time)))
  (destructuring-bind (result response-headers stream)
      (trivial-http:http-get url :headers headers)
    (declare (ignore response-headers))
    (debug-log self "Got response ~S" result)
    (setf (browser-last-get-time self) (get-universal-time))
    (if (and (>= result 200) (< result 300))
        (let ((contents (download-stream self stream)))
          (close stream)
          (incf (browser-total-bytes-retrieved self) (length contents))
          contents)
        nil)))


(defmethod download-stream ((self browser) stream)
  (let ((total-size 0))
    (flet ((callback (buffer size)
	     (incf total-size size)
	     (debug-log self "Downloaded ~S bytes" total-size)
	     (when (browser-download-callback self)
	       (funcall (browser-download-callback self) buffer size))))
      (contents-of-stream stream
			  :buffer-size (browser-read-size-hint self)
			  :callback #'callback))))


(defmethod report ((self browser) &key (stream *standard-output*))
  (format stream "~&Browser ~S:" self)
  (format stream "~&  Got ~S URLs" (browser-url-get-count self))
  (format stream "~&  Totalling ~S bytes" (browser-total-bytes-retrieved self))
  (when (browser-first-get-time self)
    (let ((elapsed (- (browser-last-get-time self)
		      (browser-first-get-time self))))
      (format stream "~&  In ~S seconds" elapsed)
      (when (> elapsed 0)
	(format stream "~&  For an average of ~,2F bytes/s" (/ (browser-total-bytes-retrieved self)
							       elapsed))))))



(defmethod debug-log ((self browser) &rest args)
  (let ((debug-stream (browser-debug-log-stream self)))
    (when debug-stream
      (apply #'format debug-stream args)
      (terpri debug-stream))))


(defun contents-of-stream (stream &key (buffer-size 4096) callback)
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents)
    (let ((buffer (make-string buffer-size)))
      (labels ((read-chunks ()
		 (let ((size (read-sequence buffer stream)))
		   (when callback
		     (funcall callback buffer size))
		   (if (< size buffer-size)
		       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
	(read-chunks)))))


