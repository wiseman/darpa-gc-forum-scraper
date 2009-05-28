(defstruct forum
  name
  url
  topics
  last-modified)

(defstruct topic
  url
  name
  num-posts
  messages
  last-modified)

(defstruct message
  poster
  text
  last-modified)

(defvar *data-retrieved* 0)

(defvar *db* nil)

(defun fetch-db ()
  (let ((browser (make-instance 'trivial-browser:browser)))
    (let ((fora (get-current-fora browser)))
      (setf *db* fora)
      (dolist (f fora)
	(fetch-forum browser f))
      fora)))


(defun get-forum-by-name (db name)
  (find name db :key #'forum-name :test #'equal))


(defun get-topic-by-name (topics name)
  (find name topics :key #'topic-name :test #'equal))


(defun fetch-forum (browser forum)
  (let ((topics (get-current-topics browser forum)))
    (setf (forum-topics forum) topics)
    (dolist (topic topics)
      (setf (topic-messages topic) (get-messages browser topic))))
  forum)


(defun more-recent-p (a b)
  (or (null a)
      (null b)
      (> a b)))
      

(defun update-db (db)
  (format T "~&Updating forums...~%")
  (let ((browser (make-instance 'trivial-browser:browser
				:user-agent "DARPA GC Forum scraper; jjwiseman@yahoo.com"
				:debug-log-stream *standard-output*
				:delay 1)))
    (let ((current-fora (get-current-fora browser)))
      (dolist (current-forum current-fora)
	(let ((previous-forum (get-forum-by-name db (forum-name current-forum))))
	  (cond ((null previous-forum)
		 ;; Completely new forum.
		 (format T "Forum ~S is new.~%" (forum-name current-forum))
		 (push current-forum db)
		 (fetch-forum browser current-forum))
		((more-recent-p (forum-last-modified current-forum)
				(forum-last-modified previous-forum))
		 ;; Some topics were added or updated.
		 (format T "Forum ~S has been updated.~%" (forum-name current-forum))
		 (update-forum browser previous-forum current-forum)
		 (setf (forum-last-modified previous-forum)
		       (forum-last-modified current-forum)))
		(T
		 ;; No change.
		 )))))
    (format T "~&Done updating forums.~%")
    (trivial-browser:report browser)
    db))


(defun update-forum (browser old-forum new-forum)
  (let ((current-topics (get-current-topics browser new-forum)))
    (dolist (current-topic current-topics)
      (let ((old-topic (get-topic-by-name (forum-topics old-forum)
					  (topic-name current-topic))))
	(cond ((null old-topic)
	       ;; Get completely new topic.
	       (format T "  Topic ~s is new.~%" (topic-name current-topic))
	       (setf (topic-messages current-topic) (get-messages browser current-topic))
	       (push current-topic (forum-topics old-forum))
	       (setf (forum-last-modified old-forum)
		     (forum-last-modified new-forum)))
	      ((more-recent-p (topic-last-modified current-topic)
			      (topic-last-modified old-topic))
	       ;; Some messages were changed.
	       (format T "Topic ~S has been updated.~%" (topic-name current-topic))
	       (setf (topic-messages old-topic) (get-messages browser current-topic))
	       (setf (topic-last-modified old-topic)
		     (topic-last-modified current-topic)))
	      
	      (T
	       )))))
  old-forum)






(defparameter *darpa-forum-url* "http://dtsn.darpa.mil/ibb/categoryindex.aspx?boardID=1")

(defparameter *eol* '(#.(code-char 10) #.(code-char 13)))


(defmacro with-line-iterator ((iterator-name string) &body body)
  (let ((string-var (gensym))
	(start-var (gensym))
	(len-var (gensym)))
    `(let* ((,string-var ,string)
	    (,start-var 0)
	    (,len-var (length ,string-var)))
      (labels ((,iterator-name (&key (skip-empty NIL))
		 (if (>= ,start-var ,len-var)
		     nil
		     (let ((eol (position-if #'(lambda (ch)
						 (member ch *eol*))
					     ,string-var
					     :start ,start-var)))
		       (let ((line (subseq ,string-var ,start-var (or eol ,len-var))))
			 (setf ,start-var (+ (or eol ,len-var) 1))
			 (if (and (= (length line) 0)
				  skip-empty)
			     (,iterator-name :skip-empty skip-empty)
			     line))))))
	,@body))))


(defmacro for-lines ((line-var string &optional (iterator-name (gensym))) &body body)
  `(with-line-iterator (,iterator-name ,string)
    (do ((,line-var (,iterator-name) (,iterator-name)))
	((null ,line-var))
      ,@body)))
	
	    
	
	

(defun do-lines (string fn &optional (start 0) (len (length string)))
  (when (< start len)
    (let ((eol (position-if #'(lambda (ch)
				(member ch *eol*))
			    string
			    :start start)))
      (if eol
	  (progn
	    (funcall fn (subseq string start eol))
	    (do-lines string fn (+ eol 1) len))
	  (funcall fn (subseq string start))))))



(defparameter *forum-url-and-title-re* (cl-ppcre:create-scanner "href=.(/ibb/topics.aspx.forumID=[^\"]*).*>([^<]+)</a>.*"))
(defparameter *forum-timestamp-1-re* (cl-ppcre:create-scanner "<td.*>([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+(?::[0-9]+)* (AM|PM))"))
(defparameter *forum-timestamp-2-re* (cl-ppcre:create-scanner "<td.*>([0-9]+/[0-9]+/[0-9]+)"))

(defun get-timestamp (line)
  (let ((g (groups *forum-timestamp-1-re* line)))
    (if g
	(first g)
	(let ((g (groups *forum-timestamp-2-re* line)))
	  (if g
	      (first g)
	      nil)))))

(defun get-current-fora (browser)
  (let ((page (tidy (trivial-browser:get-url browser *darpa-forum-url*)))
	(fora '()))
    (for-lines (line page next-line)
      (let ((groups (groups *forum-url-and-title-re* line)))
	(when groups (format T "~&Forum title: ~S" groups))
	(assert (or (null groups) (= (length groups) 2)))
	(when groups
	  (let ((url (merge-urls *darpa-forum-url* (first groups)))
		(name (second groups)))
	    ;; Get line with timestamp.
	    (let ((date (loop
			 do (let ((d (get-timestamp (next-line :skip-empty T))))
			      (when d (return d))))))
	      (assert date
		      ()
		      "Can't find last-modified date for forum ~S" name)
	      (let ((f (make-forum :url url
				   :name name
				   :last-modified (net.telent.date:parse-time date))))
		(format T "~&Forum ~S last modified on ~A" name date)
		(force-output *standard-output*)
		(unless (forum-last-modified f)
		  (warn "Unable to parse timestamp ~S for forum ~S." (first groups) name))
		(push f fora)))))))
    (reverse fora)))


;(defun merge-urls (base u)
;  (with-output-to-string (s)
;    (puri:render-uri (puri:merge-uris u base) s)))

(defun merge-urls (base u)
  (araneida:urlstring (araneida:merge-url (araneida:parse-urlstring base) u)))

(defparameter *topic-info-re* (cl-ppcre:create-scanner "<td.*<a href='(topic.asp.topic_id=.*&forum_id=.*&Topic_Title=.*&forum_title=.*&S=)'>([^<]+)</a>.*>([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+ (AM|PM)).*"))

(defun get-current-topics (browser forum)
  (let ((page (trivial-browser:get-url browser (forum-url forum)))
	(topics '()))
    (trace groups)
    (for-lines (line page next-line)
      (let ((groups (groups *topic-info-re* line)))
	(assert (or (null groups) (= (length groups) 4)))
	(when groups
	  (let ((url (merge-urls (forum-url forum) (first groups)))
		(name (second groups))
		(date (net.telent.date:parse-time (third groups))))
	    (unless date
	      (warn "Unable to parse timestamp ~S for topic ~S." (third groups) name))
	    (push (make-topic :url url
			      :name name
			      :last-modified date)
		  topics)))))
    (reverse topics)))

(defparameter *update-topic-re* (cl-ppcre:create-scanner "<td.*<a href='(topic.asp.topic_id=.*&forum_id=.*&Topic_Title=.*&forum_title=.*&S=)'>([^<]+)</a>.*>([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+ (AM|PM)).*"))

(defun update-topics (browser forum)
  (let ((page (trivial-browser:get-url browser (forum-url forum)))
	(topics '()))
    (for-lines (line page next-line)
      (let ((groups (groups *update-topic-re* line)))
	(assert (or (null groups) (= (length groups) 4)))
	(when groups
	  (let ((url (merge-urls (forum-url forum) (first groups)))
		(name (second groups))
		(date (net.telent.date:parse-time (third groups))))
	    (unless date
	      (warn "Unable to parse timestamp ~S for topic ~S." (third groups) name))
	    (push (make-topic :url url
			      :name name
			      :last-modified date)
		  topics)))))
    (reverse topics)))

(defun node-attr-value (node attr)
  (second (assoc attr (xmls:node-attrs node) :test #'string=)))


(defun has-node-with-class (node name class max-depth)
  (find-node-if node
		#'(lambda (child)
		    (and (node-p child)
			 (string= (xmls:node-name child) name)
			 (string= (node-attr-value child "class") class)))
		:max-depth max-depth))


(defun find-node-with-class (root name class)
  (find-node-if root
		#'(lambda (n)
		    (and (node-p n)
			 (string= (xmls:node-name n) name)
			 (string= (node-attr-value n "class") class)))))

		      

(defun get-messages (browser topic)
  (let* ((page (trivial-browser:get-url browser (topic-url topic)))
	 (tidy (tidy page)))
    (let ((document (xmls:parse tidy)))
      (let ((post-nodes (collect-nodes-if
			 document
			 #'(lambda (child)
			     (and (node-p child)
				  (string= (xmls:node-name child) "tr")
				  (has-node-with-class child "td" "c7" 1)
				  (has-node-with-class child "span" "c8" 2)
				  (has-node-with-class child "span" "c9" 5))))))
	(let ((messages (mapcar #'(lambda (post-node)
				    (let ((poster-node (find-node-with-class post-node "td" "c7"))
					  (date-node (find-node-with-class post-node "span" "c8"))
					  (text-node (find-node-with-class post-node "span" "c9")))
				      (let ((date (net.telent.date:parse-time
						   (subseq (substitute-if #\space
									  #'(lambda (c)
									      (member c *eol*))
									  (car (xmls:node-children date-node)))
							   9)
						   :default-zone 0)))
					(unless date
					  (warn "Unable to parse timestamp ~S for message ~S by ~S." date-node post-node poster-node))
					(make-message :poster (car (xmls:node-children poster-node))
						      :text (concat-xml (xmls:node-children text-node))
						      :last-modified date))))
				post-nodes)))
	  (assert (> (length messages) 0))
	  messages)))))

(defun concat-xml (nodes)
  (with-output-to-string (s)
    (dolist (n nodes)
      (format s "~A" (xmls:toxml n :with-namespace NIL)))))


(defun groups (regex line)
  (multiple-value-bind (a b starts ends)
      (cl-ppcre:scan regex line)
    (declare (ignore a b))
    (let ((groups '()))
      (dotimes (i (length starts) (reverse groups))
	(push (subseq line (elt starts i) (elt ends i))
	      groups)))))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

#+allegro
(defun tidy (string)
  (let ((the-pid nil)
	(status nil)
	(signal nil))
    (let ((tidied
	   (unwind-protect
		(progn
		  (with-open-file (out "tidy-in.html" :direction :output
				       :if-exists :supersede)
		    (format out "~A" string))
		  (multiple-value-bind (stream whatever pid)
		      (run-shell-command "/usr/bin/tidy -asxhtml -wrap 1200"
					 :wait NIL
					 :input "tidy-in.html"
					 :error-output "/dev/null"
					 :if-error-output-exists :append
					 :output :stream)
		    (declare (ignore whatever))
		    (with-open-stream (stream stream)
		      (setf the-pid pid)
		      (trivial-browser:contents-of-stream stream))))
	     (progn
	       (ignore-errors (delete-file "tidy-in.html"))
	       (when the-pid
		 (multiple-value-bind (stat p sig)
		     (system:reap-os-subprocess :pid the-pid :wait T)
		   (declare (ignore p))
		   (setf status stat
			 signal sig)))))))
      (unless (or (and (= status 0) (not signal))
		  (and (= status 1) (not signal) (> (length tidied) 0)))
	(error "Tidy failed with status: ~S signal ~S." status signal))
      tidied)))

#+sbcl
(defun tidy (string)
  (unwind-protect
       (progn
	 (with-open-file (out "tidy-in.html" :direction :output
			      :if-exists :supersede)
	   (format out "~A" string))
	 (let ((process (run-program "/usr/bin/tidy"
				     '("-asxhtml" "-wrap" "1200")
				     :wait NIL
				     :input "tidy-in.html"
				     :error nil
				     :output :stream)))
	   (trivial-browser:contents-of-stream (process-output process))))
    (ignore-errors (delete-file "tidy-in.html"))))

#+openmcl
(defun tidy (string)
  (unwind-protect
       (progn
	 (with-open-file (out "tidy-in.html" :direction :output
			      :if-exists :supersede)
	   (format out "~A" string))
	 (let ((process (run-program "/usr/bin/tidy"
				     '("-asxhtml" "-wrap 1200")
				     :wait NIL
				     :input "tidy-in.html"
				     :error nil
				     :output :stream)))
	   (trivial-browser:contents-of-stream (external-process-output-stream process))))
    (ignore-errors (delete-file "tidy-in.html"))))


(defun map-nodes (root predicate &key max-depth)
  (map-nodes-aux root predicate 0 max-depth))

(defun map-nodes-aux (root predicate current-depth max-depth)
  (unless (and max-depth (> current-depth max-depth))
;;    (format T "~&~VT~S" (* current-depth 4) (if (node-p root) (xmls:node-name root)
;;						(format nil "~A..." (subseq root 0 (min 10 (length root))))))
    (funcall predicate root)
    (when (node-p root)
      (dolist (child (xmls:node-children root))
	(map-nodes-aux child predicate (+ current-depth 1) max-depth)))))

(defun find-node-if (root predicate &key max-depth)
  (map-nodes root
	     #'(lambda (node)
		 (when (funcall predicate node)
		   ;;(format T "~&Found it: ~S" node)
		   (return-from find-node-if node)))
	     :max-depth max-depth))

(defun collect-nodes-if (root predicate &key max-depth)
  (let ((nodes '()))
    (map-nodes root
	       #'(lambda (node)
		   (when (funcall predicate node)
		     ;;(format T "~&WOO: ~S" node)
		     (push node nodes)))
	       :max-depth max-depth)
    (reverse nodes)))

(defun node-p (e)
  (consp e))



(defun flatten-messages (db)
  (let ((messages '()))
    (dolist (forum db)
      (dolist (topic (forum-topics forum))
	(dolist (message (topic-messages topic))
	  (push (list forum topic message) messages))))
    messages))

(defun generate-rss (db stream &key (most-recent 100))
  (let ((messages (flatten-messages db)))
    (setf messages (sort messages #'>
			 :key #'(lambda (m) (message-last-modified (third m)))))
    (when most-recent
      (setf messages (subseq messages 0 (min (length messages) most-recent))))
    (let ((rss
	   `("rss" (("version" "0.91"))
	     ("channel" ()
	      ("title" () "DARPA Grand Challenge Forum")
	      ("link" () "http://dtsn.darpa.mil/grandc/forum/")
	      ,@(mapcar #'generate-message-rss messages)))))
      (format stream "~A" (xmls:toxml rss :with-namespace NIL)))))

(defun generate-message-rss (m)
  (destructuring-bind (forum topic message)
      m
    `("item" ()
      ("title" () ,(format nil "~A : ~A" (forum-name forum) (topic-name topic)))
      ("link" () ,(format nil "~A#~A"
			  (topic-url topic)
			  (sxhash (format nil "~A:~A"
					  (message-poster message)
					  (message-last-modified message)))))
      ("pubDate" () ,(net.telent.date:universal-time-to-rfc2822-date (+ (message-last-modified message)
									#.(* 8 60 60))
								     8))
      ("description" ()
       ,(format nil "<p>Posted by ~A</p>"
		(message-poster message))
       ,(hrefify (message-text message))))))

(defun hrefify (html)
  (let ((url-indices (find-urls html)))
    (with-output-to-string (s)
      (unless (and url-indices (= (caar url-indices) 0))
	(format s "~A" (subseq html 0 (caar url-indices))))
      (do ((indices url-indices (cdr indices)))
	  ((endp indices))
	(let ((url-start (car (first indices)))
	      (url-end (cdr (first indices)))
	      (next-url-start (if (cdr indices)
				  (car (second indices))
				  (length html))))
	  (let ((url (subseq html url-start url-end)))
	    (format s "<a href=\"~A\">~A</a>~A"
		    url
		    url
		    (subseq html url-end next-url-start))))))))


(defun read-db (file)
  (if (probe-file file)
      (with-open-file (db-stream file :direction :input)
	(read db-stream))
      '()))

(defun write-db (db file)
  (with-open-file (db-stream file :direction :output
			     :if-exists :error)
    (with-standard-io-syntax
     (format db-stream "~S" db))))


(defun db-last-modified (db)
  (if db
      (reduce #'max db :key #'forum-last-modified)
      nil))

(defun update-rss (db-file rss-file &key (most-recent 100) (force NIL))
  (let ((*print-readably* NIL)
	(*print-escape* NIL))
    (let ((db (read-db db-file)))
;;      (assert (> (length db) 10))
      (let ((last-modified (db-last-modified db)))
	(setf db (update-db db))
	(cerror "Continue anyway with the ~S entries." "Database has only ~S entries; are you sure it's the right one?" (length db))
	(if (or (more-recent-p (db-last-modified db) last-modified)
		force)
	    (progn
	      (format T "~&Regenerating RSS...~%")
	      (ignore-errors (delete-file "darpa.db.backup"))
	      (when (probe-file db-file)
		(rename-file db-file "darpa.db.backup"))
	      (write-db db db-file)
	      (with-open-file (rss-stream rss-file :direction :output
					  :if-exists :supersede)
			      (generate-rss db rss-stream :most-recent most-recent))
	      T)
	  (progn
	    (format T "~&No changes.~%")
	    NIL))))))



(defun insert-system-message (message-subject message-text db-file rss-file &key (most-recent 100))
  (let ((*print-readably* NIL)
	(*print-escape* NIL))
    (let ((db (read-db db-file)))
      (let* ((system-forum (get-forum-by-name db "System Messages"))
	     (message (make-message :poster "jjwiseman"
				    :text message-text
				    :last-modified (get-universal-time)))
	     (topic (make-topic :url "http://gc.heavymeta.org/"
				:name message-subject
				:num-posts nil
				:messages (list message)
				:last-modified (get-universal-time))))
	(push topic (forum-topics system-forum))
	(format T "~&Regenerating RSS...~%")
	(ignore-errors (delete-file "darpa.db.backup"))
	(rename-file db-file "darpa.db.backup")
	(write-db db db-file)
	(with-open-file (rss-stream rss-file :direction :output
				    :if-exists :supersede)
	  (generate-rss db rss-stream :most-recent most-recent))))))

