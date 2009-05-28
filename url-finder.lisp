(defparameter *url-protocols*
  '("http" "ftp" "https" "telnet" "gopher" "file" "mailto" "mms"))

(defparameter *whitespace-chars*
  '(#\space #.(code-char 10) #.(code-char 13) #\tab))


(defun find-url-start (string start protocols)
  (dolist (protocol protocols)
    (let ((prefix (format nil "~A://" protocol)))
      (let ((url-start (search prefix string
			       :start2 start
			       :test #'(lambda (c1 c2)
					 (eql (char-upcase c1)
					      (char-upcase c2))))))
	(when url-start
	  (return-from  find-url-start url-start)))))
  nil)

(defun safe-min (a b)
  (if (null a)
      b
      (if (null b)
	  a
	  (min a b))))


(defun find-url-end (string start)
  (safe-min
   (safe-min 
    (position-if #'(lambda (c)
		     (or (eql c #\>)
			 (eql c #\<)
			 (eql c #\))
			 (member c *whitespace-chars*)))
		 string
		 :start start)
    (search "&gt;" string :start2 start))
   (length string)))

(defun find-url (string &key (start 0) (protocols *url-protocols*))
  (let ((url-start (find-url-start string start protocols)))
    (if (not url-start)
	nil
	(values url-start (find-url-end string url-start)))))

(defun find-urls (string &key (start 0) (protocols *url-protocols*))
    (labels ((find-next (start indices)
	       (multiple-value-bind (url-start url-end)
		   (find-url string :start start :protocols protocols)
		 (if (null url-start)
		     indices
		     (find-next url-end (cons (cons url-start url-end)
					      indices))))))
      (reverse (find-next start '()))))


(defparameter *test-data*
  '(("df asdfjkasdf http://foo/ dfg dsfgsdfg"
     ("http://foo/"))
    ("ftp://wiseman@foo>"
     ("ftp://wiseman@foo"))
    ("http://foo/ asdfasdf"
     ("http://foo/"))
    ("ftp://foo"
     ("ftp://foo"))
    ("sd fsadf ftp://foo"
     ("ftp://foo"))
    ("asdfasdf <http://foo>"
     ("http://foo"))
    ("asdfasdf <http://foo"
     ("http://foo"))
    ("<ftp://foo/asdfasdf>"
     ("ftp://foo/asdfasdf"))
    ("i want to look at http://www.woo.null/ and http://zzz.sleep.null/"
     ("http://www.woo.null/" "http://zzz.sleep.null/"))
    ("<http://foo.com/><ftp://foo.com/>"
     ("http://foo.com/" "ftp://foo.com/"))
    ("my thinking is &lt;ftp://foo.bar/&gt;"
     ("ftp://foo.bar/"))
    ("my thinking is &lt;FTP://foo.bar/&gt;"
     ("FTP://foo.bar/"))
    ("my thinking is &lt;FTP://foo.bar/&gt; blah"
     ("FTP://foo.bar/"))))


(defun run-test (test)
  (let ((failed-p NIL)
	(string (first test))
	(result (second test)))
    (let ((indices (find-urls string)))
      (if (not (= (length indices) (length result)))
	  (progn
	    (setf failed-p T)
	    (format T "~&'~A' failed (got ~S) (~S)."
		    string
		    indices
		    result))
	  (do ((is indices (cdr is))
	       (rs result (cdr rs)))
	      ((endp is))
	    (let* ((span (car is))
		   (r (car rs))
		   (url (subseq string (car span) (cdr span))))
	      (unless (string-equal url r)
		(format T "~&Should have gotten ~S instead of ~S."
			r
			url)
		(setf failed-p T))))))
    (not failed-p)))

(defun run-tests ()
  (let ((num-passed (reduce #'+
			    (mapcar #'(lambda (test)
					(if (run-test test)
					    1
					    0))
				    *test-data*))))
    (format T "~&Passed ~S of ~S." num-passed (length *test-data*))))
	 
     