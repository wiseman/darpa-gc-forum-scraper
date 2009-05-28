(in-package :xmls)

(defun generate-xml (e s indent namespace)
  "Renders a lisp node tree to an xml string stream."
  (if (> indent 0) (incf indent))
  (etypecase e
    (list
     (progn
       (dotimes (i (* 2 (- indent 2)))
         (write-char #\Space s))
       (if namespace
	   (progn
	     (format T "~&Writing namespace")
	     (format s "<~A~@[ xmlns=\"~A\"~]" (node-name e) (node-ns e)))
	   (format s "<~A" (node-name e)))
       (loop for a in (node-attrs e)
             do (progn
                  (write-char #\Space s)
                  (write-string (first a) s)
                  (write-char #\= s)
                  (write-char #\" s)
                  (write-escaped (second a) s)
                  (write-char #\" s))))
     (if (null (node-children e))
         (progn
           (write-string "/>" s)
           (if (> indent 0) (write-char #\Newline s)))
	 (progn
           (write-char #\> s)
           (if (> indent 0) (write-char #\Newline s))
           (mapcan (lambda (c) (generate-xml c s indent namespace)) (node-children e))
           (if (> indent 0)
               (progn
                 (dotimes (i (* 2 (- indent 2)))
                   (write-char #\Space s))))
	   (format s "</~A>" (node-name e))
           (if (> indent 0) (write-char #\Newline s)))))
    (number
     (generate-xml (translate-raw-value e) s indent namespace))
    (symbol
     (generate-xml (translate-raw-value e) s indent namespace))
    (string 
     (progn
       (if (> indent 0)
           (progn
             (dotimes (i (* 2 (- indent 2)))
               (write-char #\Space s))))
       (write-escaped e s)
       (if (> indent 0) (write-char #\Newline s))))))

(defun write-xml (e s &key (indent nil) (with-namespace T))
  "Renders a lisp node tree to an xml stream.  Indents if indent is non-nil."
  (generate-xml e s (if indent 1 0) with-namespace))

(defun toxml (e &key (indent nil) (with-namespace T))
  "Renders a lisp node tree to an xml string."
  (with-output-to-string (s)
    (write-xml e s :indent indent :with-namespace with-namespace)))


(defun write-escaped (string stream)
  "Writes string to stream with all character entities escaped."
  (coerce string 'string)
  (when (eq stream t) (setf stream *standard-output*))
  (let ((*print-escape* NIL))
    (dotimes (i (length string))
      (let* ((char (char string i))
	     (code (char-code char)))
	(if (> code 255)
	    (format stream "&#~S;" code)
	  (progn
;	    (print (svref xmls::*char-escapes* code))
	    (write-sequence (svref *char-escapes* code) stream)
;	    (write-sequence (svref xmls::*char-escapes* code) *standard-output*)
	    ))))))


(defun resolve-entity (ent)
  "Resolves the xml entity ENT to a character.  Numeric entities are
converted using CODE-CHAR, which only works in implementations that
internally encode strings in US-ASCII, ISO-8859-1 or UCS."
  (or (and (>= (length ent) 2)
           (char= (char ent 0) #\#)
           (code-char
            (if (char= (char ent 1) #\x)
                (parse-integer ent :start 2 :end (- (length ent) 1) :radix 16)
                (parse-integer ent :start 1 :end (- (length ent) 1)))))
      (second (find ent *entities* :test #'string= :key #'first))
      (error "Unable to resolve entity ~S" ent)))
