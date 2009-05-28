(setf *load-verbose* NIL)

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

#+sbcl
(require :asdf)

#+franz
(unless (find-package "ASDF")
  (load "/usr/local/bin/acl70/siteinit.cl"))

(push "/Users/grandchallengeweb/src/darpa-gc-forum-scraper/" asdf:*central-registry*)

(handler-bind ((warning #'ignore-warning))
  (asdf:operate 'asdf:load-op :darpa-gc-forum-scraper :verbose NIL))

(update-rss "darpa-gc-forum.db"
	    "darpa-gc-forum.xml")
