(in-package :asdf)

(defsystem :darpa-gc-forum-scraper
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :licence "MIT"
    :depends-on (:trivial-browser
		 :cl-ppcre
		 :araneida
		 :net-telent-date
		 :xmls)
    :components ((:file "xmls-patch")
		 (:file "url-finder")
		 (:file "darpa-scraper"
			:depends-on ("xmls-patch" "url-finder"))))
