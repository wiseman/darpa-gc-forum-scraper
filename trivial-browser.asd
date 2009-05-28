(in-package :asdf)

(defsystem :trivial-browser
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :licence "MIT"
    :depends-on (:trivial-http)
    :components ((:file "trivial-browser-package")
		 (:file "trivial-http-patches")
	         (:file "trivial-browser"
			:depends-on ("trivial-browser-package"))))
