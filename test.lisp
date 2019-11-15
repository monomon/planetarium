(with-open-file (str "planets.sexp")
  (print (read str)))

(pushnew #P"/usr/local/lib"
		 cffi:*foreign-library-directories*
		 :test #'equal)
