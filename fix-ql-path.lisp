(setf ql:*local-project-directories* (append (list #P"/mnt/data/Projects/planetarium/") ql:*local-project-directories*))
;; "/usr/lib/x86_64-linux-gnu/libSDL2-2.0.so.0"
(ql:quickload 'planetarium)
(planetarium::main)
(ql:update-all-dists)
(merge-pathnames (asdf:system-source-directory :planetarium) "planets.sexp")
(ql:update-client)
(ql:quickload 'cffi)

(setf cffi:*foreign-library-directories* (list #P"/usr/local/lib/" #P"/usr/lib/" #P"/usr/lib/x86_64-linux-gnu/"))
(ql:quickload 'sdl2)
(progn
  (require 'uiop)
  (setf (uiop:getenv "PATH") (concatenate 'string (uiop:getenv "PATH") ":" "/usr/lib/x86_64-linux-gnu/")))

(setf (uiop:getenv "LD_LIBRARY_PATH") (concatenate 'string "/usr/lib/x86_64-linux-gnu/" ":" "/usr/local/lib/" ":" "/usr/lib"))
