(require :cl-opengl)
(require :cl-glu)
(require :sdl2)
(require :3d-vectors)
(require :3d-matrices)

(setf (uiop:getenv "LD_LIBRARY_PATH") (concatenate 'string "/usr/lib/x86_64-linux-gnu/" ":" "/usr/local/lib/" ":" "/usr/lib"))

(let* ((min-radius -1e9)
	   (max-radius 1e9)
	   (proj (3d-matrices:mperspective 75 1 min-radius max-radius))
	   (view (3d-matrices:mlookat
			  (3d-vectors:vec 0 0 1e9)
			  (3d-vectors:vec 0 0 0)
			  (3d-vectors:vec 0 1 0)))
	   (speed 1.2e4))
  (3d-matrices:with-fast-matref (access-mat view 4)
	(format t "~A~%" (3d-vectors:vec3 (access-mat 2 0) (access-mat 2 1) (access-mat 2 2)))
	)
  (format t "~A~%" (3d-matrices:write-matrix (3d-matrices:nmscale proj (3d-vectors:vec3 1.1 1.1 1.1)) nil))
  )

(sdl2:with-init (:everything)
  (sdl2:with-window (win :flags '(:opengl))
	(sdl2:with-gl-context (gl-context win)
	  (sdl2:gl-make-current win gl-context)
	  (gl:viewport 0 0 800 800)
	  (gl:enable :depth-test)
	  (let ((viewmatrix (3d-matrices:mlookat
						 (3d-vectors:vec3 0 0 -1)
						 (3d-vectors:vec3 0 0 0)
						 (3d-vectors:vec3 0 0 -1)))
			(proj (3d-matrices:mortho
				   -1 1
				   -1 1
				   -1 1))
			(q (glu:new-quadric)))
		(gl:load-matrix (3d-matrices:marr (3d-matrices:mtranspose (3d-matrices:m* viewmatrix proj))))
		(glu:sphere q 0.5 30 30)))))
