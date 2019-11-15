(require :cl-opengl)
(require :3d-vectors)
(require :3d-matrices)


(let* ((min-radius -1e9)
	   (max-radius 1e9)
	   (proj (3d-matrices:mortho
			  min-radius max-radius
			  min-radius max-radius
			  min-radius max-radius
			  ))
	   (view (3d-matrices:mlookat
			  (3d-vectors:vec 0 0 1e9)
			  (3d-vectors:vec 0 0 0)
			  (3d-vectors:vec 0 1 0)))
	   (speed 1.2e4))
  (3d-matrices:with-fast-matref (access-mat view 4)
	(format t "~A~%" (3d-vectors:vec3 (access-mat 2 0) (access-mat 2 1) (access-mat 2 2)))
	)
  (format t "~A~%" (3d-matrices:nmscale proj (3d-vectors:vec3 1.1 1.1 1.1))))
