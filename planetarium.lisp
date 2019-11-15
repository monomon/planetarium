;;;; planetarium.lisp

(in-package #:planetarium)
(require :sdl2)
(require :cl-opengl)
(require :cl-glu)
(require :3d-vectors)
(require :3d-matrices)

;; number of segments in lat/lon lines
(defparameter *sphere-resolution* 30)
(defparameter *sphere-radius* 1)
;; number of segments in orbit ellipses
(defparameter *orbit-resolution* 50)
;; translate ticks to hours
(defparameter *hours-per-tick* 1)
(defparameter *camera-speed* 0.5)
(defparameter *movement-speed* 1e8)
;; angular speed of camera
(defparameter *rotation-speed* (/ cl:pi 12.0)) ;; degrees
(defparameter *max-scene-radius* 7e9)


(defstruct planet
  name
  diameter
  rot-period
  length-day
  perihelion
  aphelion
  orbit-period
  orbit-velocity
  orbit-inclination
  orbit-obliquity)

(defstruct camera
  "viewmat is 4x4, viewbox is 3x2"
  viewmat
  viewbox)

(defun main ()
  (sdl2:with-init (:everything)
	(format t "Using SDL version ~A.~A.~A~%"
			sdl2-ffi:+sdl-major-version+
			sdl2-ffi:+sdl-minor-version+
			sdl2-ffi:+sdl-patchlevel+)
	(let ((planets (initialize-planets (load-planet-data)))
		  (camera (initialize-camera)))
	  (format t "view~%~A~%" (3d-matrices:write-matrix (camera-viewmat camera) nil))
	  (format t "projection~%~A~%" (3d-matrices:write-matrix (camera-viewbox camera) nil))
	  (sdl2:with-window (win :flags '(:opengl))
		(sdl2:with-gl-context (gl-context win)
		  (setup-gl win gl-context camera)
		  (main-loop win #'render planets camera))))))

(defun main-loop (win render-fn planets camera)
  (sdl2:with-event-loop (:method :poll)
	(:idle ()
		   (funcall render-fn planets)
		   (sdl2:gl-swap-window win))
	(:quit () t)
	(:keyup (:keysym keysym)
			(let ((sc-value (sdl2:scancode-value keysym)))
			  (cond ((sdl2:scancode= sc-value :scancode-escape) (sdl2:push-event :quit)))))
	(:keydown (:keysym keysym)
			  (let ((sc-value (sdl2:scancode-value keysym))
					(viewbox (camera-viewbox camera))
					(viewmat (camera-viewmat camera))
					)
				(cond ((sdl2:scancode= sc-value :scancode-w)
					   (format t "forward~%")
					   (let ((trans (3d-matrices:mtranslation
									 (3d-vectors:v* (get-forward-vector viewmat) *movement-speed* -1.0))))
						 (3d-matrices:nm* viewmat trans)
						 (let ((ratio (/ (3d-matrices:mcref viewbox 0 0) (3d-matrices:mcref viewmat 2 3))))
						   (3d-matrices:nmscale viewbox (3d-vectors:vec3 ratio ratio ratio)))
						 ;; (3d-matrices:nm* viewbox trans)
						 ))
					  ((sdl2:scancode= sc-value :scancode-s)
					   (format t "backward~%")
					   (let ((trans (3d-matrices:mtranslation
									 (3d-vectors:v* (get-forward-vector viewmat) *movement-speed*))))
						 (3d-matrices:nm* viewmat trans)
						 (let ((ratio (/ (3d-matrices:mcref viewbox 0 0) (3d-matrices:mcref viewmat 2 3))))
						   (format t "Ratio is ~A~%" ratio)
						   (3d-matrices:nmscale viewbox (3d-vectors:vec3 ratio ratio ratio)))
						 ))
					  ((sdl2:scancode= sc-value :scancode-a)
					   (format t "roll left~%")
					   (3d-matrices:nmrotate viewmat
											 3d-vectors:+vz+ (* -1 *rotation-speed*)))
					  ((sdl2:scancode= sc-value :scancode-d)
					   (format t "roll right~%")
					   (3d-matrices:nmrotate viewmat 3d-vectors:+vz+ *rotation-speed*))
					  ((sdl2:scancode= sc-value :scancode-up)
					   (format t "pitch down~%")
					   (3d-matrices:nmrotate viewmat 3d-vectors:+vx+ (* -1 *rotation-speed*)))
					  ((sdl2:scancode= sc-value :scancode-down)
					   (format t "pitch up~%")
					   (3d-matrices:nmrotate viewmat 3d-vectors:+vx+ *rotation-speed*))
					  ((sdl2:scancode= sc-value :scancode-left)
					   (format t "look left~%")
					   (3d-matrices:nmrotate viewmat 3d-vectors:+vy+ (* -1 *rotation-speed*)))
					  ((sdl2:scancode= sc-value :scancode-right)
					   (format t "look right~%")
					   (3d-matrices:nmrotate viewmat 3d-vectors:+vy+ *rotation-speed*)))
				(update-camera camera)
				))))

(defun load-planet-data ()
  (with-open-file (str "planets.sexp")
	(read str)))

(defun initialize-planets (planet-data)
  (mapcar (lambda (p-data) (apply #'make-planet p-data)) planet-data))

(defun get-forward-vector (mat)
  (3d-matrices:with-fast-matref (access-mat mat 4)
	(3d-vectors:vec3 (access-mat 2 0) (access-mat 2 1) (access-mat 2 2))))

(defun initialize-camera ()
  (let ((neg-radius (* -1 *max-scene-radius*)))
	(make-camera
	 :viewmat (3d-matrices:mlookat
			   (3d-vectors:vec 0 0 neg-radius)
			   (3d-vectors:vec 0 0 0)
			   (3d-vectors:vec 0 1 0))
	 :viewbox (3d-matrices:m*
			   (3d-matrices:mortho
				neg-radius *max-scene-radius*
				neg-radius *max-scene-radius*
				neg-radius *max-scene-radius*)
			   (3d-matrices:mtranslation
				(3d-vectors:v* 3d-vectors:+vz+ *max-scene-radius*))))))

(defun update-camera (cam)
  (let ((viewmat (camera-viewmat cam))
		(viewbox (camera-viewbox cam)))

	;; (gl:matrix-mode :modelview)
	(gl:load-matrix (3d-matrices:marr
					 (3d-matrices:mtranspose
					  (3d-matrices:m* viewbox viewmat))))
	(format t "view~%~A~%" (3d-matrices:write-matrix viewmat nil))

	;; (gl:matrix-mode :projection)
	;; (gl:load-matrix (3d-matrices:marr (3d-matrices:mtranspose viewbox)))
	(format t "projection~%~A~%" (3d-matrices:write-matrix viewbox nil))))

(defun setup-gl (win gl-context camera)
  (format t "Setting up window~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 800 800)
  (update-camera camera)
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun uv-to-xyz (u v r)
  (list
   (* (cos u) (cos v) r)
   (* (cos u) (sin v) r)
   (* (sin u) r)))

(defun draw-points (points)
  (dolist (point points)
	(destructuring-bind (x y z) point
	  (gl:vertex x y z))))

(defun orbit-angle-from-time (time planet)
  "Allow for negative orbit period, though it doesn't happen"
  (* 2 pi (/
		   (mod time (planet-orbit-period planet))
		   (abs (planet-orbit-period planet)))))

(defun calculate-position-from-time (planet time)
  "Calculate position for a planet at the given time"
  (calculate-orbit-position
   planet
   (orbit-angle-from-time
	time
	planet)))

(defun calculate-orbit-position (planet theta)
  "Calculate rectangular coordinates in space from angle"
  (list (* (planet-aphelion planet) 1e6 (cos theta))
		(* (planet-perihelion planet) 1e6 (sin theta))
		0))

(defun draw-orbit (p)
  (let ((step (/ (* 2 pi) *sphere-resolution*)))
	(gl:begin :line-loop)
	(gl:color 0.4 0.4 0.6 0.2)
	(dotimes  (i (1+ *sphere-resolution*))
	  (destructuring-bind (x y z) (calculate-orbit-position p (* i step))
		(gl:vertex x y z)))
	(gl:end)))

(defun draw-planet (p time)
  (gl:push-matrix)
  (gl:rotate (planet-orbit-inclination p) 0 1 0)
  (gl:push-matrix)
  (draw-orbit p)
  (destructuring-bind (x y z) (calculate-position-from-time p time) (gl:translate x y z))
  (gl:color 1.0 0.0 0.0 1.0)
  (let ((q (glu:new-quadric)))
			  (glu:sphere q
						  (max (* 1e3 (/ (planet-diameter p) 2.0)) 2e7)
						  *sphere-resolution*
						  *sphere-resolution*))
  (gl:pop-matrix)
  (gl:pop-matrix))

(defun render (planets)
  (gl:clear :color-buffer)
  (gl:light :light0 :position (vector .8 .8 .8 1))
  (gl:enable :blend :texture-2d)
  (gl:color 1.0 0.0 0.0 1.0)
  ;; (gl:matrix-mode :modelview)
  (dolist (planet planets)
	(draw-planet planet (* (sdl2:get-ticks) *hours-per-tick*)))
  ;; draw sun
  (gl:color 1.0 1.0 0.0 1.0)
  (let ((q (glu:new-quadric)))
  	(glu:sphere q
  				(* 695700 1e2)
  				*sphere-resolution*
  				*sphere-resolution*))
  (gl:flush))
