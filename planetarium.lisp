;;;; planetarium.lisp

;; Use row-major format, like the matrix library
;; Transpose before passing to opengl

(in-package #:planetarium)
(require :sdl2)
(require :cl-opengl)
(require :cl-glu)
(require :cl-jpeg)
(require :3d-vectors)
(require :3d-matrices)
(require :alexandria)

;; number of segments in lat/lon lines
(defparameter *sphere-resolution* 50)
(defparameter *sphere-radius* 1)
;; number of segments in orbit ellipses
(defparameter *orbit-resolution* 1200)
;; translate ticks to hours
(defparameter *hours-per-tick* 0.1)
;; amount to change time speed by
(defparameter *time-increment* 1e-2)
(defparameter *movement-speed* 10)
;; angular speed of camera
(defparameter *rotation-speed* (/ cl:pi 12.0)) ;; degrees
(defparameter *max-scene-radius* 1e10)
(defparameter *scale-factor* 1e-8)
(defparameter *scale-planets* t)
(defparameter *draw-orbits* t)
(defparameter *sun-radius* 695700)


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
  orbit-obliquity
  texture-file)

(defstruct planet-state
  position
  texture-id)

(defstruct star
  name
  diameter)

(defstruct camera
  viewmat
  viewbox)

(defun main ()
  (sdl2:with-init (:everything)
	  (format t "Using SDL version ~A.~A.~A~%"
			      sdl2-ffi:+sdl-major-version+
			      sdl2-ffi:+sdl-minor-version+
			      sdl2-ffi:+sdl-patchlevel+)
	  (let* ((planets (initialize-planets (load-planet-data)))
		       (planet-state (initialize-planet-state planets))
		       (sun (make-star
				         :name "sun"
				         :diameter (* *sun-radius* 2)))
		       (camera (initialize-camera)))
	    (format t "view~%~A~%" (3d-matrices:write-matrix (camera-viewmat camera) nil))
	    (format t "projection~%~A~%" (3d-matrices:write-matrix (camera-viewbox camera) nil))
      (format t "planet state: ~A~%" (maphash #'(lambda (k v) (format nil "~a => ~a~%" k v)) planet-state))
	    (sdl2:with-window (win :flags '(:opengl))
		    (sdl2:with-gl-context (gl-context win)
		      (setup-gl win gl-context camera)
		      (main-loop win #'render planets planet-state sun camera))))))

(defun main-loop (win render-fn planets planet-state sun camera)
  (sdl2:with-event-loop (:method :poll)
	  (:idle ()
		       (let ((time (* (sdl2:get-ticks) *hours-per-tick*))
				         (all-objs (cons sun planets)))
			       (funcall #'update-planet-positions
					            planets
					            planet-state
					            time)
			       ;; (sort-planets all-objs planet-positions camera)
			       (funcall render-fn
					            all-objs
					            planet-state
					            time)
			       (sdl2:gl-swap-window win)))
	  (:quit () t)
	  (:keyup (:keysym keysym)
			      (let ((sc-value (sdl2:scancode-value keysym)))
			        (cond ((sdl2:scancode= sc-value :scancode-escape) (sdl2:push-event :quit)))))
	  (:keydown (:keysym keysym)
			        (let* ((sc-value (sdl2:scancode-value keysym))
					           (viewbox (camera-viewbox camera))
					           (viewmat (camera-viewmat camera))
					           (forward-vector (3d-vectors:vunit (get-vector-from-row viewmat 2)))
					           (camera-updated nil)
					           )
				        (cond ((sdl2:scancode= sc-value :scancode-w)
					             (format t "forward~%")
					             (let* ((trans-vector (3d-vectors:v* forward-vector *movement-speed*))
							                (trans (3d-matrices:mtranslation
									                    trans-vector))
							                (viewcopy (3d-matrices:mcopy viewmat)))
						             (3d-matrices:nm* viewmat trans)
						             (setf camera-updated t)
						             ))
					            ((sdl2:scancode= sc-value :scancode-s)
					             (format t "backward~%")
					             (let* ((trans (3d-matrices:mtranslation
									                    (3d-vectors:v* forward-vector *movement-speed* -1.0))))
						             (3d-matrices:nm* viewmat trans)
						             (setf camera-updated t)
						             ))
					            ((sdl2:scancode= sc-value :scancode-a)
					             (format t "roll left~%")
					             (3d-matrices:nmrotate viewmat
											                       3d-vectors:+vz+ (* -1 *rotation-speed*))
					             (setf camera-updated t))
					            ((sdl2:scancode= sc-value :scancode-d)
					             (format t "roll right~%")
					             (3d-matrices:nmrotate viewmat
											                       3d-vectors:+vz+ *rotation-speed*)
					             (setf camera-updated t))
					            ((sdl2:scancode= sc-value :scancode-t)
                       (format t "toggle scale planets~%")
					             (if (equal *scale-planets* nil)
						               (setf *scale-planets* t)
						               (setf *scale-planets* nil)))
					            ((sdl2:scancode= sc-value :scancode-up)
					             (format t "pitch down~%")
					             (3d-matrices:nmrotate viewmat 3d-vectors:+vx+ (* -1 *rotation-speed*)))
					            ((sdl2:scancode= sc-value :scancode-down)
					             (format t "pitch up~%")
					             (3d-matrices:nmrotate viewmat 3d-vectors:+vx+ *rotation-speed*))
					            ((sdl2:scancode= sc-value :scancode-left)
					             (format t "look left~%")
					             (3d-matrices:nmrotate viewmat 3d-vectors:+vy+ *rotation-speed*))
					            ((sdl2:scancode= sc-value :scancode-right)
					             (format t "look right~%")
					             (3d-matrices:nmrotate viewmat 3d-vectors:+vy+ (* -1 *rotation-speed*)))
					            ((sdl2:scancode= sc-value :scancode-period)
                       (format t "increase time speed~%")
					             (incf *hours-per-tick* *time-increment*))
					            ((sdl2:scancode= sc-value :scancode-comma)
                       (format t "decrease time speed~%")
					             (decf *hours-per-tick* *time-increment*))
					            ((sdl2:scancode= sc-value :scancode-r)
                       (format t "reset camera~%")
					             (setf camera (initialize-camera)))
					            ((sdl2:scancode= sc-value :scancode-o)
                       (format t "toggle orbits~%")
					             (setf *draw-orbits* (not *draw-orbits*)))
					            ((sdl2:scancode= sc-value :scancode-8)
                       (format t "fast travel~%")
					             (setf *movement-speed* 10))
					            ((sdl2:scancode= sc-value :scancode-6)
                       (format t "slow travel~%")
					             (setf *movement-speed* 1)))
				        (let ((time (* (sdl2:get-ticks) *hours-per-tick*)))
				          (update-camera camera)
				          (update-planet-positions planets planet-state time)
				          (let ((all-objs (cons sun planets)))
					          ;; (when camera-updated (sort-planets all-objs planet-positions camera))
					          (funcall render-fn
							               all-objs
							               planet-state
							               time)))
				        ))))

(defun load-planet-data ()
  (with-open-file (str (merge-pathnames (asdf:system-source-directory :planetarium) "planets.sexp"))
	  (read str)))

(defun initialize-planets (planet-data)
  "Read the planets from disk and turn into planet instances, which contain read-only reference information."
  (mapcar (lambda (p-data) (apply #'make-planet p-data)) planet-data))

(defun initialize-planet-state (planet-data)
  "Initialize volatile state like positions, textures."
  (let* ((planet-state (make-hash-table))
         (num-textures (count-if #'(lambda (p) (not (null (planet-texture-file p)))) planet-data))
         (texture-ids (gl:gen-textures num-textures)))
    (format t "generated textures: ~A~%" texture-ids)
	  (setf (gethash "sun" planet-state)
          (make-planet-state
           :position (3d-vectors:vec3 0 0 0)
           :texture-id nil))
	  (dolist (p planet-data)
	    (setf
	     (gethash (planet-name p) planet-state)
	     (make-planet-state
        :position (3d-vectors:vec3 0 0 0)
        :texture-id (if (planet-texture-file p)
                        (make-texture (merge-pathnames (planet-texture-file p) "textures/") (pop texture-ids))
                        nil))))
	  planet-state))

(defun make-texture (texture-file texture-id)
  "Load texture into opengl and return texture id"
  (format t "loading texture from ~a with id ~a~%" texture-file texture-id)
  (handler-case
      (multiple-value-bind (img-data width height) (cl-jpeg:decode-image texture-file)
        (gl:bind-texture :texture-2d texture-id)
        (let ((data (make-array (list (* width height 3))
                                :element-type (array-element-type img-data)
                                :displaced-to img-data)))
          (format t "Loaded data. Image dimensions: ~Ax~A~%" width height)
          ;; copy data to texture
          (let ((level-of-detail 0)
                (internal-format :rgb)
                (border 0)
                (format :rgb)
                (data-type :unsigned-byte))
            (gl:tex-image-2d :texture-2d
                             level-of-detail
                             internal-format
                             width
                             height
                             border
                             format
                             data-type
                             data))
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)))
    (error (e)
      (format t "error while creating tetxure: ~a~%" e)
      (gl:delete-textures (list texture-id))))
    texture-id)

(defun update-planet-positions (planets planet-state time)
  "Update planet positions for given time"
  (dolist (p planets)
	  (setf
	   (planet-state-position (gethash (planet-name p) planet-state))
	   (calculate-position-from-time p time)))
  planet-state)

(defun get-vector-from-row (mat row)
  (3d-matrices:with-fast-matref (access-mat mat 4)
	  (3d-vectors:vec3 (access-mat row 0) (access-mat row 1) (access-mat row 2))))

(defun initialize-camera ()
  (let ((neg-radius (* -1 *max-scene-radius* *scale-factor*)))
	  (make-camera
	   :viewmat (3d-matrices:mlookat
			         (3d-vectors:vec 0 0 neg-radius)
			         (3d-vectors:vec 0 0 0)
			         (3d-vectors:vec 0 1 0))
	   :viewbox (3d-matrices:mperspective
			         90 1 0.1 (* *max-scene-radius* *scale-factor* 2)))))

(defun update-camera (cam)
  "Load the view matrix. OpenGL matrices are column-major, so transpose before loading"
  (let ((viewmat (camera-viewmat cam))
		    (viewbox (camera-viewbox cam)))

	  (gl:load-matrix (3d-matrices:marr
					           (3d-matrices:mtranspose
					            (3d-matrices:m* viewbox viewmat))))))

(defun setup-gl (win gl-context camera)
  (format t "Setting up window~%")
  (sdl2:gl-make-current win gl-context)
  (gl:enable :depth-test)
  (gl:enable :texture-2d)
  ;; (gl::tex-gen-i :s :texture-gen-mode :sphere-map)
  ;; (gl::tex-gen-i :t :texture-gen-mode :sphere-map)
  (gl:depth-func :lequal)
  (gl:viewport 0 0 800 800)
  (update-camera camera)
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun uv-to-xyz (u v r)
  "uv spherical coordinates to rectangular"
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
  (3d-vectors:vec3 (* (planet-aphelion planet) 1e6 (cos theta) *scale-factor*)
				           (* (planet-perihelion planet) 1e6 (sin theta) *scale-factor*)
				           0))

(defun draw-orbit (p)
  (let ((step (/ (* 2 pi) *sphere-resolution*)))
	  (gl:begin :line-loop)
	  (gl:color 0.2 0.2 0.3 0.2)
	  (dotimes  (i (1+ *sphere-resolution*))
	    (let ((pos (calculate-orbit-position p (* i step))))
		    (gl:vertex
		     (3d-vectors:vx pos)
		     (3d-vectors:vy pos)
		     (3d-vectors:vz pos))))
	  (gl:end)))

(defun draw-planet (p ps time)
  (gl:push-matrix)
  (gl:rotate (planet-orbit-inclination p) 0 1 0)
  ;; (gl:push-matrix)
  (when *draw-orbits* (draw-orbit p))
  (let ((pp (planet-state-position ps)))
    (gl:translate
     (3d-vectors:vx pp)
     (3d-vectors:vy pp)
     (3d-vectors:vz pp)))
  (gl:color 1.0 0.0 0.0 1.0)
  (let ((q (glu:new-quadric)))
    (if (planet-state-texture-id ps)
        (progn (gl:enable :texture-gen-s)
               (gl:enable :texture-gen-t)
               (glu:quadric-texture q :true)
               (gl:bind-texture :texture-2d (planet-state-texture-id ps)))
      (glu:quadric-texture q :false))
	  (glu:sphere q
				        (if *scale-planets*
					          (max (* 1e3 (/ (planet-diameter p) 2.0) *scale-factor*) 1e-2)
					          (* (/ (planet-diameter p) 2.0) *scale-factor*))
				        *sphere-resolution*
				        *sphere-resolution*))
  (gl:pop-matrix))

(defun draw-star (s time)
  (gl:push-matrix)
  (gl:color 1.0 1.0 0.0 1.0)
  (let ((q (glu:new-quadric)))
	  (glu:sphere q
				        (if *scale-planets*
					          (max (* 20 (/ (star-diameter s) 2.0) *scale-factor*) 1e-2)
					          (* (/ (star-diameter s) 2.0) *scale-factor*))
				        *sphere-resolution*
				        *sphere-resolution*))
  (gl:pop-matrix))

(defun get-z-position-in-camera (obj pp camera)
  (3d-vectors:vunit (get-vector-from-row
					           (3d-matrices:m*
					            (camera-viewmat camera)
					            (3d-matrices:mtranslation (gethash (slot-value obj 'name) pp)))
					           2)))

(defun compare-planets (p1 p2 pp camera)
  (3d-vectors:v<
   (get-z-position-in-camera p1 pp camera)
   (get-z-position-in-camera p2 pp camera)))

(defun sort-planets (planets planet-state camera)
  "sort by z axis relative to camera"
  (sort planets (lambda (p1 p2)
				          (compare-planets p1 p2 planet-state camera))))

(defun render (objects planet-state time)
  (handler-bind
	    ((error #'(lambda (e) (format t "~A~%" e))))
	  (gl:clear :depth-buffer-bit :color-buffer-bit)
	  (gl:light :light0 :position (vector .8 .8 .8 1))
	  (dolist (obj objects)
	    (typecase obj
		    (planet (draw-planet obj
							               (gethash (planet-name obj) planet-state)
							               time))
		    (star (draw-star obj time))
		    ))
	  (gl:flush)))
