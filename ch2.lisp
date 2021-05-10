;; #!/usr/bin/env sbcl --script
;; https://lisp-lang.org/style-guide/
;; https://lispcookbook.github.io/cl-cookbook/arrays.html
;; https://lispcookbook.github.io/cl-cookbook/getting-started.html
;; https://lispcookbook.github.io/cl-cookbook/arrays.html
;; vector v/s array: https://stackoverflow.com/questions/28908249/difference-between-array-vector-types-in-common-lisp
;; http://www.gigamonkeys.com/book/collections.html
;; https://common-lisp.net/project/metabang-bind/user-guide.html
;; http://clhs.lisp.se/Body/m_w_out_.htm
;; SLIME: https://www.youtube.com/watch?v=_B_4vhsmRRI
;; https://ambrevar.xyz/modern-common-lisp/
;; to change package, use ", set package"

;; Destructuring bind:
;; http://www.lispworks.com/documentation/HyperSpec/Body/m_destru.htm

(declaim (optimize (speed 0) (safety 3) (debug 3)))
(defpackage :ch2 (:use :common-lisp))
;; for uiop
(require :asdf)
(in-package ch2)

(defparameter +w+ 500)
(defparameter +h+ 500)
(defparameter +r+ 100)
(defparameter +c+ (list (/ +w+ 2) (/ +h+ 2)))
(defun v3 (x y z) (list x y z))
(defun v3->x (v) (elt v 0))
(defun v3->y (v) (elt v 1))
(defun v3->z (v) (elt v 2))


;;  check that this is indeed correct
(defun v3cross (p q)
  (defun component (f g c d)
    (-  (*  (funcall f c) (funcall g d))
	(* (funcall g c) (funcall f d))))
  (v3 (component #'v3->y #'v3->z p q)
      (component #'v3->z #'v3->x p q)
      (component #'v3->x #'v3->y p q)))

(v3cross (v3 1 0 0) (v3 1 0 0))
(v3cross (v3 1 0 0) (v3 0 1 0))
(v3cross (v3 1 0 0) (v3 0 0 1))

(v3cross (v3 0 1 0) (v3 1 0 0))
(v3cross (v3 0 1 0) (v3 0 1 0))
(v3cross (v3 0 1 0) (v3 0 0 1))

(v3cross (v3 0 0 1) (v3 1 0 0))
(v3cross (v3 0 0 1) (v3 0 1 0))
(v3cross (v3 0 0 1) (v3 0 0 1))

(defun v2 (x y) (list x y))

(defun v2->x (v)
  (elt v 0))

(defun v2->y (v)
  (elt v 1))


(defun v2+ (a b)
  (v2 (+ (elt a 0) (elt b 0))
      (+ (elt a 1) (elt b 1))))

(defun v2- (a b)
  (v2 (- (elt a 0) (elt b 0))
      (- (elt a 1) (elt b 1))))

(defun v2* (c v)
  (v2 (* c (elt v 0))
      (* c (elt v 1))))

(defun v2floor (v)
  (v2 (floor (v2->x v)) (floor (v2->y v))))

;; P = aA + bB + cC
;; a + b + c = 1 from geometry
;; P = (1 - b - c) A + bC + cC
;; 
;; P = (1 - u - v) A + uB + vC
;; P = A + u(B - A) + v(C - A)
;; P = A + uAB + vAC
;; 0 = (A - P) + uAB + vAC
;; 0 = PA + uAB + vAC
;; 0 = (PA, AB, AC)^T (u, v, 1)
;; These are two eqns:
;; ------------------
;; 0 = (PAx, ABx, BCx) . (u, v, 1)
;; 0 = (PAy, ABy, BCy) . (u, v, 1)
;; coeff of A is (1 - u - v). Will ne negative if 1 - ((u + v)/w) < 0 or
;; fabs(u + v) > fabs(w).
(defun bary (tri p)
  (let* ((a (v3->x tri))
	 (b (v3->y tri))
	 (c (v3->z tri))
	 (pa (v2- a p))
	 (ab (v2- b a))
	 (ac (v2- c a))
	 (orth (list ac ab pa))
	 (crossv  (funcall #'v3cross
			   (mapcar #'v2->x orth)
			   (mapcar #'v2->y orth)))
	 (u (v3->x crossv))
	 (v (v3->y crossv))
	 ;;  w = normalization constant.
	 (w (v3->z crossv)))
    
    (if (>  (abs (+ u v)) (abs w))
	(v3 -1 1 1)
	(v3 (- 1 (/ (+ u v) w))
	    (/ v w)
	    (/ u  w)))))


(bary (list '(3 0) '(0 3) '(0 0)) '(3 0))
(bary (list '(3 0) '(0 3) '(0 0)) '(0 3))
(bary (list '(3 0) '(0 3) '(0 0)) '(0 0))
(bary (list '(3 0) '(0 3) '(0 0)) '(4 0))
(bary (list '(3 0) '(0 3) '(0 0)) '(0 4))
(bary (list '(3 0) '(0 3) '(0 0)) '(4 4))

;; turn on a pixel
(defun color-pixel (img p)
  (setf (aref img (v2->x p) (v2->y p)) 1.0))

;; turn on pixels
(defun color-pixels (img ps) (loop for p in ps do (color-pixel img p)))

(defun img-to-str (img)
   (with-output-to-string (str)
     (format str  "P3 ~d ~d 255 " +w+ +h+)
     (loop for xcur from 0 below +w+
	   do (loop for ycur from 0 below +h+
		    for color = (floor (* 255 (aref img xcur ycur)))
		    do (format str "~a ~a ~a " color color color)))))

(defun write-img-to-file (img relative-file-path)
  (let ((full-path (merge-pathnames (uiop:getcwd) relative-file-path)))
    (format t "writing image to path |~a|..." full-path)
    (with-open-file (f full-path
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format f (img-to-str img)))
    (format t " done writing image.")))


;; assumption x0 < x1
;; assumption dx > dy
;; https://github.com/ssloy/tinyrenderer/wiki/Lesson-1:-Bresenham%E2%80%99s-Line-Drawing-Algorithm#third-attempt
(defun lerp (time start end)
  (+ (* (- 1 time) start)
     (* time end)))

;; assumption: delta x > delta y, x1 > x0
(defun plot-line-low (x0 y0 x1 y1)
  (loop for xcur from x0 to x1
	for xt = (/ (- xcur x0) (- x1 x0))
	collect (list xcur (floor (lerp xt y0 y1)))))

;; assumption: delta y > delta x, y1 > y0
(defun plot-line-high (x0 y0 x1 y1)
  (loop for ycur from y0 to y1
	for yt = (/ (- ycur y0) (- y1 y0))
	collect (list (floor (lerp yt x0 x1)) ycur)))

(defun plot-line (p q)
  (let ((Dx (abs (- (v2->x p) (v2->x q))))
	(Dy (abs (- (v2->y p) (v2->y q)))))
    (if (> Dx Dy)
	(if (< (v2->x p) (v2->x q)) ;; from smaller x -> larger x
	    (plot-line-low (v2->x p) (v2->y p) (v2->x q) (v2->y q))
	    (plot-line-low (v2->x q) (v2->y q) (v2->x p) (v2->y p)))
	(if (< (v2->y p) (v2->y q)) ;; smaller y -> larger y
	    (plot-line-high (v2->x p) (v2->y p) (v2->x q) (v2->y q))
	    (plot-line-high (v2->x q) (v2->y q) (v2->x p) (v2->y p))))))

;; returns pair of vec2 (minx, miny) (maxx, maxy)
(defun aabb (pts)
  (let
      ((xs (mapcar #'v2->x pts))
       (ys (mapcar #'v2->y pts)))
    (list (v2 (apply #'min xs) (apply #'min ys))
	  (v2 (apply #'max xs) (apply #'max ys)))))

(aabb (list  (v2 0 0)
	     (v2 0 1)
	     (v2 1 0)
	     (v2 1 1)))

(defun plot-tri (tri)
  (destructuring-bind (minv maxv) (aabb tri)
    (loop for x from (v2->x minv) to (v2->x maxv)
	  append (loop :for y from (v2->y minv) to (v2->y maxv)
			:for p = (v2 x y)
			:when (>= (apply #'min (bary tri p)) 0)
			  collect p))))

(plot-tri (v3 (v2 0 0) (v2 0 5) (v2 5 0)))


(defun polar-project (c r theta)
  (v2floor (v2+ c (v2 (* r (cos theta))
		      (* r (sin theta))))))

(defun ch2-main ()
  (let ((img (make-array (list +w+ +h+) :initial-element 0.5)))
    (color-pixels img (plot-tri (v3 +c+
				    (v2+ +c+ (v2 +r+ 0))
				    (v2+ +c+ (v2 0 +r+)))))
    (write-img-to-file img "out-ch2.ppm")))


