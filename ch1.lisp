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
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(defpackage :ch1 (:use :common-lisp))
;; for uiop
(require :asdf)
(in-package ch1)

(defparameter +w+ 500)
(defparameter +h+ 500)
(defparameter +r+ 100)
(defparameter +c+ (vector (/ +w+ 2) (/ +h+ 2)))


(defun v2 (x y)
    (vector x y))

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

(defun polar-project (c r theta)
  (v2floor (v2+ c (v2 (* r (cos theta))
		      (* r (sin theta))))))

;; entry point for chapter 1
(defun ch1-main ()
  (let ((img (make-array (list +w+ +h+) :initial-element 0.5)))
    (loop for theta from 0 to 6.2 by 0.1
	  do (color-pixels img (plot-line +c+ (v2floor (polar-project +c+ +r+ theta)))))
    (write-img-to-file img "out-ch1.ppm")))
