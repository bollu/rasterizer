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

(in-package ch1)

(defparameter +w+ 800)
(defparameter +h+ 600)

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

(defvar *img* (make-array (list +w+ +h+) :initial-element 0.0))


;; turn on a pixel
(defun color-pixel (p)
  (setf (aref *img* (v2->x p) (v2->y p)) 1.0))

;; turn on pixels
(defun color-pixels (ps) (loop for p in ps do (color-pixel p)))

(defun img-to-str ()
  (loop for xcur from 0 below +w+
        do (loop for ycur from 0 below +h+
                 for color = (aref *img* xcur ycur)
                 do (with-output-to-string (str)
                        (format str "~a ~a ~a" color color color)))))

(defun write-img-to-file (path)
  (with-open-file (f path :direction :output)
    (write (format NIL  "~d ~d" +w+ +h+))
    (write (img-to-str))))

;; assumption x0 < x1
;; assumption dx > dy
;; https://github.com/ssloy/tinyrenderer/wiki/Lesson-1:-Bresenham%E2%80%99s-Line-Drawing-Algorithm#third-attempt

(defun lerp (time start end)
  (+ (* (- 1 time) start)
     (* time end)))


;; assumption: delta x > delta y
(defun plot-line-low (x0 y0 x1 y1)
    (loop for xcur from x0 to x1
          for xt =  (/ (- xcur x0) (- x1 x0))
          collect (list xcur (lerp xt y0 y1))))

(defun plot-line (p q)
  (let ((Dx (abs (- (v2->x p) (v2->x q))))
        (Dy (abs (- (v2->y p) (v2->y q)))))
    (if (> Dx Dy)
        (plot-line-low (v2->x p) (v2->y p) (v2->x q) (v2->y q))
        (plot-line-low (v2->y p) (v2->x p) (v2->y q) (v2->x q)))))


(defun polar-project (c r theta)
  (v2+ c (v2 (* r (cos theta))
             (* r (sin theta)))))

(loop for theta from 0 to 6.14
    do (plot-line +c+ (polar-project +c+ +r+ theta)))
(write-img-to-file "out-ch1.ppm")
