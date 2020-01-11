;; Chapter 2: Welcome to Lisp

(defun our-third (x)
  (car (cdr (cdr x))))

(defun sum-greater (x y z)
  (> (+ x y) z))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

(defun askem (string)
  (format t "~A" string)
  (read))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-re (i end)
  (if (> i end)
      'done
      (progn
        (format t "~A ~A~%" i (* i i))
        (show-squares-re (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length-re (lst)
  (if (null lst)
      0
      (+ (our-length (cdr lst)) 1)))

;; Exercises

(defun our-fourth (x)
  (car (cdr (cdr (cdr x)))))

(defun greater (x y)
  (if (eql x y)
      nil
      (if (> x y)
          x
          y)))

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

(defun list-element (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
          t
          (list-element (cdr lst)))))

(defun dot (n)
  (do ((i 0 (+ i 1)))
      ((eql i n) 'done)
    (format t ".")))

(defun dot-re (n)
  (if (eql n 0)
      'done
      (progn
        (format t ".")
        (dot-re (- n 1)))))

(defun count-a (lst)
  (let ((c 0))
    (dolist (obj lst)
      (if (eql 'a obj)
          (setf c (+ c 1))))
    c))

(defun count-a-re (lst)
  (if (eql 'a (car lst))
      (+ (count-a-re (cdr lst)) 1)
      0))

(defun summit (lst)
  (let ((len 0))
    (dolist (obj (remove nil lst))
      (setf len (+ len 1)))
    len))
