;;compression: run-length encoding
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))
(defun compr (elem n lst)
  (if (null lst)
      (list (n-elems elem n))
      (let ((next (car lst)))
      (if (eql next elem)
	  (compr elem (+ n 1) (cdr lst))
	  (cons (n-elems elem n)
		(compr next 1 (cdr lst)))))))
(defun n-elems (elem n)
  (if (> n 1)
      (list n elem)
      elem))


(defun uncompress (lst) 
  (if (null lst)
      nil
      (let ((elem (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elem)
            (append (apply #'list-of elem)
		    rest)
		    (cons elem rest)))))

(defun list-of (n elem)
  (if (zerop n)
      nil
      (cons elem (list-of (- n 1) elem))))
      
