(ql:quickload :uiop)
(load "q3data.lisp")

(defun prepare (s)
  (map 'list #'uiop:split-string (uiop:split-string s :separator '(#\NewLine))))

(defun calc-order (line)
  (let* ((p1 (reverse (subseq (car line) 5)))
         (p2 (reverse (cadr line)))
         (acc 0))

    (dotimes (i (length p2))
      (let ((ac (digit-char-p (char p1 (rem i (length p1)))))
            (bc (digit-char-p (char p2 i)))
            (power i))

        (setq acc (+ acc (* bc (expt ac power))))))
    acc))

(defun calc-order-check (line)
  (let* ((p2 (cadr line))
         (acc (calc-order line))
         (p2n (read-from-string p2)))

    (dotimes (i p2n)
      (let* ((nline (list (car line) (format NIL "~a" i)))
             (nv (calc-order nline)))
        (when (eq nv acc)
          (return-from calc-order-check 0))))
    acc))

(defun calc (s)
  (reduce #'+ (map 'list #'calc-order-check (prepare s))))

(format T "Q3: ~a~%" (calc data-q3ex))
(format T "Q3: ~a~%" (calc data-q3))
