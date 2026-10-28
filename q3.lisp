(ql:quickload :uiop)
(load "q3data.lisp")

(defun prepare (s)
  (map 'list #'uiop:split-string (uiop:split-string s :separator '(#\NewLine))))

(defun calc-order (line)
  (let* ((p1 (reverse (subseq (car line) 4)))
         (p2 (reverse (cadr line)))
         (acc 0))

    (dotimes (i (length p2))
      (let ((ac (digit-char-p (char p1 (rem i (length p1)))))
            (bc (digit-char-p (char p2 i)))
            (power i))

        (format T "AC:~a BC:~a P:~a I:~a~%" ac bc power i)

        (setq acc (+ acc (* bc (expt ac power))))))
    (format T "~a => ~a~%" line acc)
    acc))

(defun calc (s)
  (reduce #'+ (map 'list #'calc-order (prepare s))))

(format T "Q1: ~a~%" (calc data-q1ex))
(format T "Q1: ~a~%" (calc data-q1))
(format T "Q2: ~a~%" (calc data-q2ex))
(format T "Q2: ~a~%" (calc data-q2))
;;(format T "Q3: ~a~%" (calc data-q3))
