(setq e (car (entsel)))
(setq p (entget e))
(princ "\np  - ")(princ p)
(setq p1 (subst (list 210 0.0 0.0 1.0) (assoc 210 p) p))
(princ "\np1 - ")(princ p1)
(entmod p1)
(entupd e)

;;;  https://forum.dwg.ru/showthread.php?t=18204
(entmakex ......
	  (append '(10) (trans p1 0 (list
				      (- (car p2) (car p1))
				      (- (cadr p2) (cadr p1))
				      (- (caddr p2) (caddr p1))
				    )
			)
	  )
	  (list 210
		(- (car p2) (car p1))
		(- (cadr p2) (cadr p1))
		(- (caddr p2) (caddr p1))
	  )
)

(trans <точка> <СК-из> <СК-в> [<признак>])


(defun round (num presc / n1)
  ;;;  округление вещественного числа до заданной точности
  ;;;    <num> - округляемое число
  ;;;    <presc> - требуемое количество знаков после запятой
  ;;;  (round 3.14159 2) вернёт 3.14
  ;;;  (round 2.71828 2) вернёт 2.72
  
  (setq n1 (* num (expt 10 presc)))
  (if (>= (rem n1 1) 0.5)
    (/ (+ (fix n1) 1) (expt 10.0 presc))
    (/ (fix n1) (expt 10.0 presc))
  )
)

(defun round6 (num)
  (round num 6)
)
;;; (entget (car (entsel)))

(setq e (car (entsel)))
(setq p (entget e))
(mapcar 'round6 (trans (cdr (assoc 10 p)) e 0))
