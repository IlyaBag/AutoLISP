(setq p1 (getpoint "\nУкажите центр окружности: "))
(setq p2 (getpoint "\nУкажите любую точку на окружности"))
(setq r1 (distance p1 p2))				; задать радиус окружности
(setq p3 (polar p1 pi (+ r1 30)))			; начальная точка удлиннённой линии
(setq p4 (polar p1 0 (+ r1 30)))			; конечная точка удлиннённой линии
(command "_line" p3 p4 "")				; удлиннённая линия
(command "_rotate" (entlast) "" p1 "_c" 90)		; поворот линии с копированием


(defun c:ca (/ crcl cProp cntr_pnt rds p1 p2 p3 p4)
  (setq crcl (car (entsel "\nВыберите окружность:")))
  (setq cProp (entget crcl))
  (if (not (= (cdr (assoc 0 cProp)) "CIRCLE"))
    (alert "Выбрана не окружность")
  ); end if
  (setq cntr_pnt (cdr (assoc 10 cProp)))
  (setq rds (cdr (assoc 40 cProp)))
  (setq p1 (polar cntr_pnt pi (+ rds 30)))
  (setq p2 (polar cntr_pnt 0 (+ rds 30)))
  (entmake (list (cons 0 "line") (cons 10 p1) (cons 11 p2)))
  (setq p3 (polar cntr_pnt (/ pi 2) (+ rds 30)))
  (setq p4 (polar cntr_pnt (* 1.5 pi) (+ rds 30)))
  (entmake (list (cons 0 "line") (cons 10 p3) (cons 11 p4)))
); end
