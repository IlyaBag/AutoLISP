(defun cross_line (/ ln cp prop pt1 pt2 ang rad sp ep)
  (setq ln (entsel "\nЛиния, которую необходимо разорвать:")
	cp (getpoint "\nЦентральная точка разрыва:"))

  (setq prop (entget (car ln))		; свойства разрываемой линии
	pt1 (cdr (assoc 10 prop))	; начальная точка разрываемой линии
	pt2 (cdr (assoc 11 prop))	; конечная точка разрываемой линии
	ang (angle pt1 pt2)		; угол разрываемой линии
	rad 1.75			; радиус дуги
	sp (polar cp ang rad)		; начальная точка дуги
	ep (polar cp (+ ang pi) rad))	; конечная точка дуги


  (command "_ARC" "_c" cp sp ep		; построить дугу по центральной точке, начальной точке и углу
           "_BREAK" ln "_f" sp ep)	; разорвать линию
); end defun cross_line
(cross_line)
