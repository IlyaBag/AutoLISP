(defun cross_line (/ ln cp osm 3dosm prop pt1 pt2 ang rd sp ep)
  (while (setq ln (car (entsel "\nЛиния, которую необходимо разорвать:")))
    (setq cp (getpoint "\nЦентральная точка разрыва:"))

    ;;; начало групповой отмены операций ------	;;;
    (command "_UNDO" "_begin")			;;;
    ;;; отключить привязки --------------------	;;;
    (setq osm (getvar "osmode")			;;;
	  3dosm (getvar "3dosmode"))		;;;
    (setvar "osmode" (+ osm 16384))		;;;
    (setvar "3dosmode" 0)			;;;
    ;;; ---------------------------------------	;;;

    (setq prop (entget ln)			; свойства разрываемой линии
	  pt1 (cdr (assoc 10 prop))		; начальная точка разрываемой линии
	  pt2 (cdr (assoc 11 prop))		; конечная точка разрываемой линии
	  ang (angle pt1 pt2)			; угол разрываемой линии
	  rd 1.75				; радиус дуги
	  sp (polar cp ang rd)			; начальная точка дуги
	  ep (polar cp (+ ang pi) rd)		; конечная точка дуги
    ); end setq

    (princ "\ncp = ")
    (princ cp)
    (princ "\nsp = ")
    (princ sp)
    (princ "\nep = ")
    (princ ep)
    (princ "\n")
    (command "_ARC" "_c" cp sp ep)		; построить дугу по центральной точке, начальной точке и конечной точке
;;;    (command "_ARC" "_c" cp sp "_a" 180	; альтернативный вариант построения дуги - по центральной точке, начальной точке и углу
    (command "_BREAK" ln "_f" sp ep)		; разорвать линию

    ;;; вернуть привязки ----------------------	;;;
    (setvar "osmode" osm)			;;;
    (setvar "3dosmode" 3dosm)			;;;
    ;;; конец групповой отмены операций -------	;;;
    (command "_UNDO" "_end")			;;;
    ;;; ---------------------------------------	;;;
    
  ); end while
); end defun cross_line


(defun c:ПЛ () (cross_line))
;;;(command "_BREAK" (entsel) "_f" (list 522.0 207.0 0.0) (list 524.0 207.0 0.0))
;;;(command "_BREAK" (car(entsel)) "_f" '(522.0 207.0 0.0) '(524.0 207.0 0.0))