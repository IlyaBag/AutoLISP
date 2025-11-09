(defun BG:cross_line_arc (/ keep_going_flag ln new_rd cp osm 3dosm prop pt1 pt2 ang sp ep)
  (setq keep_going_flag T)
  (if (null rd) (setq rd 1.75))			; если радиус дуги не определён, то устанавливаем его глобально
  
  (while keep_going_flag
    (initget "Радиус выХод")
    (setq ln (entsel "\nЛиния, которую необходимо разорвать, или [Радиус/выХод]: "))
    (cond
      ((= ln "Радиус")
       (initget (+ 2 4))			; флаги запрещают ввод нуля и отрицательных чисел
       (setq new_rd (getdist (strcat "\nНовый радиус дуги <" (rtos rd) ">: ")))
       (if new_rd				; если ввод не пустой
         (setq rd new_rd)			; присваиваем новый радиус дуги
       );end if
      );cond "Радиус"
      
      ((= ln "выХод")
       (setq keep_going_flag nil)
       (princ)
      );cond "выХод"
      
      ((and (= (type ln) 'LIST)
            (= (cdr (assoc 0 (entget (car ln)))) "LINE"))	; если выбран объект "LINE"
    
    (setq ln (car ln))
    (setq cp (getpoint "\nЦентральная точка разрыва:"))

    ;;; начало групповой отмены операций ------	;;;
    (command "_UNDO" "_begin")			;;;
    ;;; отключить привязки --------------------	;;;
    (setq osm (getvar "osmode")			;;;
	  3dosm (getvar "3dosmode"))		;;;
    (if (< osm 16384)				;;;
      (setvar "osmode" (+ osm 16384))		;;;
    )						;;;
    (setvar "3dosmode" 0)			;;;
    ;;; ---------------------------------------	;;;

    (setq prop (entget ln)			; свойства разрываемой линии
	  pt1  (cdr (assoc 10 prop))		; начальная точка разрываемой линии
	  pt2  (cdr (assoc 11 prop))		; конечная точка разрываемой линии
	  ang  (angle pt1 pt2)			; угол разрываемой линии
    );end setq

    (if (and (> ang (/ pi 2))			; если угол > 90гр. и <= 270гр.
	     (<= ang (* pi 1.5)))
        (setq ang (rem (+ ang pi) (* 2 pi)))	; увеличиваем угол на 180гр. и берем по модулю 360гр.
    );end if

    (setq sp (polar cp ang rd)			; начальная точка дуги
	  ep (polar cp (+ ang pi) rd)		; конечная точка дуги
    );end setq

    (command "_ARC" "_c" cp sp ep		; построить дугу по центральной точке, начальной точке и конечной точке
             "_BREAK" ln ;|"_f"|; sp ep)	; разорвать линию

    ;;; вернуть привязки ----------------------	;;;
    (setvar "osmode" osm)			;;;
    (setvar "3dosmode" 3dosm)			;;;
    ;;; конец групповой отмены операций -------	;;;
    (command "_UNDO" "_end")			;;;
    ;;; ---------------------------------------	;;;

      );cond 'LIST

      (
       (and (= (type ln) 'LIST)
	    (= (cdr (assoc 0 (entget (car ln)))) "LWPOLYLINE"))	; если выбран объект "LWPOLYLINE"

       (setq ln   (car ln)
	     prop (entget ln)
	     cp   (getpoint "\nЦентральная точка разрыва:"))
       ;;; найти все вершины полилинии
       (setq pts (vl-remove-if-not '(lambda (x) (= (car x) 10)) prop))
       
       (defun is_even (num)
	 (= (rem num 2) 0)
       );end defun is_even
       
       (if (not (is_even (cdr (assoc 70 prop))))	; если в dxf 70 есть битовый флаг 1 - замкнутая полилиния
	 (setq pts (append pts (list (car pts))))	; дублируем первую вершину в конце списка вершин
       );end if
       ;;; найти вершины, между которыми находится заданная точка
       (setq i 1)
       (while (< i (length pts))
	 (setq pt1 (cdr (nth (- i 1) pts))
	       pt2 (cdr (nth i pts)))
	 (if (BG:is_point_on_line pt1 pt2 cp)
	   (progn
	     (princ "\n")(princ pt1)(princ ", ")(princ cp)(princ ", ")(princ pt2)
	     (setq i (+ (length pts) 1))	; выход из цикла при нахождении точки между вершинами (i > length pts)
	   );end progn
	   (setq i (1+ i))			; если цикл завершился с i = length pts, значит заданная точка не лежит между вершинами
	 );end if
       );end while

       (if (> i (length pts))			; если найдены нужные вершины полилинии
	 ;;; if true
	 (progn
	   (setq ang  (angle pt1 pt2))		; угол разрываемого сегмента полилинии
	   (if (and (> ang (/ pi 2))		; если угол > 90гр. и <= 270гр.
		    (<= ang (* pi 1.5)))
	     (setq ang (rem (+ ang pi) (* 2 pi)))	; увеличиваем угол на 180гр. и берем по модулю 360гр.
	   );end if
	   (setq sp (polar cp ang rd)		; начальная точка дуги
		 ep (polar cp (+ ang pi) rd))	; конечная точка дуги

	   ;;; начало групповой отмены операций ------	;;;
	   (command "_UNDO" "_begin")			;;;
	   ;;; отключить привязки --------------------	;;;
	   (setq osm (getvar "osmode")			;;;
		 3dosm (getvar "3dosmode"))		;;;
	   (if (< osm 16384)				;;;
	     (setvar "osmode" (+ osm 16384))		;;;
	   )						;;;
	   (setvar "3dosmode" 0)			;;;
	   ;;; ---------------------------------------	;;;

	   (command "_ARC" "_c" cp sp ep)	; построить дугу по центральной точке, начальной точке и конечной точке
	   (setq entarc (entlast))
	   (command "_BREAK" ln sp ep)		; разорвать полилинию
	   (command "_JOIN" ln entarc (entlast) "")	; соединить разорванную полилинию с дугой

	   ;;; вернуть привязки ----------------------	;;;
	   (setvar "osmode" osm)			;;;
	   (setvar "3dosmode" 3dosm)			;;;
	   ;;; конец групповой отмены операций -------	;;;
	   (command "_UNDO" "_end")			;;;
	   ;;; ---------------------------------------	;;;
	 );end progn

	 ;;; if false
	 (princ "\nУказанная точка не лежит на полилинии")
       );end if
       
      );cond 'LWPOLYLINE
      
      (T
       (princ "\nЧто-то пошло не так. Попробуем ещё разок...")
      );cond T
      
    );end cond
  );end while
);end defun cross_line


(defun c:ПЕРЛИН() (BG:cross_line_arc))
