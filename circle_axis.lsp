;;; Построение осевых линий поперечного разреза трубопровода
(defun circle_axis (/ osm 3dosm obj prop cen r p1 p2)
  
  ;;; начало групповой отмены операций --------	;;;
  (command "_UNDO" "_begin")			;;;
  ;;; отключить привязки ----------------------	;;;
  (setq osm (getvar "osmode")			;;;
	3dosm (getvar "3dosmode"))		;;;
  (setvar "osmode" 0)				;;;
  (setvar "3dosmode" 0)				;;;
  ;;; -----------------------------------------	;;;

  (if (not (setq a (getint "\nУдлиннение оси за окружность: <30>")))
    (setq a 30)
  ); end if

  (setq obj (car (entsel "\nВыберите окружность:")))
  (setq prop (entget obj))
  (if (not (= (cdr (assoc 0 prop)) "CIRCLE"))
    (alert "ОШИБКА: Выбрана не окружность!")
  ); end if
  
  (setq cen (cdr (assoc 10 prop)))		; координаты центра окружности
  (setq r (cdr (assoc 40 prop)))		; радиус окружности
  (setq p1 (polar cen pi (+ r a)))		; начальная точка горизонтальной оси
  (setq p2 (polar cen 0 (+ r a)))		; конечная точка горизонтальной оси
  
  (command "_LINE" p1 p2 "")			; строим горизонтальную ось
  (command "_ROTATE" (entlast) "" cen "_c" 90)	; строим вертикальную ось
    
  ;;; вернуть привязки ------------------------	;;;
  (setvar "osmode" osm)				;;;
  (setvar "3dosmode" 3dosm)			;;;
  ;;; конец групповой отмены операций ---------	;;;
  (command "_UNDO" "_end")			;;;
  ;;; -----------------------------------------	;;;
  
); end defun circle_axis



(defun c:КРОСИ ()
  (circle_axis)
)