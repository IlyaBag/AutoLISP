;;; Программа для создания 3D-моделей воротниковых фланцев по параметрам,
;;; заданным в текстовом файле


;==============================================================================
(defun draw_flange (Dm Dn d1_ b_ H H1 D D1 d_ n_ D2 h_
		    / osm 3dosm ent1 ent2 ent3 flange bolt_hole_angle i
		    hole_maker next_hole_maker)
  ;;; начало отмены/undo
  (command "_UNDO" "_begin")
  
  ;;; отключить привязки
  (setq osm (getvar "osmode")
	3dosm (getvar "3dosmode")
  )
  (setvar "osmode" 0)
  (setvar "3dosmode" 0)

  ;;;  построение тела фланца
  (command "_CONE" "0,0" (/ D2 2) "_t" (+ (/ D2 2) h_) h_)		;точка 0,0,0 ; D2/2; D2/2+h; h
  (setq ent1 (entlast))
  (command "_CYLINDER" (strcat "0,0," (rtos h_)) "_d" D (- b_ h_))	;точка 0,0,h; D; b-h
  (setq ent2 (entlast))
  (command "_CONE" (strcat "0,0," (rtos b_)) (/ Dm 2) "_t" (/ Dn 2) (- H H1 b_)) ;точка 0,0,b ; Dm/2; Dn/2; H-H1-b
  (setq ent3 (entlast))
  (command "_CYLINDER" (strcat "0,0," (rtos (- H H1))) "_d" Dn H1)	;точка 0,0,(H-H1); Dn; H1
  (command "_UNION" ent1 ent2 ent3 (entlast) "")
  (setq flange (entlast))

  ;;;  вырезание центрального отверстия
  (command "_CYLINDER" "0,0,0" "_d" d1_ H)				;точка 0,0,0; d1; H
  (command "_SUBTRACT" flange "" (entlast) "")

  ;;;  вырезание отверстий под болты
  (command "_CYLINDER" "0,0,0" "_d" d_ b_)				;точка 0,0,0; d; b
  (setq hole_maker (entlast))
  (command "_MOVE" hole_maker "" (strcat (rtos (/ D1 2)) ",0") "")	;смещение D1,0
  (setq bolt_hole_angle (/ 360.0 n_))					;угол между отверстиями под болты
  (command "_ROTATE" hole_maker "" "0,0,0" (rtos (/ bolt_hole_angle 2)));точка 0,0,0; поворот на 360/n/2
  
  (setq i 0)
  (while (< i n_)
    (command "_ROTATE" hole_maker "" "0,0,0" "_copy" (rtos bolt_hole_angle)) ;точка 0,0,0; поворот на 360/n
    (setq next_hole_maker (entlast))
    (command "_SUBTRACT" flange "" hole_maker "")
    (setq hole_maker next_hole_maker)
    (setq i (1+ i))
  ); end while
  (entdel hole_maker)
  
  ;;; вернуть привязки
  (setvar "osmode" osm)
  (setvar "3dosmode" 3dosm)
  
  ;;; конец отмены/undo
  ;;;  (vla-endundomark active_document)
  (command "_UNDO" "_end")
); end defun draw_flange



;==============================================================================
; разделить строку на список подстрок по заданному разделителю
(defun BG:str_split (str br / sl ss sp)
  ;;;  (substr <строка> <начальная позиция (нач с 1)> [<длина подстроки>])
  ;;;  (vl-string-search <образец> <строка> [<начальная позиция>]) -> индекс искомого
  (setq ss str)						; substring
  (while (setq sp (vl-string-search br ss))		; space position
    (setq sl (append sl (list (substr ss 1 sp))))	; strings list
    (setq ss (substr ss (+ sp (strlen br) 1)))
  ); end while
  (setq sl (append sl (list ss)))
); end defun str_split



;==============================================================================
(defun draw_flanges_with_dimensions_from_file (path / file lineData lineDataList
					       Dm Dn d1_ b_ H H1 D D1 d_ n_ D2 h_)
  (if (setq file (open path "r"))
    (progn
      (read-line file)					; пропускаем первую строку (шапку таблицы)
      (while (setq lineData (read-line file))
        (setq lineDataList (BG:str_split lineData ";"))
	;;; (nth 0 lineDataList) - DN, не участвует в построении
        (setq Dm  (atof (nth 1 lineDataList))
              Dn  (atof (nth 2 lineDataList))
              d1_ (atof (nth 3 lineDataList))
              b_  (atof (nth 4 lineDataList))
              H	  (atof (nth 5 lineDataList))
              H1  (atof (nth 6 lineDataList))
              D	  (atof (nth 7 lineDataList))
              D1  (atof (nth 8 lineDataList))
              d_  (atof (nth 9 lineDataList))
              n_  (atoi (nth 10 lineDataList))
              D2  (atof (nth 11 lineDataList))
              h_  (atof (nth 12 lineDataList))
        ); end setq
        (draw_flange Dm Dn d1_ b_ H H1 D D1 d_ n_ D2 h_)
      ); end while
      (close file)
    ); end progn
    (alert (strcat "ERROR: File not found\n" path))
  ); end if
  (princ)
); end defun get_dimensions_from_file



;==============================================================================
(defun c:df ()
  (draw_flanges_with_dimensions_from_file
    (getfiled "Выбрать файл параметров модели" "d:\\Б****в\\МоиФайлы\\LISP\\MyLisp\\" "csv" 2)
  )
)
