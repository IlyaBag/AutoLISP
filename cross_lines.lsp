(load "useful_features")                          ; BG:is-point-on-line


(defun BG:cross-line-arc (/ keep_going_flag ln new_rd cp osm 3dosm prop pt1 pt2 ang sp ep)
;;;  Функция запрашивает выбор объекта (отрезок, компактная полилиния) и точки на нём, строит в указанной
;;;  точке дугу заданного радиуса и разрывает объект по размеру дуги. В случае полилинии полученные после
;;;  разрыва "половины" полилинии объединяются с дугой в одну полилинию.
;;;  Применение - построение дуги на пересечении труб/кабелей на схемах и планах.
  (setq keep_going_flag T)
  (if (null BG:glob_radius)                       ; если радиус дуги не определён
    (setq BG:glob_radius 1.75)                    ; устанавливаем радиус глобально
  ) ;_ end if

  (while keep_going_flag
    (initget "Радиус выХод")
    (setq ln (entsel "\nЛиния, которую необходимо разорвать, или [Радиус/выХод]: "))
    (cond
      ( ;_ cond #1
       (= ln "Радиус")
       (initget (+ 2 4))                          ; флаги запрещают ввод нуля и отрицательных чисел
       (setq new_rd (getdist (strcat "\nНовый радиус дуги <" (rtos BG:glob_radius) ">: ")))
       (if new_rd                                 ; если ввод не пустой
         (setq BG:glob_radius new_rd)             ; присваиваем новый радиус дуги
       ) ;_ end if
      )

      ( ;_ cond #2
       (= ln "выХод")
       (setq keep_going_flag nil)
       (princ)
      )

      ( ;_ cond #3
       ;; если выбран объект "LINE"
       (and (= (type ln) 'LIST)
            (= (cdr (assoc 0 (entget (car ln)))) "LINE")
       ) ;_ end and

       (setq cp   (getpoint "\nЦентральная точка разрыва:")
             ln   (car ln)                        ; разрываемая линия
             prop (entget ln)                     ; свойства разрываемой линии
             pt1  (cdr (assoc 10 prop))           ; начальная точка разрываемой линии
             pt2  (cdr (assoc 11 prop))           ; конечная точка разрываемой линии
             ang  (angle pt1 pt2)                 ; угол разрываемой линии
       ) ;_ end setq

       (if (and (> ang (/ pi 2))                  ; если угол > 90гр. и <= 270гр.
                (<= ang (* pi 1.5))
           ) ;_ end and
         (setq ang (rem (+ ang pi) (* 2 pi)))     ; увеличиваем угол на 180гр. и берем по модулю 360гр.
       ) ;_ end if

       (setq sp (polar cp ang BG:glob_radius)     ; начальная точка дуги
             ep (polar cp (+ ang pi) BG:glob_radius) ; конечная точка дуги
       ) ;_ end setq

       ;; начало групповой отмены операций ------ ;
       (command "_UNDO" "_begin")                 ;
       ;; отключить привязки -------------------- ;
       (setq osm   (getvar "osmode")              ;
             3dosm (getvar "3dosmode"))           ;
       (if (< osm 16384)                          ;
         (setvar "osmode" (+ osm 16384))          ;
       )                                          ;
       (setvar "3dosmode" 0)                      ;
       ;; --------------------------------------- ;

       (command "_ARC" "_c" cp sp ep              ; построить дугу по центральной, начальной и конечной точкам
                "_BREAK" ln ;|"_f"|; sp ep)       ; разорвать линию под дугой

       ;; вернуть привязки ---------------------- ;
       (setvar "osmode" osm)                      ;
       (setvar "3dosmode" 3dosm)                  ;
       ;; конец групповой отмены операций ------- ;
       (command "_UNDO" "_end")                   ;
       ;; --------------------------------------- ;
      )

      ( ;_ cond #4
       ;; если выбран объект "LWPOLYLINE"
       (and (= (type ln) 'LIST)
            (= (cdr (assoc 0 (entget (car ln)))) "LWPOLYLINE")
       ) ;_ end and
       (setq cp   (getpoint "\nЦентральная точка разрыва:")
             ln   (car ln)
             prop (entget ln)
       ) ;_ end setq
       ;; формируем список со всеми вершинами полилинии
       (setq vrts (vl-remove-if-not '(lambda (x) (= (car x) 10)) prop))

       ;;----------------------------------------------------------------------
       (defun is_even (num)
         (= (rem num 2) 0)
       ) ;_ end defun
       ;;----------------------------------------------------------------------

       (if (not (is_even (cdr (assoc 70 prop))))  ; если в dxf 70 есть битовый флаг 1, то полилиния замкнута
         (setq vrts (append vrts (list (car vrts)))) ; тогда дублируем первую вершину в конце списка вершин
       ) ;_ end if
       ;; ищем вершины, между которыми находится заданная точка
       (setq i 1)
       (while (< i (length vrts))
         (setq pt1 (cdr (nth (- i 1) vrts))
               pt2 (cdr (nth i vrts))
         ) ;_ end setq
         (if (BG:is-point-on-line pt1 pt2 cp)     ; если указанная точка лежит между текущими вершинами
           (setq i (+ (length vrts) 1))           ; выходим из цикла с i > length vrts
           (setq i (1+ i))                        ; если цикл завершится с i = length vrts, значит заданная точка не лежит между вершинами
         ) ;_ end if
       ) ;_ end while

       (if (not (> i (length vrts)))              ; если не найдены нужные вершины полилинии
         (princ "\nУказанная точка не лежит на полилинии")
         (progn                                   ; если вершины найдены
           (setq ang (angle pt1 pt2))             ; угол разрываемого сегмента полилинии
           (if (and (> ang (/ pi 2))              ; если угол > 90гр. и <= 270гр.
                    (<= ang (* pi 1.5))
               ) ;_ end and
             (setq ang (rem (+ ang pi) (* 2 pi))) ; увеличиваем угол на 180гр. и берем по модулю 360гр.
           ) ;_ end if
           (setq sp (polar cp ang BG:glob_radius) ; начальная точка дуги
                 ep (polar cp (+ ang pi) BG:glob_radius) ; конечная точка дуги
           ) ;_ end setq

           ;; начало групповой отмены операций -- ;
           (command "_UNDO" "_begin")             ;
           ;; отключить привязки ---------------- ;
           (setq osm   (getvar "osmode")          ;
                 3dosm (getvar "3dosmode"))       ;
           (if (< osm 16384)                      ;
             (setvar "osmode" (+ osm 16384))      ;
           )                                      ;
           (setvar "3dosmode" 0)                  ;
           ;; ----------------------------------- ;

           (command "_ARC" "_c" cp sp ep)         ; построить дугу по центральной точке, начальной точке и конечной точке
           (setq entarc (entlast))
           (command "_BREAK" ln sp ep)            ; разорвать полилинию
           (command "_JOIN" ln entarc (entlast) "") ; соединить части разорванной полилинии и дугу в одну полилинию

           ;; вернуть привязки ------------------ ;
           (setvar "osmode" osm)                  ;
           (setvar "3dosmode" 3dosm)              ;
           ;; конец групповой отмены операций --- ;
           (command "_UNDO" "_end")               ;
           ;; ----------------------------------- ;
         ) ;_ end progn
       ) ;_ end if
      )

      ( ;_ cond T
       T
       (princ "\nЧто-то пошло не так. Попробуем ещё разок...")
      )
    ) ;_ end cond
  ) ;_ end while
) ;_ end defun


(defun c:ПЕРЛИН() (BG:cross-line-arc))
