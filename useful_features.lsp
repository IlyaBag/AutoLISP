(defun is-bit-in-flag (bit_val flag)
  ;;; Проверка на вхождение бита в суммарный битовый флаг
  ;;; (is-bit-in-flag 8 127) => T
  ;;; (is-bit-in-flag 128 127) => nil
  (= (logand bit_val flag) bit_val)
)


(defun is_even (num)
  ;;; no comments
  (= (rem num 2) 0)
)


(defun BG:is-point-on-line (pt1 pt2 pt3)
  ;;; pt1, pt2 - vertices of a line
  ;;; pt3 - point to check
  ;;; (BG:is-point-on-line '(0 0) '(2 2) '(1 1)) => T
  ;;; (BG:is-point-on-line '(0 0) '(2 2) '(1 0)) => nil
  (= (distance pt1 pt2)
     (+ (distance pt1 pt3) (distance pt3 pt2))
  )
)


(defun BG:str-split (str sep / pos res)
;;;  Разделяет строку на список подстрок по заданному разделителю.
;;;  Возвращает nil если хотя бы один из аргументов не является строкой
;;;  или в качестве разделителя передана пустая строка.
;;;
;;;  Аргументы:
;;;    'str' - строка, которую нужно разделить
;;;    'sep' - строка, являющаяся разделителем
;;;
;;;  (vl-string-search <образец> <строка> [<начальная позиция>]) -> индекс
;;;  (substr <строка> <начальная позиция (нач. с 1)> [<длина подстроки>])

  (cond
    ( ;_ cond #1
     (and (= (type str) 'STR)
          (= (type sep) 'STR)
          (/= sep "")
     ) ;_ end and
     (while (setq pos (vl-string-search sep str))
       (setq res (append res (list (substr str 1 pos))))
       (setq str (substr str (+ pos (strlen sep) 1)))
     ) ;_ end while
     (append res (list str))
    )
    ( ;_ cond T
     T
     nil
    )
  ) ;_ end cond
) ;_ end defun