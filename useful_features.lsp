(defun is-bit-in-flag (bit_val flag)
  ;;; Проверка на вхождение бита в суммарный битовый флаг
  ;;; (is-bit-in-flag 8 127) => T
  ;;; (is-bit-in-flag 128 127) => nil
  (= (logand bit_val flag) bit_val)
)


(defun BG:is-point-on-line (pt1 pt2 pt3)
  ;;; pt1, pt2 - vertices of a line
  ;;; pt3 - point to check
  ;;; (BG:is-point-on-line '(0 0) '(2 2) '(1 1)) => T
  ;;; (BG:is-point-on-line '(0 0) '(2 2) '(1 0)) => nil
  (=
    (distance pt1 pt2)
    (+ (distance pt1 pt3) (distance pt3 pt2))
  )
)
