;;;(princ
;;;  (inters '(0.0 0.0 0.0) '(10.0 0.0 0.0) '(1.0 0.0 0.0) '(1.0 1.0 0.0))
;;;)

(defun is_point_on_line (pt1 pt2 pt3)
  ;;;  pt1, pt2 - endpoints of a line
  ;;;  pt3 - point to check
  (= 
    (distance pt1 pt2)
    (+ (distance pt1 pt3) (distance pt3 pt2))
  )
);end defun is_point_on_line

(princ (IS_POINT_ON_LINE '(0.0 0.0 0.0) '(10.0 0.0 0.0) '(1.0 0.0 0.0)))
(princ (IS_POINT_ON_LINE (getpoint) (getpoint) (getpoint)))


;((-1 . <Имя объекта: 1f4ca8cc9a0>) (0 . "LWPOLYLINE") (330 . <Имя объекта: 1f4a1421700>) (5 . "C2DE2") (100 . "AcDbEntity") (67 . 0) (410 . "Model") (8 . "0") (100 . "AcDbPolyline") (90 . 2) (70 . 0) (43 . 0.0) (38 . 0.0) (39 . 0.0) (10 -35313.6 4697.32) (40 . 0.0) (41 . 0.0) (42 . 0.0) (91 . 0) (10 -34577.6 4697.32) (40 . 0.0) (41 . 0.0) (42 . 0.0) (91 . 0) (210 0.0 0.0 1.0))

(setq e (car (entsel)))
(setq prop (entget e))
(setq ins_flag T)
(setq new_prop '())
(while (setq prp1 (car prop))
  (setq new_prop (append new_prop (list prp1)))
  (if (and ins_flag (= (car prp1) 91))
    (progn
      (setq ins_flag nil)
      (setq new_prop (append new_prop '((10 -34800.6 4697.32) (40 . 0.0) (41 . 0.0) (42 . 1.0) (91 . 0))))
    );end progn
  );end if
  
  (setq prop (cdr prop))
)

(entmod new_prop)
(entupd e)

