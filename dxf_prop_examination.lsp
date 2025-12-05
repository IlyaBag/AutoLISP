(setq e (car (entsel))
      e330 (cdr (assoc 330 (entget e)))
      e340s (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (member (car x) '(340 341 342 343)))) (entget e)))
)

;;;(print (entget e330))
(print e340s)
(foreach ent e340s
  (print (entget ent))
)


;;;forum.dwg.ru/showthread.php?t=65082
;;;получить им€ динамического блока
;;;работает дл€ изменЄнных блоков, не работает дл€ только вставленных
(print (cdr (assoc 2 (entget (cdr (assoc 340 (entget (cdr (assoc 360 (entget (cdr (assoc 360 (entget (cdr (assoc 360 (entget (car (entsel))))))))))))))))))
;;;работает и дл€ только вставленных, и дл€ изменЄнных блоков
(print (vla-get-effectivename (vlax-ename->vla-object (car (entsel)))))

(setq e (car (entsel)))
;; если дин. блок изменЄн и превратилс€ в анонимный
(if (wcmatch (cdr (assoc 2 (entget e))) "`*U*") ; ` - символ экранировани€
  (print (cdr (assoc 2 (entget (cdr (assoc 340 (entget (cdr (assoc 360 (entget (cdr (assoc 360 (entget (cdr (assoc 360 (entget e))))))))))))))))
  ;; если параметры дин. блока ещЄ не мен€лись, то он содержит изначальное им€
  (print (cdr (assoc 2 (entget e))))
) ;_ end if