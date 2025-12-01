(setq e (car (entsel))
      e330 (cdr (assoc 330 (entget e)))
      e340s (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (member (car x) '(340 341 342 343)))) (entget e)))
)

;;;(print (entget e330))
(print e340s)
(foreach ent e340s
  (print (entget ent))
)
