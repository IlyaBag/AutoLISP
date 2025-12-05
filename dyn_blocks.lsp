(defun _kpblc-get-dyn-block-list-prop-and-values (ent / res)
                                                 ;|
*    Функция получения списка свойств и их возможных значений для дин.блока
*    Параметры вызова:
*  ent  указатель на блок (vla-, ename или string). Строка воспринимается
   как хендл объекта. nil -> запрашивается у пользователя
|;
  (vl-load-com)
  (vl-catch-all-apply
    (function (lambda ()
                (setq ent (cond (ent)
                                (t (car (entsel "\nУкажите блок <Отмена> : ")))
                                ) ;_ end of cond
                      ) ;_ end of setq
                ) ;_ end of lambda
              ) ;_ end of function
    ) ;_ end of vl-catch-all-apply
  (if (vl-catch-all-error-p
        (vl-catch-all-apply
          (function
            (lambda ()
              (if (and (setq
                         ent (cond
                               ((= (type ent) 'ename) (vlax-ename->vla-object ent))
                               ((= (type ent) 'vla-object) ent)
                               ((= (type ent) 'str)
                                ((lambda (/ tmp)
                                   (vl-catch-all-apply
                                     (function
                                       (lambda ()
                                         (setq tmp (vla-handletoobject ent))
                                         ) ;_ end of lambda
                                       ) ;_ end of function
                                     ) ;_ end of vl-catch-all-apply
                                   tmp
                                   ) ;_ end of lambda
                                 )
                                )
                               (t nil)
                               ) ;_ end of cond
                         ) ;_ end of setq
                       (= (strcase (vla-get-objectname ent) t) "acdbblockreference")
                       (equal (vla-get-isdynamicblock
                                (vla-item
                                  (vla-get-blocks
                                    (vla-get-activedocument (vlax-get-acad-object))
                                    ) ;_ end of vla-get-blocks
                                  (vla-get-effectivename ent)
                                  ) ;_ end of vla-item
                                ) ;_ end of vla-get-isxref
                              :vlax-true
                              ) ;_ end of =
                       ) ;_ end of and
                (setq res
                       (mapcar
                         (function
                           (lambda (x / tmp)
                             (cons
                               (vla-get-propertyname x)
                               (if (vl-catch-all-error-p
                                     (vl-catch-all-apply
                                       (function
                                         (lambda ()
                                           (setq tmp (mapcar (function vlax-variant-value)
                                                             (vlax-safearray->list
                                                               (vlax-variant-value
                                                                 (vla-get-allowedvalues x)
                                                                 ) ;_ end of vlax-variant-value
                                                               ) ;_ end of vlax-safearray->list
                                                             ) ;_ end of mapcar
                                                 ) ;_ end of setq
                                           ) ;_ end of lambda
                                         ) ;_ end of function
                                       ) ;_ end of vl-catch-all-apply
                                     ) ;_ end of vl-catch-all-error-p
                                 (list "Неиндексированное значение")
                                 tmp
                                 ) ;_ end of if
                               ) ;_ end of cons
                             ) ;_ end of lambda
                           ) ;_ end of function
                         (vl-remove-if
                           (function (lambda (a)
                                       (= (strcase (vla-get-propertyname a)) "ORIGIN")
                                       ) ;_ end of lambda
                                     ) ;_ end of function
                           (vlax-safearray->list
                             (vlax-variant-value
                               (vla-getdynamicblockproperties ent)
                               ) ;_ end of vlax-variant-value
                             ) ;_ end of vlax-safearray->list
                           ) ;_ end of vl-remove-if
                         ) ;_ end of mapcar
                      ) ;_ end of setq
                (princ "\nОшибка указания примитива")
                ) ;_ end of if
              ) ;_ end of lambda
            ) ;_ end of function
          ) ;_ end of vl-catch-all-apply
        ) ;_ end of vl-catch-all-error-p
    (princ (strcat "\nОшибка выполнения :: " (itoa (getvar "errno"))))
    ) ;_ end of if
  res
  ) ;_ end of defun


(_kpblc-get-dyn-block-list-prop-and-values nil)

;; выбрать динамический блок
(setq e (car (entsel)))

(vlax-safearray->list (vlax-variant-value (vla-get-value ;|(vlax-dump-object |;(cadddr (vlax-safearray->list (vlax-variant-value (vla-getdynamicblockproperties (vlax-ename->vla-object e))))))))

(defun get-dyn-block-parameters (e / props_list_vla_obj lst_prop)
  ;; список vla-объектов свойств динамического блока
  (setq props_list_vla_obj
         (vlax-safearray->list 
           (vlax-variant-value
             (vla-getdynamicblockproperties (vlax-ename->vla-object e))
           ) ;_ end vlax-variant-value
         ) ;_ end vlax-safearray->list
  ) ;_ end setq
                ;; формируем список названий и значений свойств динамического блока
                (foreach obj props_list_vla_obj
                  (setq lst_prop (append lst_prop
                                         (list
                                           (cons
                                             (vla-get-propertyname obj)
                                             (if (< (vlax-variant-type (vla-get-value obj)) 8192)
                                               (vlax-variant-value (vla-get-value obj))
                                               (vlax-safearray->list (vlax-variant-value (vla-get-value obj)))
                                             ) ;_ end if
                                           ) ;_ end cons
                                         ) ;_ end list
                                 ) ;_ end append
                  ) ;_ end setq
                ) ;_ end foreach
  (foreach elem lst_prop
    (print elem)
  ) ;_ end foreach
) ;_ end defun

(vlax-dump-object (vlax-ename->vla-object (car (entsel)))t)