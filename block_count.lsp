;|=============================================================================
*    Возвращает список vla-указателей на динамические блоки текущего документа
* с переданным эффективным именем
*    Параметры вызова:
*    name    имя динамического блока. Не зависит от регистра. Допускается
*        применение маски ("*")
*    Примеры взова:
(_kpblc-blocks-select-dyn-by-name "Алюминиевое окно (подъем) - метрические")
 (_kpblc-blocks-select-dyn-by-name "Ал*")
=============================================================================|;
(defun _kpblc-blocks-select-dyn-by-name    (name / res)
  (vl-load-com)
  (foreach item
       (mapcar 'vlax-ename->vla-object
           (vl-remove-if
             'listp
             (mapcar 'cadr (ssnamex (ssget "_X" (list '(0 . "INSERT") (cons 2 (strcat "`*U*," name))))))
             ) ;_ end of vl-remove-if
           ) ;_ end of mapcar
    (if
      (and (vlax-property-available-p item "effectivename")
       (wcmatch (strcase (vla-get-effectivename item) t) (strcase name t))
       ) ;_ end of and
       (setq res (append res (list item)))
       ) ;_ end of if
    ) ;_ end of foreach
  res
  ) ;_ end of defun



(print (length (_kpblc-blocks-select-dyn-by-name (vla-get-effectivename (vlax-ename->vla-object (car (entsel)))))))

(sssetfirst nil (ssget "_X" (list '(0 . "INSERT") (cons 2 (strcat "`*U*," name)))))
(length (ssnamex (cdr (ssgetfirst))))


;;; выбор всех (динамических) блоков по имени указанного блока
(progn
  (setq name (vla-get-effectivename (vlax-ename->vla-object (car (entsel)))))
  (setq sset (ssadd))
  (foreach ss (_kpblc-blocks-select-dyn-by-name name)
    (ssadd (vlax-vla-object->ename ss) sset)
  ) ;_ end foreach
  (sssetfirst nil sset)
) ;_ end progn