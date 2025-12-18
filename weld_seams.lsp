                                                                                                 ;|
Программа для обозначения и нумерации сварных соединений на схемах
Название в таблице APPID: WELD_SEAMS_150

Швы обозначаются блоками, названия которых начинаются на "WELD_"
Блок содержит атрибут с тэгом "", содержащим номер сварного шва.
Каждому блоку при вставке добавляется запись xdata с кодом 1005, содержащая метку (dxf код 5),
в которую записывается 0, а впоследствии - метку следующего созданного блока сварного шва.
Метка последнего созданного шва (блока) должна также сохраняться отдельно в пользовательском
словаре или xrecord записи.

Интерфейс программы:
1. Вставить новый шов (в цикле)
2. Изменить указатель на следующий шов (пересвязать цепочку) или nil
3. Обновить всю цепочку
4. Выделение блоков швов по фильтру (мышкой или все сразу)
5. Выделить блок следующего шва относительно выделенного или указанного

|;


(defun _insert_block (blk_name)
;;;  Получает имя блока, проверяет наличие определения такого блока и, при возможности,
;;;  вставляет его экземпляр.
;;;  Возвращает ссылку на объект вставленного блока или nil, если такой блок не определён.
  (if (tblsearch "BLOCK" blk_name)
    (progn
      (command "_-INSERT" blk_name pause 1 1 0)  ; TODO: подавить возможность выбирать опции при вставке ("_CHANGE" <ename> ...)
      (entlast)
    ) ;_ end progn
  ) ;_ end if
) ;_ end defun


(defun _save-handle-to-xdata (ent app hdl / new_app)
;;;  Добавляет примитиву расширенные данные (xdata) с переданным приложением и заносит в них
;;;  переданную метку с dxf кодом 1005.
;;;  В случае наличия у примитива расширенных данных с таким же приложением и dxf кодом данные
;;;  перезаписываются новым значением.
  
  ;; Если приложение не зарегистрировано, то пробуем зарегистрировать
  ;; В случае невозможности регистрации прерываем программу
  (if (not (tblsearch "APPID" app))
    (progn
      (setq new_app (regapp app))
      (if new_app
        (print (strcat "Приложение " new_app " успешно зарегистрировано."))
        (progn
          (alert (strcat "ОШИБКА!\nПриложение " app " не может быть зарегистрировано"))
          (exit)
        )
      )
    )
  )
  ;; Запись расширенных данных в примитив
  (entmod
    (append
      (entget ent)
      (list (list -3 (list app (cons 1005 hdl))))  ; '(-3 ("WELD_SEAMS_150" (1005 . "0")))
    )
  )
)


(defun _get-last-saved-handle ()
;;;  Возвращает метку последнего включённого в цепочку блока сварного шва.
  glob_lh
)


(defun _set-last-handle (hdl)
;;;  Сохраняет переданную метку как метку последнего включённого в цепочку блока сварного шва.
  (setq glob_lh hdl)
)



(defun _get-first-saved-handle ()
;;;  Возвращает метку первого включённого в цепочку блока сварного шва.
  glob_fh
)


(defun _set-first-handle (hdl)
;;;  Сохраняет переданную метку как метку первого включённого в цепочку блока сварного шва.
  (setq glob_fh hdl)
)

(defun _get-block-attr (ent attr_name / prop is_found not_found)
;;;  Ищет в блоке атрибут с заданным имененм и возвращает указатель атрибута. 
;;;  Если атрибут не найден, возвращает nil.
  (while (not (or is_found not_found))
    (setq ent (entnext ent))                      ; TODO: entnext даёт ошибку, если ent - последний в базе
    (setq prop (entget ent))
    (cond
      ;; cond - атрибут найден
      ((and (= (cdr (assoc 0 prop)) "ATTRIB")
            (= (cdr (assoc 2 prop)) attr_name)
       ) ;_ end and
       (setq is_found T)
      )
      ;; cond - атрибут с другим именем
      ((= (cdr (assoc 0 prop)) "ATTRIB")
      )
      ;; cond - атрибут не найден
      (T
       (setq not_found T)
      )
    ) ;_ end cond
  ) ;_ end while
  (if is_found
    ent
    nil
  ) ;_ end if
) ;_ end defun


(defun _get-attr-val (ent)
;;;  Получает указатель атрибута блока и возвращает значение атрибута.
  (cdr (assoc 1 (entget ent)))
) ;_ end defun


(defun _set-attr-val (ent val / prop)
;;;  Устанавливает новое значение указанному атрибуту блока.
  (setq prop (entget ent))
  (entmod (subst (cons 1 val) (assoc 1 prop) prop))
  (entupd (cdr (assoc 330 prop)))                 ; обновить блок, содержащий данный атрибут
) ;_ end defun




(defun WD:add-weld (/ blk_name new_blk new_blk_handle prev_blk prev_att prev_att_val new_att)
;;;  вставить новый блок
;;;  получить его параметры: ссылку, метку
;;;  добавить ему xdata
;;;  найти предыдущий блок
;;;  записать в его xdata новый блок
;;;  получить номер предыдущего шва
;;;  обновить атрибут нового блока
;;;  записать метку нового блока в общие данные
  (setq blk_name "WELD_LEADER_1")  ; TODO: хранить имя блока в словаре
  (setq blk_name "свар_шов_3")   ; удалить
  ;; Вставка блока сварного шва
  (if (setq new_blk (_insert_block blk_name))
    (setq new_blk_handle (cdr (assoc 5 (entget new_blk))))
    (progn
      (alert (strcat "ОШИБКА!\nБлок " blk_name " не определён"))
      (exit)
    )
  )
  ;; Создаём у блока расширенные данные с нулевой меткой следующего блока
  (_save-handle-to-xdata new_blk "WELD_SEAMS_150" "0")  ; TODO: хранить имя приложения в словаре
  (if (_get-last-saved-handle)
    (progn
      ;; Обновление расширенных данных предыдущего блока и получение его номера шва
      (setq prev_blk (handent (_get-last-saved-handle)))  ; TODO: проверить на неудалённость
      (_save-handle-to-xdata prev_blk "WELD_SEAMS_150" new_blk_handle)
      (setq prev_att (_get-block-attr prev_blk "НОМЕР_СВ_ШВА")  ; TODO: хранить имя атрибута в словаре
            prev_att_val (_get-attr-val prev_att)
      )
      ;; Обновление атрибута нового блока
      (setq new_att (_get-block-attr new_blk "НОМЕР_СВ_ШВА"))  ; TODO: хранить имя атрибута в словаре
      (_set-attr-val new_att (itoa (1+ (atoi prev_att_val))))  ; TODO: проверка типа атрибута
    )
    ;; Сохранение метки первого блока
    (_set-first-handle new_blk_handle)
  )
  ;; Сохранение метки нового блока как последнего в цепочке
  (_set-last-handle new_blk_handle)
)


(defun WD:relink-weld ()
  (princ)
)


(defun WD:update-all-welds ()
  (princ)
)


(defun WD:select-weld-blocks ()
  (princ)
)


(defun WD:highlight-next ()
  (princ)
)


