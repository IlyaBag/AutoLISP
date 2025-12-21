;;;  Набор CRUD инструментов для работы со словарями
;;;  > dict-create
;;;  > dict-delete
;;;  > dict-add
;;;  > dict-get
;;;  > dict-put
;;;  > dict-remove


(defun dict-create (name / newdict)
;;;  Создание пользовательского словаря
;;;  <name> - строка с именем словаря
;;;  Возвращает указатель созданного словаря или nil.
  (if (not (dictsearch (namedobjdict) name))
    (progn
      ;; создание объекта нового словаря
      (setq newdict (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))
      ;; добавление нового словаря с именем к корневому словарю
      (dictadd (namedobjdict) name newdict)
    )
    (progn
      (princ (strcat "\nНе удалось создать словарь с именем \"" name "\". Это имя уже существует"))
      nil
    )
  )
)


(defun dict-delete (name / deleted)
;;;  Удаление пользовательского словаря
;;;  <name> - строка с именем словаря
;;;  Возвращает указатель удалённого словаря или nil.
  (if (dictsearch (namedobjdict) name)
    (progn
      ;; удаление записи о словаре из корневого словаря
      (setq deleted (dictremove (namedobjdict) name))
      ;; удаление объекта словаря из базы данных файла
      (entdel deleted)
    )
    (progn
      (princ (strcat "\nНе удалось удалить словарь с именем \"" name "\". Имя не найдено"))
      nil
    )
  )
)


(defun dict-add (name key data / dict xrec)
;;;  Добавление записи в словарь.
;;;  <name> - строка с именем словаря
;;;  <key> - строка с ключом записи
;;;  <data> - список точечных пар и списков для создания X-записи
;;;  Возвращает указатель добавленного объекта X-записи или nil.
  (if (setq dict (dictsearch (namedobjdict) name))
    (progn
      ;; получение указателя словаря
      (setq dict (cdr (assoc -1 dict)))
      ;; если ключ в словаре отсутствует
      (if (not (dictsearch dict key))
	(progn
	  ;; создание объекта X-записи
	  (setq xrec (entmakex (append '((0 . "XRECORD") (100 . "AcDbXrecord")) data)))
	  ;; добавление записи ключ-значение в словарь
	  (dictadd dict key xrec)
	)
	(progn
	  (princ (strcat "\nНе удалось создать запись. Ключ \"" key "\" уже существует в словаре \"" name "\""))
	  nil
	)
      )
    )
    (progn
      (princ (strcat "\nНе удалось создать запись. Словарь с именем \"" name "\" не найден"))
      nil
    )
  )
)


(defun dict-get (name key / dict data)
;;;  Получение данных записи словаря.
;;;  <name> - строка с именем словаря
;;;  <key> - строка с ключом записи
;;;  Возвращает ассоциативный список параметров X-записи или nil.
  (if (setq dict (dictsearch (namedobjdict) name))
    (progn
      ;; получение указателя словаря
      (setq dict (cdr (assoc -1 dict)))
      ;; если ключ присутствует в словаре
      (if (setq data (dictsearch dict key))
	;; получение значения записи
	data
	(progn
	  (princ (strcat "\nНе удалось получить запись. Ключ \"" key "\" отсутствует в словаре \"" name "\""))
	  nil
	)
      )
    )
    (progn
      (princ (strcat "\nНе удалось получить запись. Словарь с именем \"" name "\" не найден"))
      nil
    )
  )
)


(defun dict-put (name key data / dict deleted xrec)
;;;  Замена данных записи словаря.
;;;  <name> - строка с именем словаря
;;;  <key> - строка с ключом записи
;;;  <data> - список точечных пар и списков для создания X-записи
;;;  Возвращает указатель новой X-записи или nil.
  (if (setq dict (dictsearch (namedobjdict) name))
    (progn
      ;; получение указателя словаря
      (setq dict (cdr (assoc -1 dict)))
      ;; если ключ присутствует в словаре
      (if (dictsearch dict key)
	(progn
	  ;; удаление ключа из словаря
	  (setq deleted (dictremove dict key))
	  ;; удаление объекта X-записи из базы данных файла
	  (entdel deleted)
	  ;; создание нового объекта X-записи
	  (setq xrec (entmakex (append '((0 . "XRECORD") (100 . "AcDbXrecord")) data)))
	  ;; добавление новой записи с прежним ключом в словарь
	  (dictadd dict key xrec)
	)
	(progn
	  (princ (strcat "\nНе удалось изменить запись. Ключ \"" key "\" отсутствует в словаре \"" name "\""))
	  nil
	)
      )
    )
    (progn
      (princ (strcat "\nНе удалось изменить запись. Словарь с именем \"" name "\" не найден"))
      nil
    )
  )
)


(defun dict-remove (name key / dict data deleted)
;;;  Удаление записи словаря.
;;;  <name> - строка с именем словаря
;;;  <key> - строка с ключом записи
;;;  Возвращает указатель удалённой X-записи или nil.
  (if (setq dict (dictsearch (namedobjdict) name))
    (progn
      ;; получение указателя словаря
      (setq dict (cdr (assoc -1 dict)))
      ;; если ключ присутствует в словаре
      (if (setq data (dictsearch dict key))
	(progn
	  ;; удаление ключа из словаря
	  (setq deleted (dictremove dict key))
	  ;; удаление объекта X-записи из базы данных файла
	  (entdel deleted)
	)
	(progn
	  (princ (strcat "\nНе удалось удалить запись. Ключ \"" key "\" отсутствует в словаре \"" name "\""))
	  nil
	)
      )
    )
    (progn
      (princ (strcat "\nНе удалось удалить запись. Словарь с именем \"" name "\" не найден"))
      nil
    )
  )
)
