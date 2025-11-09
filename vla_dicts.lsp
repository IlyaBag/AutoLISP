(vlax-ldata-put "BOoK_DICT" "bOOk" 17)
(vlax-ldata-get "Book_dict" "book")
(vlax-ldata-list "Book_dict")
(vlax-ldata-test "Book_dict")
(entget (namedobjdict))

(setq vlaent (vlax-ename->vla-object (car (entsel))))
(vlax-ldata-put vlaent "bOOk" 17)
(vlax-ldata-list vlaent)
(vla-get-HasExtensionDictionary vlaent)
(entget (vlax-vla-object->ename (vla-getExtensionDictionary vlaent)))
(vlax-dump-Object vlaent)

(setq adoc (vla-get-ActiveDocument (vlax-get-acad-object)))
