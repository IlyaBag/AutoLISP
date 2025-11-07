;|
Объекты ТЕКСТ, МТЕКСТ (DXF 1), ВЫНОСКА (DXF 304)
Прочитать значение
Выбрать все текстовые объекты в чертеже
Задание и поиск префикса/суффикса
Инкремент числа в содержании
Обновление объекта
|;

(setq DXFcodes (list (cons "TEXT" 1)
		     (cons "MTEXT" 1)
		     (cons "MULTILEADER" 304)
	       )
)

(setq props (entget (car (entsel "Выберите объект, содержащий текст:")));ssget
      enttype	 (cdr (assoc 0 props))
      entcontdxf (cdr (assoc enttype DXFcodes))
      entcont	 (cdr (assoc entcontdxf props))
)