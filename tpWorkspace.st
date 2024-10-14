|opcion pelisCargadas empleados listado cp opciones|

pelisCargadas := EstudioCinematocrafico new.
pelisCargadas iniPeliculas; iniEmpleados.

opcion := (ChoicePrompter choices: #('Cargar peliculas' 'Emitir listado' 'Cargar empleados')).

[opcion isNil] whileFalse: [
	opcion := opcion printString.
	(opcion := 'Cargar empleados') ifTrue: [pelisCargadas cargarEmpleados].
	(opcion = 'Cargar peliculas') ifTrue: [pelisCargadas cargarPeliculas].
	(opcion = 'Emitir listado') ifTrue: [
		listado := pelisCargadas emitirListado.
		EstudioCinematocrafico mostrarPeliculas: listado.].
	opcion := ChoicePrompter choices: #('Cargar peliculas' 'Emitir listado' 'Cargar empleados').].




