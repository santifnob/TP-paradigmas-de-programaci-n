|opcion pelisCargadas listado cp opciones|

pelisCargadas := EstudioCinematocrafico new.
pelisCargadas iniPeliculas.

opcion := (ChoicePrompter choices: #('Cargar peliculas' 'Emitir listado')).

[opcion isNil] whileFalse: [
	opcion := opcion printString.
	(opcion = 'Cargar peliculas') ifTrue: [pelisCargadas cargarPeliculas].
	(opcion = 'Emitir listado') ifTrue: [
		listado := pelisCargadas emitirListado.
		EstudioCinematocrafico mostrarPeliculas: listado.].
	opcion := ChoicePrompter choices: #('Cargar peliculas' 'Emitir listado' 'Salir').].




