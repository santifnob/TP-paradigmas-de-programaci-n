|opcion pelisCargadas listado ingresoSueldoBas|

ingresoSueldoBas := true.

pelisCargadas := EstudioCinematocrafico new.
pelisCargadas iniPeliculas; iniEmpleados.
Pelicula iniUltCod.

opcion := (ChoicePrompter choices: #('Cargar peliculas' 'Emitir listado' 'Cargar empleados')).

[opcion isNil] whileFalse: [
	(opcion = 'Cargar empleados') ifTrue: [
		(ingresoSueldoBas) ifTrue: [
			Permanente SueldoBasico: (Prompter prompt: 'Sueldo basico común para todos los empleados permanentes:  ').
			ingresoSueldoBas := false.] .    "Estas dos lineas son para que el seteo del sueldo basico se haga solo una vez"
		pelisCargadas cargarEmpleados].
	
	(opcion = 'Cargar peliculas') ifTrue: [pelisCargadas cargarPeliculas].
	
	(opcion = 'Emitir listado') ifTrue: [
		listado := pelisCargadas emitirListado.
		EstudioCinematocrafico mostrarPeliculas: listado.].
	opcion := ChoicePrompter choices: #('Cargar peliculas' 'Emitir listado' 'Cargar empleados').].








