|opcion pelisCargadas listado ingresoSueldoBas|

ingresoSueldoBas := true.

pelisCargadas := EstudioCinematocrafico new.
pelisCargadas iniPeliculas; iniEmpleados.
Pelicula iniUltCod.

opcion := (ChoicePrompter choices: #('Cargar empleados' 'Cargar peliculas' 'Emitir listado' 'Ver peliculas cargadas')).

[opcion isNil] whileFalse: [
	(opcion = 'Cargar empleados') ifTrue: [
		(ingresoSueldoBas) ifTrue: [
			Permanente SueldoBasico: (Prompter prompt: 'Sueldo basico común para todos los empleados permanentes:  ').
			ingresoSueldoBas := false.] .    "Estas dos lineas son para que el seteo del sueldo basico para todos los permanentes se haga solo una vez"
		
		pelisCargadas cargarEmpleados.].
	
	(opcion = 'Cargar peliculas') ifTrue: [
		pelisCargadas cargarPeliculas].

	(opcion = 'Emitir listado') ifTrue: [
		listado := pelisCargadas emitirListado.
		pelisCargadas mostrarPelisFiltradas: listado].
	
	(opcion = 'Ver peliculas cargadas') ifTrue: [
		pelisCargadas mostrarPelis.].
	
	opcion := ChoicePrompter choices: #('Cargar empleados' 'Cargar peliculas' 'Emitir listado' 'Ver peliculas cargadas').]








