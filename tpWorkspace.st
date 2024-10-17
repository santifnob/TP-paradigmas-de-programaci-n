|opcion pelisCargadas listado ingresoSueldoBas|

ingresoSueldoBas := true.

pelisCargadas := EstudioCinematocrafico new.
pelisCargadas iniPeliculas; iniEmpleados.
Pelicula iniUltCod.

opcion := (ChoicePrompter choices: #('Cargar empleados' 'Cargar peliculas' 'Emitir listado')).

[opcion isNil] whileFalse: [
	(opcion = 'Cargar empleados') ifTrue: [
		(ingresoSueldoBas) ifTrue: [
			Permanente SueldoBasico: (Prompter prompt: 'Sueldo basico común para todos los empleados permanentes:  ').
			ingresoSueldoBas := false.] .    "Estas dos lineas son para que el seteo del sueldo basico se haga solo una vez"
		
		pelisCargadas cargarEmpleados.].
	
	(opcion = 'Cargar peliculas') ifTrue: [
		pelisCargadas cargarPeliculas.
		
		(pelisCargadas peliculas) do: [:p | Transcript show: 'Nombre pelicula: ',  p titulo; tab; show: 'Codigo: ', (p codigo) printString; tab; show: 'Fecha creacion: ', (p fecha) printString; tab; show: 'Presupuesto asignado: ', (p presupuestoAsignado) printString; cr. ]. ]. "Estaria bueno ampliar este mostrar peliculas y hacerlo una opcion de menu diferente"
	
	(opcion = 'Emitir listado') ifTrue: [
		listado := pelisCargadas emitirListado.
		pelisCargadas mostrarPeliculas: listado.].
	opcion := ChoicePrompter choices: #('Cargar empleados' 'Cargar peliculas' 'Emitir listado').]








