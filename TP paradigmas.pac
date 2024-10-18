| package |
package := Package name: 'TP paradigmas'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Actor;
	add: #Empleado;
	add: #EquipoDireccion;
	add: #EstudioCinematocrafico;
	add: #Pelicula;
	add: #Permanente;
	add: #Staff;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Empleado
	instanceVariableNames: 'pelicula documento nombre apellido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #EstudioCinematocrafico
	instanceVariableNames: 'peliculas empleados'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Pelicula
	instanceVariableNames: 'codigo fecha titulo empleados presupuestoAsignado presupuestoRemanente'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Empleado subclass: #Permanente
	instanceVariableNames: 'porcentajePlus'
	classVariableNames: 'SueldoBasico'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Empleado subclass: #Staff
	instanceVariableNames: 'nacionalidad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Staff subclass: #Actor
	instanceVariableNames: 'cachet'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Staff subclass: #EquipoDireccion
	instanceVariableNames: 'porcentaje'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Empleado guid: (GUID fromString: '{f1cd45e3-5dcb-4315-bb39-ee8d87cfbbed}')!
Empleado comment: ''!
!Empleado categoriesForClass!Kernel-Objects! !
!Empleado methodsFor!

apellido
"Devuelve un apellido de un empleado"
^apellido.


!

apellido: unApellido
"Setea un apellido de un empleado"
apellido := unApellido.


!

documento
"Devuelve un documento de un empleado"
^documento.

!

documento: unDocumento
"Setea un documento de un empleado"
documento := unDocumento.
!

nombre
"Devuelve un nombre de un empleado"
^nombre.

!

nombre: unNombre
"Setea un nombre de un empleado"
nombre := unNombre.

! !
!Empleado categoriesForMethods!
apellido!public! !
apellido:!public! !
documento!public! !
documento:!public! !
nombre!public! !
nombre:!public! !
!

EstudioCinematocrafico guid: (GUID fromString: '{d751dc02-d653-4b6e-afa0-a4a8840982ca}')!
EstudioCinematocrafico comment: ''!
!EstudioCinematocrafico categoriesForClass!Kernel-Objects! !
!EstudioCinematocrafico methodsFor!

agregarEmpleado: unEmpleado
"Agrega un empleado a la coleccion de empleados"
empleados add: unEmpleado.
!

agregarPelicula: unaPelicula
"Agrega una pelicula a la coleccion de peliculas"
peliculas add: unaPelicula.
!

buscarDni: unDni listaEmpleados: unaListEmp
"Recorre la coleccion de empleados hasta encontrar el mismo dni y si no lo encuentra devuelve nil"
^(unaListEmp detect: [:unEmp | unEmp documento = unDni] ifNone: [nil] ).
!

cargarEmpleados
"Carga un empleado, de cualquier tipo, a la coleccion de empleados"
	| unEmpleado tipo opcion dniRepetido|
	opcion := true.
	[opcion] whileTrue:[
			tipo := ChoicePrompter choices: #('Permanente' 'Actor' 'Equipo de dirección').
			(tipo isNil) ifFalse: 
					[
					tipo = 'Permanente' ifTrue: [unEmpleado := Permanente iniPermanente].
					tipo = 'Actor' ifTrue: [unEmpleado := Actor iniActor].
					tipo = 'Equipo de dirección' ifTrue: [unEmpleado := EquipoDireccion iniEquipoDirec].
					
					"Pensamos la logica de primero cargar el dni y validarlo en el momento para seguir la carga pero no es una implementacion factible por el tiempo que llevaria aplicar los cambios"
					
					dniRepetido := self buscarDni: (unEmpleado documento) listaEmpleados: empleados.
					
					(dniRepetido isNil) ifTrue: [self agregarEmpleado: unEmpleado].
					(dniRepetido isNil) ifFalse: [MessageBox warning: 'Ya existe un empleado con ese DNI' ].].

			opcion := MessageBox confirm: '¿Desea continuar con la carga del personal?']!

cargarPeliculas
"Carga la pelicula a la coleccion de peliculas"
|peli bandera bandera2 unEmpleado empleadoRepetido unDni totalCobrado |

bandera := true.

[bandera] whileTrue: [
	peli := Pelicula iniPelicula.
	
	bandera2 := (MessageBox confirm: '¿Desea agregar empleados a la pelicula?' ).

	(bandera2) ifTrue: [(MessageBox confirm: '¿Desea ver los empleados cargados en el sistema?') ifTrue: [self mostrarEmpleados.].].	
	
	"INGRESO DE EMPLEADOS A LA PELICULA" 
	[bandera2] whileTrue: [
		unDni := Prompter prompt: 'Ingresar un DNI:' caption: 'Ingreso de empleados'.
		unEmpleado := self buscarDni: unDni listaEmpleados: (self empleados).  "devuelve los datos del empleado si lo encontro, y 0 si no"

		(unEmpleado isNil) ifTrue: [MessageBox warning: 'El empleado que se ingreso no existe en el sistema.'].
		
		empleadoRepetido := self buscarDni: unDni listaEmpleados: (peli empleados).
		(empleadoRepetido isNil) ifFalse: [
			MessageBox warning: 'El empleado ya es parte de la pelicula'.].

		
		((unEmpleado isNil) or: [empleadoRepetido notNil]) ifFalse: [
			totalCobrado := unEmpleado calcularTotalCobrado: (peli presupuestoAsignado).
		
			(unEmpleado class = Permanente) ifTrue: [((peli calcularPreRem >= totalCobrado) and: [(peli porcentajeAcum + unEmpleado porcentajePlus) <= 100]) ifTrue: [peli agregarEmpleadoPeli: unEmpleado.] ifFalse: [MessageBox warning: 'No alcanza el presupuesto para agregar al empleado o el porcentaje del plus supera el 100'] .].
			
			(unEmpleado class = Permanente) ifFalse: [(peli calcularPreRem >= totalCobrado) ifTrue: [peli agregarEmpleadoPeli: unEmpleado.] ifFalse: [MessageBox warning: 'No alcanza el presupuesto para agregar al empleado'] .].].         "ACA SE HACE UN IF PARA VER QUE TIPO DE COMPARACION SE NECESITA PARA EL PRES REM"

		(MessageBox confirm: '¿Desea seguir añadiendo empleados?') ifFalse: [bandera2 := false].].
	
	self agregarPelicula: peli.
	
	(MessageBox confirm: '¿Desea seguir cargando peliculas?') ifFalse: [bandera := false].].!

emitirListado
"Devuelve el listado de las peliculas con vigencia de 30 dias"
|listadoFechas listadoTreintaDias| 

listadoFechas := SortedCollection new.
listadoTreintaDias := SortedCollection new.

listadoTreintaDias := (peliculas select: [:peli | ((Date today) asDays - (peli fecha) asDays) <= 30]).

listadoFechas addAll: (listadoTreintaDias collect: [:unaPeli | unaPeli fecha.]) .

^listadoFechas.



!

empleados
"Devuelve los empleados del Estudio Cinematografico"
^empleados.!

iniEmpleados
"Inicializa la coleccion de empleados"
empleados := OrderedCollection new.
!

iniPeliculas
"Inicializa la coleccion de peliculas"
peliculas := OrderedCollection new.!

mostrarEmpleados
"Muestra los empleados del Estudio cinematografico"
Transcript clear.
empleados do: [:e | Transcript show: 'Nombre: ',  e nombre ; tab; show: '| Apellido: ', e apellido; tab; show: '| DNI: ', e documento; tab. 
	(e class = Permanente) ifTrue: [Transcript show: '| Tipo: ', (e class) printString; tab ;show: '| Porcentaje plus: ', (e porcentajePlus) printString, '%' ; cr]. 
	(e class = Actor) ifTrue: [Transcript show: '| Tipo: ', (e class) printString; tab ;show: '| Nacionalidad: ', e nacionalidad; tab; show: '| Cachet: ', (e cachet) printString; cr.].
	(e class = EquipoDireccion) ifTrue: [Transcript show: '| Tipo: ', (e class) printString; tab ; show: '| Nacionalidad: ', e nacionalidad; tab; show: '| Porcentaje sobre pres. asignado: ', (e porcentaje) printString, '%'; cr. ].].
!

mostrarPelis
Transcript clear.
(self peliculas) do: [:p | Transcript show: '| Nombre pelicula: ',  p titulo; tab; show: '| Codigo: ', (p codigo) printString; tab; show: '| Fecha creacion: ', (p fecha) printString; tab; show: '| Presupuesto asignado: ', (p presupuestoAsignado) printString; tab; show: '| Presupuesto remanente: ', ((p calcularPreRem) asFloat) printString ;cr. ].!

mostrarPelisFiltradas: listadoFechas
	"Muestra la peliculas que le pasa por listado"

	| listaEmpleados peli |
	Transcript clear.
	listadoFechas do: 
			[:unaFecha |
			peli := peliculas detect: [:unaPeli | unaPeli fecha = unaFecha].
			Transcript
				show: '| TÍTULO: ' , peli titulo;
				tab;
				show: '| Fecha creación; ' , unaFecha printString;
				tab;
				show: '| Codigo: ' , peli codigo printString;
				tab;
				show: '| Presupuesto asignado: ' , peli presupuestoAsignado printString;
				tab;
				show: '| Presupuesto remanente: '
							, (peli calcularPreRem * ((100 - peli porcentajeAcum) / 100) asFloat) printString;
				tab;
				cr.
			Transcript
				tab;
				show: '--EMPLEADOS DE "' , peli titulo , '"--';
				cr.
			listaEmpleados := peli empleados.
			listaEmpleados do: 
					[:unEmpleado |
					Transcript
						tab;
						show: '| Nombre y apellido: ' , unEmpleado nombre , ' ' , unEmpleado apellido;
						tab;
						show: '| Tipo: ' , unEmpleado class printString;
						tab.
					(unEmpleado isKindOf: Permanente)
						ifTrue: 
							[Transcript
								show: '| Total cobrado: ' , ((unEmpleado calcularCobradoListado: peli calcularPreRem) asFloat) printString;
								cr].
					(unEmpleado isKindOf: Permanente)
						ifFalse: 
							[Transcript
								show: '| Total cobrado: ' , ((unEmpleado calcularTotalCobrado: peli presupuestoAsignado) asFloat) printString;
								cr]]]!

peliculas
"Devuelve las peliculas"
^peliculas.
! !
!EstudioCinematocrafico categoriesForMethods!
agregarEmpleado:!public! !
agregarPelicula:!public! !
buscarDni:listaEmpleados:!public! !
cargarEmpleados!public! !
cargarPeliculas!public! !
emitirListado!public! !
empleados!public! !
iniEmpleados!public! !
iniPeliculas!public! !
mostrarEmpleados!public! !
mostrarPelis!public! !
mostrarPelisFiltradas:!public! !
peliculas!public! !
!

Pelicula guid: (GUID fromString: '{1c07fe4d-5336-481c-869c-a1b0efeada8c}')!
Pelicula comment: ''!
!Pelicula categoriesForClass!Kernel-Objects! !
!Pelicula methodsFor!

agregarEmpleadoPeli: unEmpleado
"Agrega un empleado a una pelicula"
empleados add: unEmpleado.!

calcularPreRem
"Calcula el presupuesto remanente de una pelicula"
|acumulador|
acumulador := empleados inject: presupuestoAsignado into: [:acum :pers | acum - (pers calcularTotalCobrado: presupuestoAsignado)].
^acumulador.

!

cargarDatos
"Carga los datos de una pelicula"
self codigo: UltimoCodigo .
Pelicula incrementarUltCod.
self titulo: (Prompter prompt: 'Titulo: ').
self presupuestoAsignado: (Prompter prompt: 'Presupuesto asignado: ').
self iniPresupuestoRemanente.
fecha := self ingresarFecha.
empleados := (OrderedCollection new).





!

codigo
"Devuelve el codigo de la pelicula"
^codigo.
!

codigo: unCodigo
"Setea el codigo de una pelicula"
codigo := unCodigo.
!

empleados
"Devuelve la coleccion de empleados"
^empleados.
!

fecha
"Devuelve la fecha"
^fecha.




!

fecha: unDia mes: unMes anio: unAnio
"Dado un dia, mes y anio, devuelve una fecha"
^(Date year: unAnio month: unMes day: unDia).



!

ingresarFecha
"Lee el dia, mes y anio, y validamos (no sabemos como hacer para validar un dia bisiesto)"
|dia mes anio|
dia := (Prompter prompt: 'Día: ') asNumber.
[dia > 30] whileTrue: [
	dia := (Prompter prompt: 'Ingresar un día valido: ') asNumber.].

mes := (Prompter prompt: 'Mes: ') asNumber.
[mes > 12] whileTrue: [
	mes := (Prompter prompt: 'Ingresar un mes valido') asNumber.].

anio := (Prompter prompt: 'Año: ') asNumber.

^(self fecha: dia mes: mes anio: anio).!

iniPresupuestoRemanente
"Inicializa el presupuesto remanente"
presupuestoRemanente := presupuestoAsignado.
!

porcentajeAcum
"Pasea por la coleccion de empleados y va acumulando el porcentajePlus de los empleados permanentes de una pelicula"
|sumaTotal|
sumaTotal := empleados inject: 0 into: [:acum :unEmpleado | (unEmpleado isKindOf: Permanente) ifTrue: [acum + unEmpleado porcentajePlus] ifFalse: [acum]].
^sumaTotal.!

presupuestoAsignado
"Devuelve el presupuesto asignado"
^(presupuestoAsignado).


!

presupuestoAsignado: unPres
"Setea el presupuesto asignado"
presupuestoAsignado := unPres asNumber.


!

titulo
"Devuelve el titulo de la pelicula"
^titulo.
!

titulo: unTitulo
"Setea el titulo de una pelicula"
titulo := unTitulo.
! !
!Pelicula categoriesForMethods!
agregarEmpleadoPeli:!public! !
calcularPreRem!public! !
cargarDatos!public! !
codigo!public! !
codigo:!public! !
empleados!public! !
fecha!public! !
fecha:mes:anio:!public! !
ingresarFecha!public! !
iniPresupuestoRemanente!public! !
porcentajeAcum!public! !
presupuestoAsignado!public! !
presupuestoAsignado:!public! !
titulo!public! !
titulo:!public! !
!

!Pelicula class methodsFor!

incrementarUltCod
"Incrementa el ultimo codigo de la pelicula para su posterior uso"
UltimoCodigo := UltimoCodigo + 1.

!

iniPelicula
"Inicializa la coleccion de peliculas"
|pelicula|
pelicula := self new.
pelicula cargarDatos.
^pelicula.
!

iniUltCod
"Inicializa el atributo de ultimo codigo (este valor lo fija el programador, o sea, no le damos la prioridad al usuario)"
UltimoCodigo := 0.
! !
!Pelicula class categoriesForMethods!
incrementarUltCod!public! !
iniPelicula!public! !
iniUltCod!public! !
!

Permanente guid: (GUID fromString: '{d1f702c4-064c-4f89-bf0a-c0049bbe0a88}')!
Permanente comment: ''!
!Permanente categoriesForClass!Kernel-Objects! !
!Permanente methodsFor!

calcularCobradoListado: presupuestoRemanente
"Es el metodo polimorfico, calcula lo que gana un empleado permante para el listado"
|plus|
plus := (porcentajePlus / 100) * presupuestoRemanente.
^(plus + SueldoBasico).

!

calcularTotalCobrado: unPresupuestoAsignado
"Sirve para que el metodo sea polimorfico"
^(SueldoBasico).
!

cargarDatos
"Carga los datos de un empleado Permanente"
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self porcentajePlus: (Prompter prompt: 'Porcentaje plus: ') asNumber.
!

porcentajePlus
"Devuelve el porcentaje plus de un empleado permanente"
^porcentajePlus.!

porcentajePlus: unPorPlus
"Setea el porcentaje plus de un empleado permanente"
porcentajePlus := unPorPlus.
! !
!Permanente categoriesForMethods!
calcularCobradoListado:!public! !
calcularTotalCobrado:!public! !
cargarDatos!public! !
porcentajePlus!public! !
porcentajePlus:!public! !
!

!Permanente class methodsFor!

iniPermanente
"Inicializa un empleado permanente"
|permanente|
permanente := self new.
permanente cargarDatos.
^permanente.
!

SueldoBasico
"Devuelve el sueldo basico de los empleados permanentes"
^SueldoBasico. 
!

SueldoBasico: SB
"Setea el sueldo basico de los empleado permanentes"
SueldoBasico := SB asNumber.
! !
!Permanente class categoriesForMethods!
iniPermanente!public! !
SueldoBasico!public! !
SueldoBasico:!public! !
!

Staff guid: (GUID fromString: '{2090d6e2-ef24-4678-a7db-86ea6d7c0a4e}')!
Staff comment: ''!
!Staff categoriesForClass!Kernel-Objects! !
!Staff methodsFor!

nacionalidad
"Devuelve la nacionalidad de un staff"
^nacionalidad.
!

nacionalidad: unaN
"Setea la nacionalidad de un staff"
nacionalidad := unaN.
! !
!Staff categoriesForMethods!
nacionalidad!public! !
nacionalidad:!public! !
!

Actor guid: (GUID fromString: '{c5c18826-a101-4005-a784-685e7da6d31a}')!
Actor comment: ''!
!Actor categoriesForClass!Kernel-Objects! !
!Actor methodsFor!

cachet
"Devuelve el cachet de un actor"
^cachet.!

cachet: unCachet
"Setea un cachet del actor"
cachet := unCachet.!

calcularTotalCobrado: unPresupuestoAsignado
"Este sirve para que sea polimorfico entre los 3 tipos de empleados"
^cachet.
!

cargarDatos
"Carga los datos para un actor"
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self nacionalidad: (Prompter prompt: 'Nacionalidad: ').
self cachet: (Prompter prompt: 'Cachet: ') asNumber.

! !
!Actor categoriesForMethods!
cachet!public! !
cachet:!public! !
calcularTotalCobrado:!public! !
cargarDatos!public! !
!

!Actor class methodsFor!

iniActor
"Inicializa un nuevo actor"
|actor|
actor := self new.
actor cargarDatos.
^actor.
! !
!Actor class categoriesForMethods!
iniActor!public! !
!

EquipoDireccion guid: (GUID fromString: '{00e6f05f-8665-4b35-aabc-ce8828e7cd22}')!
EquipoDireccion comment: ''!
!EquipoDireccion categoriesForClass!Kernel-Objects! !
!EquipoDireccion methodsFor!

calcularTotalCobrado: unPresupuestoAsignado
"Sirve para que sea polimorfico, es el calculo de lo que gana un Equipo de direccion"
^((porcentaje * unPresupuestoAsignado) / 100)!

cargarDatos
"Carga los datos de un equipo de direccion"
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self nacionalidad: (Prompter prompt: 'Nacionalidad: ').
self porcentaje: (Prompter prompt: 'Porcentaje: ') asNumber.

!

porcentaje
"Devuelve el porcentaje de un Equipo de direccion"
^porcentaje.!

porcentaje: unP
"Setea un porcentaje de un equipo de direccion"
porcentaje := unP.! !
!EquipoDireccion categoriesForMethods!
calcularTotalCobrado:!public! !
cargarDatos!public! !
porcentaje!public! !
porcentaje:!public! !
!

!EquipoDireccion class methodsFor!

iniEquipoDirec
"Inicializa un Equipo de direccion"
| equipo |
equipo := self new.
equipo cargarDatos.
^equipo.! !
!EquipoDireccion class categoriesForMethods!
iniEquipoDirec!public! !
!

"Binary Globals"!

