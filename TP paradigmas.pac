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
	'C:\Users\Usuario-1\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'C:\Users\Usuario-1\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter'
	'C:\Users\Usuario-1\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'C:\Users\Usuario-1\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'C:\Users\Usuario-1\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

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
^apellido.


!

apellido: unApellido
apellido := unApellido.


!

documento
^documento.

!

documento: unDocumento
documento := unDocumento.
!

nombre
^nombre.

!

nombre: unNombre
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
empleados add: unEmpleado.
!

agregarPelicula: unaPelicula
peliculas add: unaPelicula.
!

buscarDni: unDni listaEmpleados: unaListEmp
^(unaListEmp detect: [:unEmp | unEmp documento = unDni] ifNone: [nil] ).
!

cargarEmpleados
	| unEmpleado tipo opcion |
	opcion := true.
	[opcion] whileTrue: 
			[tipo := ChoicePrompter choices: #('Permanente' 'Actor' 'Equipo de dirección').
			(tipo isNil) ifFalse: 
					[
					tipo = 'Permanente' ifTrue: [unEmpleado := Permanente iniPermanente].
					tipo = 'Actor' ifTrue: [unEmpleado := Actor iniActor].
					tipo = 'Equipo de dirección' ifTrue: [unEmpleado := EquipoDireccion iniEquipoDirec].
					self agregarEmpleado: unEmpleado.].

			opcion := MessageBox confirm: '¿Desea continuar con la carga del personal?']!

cargarPeliculas
|peli bandera bandera2 unEmpleado empleadoRepetido repetidoOno unDni totalCobrado |

bandera := true.

[bandera] whileTrue: [
	peli := Pelicula iniPelicula.
	
	bandera2 := (MessageBox confirm: '¿Desea agregar empleados a la pelicula?' ).

	(MessageBox confirm: '¿Desea ver los empleados cargados en el sistema?') ifTrue: [self mostrarEmpleados.].	
	
	"INGRESO DE EMPLEADOS A LA PELICULA" 
	[bandera2] whileTrue: [
		unDni := Prompter prompt: 'Ingresar un DNI:' caption: 'Ingreso de empleados'.
		unEmpleado := self buscarDni: unDni listaEmpleados: (self empleados).  "devuelve los datos del empleado si lo encontro, y 0 si no"

		(unEmpleado isNil) ifTrue: [MessageBox warning: 'El empleado que se ingreso no existe en el sistema.'].
		
		repetidoOno := false.
		empleadoRepetido := self buscarDni: unDni listaEmpleados: (peli empleados).
		(empleadoRepetido isNil) ifFalse: [
			MessageBox warning: 'El empleado ya es parte de la pelicula'.
			repetidoOno = true.].
		
		((unEmpleado isNil) or: [repetidoOno]) ifFalse: [
			totalCobrado := unEmpleado calcularTotalCobrado: (peli presupuestoAsignado).
		
			(unEmpleado class = Permanente) ifTrue: [((peli calcularPreRem >= totalCobrado) and: [(peli porcentajeAcum + unEmpleado porcentajePlus) <= 100]) ifTrue: [peli agregarEmpleadoPeli: unEmpleado.] ifFalse: [MessageBox warning: 'No alcanza el presupuesto para agregar al empleado o el porcentaje del plus supera el 100'] .].
			
			(unEmpleado class = Permanente) ifFalse: [(peli calcularPreRem >= totalCobrado) ifTrue: [peli agregarEmpleadoPeli: unEmpleado.] ifFalse: [MessageBox warning: 'No alcanza el presupuesto para agregar al empleado'] .].].         "ACA SE HACE UN IF PARA VER QUE TIPO DE COMPARACION SE NECESITA PARA EL PRES REM"

		(MessageBox confirm: '¿Desea seguir añadiendo empleados?') ifFalse: [bandera2 := false].].
	
	self agregarPelicula: peli.
	
	(MessageBox confirm: '¿Desea seguir cargando peliculas?') ifFalse: [bandera := false].].


!

emitirListado
|listadoFechas listadoTreintaDias| 

listadoFechas := SortedCollection new.
listadoTreintaDias := SortedCollection new.

listadoTreintaDias := (peliculas select: [:peli | ((Date today) asDays - (peli fecha) asDays) <= 30]).

listadoFechas addAll: (listadoTreintaDias collect: [:unaPeli | unaPeli fecha.]) .

^listadoFechas.



!

empleados
^empleados.!

iniEmpleados
empleados := OrderedCollection new.
!

iniPeliculas
peliculas := OrderedCollection new.!

mostrarEmpleados

empleados do: [:e | Transcript show: 'Nombre: ',  e nombre ; tab; show: '| Apellido: ', e apellido; tab; show: '| DNI: ', e documento; tab. 
	(e class = Permanente) ifTrue: [Transcript show: '| Tipo: ', (e class) printString; tab ;show: '| Porcentaje plus: ', (e porcentajePlus) printString ; cr]. 
	(e class = Actor) ifTrue: [Transcript show: '| Tipo: ', (e class) printString; tab ;show: '| Nacionalidad: ', e nacionalidad; tab; show: '| Cachet: ', (e cachet) printString; cr.].
	(e class = EquipoDireccion) ifTrue: [Transcript show: '| Tipo: ', (e class) printString; tab ; show: '| Nacionalidad: ', e nacionalidad; tab; show: '| Porcentaje sobre pres. asignado: ', (e porcentaje) printString; cr. ].].
!

mostrarPeliculas: listadoFechas
	| listaEmpleados peli|
	Transcript clear.
	listadoFechas do: 
			[:unaFecha |
			peli := peliculas detect: [:unaPeli | unaPeli fecha = unaFecha].
			Transcript
				show: 'Fecha creación; ' , unaFecha printString;
				tab;
				show: 'Codigo: ' , peli codigo;
				tab;
				show: 'título: ' , peli titulo;
				tab;
				show: 'Presupuesto asignado: ' , (peli presupuestoAsignado) printString;
				tab;
				show: 'Presupuesto remanente: ' , (peli calcularPreRem * ((100 - peli porcentajeAcum) / 100)) printString;
				tab;
				cr.
			Transcript
				tab;
				show: 'Miembros del staff: ';
				cr.
			listaEmpleados := peli empleados.
			listaEmpleados do: 
					[:unEmpleado |
					Transcript
						tab;
						show: 'Tipo: ' , unEmpleado class;
						tab;
						show: 'Nombre y apellido: ' , unEmpleado nombre , ' ' , unEmpleado apellido;
						tab.
					(unEmpleado isKindOf: Permanente) ifTrue: [Transcript show: (unEmpleado calcularCobradoListado: (peli calcularPreRem)) printString; cr].
					(unEmpleado isKindOf: Permanente) ifFalse: [Transcript show: (unEmpleado calcularTotalCobrado: (peli presupuestoAsignado) printString); cr.]]]!

peliculas
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
mostrarPeliculas:!public! !
peliculas!public! !
!

Pelicula guid: (GUID fromString: '{1c07fe4d-5336-481c-869c-a1b0efeada8c}')!
Pelicula comment: ''!
!Pelicula categoriesForClass!Kernel-Objects! !
!Pelicula methodsFor!

agregarEmpleadoPeli: unEmpleado
empleados add: unEmpleado.!

calcularPreRem
|acumulador|
acumulador := empleados inject: presupuestoAsignado into: [:acum :pers | acum - (pers calcularTotalCobrado: presupuestoAsignado)].
^acumulador.

!

cargarDatos
self codigo: UltimoCodigo .
Pelicula incrementarUltCod.
self titulo: (Prompter prompt: 'Titulo: ').
self presupuestoAsignado: (Prompter prompt: 'Presupuesto asignado: ').
self iniPresupuestoRemanente.
fecha := self ingresarFecha.
empleados := (OrderedCollection new).





!

codigo
^codigo.
!

codigo: unCodigo
codigo := unCodigo.
!

empleados
^empleados.
!

fecha
^fecha.




!

fecha: unDia mes: unMes anio: unAnio
^(Date year: unAnio month: unMes day: unDia).



!

ingresarFecha
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
presupuestoRemanente := presupuestoAsignado.
!

porcentajeAcum
|sumaTotal|
sumaTotal := empleados inject: 0 into: [:acum :unEmpleado | acum + unEmpleado porcentajePlus].
^sumaTotal!

presupuestoAsignado
^(presupuestoAsignado).


!

presupuestoAsignado: unPres
presupuestoAsignado := unPres asNumber.


!

titulo
^titulo.
!

titulo: unTitulo
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
UltimoCodigo := UltimoCodigo + 1.

!

iniPelicula
|pelicula|
pelicula := self new.
pelicula cargarDatos.
^pelicula.
!

iniUltCod
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
|plus|
plus := (porcentajePlus / 100) * presupuestoRemanente.
^(plus + SueldoBasico).

!

calcularTotalCobrado: unPresupuestoAsignado
^(SueldoBasico).
!

cargarDatos
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self porcentajePlus: (Prompter prompt: 'Porcentaje plus: ') asNumber.
!

plus
^porcentajePlus!

porcentajePlus
^porcentajePlus.!

porcentajePlus: unPorPlus
porcentajePlus := unPorPlus.
! !
!Permanente categoriesForMethods!
calcularCobradoListado:!public! !
calcularTotalCobrado:!public! !
cargarDatos!public! !
plus!accessing!public! !
porcentajePlus!public! !
porcentajePlus:!public! !
!

!Permanente class methodsFor!

iniPermanente
|permanente|
permanente := self new.
permanente cargarDatos.
^permanente.
!

SueldoBasico
^SueldoBasico. 
!

SueldoBasico: SB
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
^nacionalidad.
!

nacionalidad: unaN
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
^cachet.!

cachet: unCachet
cachet := unCachet.!

calcularTotalCobrado: unPresupuestoAsignado
^cachet.
!

cargarDatos
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self nacionalidad: (Prompter prompt: 'Nacionalidad: ').
self cachet: (Prompter prompt: 'Cachet: ') asNumber.! !
!Actor categoriesForMethods!
cachet!public! !
cachet:!public! !
calcularTotalCobrado:!public! !
cargarDatos!public! !
!

!Actor class methodsFor!

iniActor
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
^((porcentaje * unPresupuestoAsignado) / 100)!

cargarDatos
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self nacionalidad: (Prompter prompt: 'Nacionalidad: ').
self porcentaje: (Prompter prompt: 'Porcentaje: ') asNumber.

!

porcentaje
^porcentaje.!

porcentaje: unP
porcentaje := unP.! !
!EquipoDireccion categoriesForMethods!
calcularTotalCobrado:!public! !
cargarDatos!public! !
porcentaje!public! !
porcentaje:!public! !
!

!EquipoDireccion class methodsFor!

iniEquipoDirec
| equipo |
equipo := self new.
equipo cargarDatos.
^equipo.! !
!EquipoDireccion class categoriesForMethods!
iniEquipoDirec!public! !
!

"Binary Globals"!

