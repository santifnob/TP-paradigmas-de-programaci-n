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

buscarDni: unDni listaEmpleados: unaListEmp!

cargarEmpleados
	| unEmpleado tipo opcion |
	opcion := true.
	[opcion] whileTrue: 
			[tipo := ChoicePrompter choices: #('Permanente' 'Actor' 'Equipo de dirección').
			(tipo isNil) ifFalse: 
					[tipo := tipo printString.
					tipo = 'Permanente' ifTrue: [unEmpleado := Permanente iniPermanente].
					tipo = 'Actor' ifTrue: [unEmpleado := Actor iniActor].
					tipo = 'Equipo de dirección' ifTrue: [unEmpleado := EquipoDireccion iniEquipoDirec].
					self agregarEmpleado: unEmpleado.].

			opcion := MessageBox confirm: '¿Desea continuar con la carga del personal?']!

cargarPeliculas
|peli bandera bandera2 unEmpleado unDni totalCobrado|

bandera := true.

[bandera] whileTrue: [
	Pelicula iniUltCod.
	peli := Pelicula iniPelicula.
	bandera2 := true.
	
	"INGRESO DE EMPLEADOS A LA PELICULA" 
	(bandera2) ifTrue: [
		unDni := Prompter prompt: 'Ingresar un DNI:' caption: 'Ingreso de empleados'.
		unEmpleado := self buscarDni: unDni listaEmpleados: (self empleados).  "devuelve los datos del empleado si lo encontro, y 0 si no"

		(unEmpleado = 0) ifTrue: [MessageBox notify: 'El empleado que ingreso no existe en el sistema'].
		
		(unEmpleado = 0) ifFalse: [
			totalCobrado := unEmpleado calcularTotalCobrado: (peli presupuestoAsignado).
			((peli calcularPreRem >= totalCobrado) and: [(peli porcentajeAcum + unEmpleado porcentajePlus) <= 100]) ifTrue: [peli agregarEmpleado: unEmpleado.].].

		(MessageBox confirm: '¿Desea seguir añadiendo empleados?') ifTrue: [bandera2 := false]. ].
	
	self agregarPelicula: peli].
	
	(MessageBox confirm: '¿Desea seguir cargando peliculas?') ifTrue: [bandera := false].


!

emitirListado
|listado| 
listado := OrderedCollection new.
listado select: [:peli | ((Date today) asDays - (peli fecha) asDays) <= 30].
listado sortUsing: [:peli1 :peli2 | peli1 fecha < peli2 fecha].
^listado.



!

empleados
^empleados.!

iniEmpleados
empleados := OrderedCollection new.
!

iniPeliculas
peliculas := OrderedCollection new.!

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
peliculas!public! !
!

!EstudioCinematocrafico class methodsFor!

mostrarPeliculas: pelis
	| listaEmpleados |
	Transcript clear.
	pelis do: 
			[:peli |
			Transcript
				show: 'Fecha creación; ' , peli fecha printString;
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
			listaEmpleados := pelis empleados.
			listaEmpleados do: 
					[:unEmpleado |
					Transcript
						tab;
						show: 'Tipo: ' , unEmpleado class;
						tab;
						show: 'Nombre y apellido: ' , unEmpleado nombre , ' ' , unEmpleado apellido;
						tab.
					(unEmpleado isKindOf: Permanente) ifTrue: [Transcript show: (unEmpleado calcularCobradoListado: (peli calcularPreRem)) printString; cr].
					(unEmpleado isKindOf: Permanente) ifFalse: [Transcript show: (unEmpleado calcularTotalCobrado: (peli presupuestoAsignado) printString); cr.]]]
! !
!EstudioCinematocrafico class categoriesForMethods!
mostrarPeliculas:!public! !
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

cargarPelicula
titulo:= Prompter prompt: 'Ingrese titulo: '.
fecha:= Prompter prompt: 'Ingrese fecha: '.
presupuestoAsignado := Prompter prompt: 'Ingrese presupuesto asignado: '!

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

fechaValida: dia mes: mes anio: anio
|f|
    ^[ f := Date newDay: dia month: mes year: anio. f]
        on: Error
        do: [ :ex | nil ]. !

ingresarFecha
    | dia mes anio f|
    [ 
        dia := (Prompter prompt: 'Día: ') asNumber.
        mes := (Prompter prompt: 'Mes: ') asNumber.
        anio := (Prompter prompt: 'Año: ') asNumber.
        f := self fechaValida: dia mes: mes anio: anio.
        f isNil 
            ifTrue: [ MessageBox warning: 'Fecha no válida. Intenta de nuevo.']
    ] whileTrue: [ f isNil ].
  Transcript show: 'Fecha válida: ', f printString; cr.

    Transcript show: 'Fecha válida: ', f printString; cr.!

iniPresupuestoRemanente
presupuestoRemanente := presupuestoAsignado.
!

porcentajeAcum
|sumaTotal|
sumaTotal := empleados inject: 0 into: [:acum :unEmpleado | acum = acum + unEmpleado porcentajePlus].
^sumaTotal!

presupuestoAsignado
^(presupuestoAsignado).


!

presupuestoAsignado: unPres
presupuestoAsignado := unPres.


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
cargarPelicula!public! !
codigo!public! !
codigo:!public! !
empleados!public! !
fecha!public! !
fecha:mes:anio:!public! !
fechaValida:mes:anio:!public! !
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
self porcentajePlus: (Prompter prompt: 'Plus: ').!

cargarPermanente
SueldoBasico := Prompter prompt: 'Ingrese sueldo basico: '.
porcentajePlus := Prompter prompt: 'Ingrese plus: '!

plus
^porcentajePlus!

porcentajePlus
^porcentajePlus.!

porcentajePlus: unPorPlus
porcentajePlus := unPorPlus.
!

sueldoBasico
	^SueldoBasico! !
!Permanente categoriesForMethods!
calcularCobradoListado:!public! !
calcularTotalCobrado:!public! !
cargarDatos!public! !
cargarPermanente!public! !
plus!accessing!public! !
porcentajePlus!public! !
porcentajePlus:!public! !
sueldoBasico!accessing!public! !
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
SueldoBasico := SB.
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
self cachet: (Prompter prompt: 'Cachet: ').! !
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
self porcentaje: (Prompter prompt: 'Porcentaje: ').

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

