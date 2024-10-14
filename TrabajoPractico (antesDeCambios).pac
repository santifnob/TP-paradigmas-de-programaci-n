| package |
package := Package name: 'TrabajoPractico'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Actor;
	add: #EquipoDireccion;
	add: #EstudioCinematocrafico;
	add: #Pelicula;
	add: #Permanente;
	add: #Personal;
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

Object subclass: #EstudioCinematocrafico
	instanceVariableNames: 'peliculas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Pelicula
	instanceVariableNames: 'codigo fecha titulo personales presupuestoAsignado presupuestoRemanente'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Personal
	instanceVariableNames: 'pelicula documento nombre apellido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Personal subclass: #Permanente
	instanceVariableNames: 'plus'
	classVariableNames: 'SueldoBasico'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Personal subclass: #Staff
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

EstudioCinematocrafico guid: (GUID fromString: '{d751dc02-d653-4b6e-afa0-a4a8840982ca}')!
EstudioCinematocrafico comment: ''!
!EstudioCinematocrafico categoriesForClass!Kernel-Objects! !
!EstudioCinematocrafico methodsFor!

agregarPelicula: unaPelicula
peliculas add: unaPelicula.
!

cargarPeliculas
|peli persona bandera|
bandera := true.
[bandera] whileTrue: [
	Pelicula iniUltCod.
	peli := Pelicula iniPelicula.
	]
!

emitirListado
|listado| 
listado := OrderedCollection new.
listado select: [:peli | ((Date today) asDays - (peli fecha) asDays) <= 30].
listado sortUsing: [:peli1 :peli2 | peli1 fecha < peli2 fecha].
^listado.



!

iniPeliculas
peliculas := OrderedCollection new.!

peliculas
^peliculas.
! !
!EstudioCinematocrafico categoriesForMethods!
agregarPelicula:!public! !
cargarPeliculas!public! !
emitirListado!public! !
iniPeliculas!public! !
peliculas!public! !
!

!EstudioCinematocrafico class methodsFor!

mostrarPeliculas: pelis
Transcript clear.
pelis do: [:peli | Transcript show: 'Fecha creación; ', (peli fecha) printString; tab; show: 'Codigo: ', peli codigo; tab; show: 'título: ', peli titulo; tab; show: 'Presupuesto asignado: ', (peli presupuestoAsignado) printString; tab; show: 'Presupuesto remanente: ', (peli presupuestoRemanente) printString; tab; cr.
	Transcript tab; show: 'Miembros del staff: '; cr.
	pelis personales do: [:personal | Transcript tab; show: '-Tipo: ', personal class; tab; show: 'Nombre y apellido: ', personal nombre, ' ', personal apellido; tab; show: 'Total cobrado: ', personal calcularTotalCobrado; cr. ] ].



! !
!EstudioCinematocrafico class categoriesForMethods!
mostrarPeliculas:!public! !
!

Pelicula guid: (GUID fromString: '{1c07fe4d-5336-481c-869c-a1b0efeada8c}')!
Pelicula comment: ''!
!Pelicula categoriesForClass!Kernel-Objects! !
!Pelicula methodsFor!

addPersonal: unPersonal
personales add: unPersonal.


!

calcularPreRem
|acumulador|
acumulador := personales inject: presupuestoAsignado into: [:acum :pers | acum - pers calcularTotalCobrado].
^acumulador.

!

cargaPersonal
|unPersonal tipo opcion|
opcion := true.
[opcion] whileTrue: [
	tipo := ChoicePrompter choices: #('Permanente' 'Actor' 'Equipo de dirección').
	(tipo isNil) ifFalse: [
		tipo := tipo printString.
		(tipo = 'Permanente') ifTrue: [unPersonal := Permanente iniPermanente].
		(tipo = 'Actor') ifTrue: [unPersonal := Actor iniActor].
		(tipo = 'Equipo de dirección') ifTrue: [unPersonal := EquipoDireccion iniEquipoDirec].
		((unPersonal calcularTotalCobrado) < (self calcularPreRem)) ifTrue: [self addPersonal: unPersonal] ifFalse: [MessageBox notify: 'No se puede agregar al personal, su total cobrado pasa el presupuesto remanente.'] ].
	
	opcion := MessageBox confirm: 'Desea continuar con la carga del personal?'.]

 
!

cargarDatos
|dia mes anio|
self codigo: UltimoCodigo .
Pelicula incrementarUltCod.
self titulo: (Prompter prompt: 'Titulo: ').
self presupuestoAsignado: (Prompter prompt: 'Presupuesto asignado: ').
self iniPresupuestoRemanente.
dia := Prompter prompt: 'Dia: '.
mes := Prompter prompt: 'Mes: '.
anio := Prompter prompt: 'Año: '.
self fecha: dia mes: mes anio: anio.
personales := (OrderedCollection new).





!

codigo
^codigo.
!

codigo: unCodigo
codigo := unCodigo.
!

fecha
^fecha.




!

fecha: unDia mes: unMes anio: unAnio
|unaFecha| 
unaFecha := Date new.
unaFecha day: unDia month: unMes year: unAnio.
^unaFecha.



!

iniPresupuestoRemanente
presupuestoRemanente := presupuestoAsignado.
!

personales
^personales.
!

presupuestoAsignado
^(presupuestoAsignado asFloat).


!

presupuestoAsignado: unPres
presupuestoAsignado := unPres asFloat.


!

presupuestoRemanente
^(presupuestoRemanente asFloat).!

presupuestoRemanente: unP
presupuestoRemanente := unP.
!

titulo
^titulo.
!

titulo: unTitulo
titulo := unTitulo.
! !
!Pelicula categoriesForMethods!
addPersonal:!public! !
calcularPreRem!public! !
cargaPersonal!public! !
cargarDatos!public! !
codigo!public! !
codigo:!public! !
fecha!public! !
fecha:mes:anio:!public! !
iniPresupuestoRemanente!public! !
personales!public! !
presupuestoAsignado!public! !
presupuestoAsignado:!public! !
presupuestoRemanente!public! !
presupuestoRemanente:!public! !
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

Personal guid: (GUID fromString: '{f1cd45e3-5dcb-4315-bb39-ee8d87cfbbed}')!
Personal comment: ''!
!Personal categoriesForClass!Kernel-Objects! !
!Personal methodsFor!

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

!

pelicula
^pelicula.
!

pelicula: unaPelicula
pelicula := unaPelicula.

! !
!Personal categoriesForMethods!
apellido!public! !
apellido:!public! !
documento!public! !
documento:!public! !
nombre!public! !
nombre:!public! !
pelicula!public! !
pelicula:!public! !
!

Permanente guid: (GUID fromString: '{d1f702c4-064c-4f89-bf0a-c0049bbe0a88}')!
Permanente comment: ''!
!Permanente categoriesForClass!Kernel-Objects! !
!Permanente methodsFor!

calcularTotalCobrado
^(plus + SueldoBasico).
!

cargarDatos
self nombre: (Prompter prompt: 'Nombre: ').
self apellido: (Prompter prompt: 'Apellido: ').
self documento: (Prompter prompt: 'DNI: ').
self plus: (Prompter prompt: 'Plus: ').!

plus
^plus.!

plus: unPlus
plus := unPlus asFloat.! !
!Permanente categoriesForMethods!
calcularTotalCobrado!public! !
cargarDatos!public! !
plus!public! !
plus:!public! !
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
SueldoBasico := SB asFloat.
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
cachet := unCachet asFloat.!

calcularTotalCobrado
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
calcularTotalCobrado!public! !
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

calcularTotalCobrado
^((porcentaje * pelicula presupuestoAsignado) / 100)!

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
calcularTotalCobrado!public! !
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

