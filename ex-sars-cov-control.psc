Funcion registro <- registroEmpleados (matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)

	Definir x, nombre, direccion, localidad Como Caracter
	Definir i, estadoSalud, contactoCovid, comorbilidades, comorbilidadesFamilia, numConv, posible_contagio Como Entero
	Definir temperatura como Real 
	
	Para i <- 1 Hasta n Hacer
		Escribir "Ingrese la información del empleado #", i, ":"
		
		Escribir ''
		Escribir '-> Datos personales:'
		Escribir ''
		
		Escribir "- Nombre:"
		Leer nombre
		matriz_empleados_string[i, 1] <- nombre
		
		Escribir "- Dirección:"
		Leer direccion
		matriz_empleados_string[i, 2] <- direccion
		
		Escribir "- Localidad:"
		Leer localidad
		matriz_empleados_string[i, 3] <- localidad
		
		Escribir "- Número de personas con las que convive:"
		Leer numConv
		matriz_empleados_int[i, 1] <- numConv
		
		Escribir ''
		Escribir '-> Datos clinicos:'
		Escribir ''
		
		Escribir '- Estado de salud,'
		Escribir '-- (1: Muy deficiente, 2: Deficiente, 3: Moderado, 4: Bueno, 5: Excelente):'
		Leer estadoSalud
		
		Mientras estadoSalud > 5 | estadoSalud < 1 Hacer
			Escribir 'Digita un valor válido | Estado de salud, (1: Muy deficiente, 2: Deficiente, 3: Moderado, 4: Bueno, 5: Excelente):'
			leer estadoSalud
		FinMientras
		
		matriz_empleados_int[i, 2] <- estadoSalud
		
		Escribir "- ¿Ha tenido Covid-19?  (1: Sí, 2: No)"
		Leer contactoCovid
		
		Mientras contactoCovid > 2 | contactoCovid < 1 Hacer
			Escribir 'Digita un valor válido | ¿Ha tenido Covid-19?, (1: Sí, 2: No):'
			leer contactoCovid
		FinMientras
		
		matriz_empleados_int[i, 3] <- contactoCovid
		
		si contactoCovid == 1 Entonces
			Escribir 'Digita el nivel de afectación:'
			Escribir '(1: Sin síntomas o síntomas leves, 2: Síntomas moderados, 3: Síntomas moderadamente graves,'
			Escribir ' 4: Síntomas graves, 5: Síntomas críticos)'
			leer nivelAfectacion
			
			Mientras nivelAfectacion > 5 | nivelAfectacion < 1 Hacer
				Escribir 'Digita un valor válido | (1: Sin síntomas o síntomas leves, 2: Síntomas moderados, 3: Síntomas moderadamente graves,'
				Escribir ' 4: Síntomas graves, 5: Síntomas críticos)'
				leer nivelAfectacion
			FinMientras
			
			matriz_empleados_int[i, 4] <- nivelAfectacion
		FinSi
		
		Escribir "- ¿El empleado tiene comorbilidades?, (1: Si, 2: No):"
		Leer comorbilidades
		
		Mientras comorbilidades > 2 | comorbilidades < 1 Hacer
			Escribir 'Digita un valor válido | ¿El empleado tiene comorbilidades?, (1: Si, 2: No):'
			leer comorbilidades
		FinMientras
		
		matriz_empleados_int[i, 5] <- comorbilidades
		
		Escribir "- ¿El empleado tiene familiares con comorbilidades? (1: Si, 2: No):"
		Leer comorbilidadesFamilia
		
		Mientras comorbilidadesFamilia > 2 | comorbilidadesFamilia < 1 Hacer
			Escribir 'Digita un valor válido | ¿El empleado tiene familiares con comorbilidades?, (1: Si, 2: No)'
			leer comorbilidadesFamilia
		FinMientras
		
		matriz_empleados_int[i, 6] <- comorbilidades
		
		Escribir "- Temperatura actual del empleado:"
		Leer temperatura
		matriz_empleados_bool[i, 1] <- temperatura
		
		Escribir "- ¿Ha tenido contacto recientemente con una persona que porta el virus? (1: Sí, 2: No)"
		Leer posible_contagio
		
		Mientras posible_contagio > 2 | posible_contagio < 1 Hacer
			Escribir 'Digita un valor válido | ¿Ha tenido contacto recientemente con alguien que porta el virus? (1: Sí, 2: No)'
			leer posible_contagio
		FinMientras
		
		matriz_empleados_int[i, 7] <- posible_contagio
		
	FinPara
	
FinFuncion

Funcion imprimirMatriz(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
	Definir i Como Entero
	Definir  posibilidad_contagio, tuvo_covid, estado_salud_act, nivel_afectacion, tiene_cormobilidades, tiene_fam_cormobilidades como caracter
	
	Escribir "____________________________________________"
	Escribir "INFORME DE CONTROL DE EMPLEADOS"
	Escribir "____________________________________________"
	
	Para i <- 1 Hasta n Hacer
		
		Si matriz_empleados_int[i, 7] == 1  Entonces
			posibilidad_contagio = "Sí"
		Sino
			posibilidad_contagio = "No"
		FinSi
		
		si matriz_empleados_int[i, 2] == 1 entonces 
			estado_salud_act = "Muy deficiente"
		FinSi
		
		si matriz_empleados_int[i, 2] == 2 entonces 
			estado_salud_act = "Deficiente"
		FinSi
		
		si matriz_empleados_int[i, 2] == 3 entonces 
			estado_salud_act = "Moderado"
		FinSi
		
		si matriz_empleados_int[i, 2] == 4 entonces 
			estado_salud_act = "Bueno"
		FinSi
		
		si matriz_empleados_int[i, 2] == 5 entonces 
			estado_salud_act = "Excelente"
		FinSi
		
		si matriz_empleados_int[i, 4] == 1 entonces 
			nivel_afectacion = "Sin síntomas o síntomas leves"
		FinSi
		
		si matriz_empleados_int[i, 4] == 2 entonces 
			nivel_afectacion = "Síntomas moderados"
		FinSi
		
		si matriz_empleados_int[i, 4] == 3 entonces 
			nivel_afectacion = "Síntomas moderadamente graves"
		FinSi
		
		si matriz_empleados_int[i, 4] == 4 entonces 
			nivel_afectacion = "Síntomas graves"
		FinSi
		
		si matriz_empleados_int[i, 4] == 5 entonces 
			nivel_afectacion = "Síntomas críticos"
		FinSi
		
		si matriz_empleados_int[i, 5] == 1 entonces 
			tiene_cormobilidades = 'Sí'
		FinSi
		
		si matriz_empleados_int[i, 5] == 2 entonces 
			tiene_cormobilidades = 'No'
		FinSi
		
		si matriz_empleados_int[i, 6] == 1 entonces 
			tiene_fam_cormobilidades = 'Sí'
		FinSi
		
		si matriz_empleados_int[i, 6] == 2 entonces 
			tiene_fam_cormobilidades = 'No'
		FinSi
		
		
		Escribir "-> Empleado #", i, ":"
		Escribir "- Nombre: ", matriz_empleados_string[i, 1]
		Escribir "- Dirección: ", matriz_empleados_string[i, 2]
		Escribir "- Localidad: ", matriz_empleados_string[i, 3]
		Escribir "- Número de personas con las que convive: ", matriz_empleados_int[i, 1]
		Escribir "- Estado de salud actual: ", estado_salud_act
		si matriz_empleados_int[i, 3] == 1 Entonces
			Escribir "-- ¿Ha tenido Covid-19?: Si"
			Escribir "-- Nivel de afectación: ", nivel_afectacion
		FinSi
		Escribir "- ¿Tiene comorbilidades?: ", tiene_cormobilidades
		Escribir "- ¿Tiene familiares con comorbilidades?: ", tiene_fam_cormobilidades
		Escribir "- Temperatura actual del empleado: ", matriz_empleados_bool[i, 1]
		Escribir "- ¿Ha tenido contacto con alguien con el virus recientemente?: ", posibilidad_contagio
		Escribir "____________________________________________"
		
	FinPara
FinFuncion

Funcion imprimirMenu()
	Escribir ""
	Escribir "Seleccione una opcion para continuar"
	Escribir "*********************************************************************************"
	Escribir " 1: Agregar usuario"
	Escribir " 2: Listar usuarios"
	Escribir " 3: Actualizacion diaria diario"
	Escribir " 4: Listar empleados disponibles para trabajar"
	Escribir " 0: Salir"
	Escribir "*********************************************************************************"
FinFuncion


Funcion asiste <- analizarEmpleado(temperatura, contactoCovid, estadoSalud, comorbilidades, TEMPERATURA_UMBRAL)
    Definir puedeAsistir Como Logico
	
    puedeAsistir <- Falso
    
    Si (temperatura <= TEMPERATURA_UMBRAL) Y (contactoCovid <> 1) Y (estadoSalud >= 3) Y (comorbilidades <> 1) Entonces
        puedeAsistir <- Verdadero
    FinSi
    
    asiste <- puedeAsistir
FinFuncion


Funcion actualizarDatosDiarios(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
	Definir numEmpleado Como Entero
    Definir contactoCovid, estadoSalud Como Cadena
	
    Escribir "Ingrese el número del empleado que desea actualizar:"
    Leer numEmpleado
	
	
    Si numEmpleado >= 1 Y numEmpleado <= n Entonces
        Escribir "Actualización de datos diarios del empleado #", numEmpleado, ":"
        Escribir "Nombre: ", matriz_empleados_string[numEmpleado, 1]
		
        Escribir "Temperatura actual del empleado:"
        Leer temperatura
        matriz_empleados_bool[numEmpleado, 1] <- temperatura
		
		Escribir "- ¿Ha tenido contacto recientemente con una persona que porta el virus? (1: Sí, 2: No)"
		Leer posible_contagio
		
		Mientras posible_contagio > 2 | posible_contagio < 1 Hacer
			Escribir 'Digita un valor válido | ¿Ha tenido contacto recientemente con alguien que porta el virus? (1: Sí, 2: No)'
			leer posible_contagio
		FinMientras
		
		matriz_empleados_int[numEmpleado, 7] <- posible_contagio
		
		Escribir '- Estado de salud actual, (1: Muy deficiente, 2: Deficiente, 3: Moderado, 4: Bueno, 5: Excelente):'
		Leer estado_salud_act
		
		Mientras estado_salud_act > 5 | estado_salud_act < 1 Hacer
			Escribir 'Digita un valor válido | Estado de salud, (1: Muy deficiente, 2: Deficiente, 3: Moderado, 4: Bueno, 5: Excelente):'
			leer estado_salud_act
		FinMientras
		
		matriz_empleados_int[numEmpleado, 2] <- estado_salud_act
		
        Escribir "____________________________________________"
    Sino
        Escribir "Número de empleado no válido. Por favor, intente de nuevo."
    FinSi
FinFuncion

Funcion listarEmpleadosDisponibles(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
    Definir i Como Entero
    Definir puedeAsistir Como Logico
    Definir ningunEmpleadoPuedeAsistir Como Logico
    ningunEmpleadoPuedeAsistir <- Verdadero
	
    Escribir "____________________________________________"
    Escribir "LISTADO DE EMPLEADOS APTOS PARA TRABAJAR"
    Escribir "____________________________________________"
	
    Para i <- 1 Hasta n Hacer
	    TEMPERATURA_UMBRAL = 37.5;
        puedeAsistir <- analizarEmpleado(matriz_empleados_bool[i, 1], matriz_empleados_int[i, 7], matriz_empleados_int[i, 2], matriz_empleados_int[i, 5], TEMPERATURA_UMBRAL)
		
		Si puedeAsistir == Verdadero Entonces
			ningunEmpleadoPuedeAsistir <- Falso
			Escribir "-> Empleado #", i, ":"
			Escribir "- Nombre: ", matriz_empleados_string[i, 1]
			Escribir "- Puede asistir al trabajo:"
			Escribir "- Sí"
			Escribir "____________________________________________"
		SiNo
			
		FinSi
    FinPara
	
	Escribir ''
	Escribir "____________________________________________"
    Escribir "LISTADO DE EMPLEADOS NO APTOS PARA TRABAJAR"
    Escribir "____________________________________________"
	
    Para i <- 1 Hasta n Hacer
	    TEMPERATURA_UMBRAL = 37.5;
        puedeAsistir <- analizarEmpleado(matriz_empleados_bool[i, 1], matriz_empleados_int[i, 7], matriz_empleados_int[i, 2], matriz_empleados_int[i, 5], TEMPERATURA_UMBRAL)
		
		Si puedeAsistir == Falso Entonces
			ningunEmpleadoPuedeAsistir <- Falso
			Escribir "-> Empleado #", i, ":"
			Escribir "- Nombre: ", matriz_empleados_string[i, 1]
			Escribir "- Puede asistir al trabajo:"
			Escribir "- No, reporte a recursos humanos"
			Escribir "____________________________________________"
		SiNo
			
		FinSi
    FinPara
	
	
    Si ningunEmpleadoPuedeAsistir Entonces
		Escribir ''
		Escribir '--> Ningún trabajador puede asistir.'
	SiNo
		
	FinSi
FinFuncion




Algoritmo ControlDatosPersonalesClinicosTrabajadores
	
	Definir n, opcion Como Entero
	Definir matriz_empleados_string Como Caracter
	Definir matriz_empleados_int Como Entero
	definir TEMPERATURA_UMBRAL Como Real
	
	TEMPERATURA_UMBRAL = 37.5;
	
	Escribir "** Bienvenido al control de datos personales clinicos de trabajadores **"
	Escribir "*********************************************************************************"
	
	Repetir
		imprimirMenu()
		leer opcion
		
		Si opcion == 1
			
			Escribir "Ingrese el número de empleados que desea registrar:"
			Leer n
			
			Dimension matriz_empleados_string(n, 3)
			Dimension matriz_empleados_int(n, 7)
			Dimension matriz_empleados_bool(n, 1)
			empleados <- registroEmpleados(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
		FinSi
		
		Si opcion == 2
			Si n > 0
				imprimirMatriz(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
			Sino
				Escribir "No hay usuarios registrados"
			FinSi
		FinSi
		
		Si opcion == 3
			Si n > 0
				actualizarDatosDiarios(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
			Sino
				Escribir "No hay usuarios registrados"
			FinSi
		FinSi
		
		Si opcion == 4	
			Si n > 0
				listarEmpleadosDisponibles(matriz_empleados_string, matriz_empleados_int, matriz_empleados_bool, n)
			Sino
				Escribir "No hay usuarios registrados"
			FinSi
		FinSi
		
	Hasta Que (opcion == 0)

FinAlgoritmo
