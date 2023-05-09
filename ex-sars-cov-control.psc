Funcion registro <- registroEmpleados (empleados, n)
	Definir x Como Cadena
	Definir i Como Entero
	Definir nombre, direccion, localidad, estadoSalud, temperatura, contactoCovid, comorbilidades, comorbilidadesFamilia, numConv, posible_contagio Como Cadena
			
	Para i <- 1 Hasta n Hacer
		Escribir "Ingrese la información del empleado", i, ":"
		
		Escribir "Nombre:"
		Leer nombre
		empleados[i, 1] <- nombre
		
		Escribir "Dirección:"
		Leer direccion
		empleados[i, 2] <- direccion
		
		Escribir "Localidad:"
		Leer localidad
		empleados[i, 3] <- localidad
		
		Escribir "Número de personas con las que convive:"
		Leer numConv
		empleados[i, 4] <- numConv
		
		Escribir "Estado de salud actual: califique de 1 a 5. Donde 1 es mal y 5 es optimo:"
		Leer estadoSalud
		empleados[i, 5] <- estadoSalud
		
		Escribir "¿Ha tenido Covid-19?  (1: Sí, 2: No)"
		Leer contactoCovid
		empleados[i, 6] <- contactoCovid
		
		Escribir "Comorbilidades del empleado:"
		Leer empleados[i, 7] <- comorbilidades
		
		Escribir "Comorbilidades de familiares:"
		Leer empleados[i, 8] <- comorbilidadesFamilia
		
		Escribir "Temperatura actual del empleado:"
		Leer temperatura
		empleados[i, 9] <- temperatura
		
		Escribir "Ha tenido contacto con alguien con el virus recientemente (1: Sí, 2: No)"
		Leer posible_contagio
		empleados[i, 10] <- posible_contagio
	FinPara
		
FinFuncion

Funcion imprimirMatriz(empleados, n)
	Definir i Como Entero
	Definir  posibilidad_contagio, tuvo_covid como caracter
		
		Escribir "____________________________________________"
		Escribir "INFORME DE CONTROL DE EMPLEADOS"
		Escribir "____________________________________________"
		
		Para i <- 1 Hasta n Hacer
			
			Si empleados[i, 10] == "1"  Entonces
				posibilidad_contagio = "Sí"
			Sino
				posibilidad_contagio = "No"
			FinSi
			
			Si empleados[i, 6] == "1" Entonces
				tuvo_covid = "Sí"
			Sino
				tuvo_covid = "No"
			FinSi
	
			
			Escribir "Empleado", i, ":"
			Escribir "Nombre:", empleados[i, 1]
			Escribir "Dirección:", empleados[i, 2]
			Escribir "Localidad:", empleados[i, 3]
			Escribir "Número de personas con las que convive:", empleados[i, 4]
			Escribir "Estado de salud actual:", empleados[i, 5]
			Escribir "¿Ha tenido Covid-19?:", tuvo_covid
			Escribir "Comorbilidades del empleado:", empleados[i, 7]
			Escribir "Comorbilidades de familiares:", empleados[i, 8]
			Escribir "Temperatura actual del empleado:", empleados[i, 9]
			Escribir "Contacto con alguien con el virus recientemente:", posibilidad_contagio
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
	Escribir " 4: Listar empleados disponibles para tarbajar"
	Escribir " 0: Salir"
	Escribir "*********************************************************************************"
FinFuncion


Funcion asiste <- analizarEmpleado(temperatura, contactoCovid, estadoSalud, TEMPERATURA_UMBRAL)
    Definir puedeAsistir Como Logico
	
    puedeAsistir <- Falso
    
    Si (temperatura <= TEMPERATURA_UMBRAL) Y (contactoCovid <> "1") Y (estadoSalud >= "3") Entonces
        puedeAsistir <- Verdadero
    FinSi
    
    asiste <- puedeAsistir
FinFuncion

Funcion listarEmpleadosDisponibles(empleados, n)
    Definir i Como Entero
    Definir puedeAsistir Como Logico
	
    Escribir "____________________________________________"
    Escribir "LISTADO DE EMPLEADOS APTOS PARA TRABAJAR"
    Escribir "____________________________________________"
	
    Para i <- 1 Hasta n Hacer
	    TEMPERATURA_UMBRAL = "37.5";
        puedeAsistir <- analizarEmpleado(empleados[i, 9], empleados[i, 10], empleados[i, 5], TEMPERATURA_UMBRAL)
		
	    Escribir "Empleado", i, ":"
        Escribir "Nombre:", empleados[i, 1]
        Escribir "Puede asistir al trabajo:"
		
		Si puedeAsistir Entonces 
			Escribir "Sí"
		SiNo
			Escribir "No"
		FinSi
	    Escribir "____________________________________________"
	FinPara
FinFuncion


Funcion actualizarDatosDiarios(empleados, n)
	Definir numEmpleado Como Entero
    Definir temperatura, contactoCovid, estadoSalud Como Cadena
	
    Escribir "Ingrese el número del empleado que desea actualizar:"
    Leer numEmpleado
	
	
	
    Si numEmpleado >= 1 Y numEmpleado <= n Entonces
        Escribir "Actualización de datos diarios del empleado", numEmpleado, ":"
        Escribir "Nombre:", empleados[numEmpleado, 1]
		
        Escribir "Temperatura actual del empleado:"
        Leer temperatura
        empleados[numEmpleado, 9] <- temperatura
		
        Escribir "Ha tenido contacto con alguien con el virus recientemente (1: Sí, 2: No)"
        Leer contactoCovid
        empleados[numEmpleado, 10] <- contactoCovid
		
        Escribir "Estado de salud actual: califique de 1 a 5. Donde 1 es mal y 5 es optimo:"
        Leer estadoSalud
        empleados[numEmpleado, 5] <- estadoSalud
		
        Escribir "____________________________________________"
    Sino
        Escribir "Número de empleado no válido. Por favor, intente de nuevo."
    FinSi
FinFuncion


Algoritmo ControlDatosPersonalesClinicosTrabajadores
	
	Definir n, opcion Como Entero
	Definir empleados, matriz_empleados Como Caracter
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
			
			Dimension matriz_empleados(n, 10)
			empleados <- registroEmpleados(matriz_empleados, n)
		FinSi
		
		Si opcion == 2
			imprimirMatriz(matriz_empleados, n)
		FinSi
		
		Si opcion == 3
			actualizarDatosDiarios(matriz_empleados, n)
		FinSi
		
		Si opcion == 4
			listarEmpleadosDisponibles(matriz_empleados, n)
		FinSi
		
	Hasta Que (opcion == 0)

FinAlgoritmo
