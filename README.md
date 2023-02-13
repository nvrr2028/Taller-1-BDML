#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: GEIH DANE                                           #
#**************************************************************************************#


# Predicting income - GEIH

# Abstract

La brecha fiscal es un de los principales problemas del mundo, esta, es la diferencia entre la cantidad de impuestos que el gobierno puede recaudar contra la cantidad de impuestos que realmente recauda. En Colombia, según Bonet (2016), la brecha fiscal en el 2014 fue de aproximadamente 46 billones de pesos, lo que demuestra un gran desperdicio de eficiencia tributaria. Es importante reducir esta brecha debido a que se debe mantener un sistema tributario justo, eficiente y efectivo, para garantizar el mayor recaudo posible para el estado y poder brindar servicios esenciales a la población. Para esto, se realizará un estudio de predicción de ingresos, el cual podría potencialmente ayudar a acumular casos de fraude que lograría la reducción de esta brecha. En términos generales el estudio concluye que en Colombia hay brechas salariales por discriminación por género, y además, el modelo propuesto puede ser un buen predictor del salario, ya que es efectivo prediciendo muestra dentro del grueso de la población pero no tanto en observaciones atípicas. 

# Document

La carpeta de "document" contiene el documento final con el desarrollo del ejercicio.

# Data files

Todos los datos se encuentran fueron scrappeados del repositorio que se encuentra en la página "https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html". De allí, se tomaron 10 piezas de datos que se unieron en una sola base, cuyo código se puede encontrar en el script "Taller-1-BDML" de la carpeta "scripts".

# Code files

En análisis del ingreso se desarrolla en R version 4.2.2 (2022-10-31 ucrt).
El código principal para correr el ejercicio es "Taller 1 BDML", el cual contiene cada uno de los puntos solicitados. No obstante, también exite un código particular para cada uno de los puntos, en el caso de que sea necesaro utilizarlos.
Por lo tanto, la carpeta de "scripts" contiene: 
- Taller 1 BDML
- PUNTO 2. TALLER 1
- PUNTO 3. TALLER 1
- PUNTO 4. TALLER 1
- PUNTO 5. TALLER 1

# Graphs

Todas las gráficas se pueden encontrar en la carpeta "views". Así mismo, los códigos con los que fueron generados se encuentran en el script "Taller-1-BDML" de la carpeta "scripts".

# Data dictionary
    ing_hr: variable continua que representa el ingreso laboral nominal por hora, incluyendo todas las ocupaciones, propinas y comisiones.
    ing_m: variable continua que representa el ingreso laboral nominal mensual, incluyendo todas las ocupaciones, propinas y comisiones.
    lnwage: logaritmo natural del salario 
    maxEducLevel: variable categórica sobre el máximo nivel de educación alcanzado. 
        1: Ninguno.
        2: Preescolar.
        3: Primaria incompleta.
        4: Primaria completa.
        5: Secundaria incompleta.
        6: Secundaria completa.
        7: Terciaria
        9: N/A.
    age: variable continua que representa la edad.
    totalHoursWorked: variable continua que representa el número total de horas trabajadas la semana anterior.
    formal: variable binaria que toma el valor de 1 si el trabajador si cotiza a seguridad social. Es una proxy de formalidad.
    estrato1: variable categórica para el estrato socieconómico. Toma valores entre 1 a 6.
    fulltime: variable binaria que toma el valor de 1 si el trabajador trabajó más de 40 horas la semana pasada. Es una proxy del tipo de contrato.
    relab: variable categórica para el tipo de ocupación.
        1: Obrero o empleado de empresa particular.
        2: Obrero o empleado del gobierno.
        3: Empleado doméstico.
        4: Trabajador por cuenta propia.
        5: Patrón o empleador.
        6: Trabajador familiar sin remuneración.
        7: Trabajador sin remuneracin en empresas o negocios de otros hogares.
        8: Jornalero o peón.
        9: Otro.
    sizeFirm: variable categórica para el tamaño de la empresa.
        1: Independiente.
        2: 2-5 empleados.
        3: 6-10 empleados.
        4: 11-50 empleados.
        5: >50 empleados.

