#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: GEIH DANE                                           #
#**************************************************************************************#


# Predicting income

# Abstract
La brecha fiscal es un de los principales problemas del mundo, esta, es la diferencia entre la cantidad de impuestos que el gobierno puede recaudar contra la cantidad de impuestos que realmente recauda. En Colombia, según Bonet (2016), la brecha fiscal en el 2014 fue de aproximadamente 46 billones de pesos, lo que demuestra un gran desperdicio de eficiencia tributaria. Es importante reducir esta brecha debido a que se debe mantener un sistema tributario justo, eficiente y efectivo, para garantizar el mayor recaudo posible para el estado y poder brindar servicios esenciales a la población. Para esto, se realizará un estudio de predicción de ingresos, el cual podría potencialmente ayudar a acumular casos de fraude que lograría la reducción de esta brecha. En términos generales el estudio concluye que en Colombia hay brechas salariales por discriminación por género, y además, el modelo propuesto puede ser un buen predictor del salario, ya que es efectivo prediciendo muestra dentro del grueso de la población pero no tanto en observaciones atípicas. 

# Data files
Todos los datos se encuentran fueron scrappeados del repositorio que se encuentra en la página "https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html". De allí, se tomaron 10 piezas de datos que se unieron en una sola base, cuyo código se puede encontrar en el script "Taller-1-BDML" de la carpeta "scripts".


# Code files

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

This is the template repository for the problem sets.

The repo should contain at least four folders:

- `document`: contains your final document in `pdf` format. Ideally, the document should pull figures and tables directly from the `views` folder. I've included a latex template I created for the Thesis Seminar. 
- `scripts`: contains all your scripts
- `stores`: contains all the data sets used. If files are "too big" for GitHub, include a document describing where people can access the data.
- `views`: contains all figures and tables



## Some general reminders: 

- It is essential how you write up the document. Be sure to be organized and consistent in explaining your equations and findings. Make sure that there are no compilation errors.
- Write understandable code, separating and commenting on each section. Coding, like in writing, style is critical for readability. If the code is well written, it should be self-contained. There is no need to write everything you did. I encourage you to follow the [tidyverse style guide](https://style.tidyverse.org/)

