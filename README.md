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
- lnwage: logaritmo natural del salario 
- age: la edad de la persona
- totalHoursWorked: 
- maxEducLevel3                          =1 si el nivel máximo de educación es primaria incompleta
- maxEducLevel4                          =1 si el nivel máximo de educación es primaria completa
- maxEducLevel5                          =1 si el nivel máximo de educación es secundaria incompleta
- maxEducLevel6                          =1 si el nivel máximo de educación es secundaria completa
- maxEducLevel7                          =1 si el nivel máximo de educación es terciaria
- estrato12                              =1 si la persona es de estrato 2
- estrato13                              =1 si la persona es de estrato 3
- estrato14                              =1 si la persona es de estrato 4
- estrato15                              =1 si la persona es de estrato 5
- estrato16                              =1 si la persona es de estrato 6
- sizeFirm2                              =1 si la empresa en que trabaja la persona tiene de 2 a 5 trabajadores
- sizeFirm3                              =1 si la empresa en que trabaja la persona tiene de 6 a 10 trabajadores
- sizeFirm4                              =1 si la empresa en que trabaja la persona tiene de 11 a 50 trabajadores
- sizeFirm5                              =1 si la empresa en que trabaja la persona tiene más de 50 trabajadores





This is the template repository for the problem sets.

The repo should contain at least four folders:

- `document`: contains your final document in `pdf` format. Ideally, the document should pull figures and tables directly from the `views` folder. I've included a latex template I created for the Thesis Seminar. 
- `scripts`: contains all your scripts
- `stores`: contains all the data sets used. If files are "too big" for GitHub, include a document describing where people can access the data.
- `views`: contains all figures and tables



## Some general reminders: 

- It is essential how you write up the document. Be sure to be organized and consistent in explaining your equations and findings. Make sure that there are no compilation errors.
- Write understandable code, separating and commenting on each section. Coding, like in writing, style is critical for readability. If the code is well written, it should be self-contained. There is no need to write everything you did. I encourage you to follow the [tidyverse style guide](https://style.tidyverse.org/)

