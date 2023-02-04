#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: GEIH DANE                                           #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

list.of.packages = c("readr", "readxl", "lubridate", "tidyverse", "pacman", "rio", 
                     "skimr", "caret", "rvest", "stargazer", "rlist")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# An income prediction model can help identify vulnerable individuals and families
# that may need further assistance.

# ------------------------------------------------------------------------------------ #
# 2. Data
# ------------------------------------------------------------------------------------ #

# 2a. Descripción de la base de datos (GEIH - DANE) ---------------------------------- #

# La Gran Encuesta Integrada de Hogares (GEIH) del DANE, tiene como objetivo suministrar 
# información sobre el mercado laboral colombiano y las condiciones sociodemográficas de 
# la población en el territorio nacional.

# 2b. Cargar los datos --------------------------------------------------------------- #

# Link general: https://ignaciomsarmiento.github.io/GEIH2018 sample/

# Establecer semilla
set.seed(0000) 
# Base provisional para obtener los nombres de las columnas
prov <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true") 
# Vector con los nombres de las columans
names <- colnames(prov)
# Loop para obtener la información de los 10 chuncks.
links <- list()
for (i in 1:10) {
  links[[i]] <- import(paste("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html", sep=""))
}
# Juntar la base de datos por filas.
data <- list.rbind(links)
colnames(data) <- names

# 2c. Limpiar datos ------------------------------------------------------------------ #

#### Variables de interés
# y_ingLab_m_ha - labor income salaried - nominal hourly - all occ. (includes tips and commissions)
# age - edad
# dsi - =1 if unemployed; =0 otherwise

# ¿Qué valores toma la variable objetivo? 
ini <- data %>% 
  group_by(y_ingLab_m_ha) %>%
  summarise(n = n())
# La variable ingreso por hora tiene datos vacíos y nulos.

base <- data %>%
  # 1. Renombrando variable de interés
  rename(ing_hr=y_ingLab_m_ha) %>%                             # y_ingLab_m_ha pasaría a ser ing_hr
  # 2. Filtrando por edad y empleado
  filter(age>=18,                                              # Población mayor a 18
         dsi==0) %>%                                           # Población ocupada
  # 3. Filtrando por datos vacíos y nulos
  mutate_at(c('ing_hr'), ~na_if(., 0)) %>%                     # Reemplazar valores nulos de ing_hr por NA
  filter(!is.na(ing_hr))                                       # Filtrando por vacios en ing_hr

# 2d. Análisis de la base de datos --------------------------------------------------- #

#### Variable dependiente (Y)
#       y_ingLab_m_ha - labor income salaried - nominal hourly - all occ. (includes tips and commissions)
#       totalHoursWorked - total hours worked previous week
#       Ingreso total - Ingreso total

#### Variables explicativas (X)
#       maxEducLevel - max. education level attained
#       age - edad
#       oficio - occupation
#       formal - =1 if formal (social security); =0 otherwise
#       Informal - =1 if informal (social security); =0 otherwiseinformal
#       Sex - =1 male, =0 female
#       estrato1 - Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y rest
#       full-time - Trabaja más de 40 horas a la semana, construida a partir de hoursWorkUsual (usual weekly hours worked - principal occ.)
#       relab -	type of occupation
#       sizeFirm - size of the firm by categories

# Crear variable full-time 
base <- base %>%
  mutate(fulltime=ifelse(hoursWorkUsual>=40, 1, 0)) # El tipo de contrato es tiempo completo si trabaja más de 40 horas a la semana

### Estadística descriptiva: análisis preliminar 
base1 <- base %>%
  select(ing_hr, maxEducLevel, age, oficio, formal, informal, sex, estrato1, fulltime, p6240, relab, sizeFirm) %>% # Seleccionar variables de interés
  drop_na()

any(is.na(base1)) # No hay datos vacios

stargazer(base1, header=FALSE, type='text',title="Variable")

### Mapa de correlaciones 


### Análisis por variable

# maxEducLevel - max. education level attained
summary(data$maxEducLevel)
box_plot <- ggplot(data=data , mapping = aes(as.factor(maxEducLevel) , y_ingLab_m)) + 
  geom_boxplot() 
box_plot

box_plot <- box_plot +
  geom_point(aes(colour=as.factor(formal))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Informal" , "1"="Formal") , name = "Empleo")
box_plot
### Cambiar color ###

# age - edad
summary(data$age)
ggplot(data = data , mapping = aes(x = age , y = y_ingLab_m)) +
  geom_point(col = "green" , size = 0.8)

ggplot(data = data , 
       mapping = aes(x = age , y = y_ingLab_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

ggplot(data = data , mapping = aes(x = totalHoursWorked , y = y_ingLab_m)) +
  geom_point(col = "orange" , size = 0.8)

# totalHoursWorked - cuantas horas en total trabajo
summary(data$totalHoursWorked)

ggplot(data = data , mapping = aes(x = age , y = totalHoursWorked )) +
  geom_point(col = "purple" , size = 0.8)

ggplot(data = data , 
       mapping = aes(x = totalHoursWorked , y = y_ingLab_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

# oficio - occupation
summary(data$occupation)

box_plot2 <- ggplot(data=data , mapping = aes(as.factor(occupation) , y_ingLab_m)) + 
  geom_boxplot() 
box_plot2

box_plot2 <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
box_plot2
### Cambiar color ###

# formal - =1 if formal (social security); =0 otherwise
summary(data$formal)

ingreso_tipo_empleo <- ggplot(data=data) + 
  geom_histogram(mapping = aes(x=y_ingLab_m , group=as.factor(formal) , fill=as.factor(formal)))
ingreso_tipo_empleo

# Sex - =1 male, =0 female
summary(data$Sex)
ingreso_sexo <- ggplot(data=data) + 
  geom_histogram(mapping = aes(x=y_ingLab_m , group=as.factor(sex) , fill=as.factor(sex)))
ingreso_sexo
ingreso_sexo + scale_fill_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

## Estrato socioeconómico: estrato1 - Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y rest 
base1 %>%
  group_by(estrato1) %>%
  summarise(n = n())

# Gráfico
ggplot(base1) + 
  geom_boxplot(mapping = aes(as.factor(estrato1) , ing_hr, fill=as.factor(estrato1))) + 
  labs(x = "Estrato", y = "Ingreso por hora (pesos)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

## Tipo de contrato (tiempo completo o no): fulltime - Trabaja más de 40 horas a la semana
base1 %>%
  group_by(fulltime) %>%
  summarise(n = n())

# Gráfico
ggplot(base1) + 
  geom_bar(mapping = aes(as.factor(fulltime), ing_hr, group=as.factor(formal), fill=as.factor(formal)), 
               position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Tipo de contrato (tiempo completo)", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels=c("0"='No', "1"='Si')) +
  scale_fill_manual(values = c("0"="#ffc425" , "1"="#00aedb") , label = c("0"="Informal" , "1"="Formal")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

## Tipo de ocupación: relab -	type of occupation
base1 %>%
  group_by(relab) %>%
  summarise(n = n())

# Gráfico
labels = c('Obrero o empleado de empresa particular', 'Obrero o empleado del gobierno', 'Empleado doméstico', 'Jornalero o peon')
ggplot(base1) + 
  geom_bar(mapping = aes(as.factor(relab) , ing_hr, fill=as.factor(relab)), 
           position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Tipo de ocupación", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels = function(x) str_wrap(labels, width = 10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) 

## Tamaño de la empresa: sizeFirm - size of the firm by categories

# ¿Qué categorías tiene sizefirm?
base1 %>%
  group_by(sizeFirm) %>%
  summarise(n = n()) 

# ¿Cuál es el ingreso por hora mediano de acuerdo con el tamaño de la empresa?
base1 %>% 
  group_by(sizeFirm) %>% 
  summarise('median_ing' = round(median(ing_hr), digits = 0)) 

# Gráfico
labels1 = c('Independiente', '2-5 empleados', '6-10 empleados', '11-50 empleados', '>50 empleados')
ggplot(base1) + 
  geom_bar(mapping = aes(as.factor(sizeFirm) , ing_hr, fill=as.factor(sizeFirm)), 
           position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Tamaño de la empresa", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels = function(x) str_wrap(labels1, width = 6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) 

# ------------------------------------------------------------------------------------ #
# 3. Age-wage profile
# ------------------------------------------------------------------------------------ #

# 3a. Tabla de regresión ------------------------------------------------------------- #

#Primero creamos la variable edad al cuadrado
base1$age2 <- base1$age^2
#base1$logw <-base1$log(ing_hr)

#Procedemos a hacer la regresión
regw_age<- lm(log(ing_hr)~ age+ age2, data = base1)
stargazer(regw_age, type = "text")

# 3b. Interpretación de los coeficientes --------------------------------------------- #

# 3c. Discusión of the model's in sample fit ----------------------------------------- #
ggplot(data=base1, aes(x=age, y=log(ing_hr)))+ 
    geom_point() +
  stat_smooth(formula = 'y ~ x', method = lm, se = FALSE, 
              size = 1, color="blue") +  
  #stat_smooth(formula = 'y ~ x+ x^2', method = lm, se = FALSE,                              #creempos que esta sobra
              #size = 1, color="red")+
  theme_bw() +
  labs(x = "Edad",  
       y = "Ingreso",
       title = "Model Sample fit") 
  

# 3d. Gráfico de la estimación del perfil edad-ganancias ----------------------------- #
lmw_summary <- summary(regw_age)$coefficients

coefswage = data.frame(
  Features = rownames(lmw_summary),
  Estimate = lmw_summary[,'Estimate'],
  std_error = lmw_summary[,'Std. Error']
)

alpha = 0.05 # 95% Confidence Interval
coefswage$lower = coefswage$Estimate - qnorm(alpha/2) * coefswage$std_error
coefswage$upper = coefswage$Estimate + qnorm(alpha/2) * coefswage$std_error
coefswage = coefswage[!(coefswage$Features == '(Intercept)'),]

ggplot(coefswage) +
  geom_vline(xintercept = 0, linetype = 4)+
  geom_point(aes(x = Estimate, y = Features)) + #point estimate
  geom_segment(aes(y = Features, yend = Features, x = lower, xend = upper),
     arrow = arrow(angle = 90, ends = 'both', 
    length = unit(0.1, 'cm'))) + #segment representing the CI
  labs(x = 'Coeffienient estimate') +
  theme_bw()
# ------------------------------------------------------------------------------------ #
# 4. The gender earnings GAP
# ------------------------------------------------------------------------------------ #




# ------------------------------------------------------------------------------------ #
# 5. Predicting earnings
# ------------------------------------------------------------------------------------ #
