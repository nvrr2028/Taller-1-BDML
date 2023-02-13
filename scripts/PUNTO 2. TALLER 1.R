#PUNTO 2. 
#cosas de otros puntos ------------
# Limpiar el espacio
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

list.of.packages = c("readr", "readxl", "lubridate", "tidyverse", "pacman", "rio", 
                     "skimr", "caret", "rvest", "stargazer", "rlist", "Hmisc", 
                     "corrplot", "dplyr", "boot", "caret","Ecdat","ggplot2", "car")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

set.seed(9876) 
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

#### Variables explicativas (X)
#       maxEducLevel - max. education level attained
#       age - edad
#       formal - =1 if formal (social security); =0 otherwise
#       totalHoursWorked - total hours worked previous week
#       Sex - =1 male, =0 female
#       estrato1 - Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y rest
#       full-time - Trabaja más de 40 horas a la semana, construida a partir de hoursWorkUsual (usual weekly hours worked - principal occ.)
#       relab -	type of occupation
#       sizeFirm - size of the firm by categories

# Crear variable full-time 
base <- base %>%
  mutate(fulltime=ifelse(totalHoursWorked>=40, 1, 0))                   # El tipo de contrato es tiempo completo si trabaja más de 40 horas a la semana

### Estadística descriptiva: análisis preliminar 
base2 <- base %>%
  select(ing_hr, maxEducLevel, age,totalHoursWorked, formal, sex, estrato1, fulltime, relab, sizeFirm) %>% # Seleccionar variables de interés
  drop_na()
any(is.na(base2)) # No hay datos vacíos

stargazer(base2, header=FALSE, type='text',title="Variable")

### Mapa de correlaciones 
corrm <- base2
colnames(corrm) <- c("Ingreso por hora", "Máximo nivel de educación", "Edad", "Total de horas trabajadas", "Formal",
                     "Sexo", "Estrato", "Tipo de contrato (fulltime)", "Tipo de ocupación", "Tamaño de la firma")
res2 <- rcorr(as.matrix(corrm)) # Coeficientes de correlación

corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$p, sig.level = 0.05, insig = "blank", tl.col="black") # Las correlaciones no signitificativas se eliminan

## Transformación de variables categoricas a dummy ##

base2  <- base2 %>%
  mutate(age2=age^2 , 
         lnwage=log(ing_hr))

#### Cambio de MaxEducLevel ####
base2 %>%
  group_by(maxEducLevel) %>%
  summarise(n = n())

base2$maxEducLevel <- as.factor(base2$maxEducLevel)
base2$relab <- as.factor(base2$relab) 
base2$estrato1<- as.factor(base2$estrato1)
base2$sizeFirm<- as.factor(base2$sizeFirm)


### Análisis por variable
# maxEducLevel - max. education level attained
summary(base2$maxEducLevel)
box_plot <- ggplot(data=base2 , mapping = aes(as.factor(maxEducLevel) , ing_hr)) + 
  geom_boxplot() 
box_plot

box_plot <- box_plot +
  geom_point(aes(colour=as.factor(formal))) +
  scale_color_manual(values = c("0"="indianred3" , "1"="steelblue") , label = c("0"="Informal" , "1"="Formal") , name = "Empleo") +
  labs(x = "Nivel de educación", y = "Ingreso por hora (pesos)")
box_plot

# age - edad y Horas
summary(base2$age)
ggplot(data = base2, mapping = aes(x = age , y = ing_hr)) +
  geom_point(col = "green3" , size = 0.8) +
  labs(x = "Edad del encuestado", y = "Ingreso por hora (pesos)") 

ggplot(data = base2 , mapping = aes(x = totalHoursWorked , y = ing_hr)) +
  geom_point(col = "indianred3" , size = 0.8) + 
  labs(x = "Total de horas trabajadas por semana", y = "Ingreso por hora (pesos)")

# Sex - =1 male, =0 female # Crea el gráfico de barras con la media de ingresos por género
labels1 = c('Mujer', 'Hombre')
ggplot(base2) + 
  geom_bar(mapping = aes(as.factor(sex) , ing_hr, fill=as.factor(sex)), 
           position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Género", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels = function(x) str_wrap(labels1, width = 6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

## totalHoursWorked: total hours worked previous week
# Gráfico
ggplot(base2) + 
  geom_point(mapping = aes(totalHoursWorked , ing_hr, color=totalHoursWorked)) + 
  labs(x = "Horas totales trabajadas", y = "Ingreso por hora (pesos)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

## Estrato socioeconómico: estrato1 - Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y rest 
base2 %>%
  group_by(estrato1) %>%
  summarise(n = n())

base2 %>% 
  group_by(estrato1) %>% 
  summarise('median_ing' = round(median(ing_hr), digits = 0)) 

# Gráfico
ggplot(base2) + 
  geom_boxplot(mapping = aes(as.factor(estrato1) , ing_hr, fill=as.factor(estrato1))) + 
  labs(x = "Estrato", y = "Ingreso por hora (pesos)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

## Tipo de contrato (tiempo completo o no): fulltime - Trabaja más de 40 horas a la semana
base2 %>%
  group_by(fulltime) %>%
  summarise(n = n())

# Gráfico
ggplot(base2) + 
  geom_bar(mapping = aes(as.factor(fulltime), ing_hr, group=as.factor(formal), fill=as.factor(formal)), 
           position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Tipo de contrato (tiempo completo)", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels=c("0"='No', "1"='Si')) +
  scale_fill_manual(values = c("0"="#ffc425" , "1"="#00aedb") , label = c("0"="Informal" , "1"="Formal")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

## Tipo de ocupación: relab -	type of occupation
base2 %>%
  group_by(relab) %>%
  summarise(n = n())

# Gráfico
labels = c('Obrero o empleado de empresa particular', 'Obrero o empleado del gobierno', 'Empleado doméstico', 'Jornalero o peon')
ggplot(base2) + 
  geom_bar(mapping = aes(as.factor(relab) , ing_hr, fill=as.factor(relab)), 
           position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Tipo de ocupación", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels = function(x) str_wrap(labels, width = 10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) 

## Tamaño de la empresa: sizeFirm - size of the firm by categories

# ¿Qué categorías tiene sizefirm?
base2 %>%
  group_by(sizeFirm) %>%
  summarise(n = n()) 

# ¿Cuál es el ingreso por hora mediano de acuerdo con el tamaño de la empresa?
base2 %>% 
  group_by(sizeFirm) %>% 
  summarise('median_ing' = round(median(ing_hr), digits = 0)) 

# Gráfico
labels1 = c('Independiente', '2-5 empleados', '6-10 empleados', '11-50 empleados', '>50 empleados')
ggplot(base2) + 
  geom_bar(mapping = aes(as.factor(sizeFirm) , ing_hr, fill=as.factor(sizeFirm)), 
           position = "dodge", stat = "summary", fun = "median") + 
  labs(x = "Tamaño de la empresa", y = "Ingreso por hora (pesos)") +
  scale_x_discrete(labels = function(x) str_wrap(labels1, width = 6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) 
