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
                     "skimr", "caret", "rvest", "stargazer", "rlist", "Hmisc", 
                     "corrplot", "dplyr")

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
  mutate(hoursworked=hoursWorkUsual+hoursWorkActualSecondJob) %>%  # Total de horas trabajadas en el trabajo principal y secundario
  mutate(fulltime=ifelse(hoursworked>=40, 1, 0))                   # El tipo de contrato es tiempo completo si trabaja más de 40 horas a la semana

### Estadística descriptiva: análisis preliminar 
base2 <- base %>%
  select(ing_hr, maxEducLevel, age, oficio,totalHoursWorked, formal, sex, estrato1, fulltime, relab, sizeFirm) %>% # Seleccionar variables de interés
  drop_na()

any(is.na(base2)) # No hay datos vacíos

stargazer(base2, header=FALSE, type='text',title="Variable")

### Mapa de correlaciones 
corrm <- base2
colnames(corrm) <- c("Ingreso por hora", "Máximo nivel de educación", "Edad", "Oficio", "Total de horas trabajadas", "Formal",
                    "Sexo", "Estrato", "Fulltime", "Tipo de ocupación", "Tamaño de la firma")
res2 <- rcorr(as.matrix(corrm)) # Coeficientes de correlación

corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank", tl.col="black") # Las correlaciones no signitificativas se eliminan

### Análisis por variable

# maxEducLevel - max. education level attained
summary(base2$maxEducLevel)
box_plot <- ggplot(data=base2 , mapping = aes(as.factor(maxEducLevel) , ing_hr)) + 
  geom_boxplot() 
box_plot

box_plot <- box_plot +
  geom_point(aes(colour=as.factor(formal))) +
  scale_color_manual(values = c("0"="indianred3" , "1"="steelblue") , label = c("0"="Informal" , "1"="Formal") , name = "Empleo")
box_plot ##SI

# age - edad
summary(base2$age)
ggplot(data = base2, mapping = aes(x = age , y = ing_hr)) +
  geom_point(col = "green" , size = 0.8)
## NO

ggplot(data = base2 , 
       mapping = aes(x = age , y = ing_hr , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point() ## SI

ggplot(data = base1 , mapping = aes(x = totalHoursWorked , y = ing_hr)) +
  geom_point(col = "indianred3" , size = 0.8)

## SÍ

# totalHoursWorked - cuantas horas en total trabajo
summary(base2$totalHoursWorked)

ggplot(data = base2 , mapping = aes(x = age , y = totalHoursWorked )) +
  geom_point(col = "steelblue" , size = 0.8)
## NO
ggplot(data = base2 , 
       mapping = aes(x = totalHoursWorked , y = ing_hr , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()
## NO
# oficio - occupation
summary(base1$oficio)

ggplot(data = base2 , 
       mapping = aes(x = oficio , y = ing_hr , group=as.factor(sex) , color=as.factor(sex))) +
  geom_point()
## SI

box_plot2 <- ggplot(data=base2 , mapping = aes(as.factor(occupation) , ing_hr)) + 
  geom_boxplot() 
box_plot2
## Cambiar
box_plot2 <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
box_plot2
### NO

# formal - =1 if formal (social security); =0 otherwise
summary(base2$formal)

ingreso_tipo_empleo <- ggplot(data=base2) + 
  geom_histogram(mapping = aes(x=ing_hr , group=as.factor(formal) , fill=as.factor(formal)))
ingreso_tipo_empleo
##NO
# Sex - =1 male, =0 female
summary(base2$Sex)
ingreso_sexo <- ggplot(data=base2) + 
  geom_histogram(mapping = aes(x=ing_hr , group=as.factor(sex) , fill=as.factor(sex)))
ingreso_sexo
ingreso_sexo + scale_fill_manual(values = c("0"="indianred3" , "1"="steelblue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
## SI

## Estrato socioeconómico: estrato1 - Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y rest 
base2 %>%
  group_by(estrato1) %>%
  summarise(n = n())

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

# ------------------------------------------------------------------------------------ #
# 3. Age-wage profile
# ------------------------------------------------------------------------------------ #

# 3a. Tabla de regresión ------------------------------------------------------------- #

#Primero creamos la variable edad al cuadrado y el logaritmo del salario
base$age2 <- base$age^2
base$lnwage <- log(base$ing_hr)

#Procedemos a hacer la regresión
regw_age2<- lm(lnwage~ age+ age2, data = base)
stargazer(regw_age2, type = "text")
stargazer(regw_age2, type = "latex")

# 3b. Interpretación de los coeficientes --------------------------------------------- #

# 3c. Discusión of the model's in sample fit ----------------------------------------- #

# 3d. Gráfico de la estimación del perfil edad-ganancias ----------------------------- #

# Perfil edad-ganancias
##Veamos el gráfico de la regresión en cuestión
regw_age2<- lm(lnwage~ age+ age2, data = base)
stargazer(regw_age2, type = "text")

ggplot(base, 
       aes(x = age, 
           y = lnwage)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

# Bootstrap para construir los intervalos de confianza
lmw_summary <- summary(regw_age2)$coefficients

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
  labs(x = 'Coeffiecient estimate') +
  theme_bw()

# ------------------------------------------------------------------------------------ #
# 4. The gender earnings GAP
# ------------------------------------------------------------------------------------ #

#### Variable dependiente (Y)
#       y_ingLab_m - Labor income salaried - nominal monthly - all occ. (includes tips and commission
#       ing_hr - labor income salaried - nominal hourly - all occ. (includes tips and commissions)

#### Variables explicativas (X)
#       maxEducLevel - max. education level attained
#       age - edad
#       age2 - edad^2
#       formal - =1 if formal (social security); =0 otherwise
#       Sex - =1 male, =0 female
#       full-time - Trabaja más de 40 horas a la semana, construida a partir de hoursWorkUsual (usual weekly hours worked - principal occ.)
#       relab -	type of occupation

# Base de datos punto 4
base4 <- base %>%
  select(ing_hr, y_ingLab_m, maxEducLevel, age, formal, sex, fulltime, relab) %>% # Seleccionar variables de interés
  mutate(female=ifelse(sex==1, 0, 1), age2=age^2) %>%
  drop_na()

# a. Begin by estimating and discussing the unconditional wage gap:
set.seed(1111)

reg4a_m <- lm(y_ingLab_m ~ female, data=base4)
reg4a_hr <- lm(ing_hr ~ female, data=base4)

# b. Equal Pay for Equal Work?



# COMPARACIÓN REGRESIONES
stargazer()

# c. Predicted age-wage profile


# ------------------------------------------------------------------------------------ #
# 5. Predicting earnings
# ------------------------------------------------------------------------------------ #
set.seed(10101)

## a. 

#use 70% of the dataset as a training set and 30% as a test set. La base1 tiene variables que nos interesan
sample <- sample(c(TRUE, FALSE), nrow(base1), replace=TRUE, prob=c(0.7,0.3))

train  <- base1[sample, ]
test   <- base1[!sample, ]

##b. 

## Primer modelo ##
model1<-lm(ing_hr~1,data=train)
stargazer(model1, type="text")

test$model1<-predict(model1,newdata = test)

with(test,mean((ing_hr-model1)^2))

## Segundo modelo ##

model2<-lm(ing_hr~totalHoursWorked,data=train)
test$model2<-predict(model2,newdata = test)

with(test,mean((ing_hr-model2)^2))

## Tercer modelo ##
###Desglosamos la variable categórica maxEducLevel, que contiene 9 categorías de niveles educativos.
base <- base %>% 
  mutate(maxprescolar=ifelse(maxEducLevel == 2, 1, 0))

base <- base %>% 
  mutate(maxprimariaincompleta=ifelse(maxEducLevel==3, 1, 0))

base <- base %>% 
  mutate(maxprimariacompleta=ifelse(maxEducLevel==4, 1, 0))

base <- base %>% 
  mutate(maxsecundariaincompleta=ifelse(maxEducLevel==5, 1, 0))
         
base <- base %>% 
  mutate(maxsecundariacompleta=ifelse(maxEducLevel==6, 1, 0))

base <- base %>% 
  mutate(maxterciaria=ifelse(maxEducLevel==7, 1, 0))

base <- base %>% 
  mutate(maxeducnoaplica=ifelse(maxEducLevel==9, 1, 0))

table(base$maxEducLevel)
##Ninguna observación en la muestra reportó cursar prescolar como máximo nivel educativo ni respondió "N/A" para esta pregunta, por lo que no incluimos las variables maxprescolar ni maxeducnoaplica

model3<-lm(ing_hr~totalHoursWorked+age+sex+maxprimariaincompleta+maxprimariacompleta+maxsecundariaincompleta+maxsecundariacompleta+maxterciaria+formal,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((ing_hr-model3)^2))

## Cuarto modelo ##

model4<-lm(ing_hr~totalHoursWorked+maxEducLevel+age+age^2+oficio+
             formal+sex+estrato1+fulltime+p6240+relab+sizeFirm,data=train)
test$model4<-predict(model4,newdata = test)

with(test,mean((ing_hr-model4)^2))

stargazer(model4, type = "text")


## Quinto modelo ##

model5<-lm(ing_hr~poly(age,2,raw=TRUE):poly(maxEducLevel,4,raw=TRUE):sex:formal:oficio:relab:sizeFirm+p6240+fulltime+maxEducLevel+
             estrato1+poly(sizeFirm,5,raw=TRUE):poly(totalHoursWorked,8,raw=TRUE),data=train)
test$model5<-predict(model5,newdata = test)

with(test,mean((ing_hr-model5)^2))

##c.
## comparar los MSE 
mse1<-with(test,round(mean((ing_hr-model1)^2),2))
mse2<-with(test,round(mean((ing_hr-model2)^2),2))
mse3<-with(test,round(mean((ing_hr-model3)^2),2))
mse4<-with(test,round(mean((ing_hr-model4)^2),2))
mse5<-with(test,round(mean((ing_hr-model5)^2),2))

tabla<-data.frame(mse1,mse2,mse3,mse4,mse5)
tabla

