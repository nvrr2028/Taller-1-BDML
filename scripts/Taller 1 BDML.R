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
                     "corrplot", "dplyr", "boot")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# An income predicting model could potentially assist in  agging cases of fraud that 
# could lead to thereduction of the gap.
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

stargazer(base2, header=FALSE, type='latex',title="Variable")

### Mapa de correlaciones 
corrm <- base2
colnames(corrm) <- c("Ingreso por hora", "Máximo nivel de educación", "Edad", "Total de horas trabajadas", "Formal",
                    "Sexo", "Estrato", "Tipo de contrato (fulltime)", "Tipo de ocupación", "Tamaño de la firma")
res2 <- rcorr(as.matrix(corrm)) # Coeficientes de correlación

corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank", tl.col="black") # Las correlaciones no signitificativas se eliminan

#######ERRRORRRR

## Trasnformación de variables categoricas a dummy ##

base2  <- base2 %>%
  mutate(age2=age^2 , 
         lnwage=log(ing_hr))

#### Cambio de MaxEducLevel ####
base2 %>%
  group_by(maxEducLevel) %>%
  summarise(n = n())


base2 <- base2 %>% 
  mutate(maxprescolar=ifelse(maxEducLevel == 2, 1, 0))

base2 <- base2 %>% 
  mutate(maxprimariaincompleta=ifelse(maxEducLevel==3, 1, 0))

base2 <- base2 %>% 
  mutate(maxprimariacompleta=ifelse(maxEducLevel==4, 1, 0))

base2 <- base2 %>% 
  mutate(maxsecundariaincompleta=ifelse(maxEducLevel==5, 1, 0))

base2 <- base2 %>% 
  mutate(maxsecundariacompleta=ifelse(maxEducLevel==6, 1, 0))

base2 <- base2 %>% 
  mutate(maxterciaria=ifelse(maxEducLevel==7, 1, 0))

## +maxprimariaincompleta+maxprimariacompleta+maxsecundariaincompleta+maxsecundariacompleta+maxterciaria
#### Cambio de sizefirm #### Base independiente
base2 %>%
  group_by(sizeFirm) %>%
  summarise(n = n())

base2 <- base2 %>% 
  mutate(trabajadores2a5=ifelse(sizeFirm==2, 1, 0))

base2 <- base2 %>% 
  mutate(trabajadores6a10=ifelse(sizeFirm==3, 1, 0))

base2 <- base2 %>% 
  mutate(trabajadores11a50=ifelse(sizeFirm==4, 1, 0))

base2 <- base2 %>% 
  mutate(mas50trabajadores=ifelse(sizeFirm==5, 1, 0))

## trabajadores2a5+trabajadores6a10+trabajadores11a50+mas50trabajadores

#### Cambio de relab ####
base2 %>%
  group_by(relab) %>%
  summarise(n = n())

base2 <- base2 %>%
  mutate(empleadopublico=ifelse(relab==2,1,0))
base2 <- base2 %>% 
  mutate(empleadodomestico=ifelse(relab==3,1,0))
base2 <- base2 %>%
  mutate(cuentapropia=ifelse(relab==4,1,0))
base2 <- base2 %>%
  mutate(empleador=ifelse(relab==5,1,0))
base2 <- base2 %>% 
  mutate(trabajadorfamiliarsinremuneracion=ifelse(relab==6,1,0))
base2 <- base2 %>% 
  mutate(trabajadorempresasinremuneracion=ifelse(relab==7,1,0))
base2 <- base2 %>% 
  mutate(jornalero=ifelse(relab==8,1,0))

## empleadopublico+empleadodomestico+jornalero

#### cambio de estrato1 #### Base estrato1

base2 <- base2 %>% 
  mutate(estrato2=ifelse(estrato1==2, 1, 0))

base2 <- base2 %>% 
  mutate(estrato3=ifelse(estrato1==3, 1, 0))

base2 <- base2 %>% 
  mutate(estrato4=ifelse(estrato1==4, 1, 0))

base2 <- base2 %>% 
  mutate(estrato5=ifelse(estrato1==5, 1, 0))

base2 <- base2 %>% 
  mutate(estrato6=ifelse(estrato1==6, 1, 0))


## estrato2+estrato3+estrato4+estrato5+estrato6

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

ggplot(data = base2 ,
       mapping = aes(x = age , y = ing_hr , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

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
base3  <- base %>%
                    mutate(age2=age^2 , 
                          lnwage=log(ing_hr))

#Procedemos a hacer la regresión
regw_age2<- lm(lnwage~ age+ age2, data = base3)
stargazer(regw_age2, type = "text")
stargazer(regw_age2, type = "latex")

# 3b. Interpretación de los coeficientes --------------------------------------------- #

# 3c. Discusión of the model's in sample fit ----------------------------------------- #

# 3d. Gráfico de la estimación del perfil edad-ganancias ----------------------------- #

# Perfil edad-ganancias
##Veamos el gráfico de la regresión en cuestión
regw_age2<- lm(lnwage~ age+ age2, data = base3)
stargazer(regw_age2, type = "text")

age_earnings<- ggplot(base3, 
       aes(x = age, 
           y = lnwage)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

# Big data y Machine learning


# Bootstrap para construir los intervalos de confianza
mod_peakage <- function(base3,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(lnwage~ age+ age2, data = base3, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta1 = coef[2]
  beta2 = coef[3]
  
  #3. calcular peak age
  peak_age = -(beta1/(2*beta2))
  
  return(peak_age)
}

mod_peakage(base3, 1: nrow(base3)) #comprobando que sale igual :)

#Corremos el Bootstrap
set.seed(9876)
results_peakage <- boot(base3, mod_peakage, R=1000)
results_peakage 

#ahora construyamos los confidence intervals 

#antes necesito extraer los estadísticos a values
peakage<- results_peakage$t0
bias <- colMeans(results_peakage$t)-results_peakage$t0
se <- apply(results_peakage$t,2,sd)

#para agregar en punto de peak age 
#1. creamos un data frame con el summary de nuestra regresión
lmw_summary <- data.frame(summary(regw_age2)$coefficients)

#2. extraemos los betas a escalares para plantear la fórmula
beta0 = lmw_summary[1,1]
beta1 = lmw_summary[2,1]
beta2 = lmw_summary[3,1]

wage_pa = beta0 + beta1*peakage + beta2*(peakage)^2

#3. construimos los valores para el CI
alpha = 0.05 # 95% Confidence Interval
lower = wage_pa - qnorm(alpha/2) * se
upper = wage_pa + qnorm(alpha/2) * se

#4. Agregamos el CI al gráfico
age_earnings + 
  geom_point(aes(x=peakage, y=wage_pa)) +
  geom_segment(aes(y=upper, x= lower, yend= wage_pa , xend= wage_pa),
               arrow= arrow(angle=90, ends= 'both', 
                            length = unit(0.1, 'cm'))) +
  labs(x= "Edad", y= "Ingresos", title= "Trayectoria de los ingresos a lo largo de la Edad")


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
  mutate(female=ifelse(sex==1, 0, 1), age2=age^2, ing_hr=log(ing_hr), ing_m=log(y_ingLab_m)) %>%
  drop_na()

# a. Begin by estimating and discussing the unconditional wage gap:
set.seed(1111)

reg4a_m <- lm(ing_m ~ female, data=base4)
reg4a_hr <- lm(ing_hr ~ female, data=base4)

# b. Equal Pay for Equal Work?
head(base4)
base4$maxEducLevel <- as.factor(base4$maxEducLevel) # Educación como dummy 
base4$relab <- as.factor(base4$relab) # Tipo de ocupación como dummy como dummy 
reg4c_m <-lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)
reg4c_hr <- lm(ing_hr  ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)

stargazer(reg4a_hr, reg4c_hr, type="text")

# FWL --------------
p_load("tidyverse","rio","stargazer")

#### Ingreso mensual
#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidF=lm(female~ maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidF=lm(ing_m ~ maxEducLevel + age + age2+ formal + fulltime +relab , data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_m_fwl<-lm(wageResidF~femaleResidF, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4c_m, reg4_m_fwl, type="text")

# Bootstrap para coeficientes 
eta.fn_m1 <-function(data,index){
  coefm1 <- coef(lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data = data, subset = index)) # Regresión original
  return(coefm1[2])
}

reg4m1 <- boot(base4, eta.fn_m1, R = 1000) # boot(datos, estadístico deseado, repeticiones)

eta.fn_m2 <-function(data,index){
  coefm2 <- coef(lm(wageResidF~femaleResidF, data = data, subset = index)) # Regresión FWL
  return(coefm2[2])
}

reg4m2 <- boot(base4, eta.fn_m2, R = 1000) # boot(datos, estadístico deseado, repeticiones)

# Corrección de errores estándar 
se_m1 <- sqrt(diag(vcov(reg4_m_fwl))*(9889/9877))[2]
se_m2 <- sqrt(diag(vcov(reg4c_m)))[2]

# Ingreso por horas
#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidFhr=lm(female~ maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidFhr=lm(ing_hr ~ maxEducLevel + age + age2+ formal + fulltime +relab , data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_hr_fwl<-lm(wageResidFhr~femaleResidFhr, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4c_hr, reg4_hr_fwl, type="text")

# Bootstrap para coeficientes 
eta.fn_hr1 <-function(data,index){
  coefhr1 <- coef(lm(ing_hr ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data = data, subset = index)) # Regresión original
  return(coefhr1[2])
}

reg4hr1 <- boot(base4, eta.fn_hr1, R = 1000) # boot(datos, estadístico deseado, repeticiones)

eta.fn_hr2 <-function(data,index){
  coefhr2 <- coef(lm(wageResidFhr~femaleResidFhr, data = data, subset = index)) # Regresión FWL
  return(coefhr2[2])
}

reg4hr2 <- boot(base4, eta.fn_hr2, R = 1000) # boot(datos, estadístico deseado, repeticiones)

# Corrección de errores estándar 
se_m1 <- sqrt(diag(vcov(reg4_hr_fwl))*(9889/9877))[2]
se_m2 <- sqrt(diag(vcov(reg4c_hr)))[2]

stargazer(reg4hr1, reg4hr2)

# c. Predicted age-wage profile





# ------------------------------------------------------------------------------------ #
# 5. Predicting earnings
# ------------------------------------------------------------------------------------ #
base2 <- base2 %>%
  mutate(female=ifelse(sex==1, 0, 1))
head(base2)

##a.
set.seed(10101)
#use 70% of the dataset as a training set and 30% as a test set.

sample <- sample(c(TRUE, FALSE), nrow(base2), replace=TRUE, prob=c(0.7,0.3))

train  <- base2[sample, ]
test   <- base2[!sample, ]

##b. 

## Modelo ln(w) en función de la edad y edad cuadrática ##
regw_age2train <- lm(lnwage ~ age+ age2, data=train)
stargazer(regw_age2train, type="text")

test$regw_age2train<-predict(regw_age2train,newdata = test)

with(test,mean((lnwage-regw_age2train)^2))

## Modelo ln(w) en función del género ##
reg4a_hrtrain <- lm(lnwage ~ female, data=train)
stargazer(reg4a_hrtrain, type="text")

test$reg4a_hrtrain<-predict(reg4a_hrtrain,newdata = test)

with(test,mean((lnwage-reg4a_hrtrain)^2))

## Primer modelo ##
model1<-lm(lnwage~1,data=train)
stargazer(model1, type="text")

test$model1<-predict(model1,newdata = test)

with(test,mean((lnwage-model1)^2))

## Segundo modelo ##

model2<-lm(lnwage~totalHoursWorked,data=train)
test$model2<-predict(model2,newdata = test)

with(test,mean((lnwage-model2)^2))

## Tercer modelo ##
###Desglosamos la variable categórica maxEducLevel, que contiene 9 categorías de niveles educativos.

##Ninguna observación en la muestra reportó cursar prescolar como máximo nivel educativo ni respondió "N/A" para esta pregunta, por lo que no incluimos las variables maxprescolar ni maxeducnoaplica

model3<-lm(lnwage~totalHoursWorked+age+sex+maxprimariaincompleta+maxprimariacompleta+maxsecundariaincompleta+maxsecundariacompleta+maxterciaria+formal,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((lnwage-model3)^2))
## Cuarto modelo ##

model4<-lm(lnwage~totalHoursWorked+age+age^2+maxprimariaincompleta+maxprimariacompleta+maxsecundariaincompleta+
             maxsecundariacompleta+maxterciaria+formal+sex+
             estrato2+estrato3+estrato4+estrato5+estrato6+fulltime+
             empleadopublico+empleadodomestico+jornalero+
             trabajadores2a5+trabajadores6a10+trabajadores11a50+mas50trabajadores,data=train)

test$model4<-predict(model4,newdata = test)

with(test,mean((lnwage-model4)^2))

stargazer(model4, type = "text")


## Quinto modelo ##
model5<-lm(lnwage~poly(age,2,raw=TRUE):sex:formal:maxprimariacompleta+poly(age,2,raw=TRUE):sex:formal:maxterciaria+poly(age,2,raw=TRUE):sex:formal:maxprimariaincompleta+
             poly(age,2,raw=TRUE):sex:formal:estrato2+poly(age,2,raw=TRUE):sex:formal:estrato3+poly(age,2,raw=TRUE):sex:formal:estrato4+
           poly(age,2,raw=TRUE):sex:formal:estrato5+poly(age,2,raw=TRUE):sex:formal:estrato6+poly(totalHoursWorked,5,raw=TRUE):sex:formal:maxterciaria+
             poly(totalHoursWorked,5,raw=TRUE):sex:formal:maxprimariacompleta+poly(totalHoursWorked,5,raw=TRUE):sex:formal:estrato2+ 
           poly(totalHoursWorked,5,raw=TRUE):sex:formal:estrato6+maxprimariaincompleta+maxprimariacompleta+maxsecundariaincompleta+
             maxsecundariacompleta+maxterciaria+formal+sex+estrato2+estrato3+estrato4+estrato5+estrato6+fulltime+
             empleadopublico+empleadodomestico+jornalero+trabajadores2a5+trabajadores6a10+trabajadores11a50+mas50trabajadores, data=train)
            
test$model5<-predict(model5,newdata = test)

with(test,mean((lnwage-model5)^2))

## comparar los MSE 
msew_age2<-with(test,round(mean((lnwage-reg4a_hrtrain )^2),2))
msew_fem<-with(test,round(mean((lnwage-reg4a_hrtrain )^2),2))
mse1<-with(test,round(mean((lnwage-model1)^2),2))
mse2<-with(test,round(mean((lnwage-model2)^2),2))
mse3<-with(test,round(mean((lnwage-model3)^2),2))
mse4<-with(test,round(mean((lnwage-model4)^2),2))
mse5<-with(test,round(mean((lnwage-model5)^2),2))

tabla<-data.frame(msew_age2,msew_fem,mse1,mse2,mse3,mse4,mse5)
tabla

##d.         

