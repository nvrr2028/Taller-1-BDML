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
                     "corrplot", "dplyr", "boot", "caret","Ecdat","ggplot2", "car")

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

# Bootstrap para construir los intervalos de confianza------------------

#Función para peakage
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


#Calculemos peak wage
mod_peakwage <- function(base3,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(lnwage~ age+ age2, data = base3, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1]
  beta1 = coef[2]
  beta2 = coef[3]
  
  #3. calcular peak age
  peak_age = -(beta1/(2*beta2))
  
  #4. calcular peak wage
  wage_pa = beta0 + beta1*peakage + beta2*(peakage)^2
  
  return(wage_pa)
}

results_peakwage <- boot(base3, mod_peakwage, R=1000)
results_peakwage

#antes necesito extraer los estadísticos a values
peakwage<- results_peakwage$t0
bias <- colMeans(results_peakwage$t)-results_peakwage$t0
se <- apply(results_peakwage$t,2,sd)

#construimos los valores para el CI
alpha = 0.05 # 95% Confidence Interval
lower = peakwage - qnorm(alpha/2) * se
upper = peakwage + qnorm(alpha/2) * se

#4. Agregamos el CI al gráfico -------------

age_earnings + 
  geom_point(aes(x=peakage, y=peakwage)) +
  geom_segment(aes(y=lower, x= peakage, yend= upper , xend= peakage, colour="#F23DB3"),
               arrow= arrow(angle=90, ends= 'both', 
                            length = unit(0.3, 'cm'))) +
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
  mutate(female=ifelse(sex==1, 0, 1),sex=ifelse(sex==1, 0, 1), age2=age^2, ing_hr=log(ing_hr), ing_m=log(y_ingLab_m)) %>% # Transformaciones adicionales a las variables
  drop_na() #Eliminando NAs

base4$maxEducLevel <- as.factor(base4$maxEducLevel) # Educación como dummy 
base4$relab <- as.factor(base4$relab) # Tipo de ocupación como dummy
base4$sex <- as.factor(base4$sex)

### a. Begin by estimating and discussing the unconditional wage gap:
set.seed(1111)

reg4a_m <- lm(ing_m ~ female, data=base4)   # Ingreso mensual ~ Female
reg4a_hr <- lm(ing_hr ~ female, data=base4) # Ingreso por hora ~ Female

stargazer(reg4a_m, reg4a_hr, type="latex")

### b. Equal Pay for Equal Work?
reg4c_m <-lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4) # (Conditional wage gap) Ingreso mensual ~ Female + Other explanatory variables
reg4c_hr <- lm(ing_hr  ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4) # (Conditional wage gap) Ingreso por hora ~ Female + Other explanatory variables

stargazer(reg4a_hr, reg4c_hr, type="text")

## FWL --------------
#p_load("tidyverse","rio","stargazer")

# Ingreso mensual
#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidF=lm(female~ maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidF=lm(ing_m ~ maxEducLevel + age + age2+ formal + fulltime +relab , data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_m_fwl<-lm(wageResidF~femaleResidF, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4a_m, reg4c_m, reg4_m_fwl, type="text")

### Bootstrap para coeficientes -------------

# Regresión original
eta.fn_m1 <-function(data,index){
  coefm1 <- coef(lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data = data, subset = index)) # Regresión original
  return(coefm1[2])
}
reg4m1 <- boot(base4, eta.fn_m1, R = 1000) # boot(datos, estadístico deseado, repeticiones)

# Regresión FWL
eta.fn_m2 <-function(data,index){
  coefm2 <- coef(lm(wageResidF~femaleResidF, data = data, subset = index)) # Regresión FWL
  return(coefm2[2])
}
reg4m2 <- boot(base4, eta.fn_m2, R = 1000) # boot(datos, estadístico deseado, repeticiones)

femalecoef<- reg4m2$t0

# Corrección de errores estándar 
se_m1 <- sqrt(diag(vcov(reg4c_m)))[2]
se_m2 <- sqrt(diag(vcov(reg4_m_fwl))*(9889/9877))[2]

## Resumen del bootstrap
boot_ing_m <- matrix(NA, ncol=3, nrow = 4)
boot_ing_m[, 1] <- c("", "Coeficiente", "Sesgo", "Errores estándar")
boot_ing_m[1, ] <- c("", "Modelo original", "Modelo FWL")
boot_ing_m[2, 2:3] <- c(reg4m1$t0, reg4m2$t0)
boot_ing_m[3, 2:3] <- c(0.0004555001, 8.962507e-05)
boot_ing_m[4, 2:3] <- c(se_m1, se_m2)
stargazer(boot_ing_m, type="text")

# Ingreso por horas
#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidFhr=lm(female~ maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidFhr=lm(ing_hr ~ maxEducLevel + age + age2+ formal + fulltime +relab , data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_hr_fwl<-lm(wageResidFhr~femaleResidFhr, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4a_hr, reg4c_hr, reg4_hr_fwl, type="text")

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
se_hr1 <- sqrt(diag(vcov(reg4c_hr)))[2]                 # Error estándar ajustado para la regresión FWL
se_hr2 <- sqrt(diag(vcov(reg4_hr_fwl))*(9889/9877))[2]  # Error estándar regresión original

## Resumen del bootstrap
boot_ing_hr <- matrix(NA, ncol=3, nrow = 4)
boot_ing_hr[, 1] <- c("", "Coeficiente", "Sesgo", "Errores estándar")
boot_ing_hr[1, ] <- c("", "Modelo original", "Modelo FWL")
boot_ing_hr[2, 2:3] <- c(reg4hr1$t0, reg4hr2$t0)
boot_ing_hr[3, 2:3] <- c(0.0005964313, 0.0003360888)
boot_ing_hr[4, 2:3] <- c(se_hr1, se_hr2)
stargazer(boot_ing_hr, type="text")

# c. Predicted age-wage profile

age_wage_sex<- ggplot(base4, 
            aes(x = age, 
                y = ing_m, color= sex)) +
  geom_point(size=2, color="#69b3a2AA") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), aes(group=sex))+
  scale_color_manual(labels = c("Masculino", "Femenino"), values = c("steelblue", "indianred3")) +
  labs(x= "Edad", y= "Ingresos", title= "Trayectoria de los ingresos a lo largo de la edad por sexo", color= "Sexo")

#c.  Plot the predicted age-wage profile and estimate the implied ``peak ages'' with the respective confidence intervals by gender
#1. Función para peakage
mod_peakage_sex <- function(base4,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(ing_m ~ female +age + age2, base4, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1] #intercepto
  beta1 = coef[2] #female
  beta2 = coef[3] #age
  beta3 = coef[4] #age2
  
  #3. calcular peak age
  peak_age = -(beta2/(2*beta3))
  
  return(peak_age)
}

#Corremos el Bootstrap
set.seed(9876)
results_peakage_sex <- boot(base4, mod_peakage_sex, R=1000)
results_peakage_sex 

#EDAD MÁXIMO
peakage<- results_peakage_sex$t0

#2. peak wage
mod_peakwage_fem <- function(base4,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(ing_m ~ female +age + age2, base4, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1] #intercepto
  beta1 = coef[2] #female
  beta2 = coef[3] #age
  beta3 = coef[4] #age2
  
  #3. calcular peak age
  peak_age = -(beta2/(2*beta3))
  
  #4. calcular peak wage
  wage_pa_fem = beta0 + beta1*1+ beta2*(peakage)+ beta3*(peakage)^2
  return(wage_pa_fem)
}

results_peakwage_fem <- boot(base4, mod_peakwage_fem, R=1000)
results_peakwage_fem

#antes necesito extraer los estadísticos a values
peakwage_fem<- results_peakwage_fem$t0
bias_fem <- colMeans(results_peakwage_fem$t)-results_peakwage_fem$t0
se_fem <- apply(results_peakwage_fem$t,2,sd)

#construimos los valores para el CI
alpha = 0.05 # 95% Confidence Interval
lower = peakwage_fem - qnorm(alpha/2) * se_fem
upper = peakwage_fem + qnorm(alpha/2) * se_fem

#peak wage- men
mod_peakwage_m <- function(base4,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(ing_m ~ female +age + age2, base4, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1] #intercepto
  beta1 = coef[2] #female
  beta2 = coef[3] #age
  beta3 = coef[4] #age2
  
  #3. calcular peak age
  peak_age = -(beta2/(2*beta3))
  
  #4. calcular peak wage
  wage_pa_men = beta0 + beta2*(peakage)+ beta3*(peakage)^2
  
  return(wage_pa_men)
}

results_peakwage_m <- boot(base4, mod_peakwage_m, R=1000)
results_peakwage_m

#antes necesito extraer los estadísticos a values
peakwage_m<- results_peakwage_m$t0
bias_m <- colMeans(results_peakwage_m$t)-results_peakwage_m$t0
se_m <- apply(results_peakwage_m$t,2,sd)

#construimos los valores para el CI
alpha = 0.05 # 95% Confidence Interval
lower_m = peakwage_m - qnorm(alpha/2) * se_m
upper_m = peakwage_m + qnorm(alpha/2) * se_m


#3. Agregamos el CI al gráfico
age_wage_sex + 
  geom_segment(aes(y=upper, x= peakage, yend= lower , xend= peakage),
               arrow= arrow(angle=90, ends= 'both', 
                            length = unit(0.2, 'cm'))) +
  geom_segment(aes(y=upper_m, x= peakage, yend= lower_m , xend= peakage),
               arrow= arrow(angle=90, ends= 'both', 
                            length = unit(0.2, 'cm'))) +
  labs(x= "Edad", y= "Ingresos", title= "Trayectoria de los ingresos a lo largo de la Edad")


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
model1<-lm(lnwage~totalHoursWorked:sex:age,data=train)
stargazer(model1, type="text")

test$model1<-predict(model1,newdata = test)

with(test,mean((lnwage-model1)^2))

## Segundo modelo ##

model2<-lm(lnwage~totalHoursWorked:sex+maxEducLevel:sex,data=train)
test$model2<-predict(model2,newdata = test)

with(test,mean((lnwage-model2)^2))

## Tercer modelo ##

model3<-lm(lnwage~totalHoursWorked^2+age^2+sex+maxEducLevel+formal,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((lnwage-model3)^2))

## Cuarto modelo ##

model4<-lm(lnwage~totalHoursWorked:formal+age+age^2+maxEducLevel+formal+sex+
             estrato1+sizeFirm,data=train)
test$model4<-predict(model4,newdata = test)

with(test,mean((lnwage-model4)^2))

stargazer(model4, type = "text")

## Quinto modelo ##
model5<-lm(lnwage~totalHoursWorked:formal:sex+age+age^2+maxEducLevel+formal+sex+
             +estrato1,data=train)
test$model5<-predict(model5,newdata = test)

with(test,mean((lnwage-model5)^2))

stargazer(model5, type = "latex")

## comparar los MSE 
msew_age2<-with(test,round(mean((lnwage-reg4a_hrtrain )^2),4))
msew_fem<-with(test,round(mean((lnwage-reg4a_hrtrain )^2),4))
mse1<-with(test,round(mean((lnwage-model1)^2),4))
mse2<-with(test,round(mean((lnwage-model2)^2),4))
mse3<-with(test,round(mean((lnwage-model3)^2),4))
mse4<-with(test,round(mean((lnwage-model4)^2),4))
mse5<-with(test,round(mean((lnwage-model5)^2),4))

comparacionmse<-data.frame(msew_age2,msew_fem,mse1,mse2,mse3,mse4, mse5)
comparacionmse
stargazer(model1,model2,model3,model4,model5, type="latex")

##c.

test$mod4predic <- predict(model4, newdata=test) 
testmod4predic <- test$mod4predic #guardamos las predicciones del modelo 4, que tiene menor MSE, sobre las observaciones de prueba

testlnwage <- test$lnwage #guardamos las observaciones de prueba

#Gráfica para comparar valores observados y predichos. Si no hubiese error de predicción, todos los puntos estarían sobre la recta
ggplot(test$base2, aes(x = testmod4predic, y = testlnwage)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "green") +
  labs(x = "Valores predichos de log(ingreso)", y = "Valores observados de log(ingreso)")

# Calculamos los errores de predicción para realizar un histograma
msemod4 = (testlnwage-testmod4predic)

ggplot(test$base2, aes(x=msemod4, fill=sex)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(msemod4)), color="blue",
             linetype="dashed")+
  labs(title="Histograma del RMSE",x="Error de predicción", y = "Frecuencia")+
  theme_classic()


##d. LOOCV modelo 4
library (boot)
glm.fit=glm(model4 ,data=base2)
cv.err =cv.glm(base2 ,glm.fit)
LOOCVm4<-cv.err$delta

## LOOCV modelo 5
glm.fit=glm(model5 ,data=base2)
cv.err =cv.glm(base2 ,glm.fit)
cv.err$delta
LOOCVm5<-cv.err$delta
##comparo los dos modelos
tabla2<-data.frame(LOOCVm4,LOOCVm5)
tabla2
stargazer(tabla2, type="latex")



