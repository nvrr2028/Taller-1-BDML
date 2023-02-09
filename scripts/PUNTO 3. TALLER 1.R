#PUNTO 3. 
#cosas de otros puntos ------------
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

set.seed(0000) 
# Base provisional para obtener los nombres de las columnas
prov <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true") 

ini <- prov %>% 
  group_by(y_ingLab_m_ha) %>%
  summarise(n = n())

basep <- prov %>%
  # 1. Renombrando variable de interés
  rename(ing_hr=y_ingLab_m_ha) %>%                             # y_ingLab_m_ha pasaría a ser ing_hr
  # 2. Filtrando por edad y empleado
  filter(age>=18,                                              # Población mayor a 18
         dsi==0) %>%                                           # Población ocupada
  # 3. Filtrando por datos vacíos y nulos
  mutate_at(c('ing_hr'), ~na_if(., 0)) %>%                     # Reemplazar valores nulos de ing_hr por NA
  filter(!is.na(ing_hr))                                       # Filtrando por vacios en ing_hr

basep <- basep %>%
  mutate(fulltime=ifelse(hoursWorkUsual>=40, 1, 0)) # El tipo de contrato es tiempo completo si trabaja más de 40 horas a la semana

# 3a. Tabla de regresión ------------------------------------------------------------- #

#Primero creamos la variable edad al cuadrado y el logaritmo del salario
basep  <- basep %>%
  mutate(age2=age^2 , 
         lnwage=log(ing_hr))

#Procedemos a hacer la regresión
regw_age2<- lm(lnwage~ age+ age2, data = basep)
stargazer(regw_age2, type = "text")
stargazer(regw_age2, type = "latex")

##Veamos el gráfico de la regresión en cuestión
age_earnings<- ggplot(basep, 
       aes(x = age, 
           y = lnwage)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

# Bootstrap para construir los intervalos de confianza
p_load("boot")

# ---------
#Primero tenemos que calcular el peak age. 

#1. creamos un data frame con el summary de nuestra regresión
#lmw_summary <- data.frame(summary(regw_age2)$coefficients)

#2. extraemos los betas a escalares para plantear la fórmula
#beta1 = lmw_summary[2,1]
#beta2 = lmw_summary[3,1]

#peak_age = -(beta1/(2*beta2))
#peak_age

#1. creamos un data frame con el summary de nuestra regresión
#coef <- lm(lnwage~ age+ age2, data = basep)$coefficients

#2. extraemos los betas a escalares para plantear la fórmula
#beta1 = coef[2]
#beta2 = coef[3]


# ahora si ----------------------------------------
mod_peakage <- function(basep,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(lnwage~ age+ age2, data = basep, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta1 = coef[2]
  beta2 = coef[3]
  
  #3. calcular peak age
  peak_age = -(beta1/(2*beta2))
  
  return(peak_age)
}

mod_peakage(basep, 1: nrow(basep)) #comprobando que sale igual :)

#Corremos el Bootstrap
set.seed(9876)
results_peakage <- boot(basep, mod_peakage, R=1000)
results_peakage

#ahora construyamos los confidence intervals 

#1. necesito extraer los estadísticos a values
peakage<- results_peakage$t0
bias <- colMeans(results_peakage$t)-results_peakage$t0
se <- apply(results_peakage$t,2,sd)

#2. construimos los valores para el CI
alpha = 0.05 # 95% Confidence Interval
lower = peakage - qnorm(alpha/2) * se
upper = peakage + qnorm(alpha/2) * se

#para agregar en punto de peak age 
#1. creamos un data frame con el summary de nuestra regresión
lmw_summary <- data.frame(summary(regw_age2)$coefficients)

#2. extraemos los betas a escalares para plantear la fórmula
beta0 = lmw_summary[1,1]
beta1 = lmw_summary[2,1]
beta2 = lmw_summary[3,1]

wage_pa = beta0 + beta1*peakage + beta2*(peakage)^2

pa = as.character(peakage)

age_earnings

ggplot(basep, 
       aes(x = age, y = lnwage))+ 
  geom_point(aes(x=peakage, y=wage_pa)) +
  geom_errorbar(aes(ymin=lower,ymax=upper,width=0.2), position = pa)
