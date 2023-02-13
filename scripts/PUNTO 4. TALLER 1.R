#PUNTO 4. 
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
# Base provisional para obtener los nombres de las columnas
prov <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true") 

ini <- prov %>% 
  group_by(y_ingLab_m_ha) %>%
  summarise(n = n())

base <- prov %>%
  # 1. Renombrando variable de interés
  rename(ing_hr=y_ingLab_m_ha) %>%                             # y_ingLab_m_ha pasaría a ser ing_hr
  # 2. Filtrando por edad y empleado
  filter(age>=18,                                              # Población mayor a 18
         dsi==0) %>%                                           # Población ocupada
  # 3. Filtrando por datos vacíos y nulos
  mutate_at(c('ing_hr'), ~na_if(., 0)) %>%                     # Reemplazar valores nulos de ing_hr por NA
  filter(!is.na(ing_hr))                                       # Filtrando por vacios en ing_hr

base <- base %>%
  mutate(fulltime=ifelse(hoursWorkUsual>=40, 1, 0)) # El tipo de contrato es tiempo completo si trabaja más de 40 horas a la semana
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

# a. Begin by estimating and discussing the unconditional wage gap:
set.seed(1111)

reg4a_m <- lm(ing_m ~ female, data=base4)   # Ingreso mensual ~ Female
reg4a_hr <- lm(ing_hr ~ female, data=base4) # Ingreso por hora ~ Female

  stargazer(reg4a_m, reg4a_hr, type="latex")

# b. Equal Pay for Equal Work?
reg4c_m <-lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4) # (Conditional wage gap) Ingreso mensual ~ Female + Other explanatory variables
reg4c_hr <- lm(ing_hr  ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4) # (Conditional wage gap) Ingreso por hora ~ Female + Other explanatory variables

  stargazer(reg4a_hr, reg4c_hr, type="text")

## FWL --------------

### Ingreso mensual
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
              formula = y ~ poly(x, 2), 
              aes(group=sex))+
  scale_color_manual(labels = c("Masculino", "Femenino"), values = c("steelblue", "indianred3")) +
                      labs(x= "Edad", y= "Ingresos", title= "Trayectoria de los ingresos a lo largo de la edad por sexo", color= "Sexo")

#c.  Plot the predicted age-wage profile and estimate the implied ``peak ages'' with the respective confidence intervals by gender
#1. Función para peakage
mod_peakage_sex <- function(base4,index){
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
