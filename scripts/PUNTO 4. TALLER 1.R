#PUNTO 4. 
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

# punto 4------------------
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
head(base4)
reg4c_m <-lm(y_ingLab_m ~ female + maxEducLevel + age + age2+ formal + fulltime, data=base4)
reg4c_hr <- lm(ing_hr  ~ female + maxEducLevel + age + age2+ formal + fulltime, data=base4)

stargazer(reg4a_hr, reg4c_hr, type="text")

# FWL --------------
p_load("tidyverse","rio","stargazer")

#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidF=lm(female~ maxEducLevel + age + age2+ formal + fulltime, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidF=lm(y_ingLab_m ~ maxEducLevel + age + age2+ formal + fulltime, data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_m_fwl<-lm(wageResidF~femaleResidF, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4c_m, reg4_m_fwl, type="text")


# FWL --------------
p_load("tidyverse","rio","stargazer")

#para mensual
#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidF=lm(female~ maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidF=lm(y_ingLab_m ~ maxEducLevel + age + age2+ formal + fulltime +relab , data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_m_fwl<-lm(wageResidF~femaleResidF, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4c_m, reg4_m_fwl, type="text")

#para horas mmmmm
#1. Residuals of female~controles
base4<-base4 %>% mutate(femaleResidFhr=lm(female~ maxEducLevel + age + age2+ formal + fulltime + relab, data=base4)$residuals) #Residuals of female~controles 
#2. Residuals of ingreso~controles (sin female) 
base4<-base4 %>% mutate(wageResidFhr=lm(ing_hr ~ maxEducLevel + age + age2+ formal + fulltime +relab , data=base4)$residuals) #<
#3. Residuals de female en ingresos
reg4_hr_fwl<-lm(wageResidFhr~femaleResidFhr, base4) #esta ya nos arroja el coef que queremos

stargazer(reg4c_hr, reg4_hr_fwl, type="text")

# grafico--------------------

# c. Predicted age-wage profile

age_wage_sex<- ggplot(base4, 
                      aes(x = age, 
                          y = ing_m, color= sex)) +
  geom_point(size=2, color="#69b3a2AA") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), aes(group=sex))+
  scale_color_manual(labels = c("Masculino", "Femenino"), values = c("steelblue", "indianred3")) +
  labs(x= "Edad", y= "Ingresos", title= "Trayectoria de los ingresos a lo largo de la edad por sexo", color= "Sexo")

# Bootstrap para construir los intervalos de confianza

#Función para peakage
mod_peakage_sex <- function(base4,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1] #intercepto
  beta1 = coef[2] #female
  beta2 = coef[3] #maxeduc
  beta3 = coef[8] #age
  beta4 = coef[9] #age2
  beta5 = coef[10] #formal
  beta6 = coef[11] #fulltime
  beta7 = coef[11] #relab
  
  #3. calcular peak age
  peak_age = -(beta3/(2*beta4))
  
  return(peak_age)
}

#Corremos el Bootstrap
set.seed(9876)
results_peakage_sex <- boot(base4, mod_peakage_sex, R=1000)
results_peakage_sex 

#peak wage
mod_peakwage_fem <- function(base4,index){
  set.seed(9876)
  #1. creamos un data frame con el summary de nuestra regresión
  coef <- lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1] #intercepto
  beta1 = coef[2] #female
  beta2 = coef[3] #maxeduc
  beta3 = coef[8] #age
  beta4 = coef[9] #age2
  beta5 = coef[10] #formal
  beta6 = coef[11] #fulltime
  beta7 = coef[11] #relab
  
  
  #3. calcular peak age
  peak_age = -(beta3/(2*beta4))
  
  #4. calcular peak wage
  wage_pa_fem = beta0 + beta1*femalecoef+ beta2*1 + beta3*(peakage)+ beta4*(peakage)^2+ beta5*1+ beta6 +beta7
  
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
  coef <- lm(ing_m ~ female + maxEducLevel + age + age2+ formal + fulltime + relab, data=base4, subset = index)$coefficients
  
  #2. extraemos los betas a escalares para plantear la fórmula
  beta0 = coef[1] #intercepto
  beta1 = coef[2] #female
  beta2 = coef[3] #maxeduc
  beta3 = coef[8] #age
  beta4 = coef[9] #age2
  beta5 = coef[10] #formal
  beta6 = coef[11] #fulltime
  beta7 = coef[11] #relab
  
  #3. calcular peak age
  peak_age = -(beta3/(2*beta4))
  
  #4. calcular peak wage
  wage_pa_men = beta0 + beta2*1 + beta3*(peakage)+ beta4*(peakage)^2+ beta5*1+ beta6 +beta7
  
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


#4. Agregamos el CI al gráfico
age_wage_sex + 
  geom_point(aes(x=peakage, y=peakwage_fem)) +
  geom_point(aes(x=peakage, y=peakwage_m)) +
  geom_segment(aes(y=lower, x= peakage, yend= upper , xend= peakage),
               arrow= arrow(angle=90, ends= 'both', 
                            length = unit(0.2, 'cm'))) +
  geom_segment(aes(y=lower_m, x= peakage, yend= upper_m , xend= peakage),
               arrow= arrow(angle=90, ends= 'both', 
                            length = unit(0.2, 'cm'))) +
  labs(x= "Edad", y= "Ingresos", title= "Trayectoria de los ingresos a lo largo de la Edad")

#solo sex---------------------- 


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
