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


