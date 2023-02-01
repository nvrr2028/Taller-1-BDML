#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodriguez Morales                     #
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
                     "skimr", "caret", "rvest")

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



# 2b. Cargar los datos --------------------------------------------------------------- #

# Link general: https://ignaciomsarmiento.github.io/GEIH2018 sample/

set.seed(0000) # Establecer semilla
data <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

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
  rename(ing_hr=y_ingLab_m_ha) %>%                       # y_ingLab_m_ha pasaría a ser ing_hr
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
#       full-time - Trabaja más de 40 horas a la semana
#       p6240 - ¿En que actividad ocupó...... la mayor parte del tiempo la semana pasada?
#       relab -	type of occupation
#       sizeFirm -size of the firm by categories

# 1. Análisis de salario por hora de acuerdo con el nivel 
data <- base %>%
  mutate(fulltime=(totalHoursWorked>=40)*1)

str(data$estrato1)

summary(basereg$oficio)


data %>%
  group_by(totalHoursWorked) %>%
  summarise(n = n())

sum(is.na(data$totalHoursWorked))

# na.omit()
# filter(!age==0)


# 2d. Análisis de la base de datos --------------------------------------------------- #


# ------------------------------------------------------------------------------------ #
# 3. Age-wage profile
# ------------------------------------------------------------------------------------ #




# ------------------------------------------------------------------------------------ #
# 4. The gender earnings GAP
# ------------------------------------------------------------------------------------ #




# ------------------------------------------------------------------------------------ #
# 5. Predicting earnings
# ------------------------------------------------------------------------------------ #
