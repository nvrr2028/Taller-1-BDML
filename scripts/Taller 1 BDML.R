#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodriguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                                  Fuente: GEIH DANE                                   #
#**************************************************************************************#

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
# Cargar los datos.
# ------------------------------------------------------------------------------------ #

# Link: https://ignaciomsarmiento.github.io/GEIH2018 sample/

## set seed
set.seed(0000)

base <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

# ------------------------------------------------------------------------------------ #
# Descripción del problema
# ------------------------------------------------------------------------------------ #

# An income prediction model can help identify vulnerable individuals and families
# that may need further assistance.

# ------------------------------------------------------------------------------------ #
# Procesar los datos
# ------------------------------------------------------------------------------------ #

# Variables de interés
# age - edad
# dsi - =1 if unemployed; =0 otherwise

# Base inicial: filtrando filtrando por edad y empleado
data <- base %>%
  filter(age>=18,   # Población mayor a 18
         dsi==0)

# ------------------------------------------------------------------------------------ #
# Variable Y y X
# ------------------------------------------------------------------------------------ #

#### Variable dependiente
# totalHoursWorked - total hours worked previous week
# Ingreso total - Ingreso total
# y_ingLab_m_ha - labor income salaried - nomial hourly - all occ. (includes tips and commissions

#### Variables explicativas 
# maxEducLevel - max. education level attained
# age - edad
# oficio - occupation
# formal - =1 if formal (social security); =0 otherwise
# Informal - =1 if informal (social security); =0 otherwiseinformal
# Sex - =1 male, =0 female
# estrato1 - Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y rest
# full-time - Trabaja más de 40 horas a la semana
# p6240 - ¿En que actividad ocupó...... la mayor parte del tiempo la semana pasada?
# relab -	type of occupation
# sizeFirm -size of the firm by categories

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
