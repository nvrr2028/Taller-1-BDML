#PUNTO 5. 
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


##d.
library (boot)
glm.fit=glm(model4 ,data=base2)
cv.err =cv.glm(base2 ,glm.fit)
LOOCVm4<-cv.err$delta

glm.fit=glm(model5 ,data=base2)
cv.err =cv.glm(base2 ,glm.fit)
cv.err$delta
LOOCVm5<-cv.err$delta

tabla2<-data.frame(LOOCVm4,LOOCVm5)
tabla2
stargazer(tabla2, type="latex")



