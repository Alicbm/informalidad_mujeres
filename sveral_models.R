library(pscl)
library(lmtest)
library(car)
library(stargazer)
library(MASS)
library(margins)

datos <- fread(file ="df_not_na.csv")

#sm <- df_not_na[DPTO == 47,]

datos[, jefe_hogar]


colnames(df_not_na)

head(data)

informal ~  edad + soltero + analfabeta + nivel_educativo + ingreso + vivienda_propia + jefe_hogar 
+ empleado_empresa_particular + empleado_gobierno + empleado_domestico + independiente + empleador 
+ familiar_sin_remuneracion + empresa_sin_remuneracion + jornalero + horas_trabajo + desea_cambiar_trabajo 
+ rural + dinero_ahorrado + tiempo_desempleado

#MODELO CON DATOS DE MIGRANTES VENEZOLANOS CON NACIONALIDAD COLOMBIANA
formula_1_logit <- glm(informal ~ nivel_educativo + rural + tiempo_desempleado + jefe_hogar + edad + ingreso + dinero_ahorrado + vivienda_propia  + empleado_domestico + horas_trabajo, 
                       family = binomial(link = "logit"), data = datos)

formula_1_probit <- glm(informal ~ nivel_educativo + rural + tiempo_desempleado + jefe_hogar + edad + ingreso + dinero_ahorrado + vivienda_propia + empleado_domestico + horas_trabajo, 
                        family = binomial(link = "probit"), data = datos)

summary(formula_1_logit) #8004.8
summary(formula_1_probit) #8351.3



############################################################################
formula_2_logit <- glm(informal ~ nivel_educativo + rural + jefe_hogar + edad + ingreso + dinero_ahorrado + vivienda_propia  + empleado_domestico + horas_trabajo + independiente, 
                       family = binomial(link = "logit"), data = datos)

formula_2_probit <- glm(informal ~ nivel_educativo + rural + jefe_hogar + edad + ingreso + dinero_ahorrado + vivienda_propia + empleado_domestico + horas_trabajo + independiente, 
                        family = binomial(link = "probit"), data = datos)

summary(formula_2_logit) #7125.2
summary(formula_2_probit) #7378.1


############################################################################
formula_3_logit <- glm(informal ~ nivel_educativo + rural + tercera_edad + tiempo_desempleado + jefe_hogar + ingreso + dinero_ahorrado + vivienda_propia  + empleado_domestico + horas_trabajo, 
                       family = binomial(link = "logit"), data = datos)

formula_3_probit <- glm(informal ~ nivel_educativo + rural + tercera_edad + tiempo_desempleado + jefe_hogar + ingreso + dinero_ahorrado + vivienda_propia + empleado_domestico + horas_trabajo, 
                        family = binomial(link = "probit"), data = datos)

summary(formula_3_logit) #7904.7
summary(formula_3_probit) #8242.7









