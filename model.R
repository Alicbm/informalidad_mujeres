library(pscl)
library(lmtest)
library(car)
library(stargazer)
library(MASS)
library(margins)

#################
# PRIMER MODELO #
#################

datos <- fread(file ="df_not_na.csv")

#MODELO CON DATOS DE MIGRANTES VENEZOLANOS CON NACIONALIDAD COLOMBIANA
formula_1_logit <- glm(informal ~  edad + soltero + analfabeta + nivel_educativo + ingreso + vivienda_propia + jefe_hogar + empleado_empresa_particular + empleado_gobierno + empleado_domestico + independiente + empleador + familiar_sin_remuneracion + empresa_sin_remuneracion + jornalero + horas_trabajo + desea_cambiar_trabajo + rural + dinero_ahorrado + tiempo_desempleado, 
                       family = binomial(link = "logit"), data = datos)

formula_1_probit <- glm(informal ~  edad + soltero + analfabeta + nivel_educativo + ingreso + vivienda_propia + jefe_hogar + empleado_empresa_particular + empleado_gobierno + empleado_domestico + independiente + empleador + familiar_sin_remuneracion + empresa_sin_remuneracion + jornalero + horas_trabajo + desea_cambiar_trabajo + rural + dinero_ahorrado + tiempo_desempleado, 
                        family = binomial(link = "probit"), data = datos)

#MODELO LOGIT
modelo_1_logit <- stepAIC(formula_1_logit, direction = "backward")
summary(modelo_1_logit)

#MODELO PROBIT
modelo_1_probit <- stepAIC(formula_1_probit, direction = "backward")
summary(modelo_1_probit)


#COMPARACION DE MODELOS
#MEDIDAS DE BONDAD

# 1. Predicciones

#modelo 1 logit
predicciones_logit_1 <- ifelse(predict(modelo_1_logit, type = "response") > 0.5, 1, 0)

#modelo 1 probit
predicciones_probit_1 <- ifelse(predict(modelo_1_probit, type = "response") > 0.5, 1, 0)

# Porcentaje de aciertos
#modelo 1 logit
porcentaje_aciertos_logit_1 <- mean(predicciones_logit_1 == datos$informal) #0.8369017

#modelo 1 probit
porcentaje_aciertos_probit_1 <- mean(predicciones_probit_1 == datos$informal) #0.8493442

porcentaje_aciertos_probit_1
porcentaje_aciertos_logit_1


#2. PSEUDO R^2

#modelo 1 logit
pR2(modelo_1_logit) #0.4518712

#modelo 1 probit
pR2(modelo_1_probit)  #0.4348195


#3. AIC
#modelo 1 logit
aic_value_logit_1 <- AIC(modelo_1_logit) #6814.553

#modelo 1 probit
aic_value_probit_1 <- AIC(modelo_1_probit) #7023.426

aic_value_logit_1
aic_value_probit_1

#4. BIC
#modelo 1 logit
bic_value_logit_1 <- BIC(modelo_1_logit) #6942.284

#modelo 1 probit
bic_value_probit_1 <- BIC(modelo_1_probit) #7144.061

bic_value_logit_1
bic_value_probit_1

#5. Estadístico de Máxima Verosimilitud
#modelo 1 logit
log_verosimilitud_logit_1 <- logLik(modelo_1_logit) #-3389.276

#modelo 1 logit
log_verosimilitud_probit_1 <- logLik(modelo_1_probit) #-3494.713

log_verosimilitud_logit_1
log_verosimilitud_probit_1





#EFECTOR MARGINALES
efectos_marginales_1_probit <- margins(modelo_1_probit)
summary(efectos_marginales_1_probit)


