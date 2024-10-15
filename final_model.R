library(pscl)
library(lmtest)
library(car)
library(stargazer)
library(MASS)
library(margins)
library(ggplot2)

data <- fread(file = "clean_data.csv")


modelo_1_logit <- glm(informal ~ nivel_educativo + rural + tiempo_desempleado + jefe_hogar + ingreso + dinero_ahorrado + vivienda_propia  + empleado_domestico + independiente, 
                       family = binomial(link = "logit"), data = data)

modelo_1_probit <- glm(informal ~ nivel_educativo + rural + tiempo_desempleado + jefe_hogar + ingreso + dinero_ahorrado + vivienda_propia + empleado_domestico + independiente, 
                        family = binomial(link = "probit"), data = data)

summary(modelo_1_logit)
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
porcentaje_aciertos_logit_1 <- mean(predicciones_logit_1 == data$informal) #0.8597037

#modelo 1 probit
porcentaje_aciertos_probit_1 <- mean(predicciones_probit_1 == data$informal) #0.8611976

porcentaje_aciertos_probit_1
porcentaje_aciertos_logit_1


#2. PSEUDO R^2

#modelo 1 logit
pR2(modelo_1_logit) #0.4805697

#modelo 1 probit
pR2(modelo_1_probit)  #0.4716146


#3. AIC
#modelo 1 logit
aic_value_logit_1 <- AIC(modelo_1_logit) #5764.111

#modelo 1 probit
aic_value_probit_1 <- AIC(modelo_1_probit) #5863.14

aic_value_logit_1
aic_value_probit_1

#4. BIC
#modelo 1 logit
bic_value_logit_1 <- BIC(modelo_1_logit) #5834.024

#modelo 1 probit
bic_value_probit_1 <- BIC(modelo_1_probit) #5933.053

bic_value_logit_1
bic_value_probit_1

#5. Estadístico de Máxima Verosimilitud
#modelo 1 logit
log_verosimilitud_logit_1 <- logLik(modelo_1_logit) #-2872.055

#modelo 1 logit
log_verosimilitud_probit_1 <- logLik(modelo_1_probit) #-2921.57

log_verosimilitud_logit_1
log_verosimilitud_probit_1

###########################################
# CALCULAR LA DISTRIBUCION DE LOS ERRORES #
###########################################

# Calcular residuos de deviance
residuos_logit_1 <- residuals(modelo_1_logit, type = "deviance")
residuos_probit_1 <- residuals(modelo_1_probit, type = "deviance")

# Calcular valores ajustados
valores_ajustados_logit_1 <- fitted(modelo_1_logit)
valores_ajustados_probit_1 <- fitted(modelo_1_probit)

#Graficar los residuos

#LOGIT
df_residuos_logit_1 <- data.frame(valores_ajustados_logit_1, residuos_logit_1)

# Graficar
ggplot(df_residuos_logit_1, aes(x = valores_ajustados_logit_1, y = residuos_logit_1)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos del modelo Logit",
       x = "Valores Ajustados",
       y = "Residuos (Deviance)") +
  theme_minimal()


#PROBIT
df_residuos_probit_1 <- data.frame(valores_ajustados_probit_1, residuos_probit_1)

# Graficar
ggplot(df_residuos_probit_1, aes(x = valores_ajustados_probit_1, y = residuos_probit_1)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos del modelo Logit",
       x = "Valores Ajustados",
       y = "Residuos (Deviance)") +
  theme_minimal()


#####
# Calcular residuos
residuos <- residuals(modelo_1_probit, type = "deviance")

# Graficar residuos
plot(residuos, main = "Residuos del modelo logit", xlab = "Índice", ylab = "Residuos")
abline(h = 0, col = "red")

hist(residuos)

#EFECTOR MARGINALES
efectos_marginales_1_logit <- margins(modelo_1_logit)
summary(efectos_marginales_1_logit)




