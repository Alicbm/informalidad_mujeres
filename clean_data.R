data <- fread(file = "variables_to_model.csv")

#eliminar valores atipicos en el ingreso

# Calcular el IQR
Q1 <- quantile(data$ingreso, 0.25)
Q3 <- quantile(data$ingreso, 0.75)
IQR <- Q3 - Q1

# Definir límites para outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

clean_data <- data[data$ingreso >= limite_inferior & data$ingreso <= limite_superior, ]

hist(clean_data$ingreso, 
     main = "Histograma de Datos Aleatorios", 
     xlab = "Valores", 
     ylab = "Frecuencia", 
     col = "blue", 
     border = "black"
)

fwrite(clean_data, "clean_data.csv")


