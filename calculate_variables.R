#VARIABLES:
#P6040: EDAD
#P6070: ESTADO CIVIL
#P6160: SABE LEER O ESCRIBIR?
#P6170: ASISTE AL COLEGIO?
#P3042: NIVEL EDUCATIVO
#INGLABO: INGRESO
#P5090: VIVIENDA PROPIA?
#P6050: JEFE DEL HOGAR
#P6430: TIPO DE TRABAJO
#P6800: HORAS DE TRABAJO SEMANAL
#P7130: Desea cambiar el trabajo que tiene actualmente?
#CLASE:  1 URBANIDAD - 2 RURALIDAD
#P7240: En caso de no tener trabajo, de donde obtendría principalmente los recursos para sus gastos y/o los de su hogar
#P6240: en que actividad ocupó ... La mayor parte del tiempo la semana pasada?
#P7350: A QUE SE DEDICO EN SU ULTIMO EMPLEO (TIPO DE TRABAJO)
#P760: desempleo entre el empleo actual y el anterior
#CANTIDAD_DE_HIJOS: ???


calculate_variables <- function() {
  #POBLACION: MUJERES OCUPADAS
  agosto <- fread(file = "agosto.csv")
  data <- agosto[P3271 == 2 & OCI == 1, ]
  
  #edad
  data[, edad := P6040]
  
  #1 tiene pareja - 0 no tiene pareja
  data[, soltero := ifelse((P6070 > 3), 1, 0)]
  
  #1 analfabeta - 0 no es analfabeta
  data[, analfabeta := ifelse(P6160 == 2, 1, 0)]
  
  #mayor nivel educativo alcanzado
  data[, nivel_educativo := P3042]
  
  #ingreso
  data[, ingreso := INGLABO]
  
  #1 tiene vivienda propia - 0 no tiene vivienda propia
  data[, vivienda_propia := ifelse(P5090 <= 2, 1, 0)]
  
  #1 jefe de hogar - 0 no es jefe de hogar
  data[, jefe_hogar := ifelse(P6050 == 1, 1, 0)]
  
  
  ###################
  # tipo de trabajo #
  ###################
  
  #1	Obrero o empleado de empresa particular
  data[, empleado_empresa_particular := ifelse(P6430 == 1, 1, 0)]
  
  #1 Obrero o empleado del gobierno
  data[, empleado_gobierno := ifelse(P6430 == 2, 1, 0)]
  
  #1 Empleado domestico
  data[, empleado_domestico := ifelse(P6430 == 3, 1, 0)]
  
  #1 Trabajador por cuenta propia
  data[, independiente := ifelse(P6430 == 4, 1, 0)]
  
  #1 Patrón o empleador
  data[, empleador := ifelse(P6430 == 5, 1, 0)]
  
  #1 Trabajador familiar sin remuneración
  data[, familiar_sin_remuneracion := ifelse(P6430 == 6, 1, 0)]
  
  #1 Trabajador sin remuneración en empresas o negocios de otros hogares
  data[, empresa_sin_remuneracion := ifelse(P6430 == 7, 1, 0)]
  
  #1 Jornalero o peón
  data[, jornalero := ifelse(P6430 == 8, 1, 0)]
  
  #####################################################################
  
  
  #horas de trabajo semanal
  data[, horas_trabajo := P6800]
  
  #1 desea cambiar el trabajo actual - 0 no desea cambiar el trabajo actual
  data[, desea_cambiar_trabajo := ifelse(P7130 == 1, 1, 0)]
  
  #1 vive en ruralidad - 0 no vive en ruralidad
  data[, rural := ifelse(CLASE == 2, 1, 0)]
  
  #1 tiene dinero ahorrado - 0 no tiene dinero ahorrado
  data[, dinero_ahorrado := ifelse((P7240 == 1 | P7240 == 2 | P7240 == 4), 1, 0)]
  
  #desempleo entre el empleo actual y el anterior 
  data[, tiempo_desempleado := P760]
  
  #1 informal - 0 formal
  data[, informal := ifelse((OCI == 1 & P6090 == 1 & P6920 == 1), 0, 1)]
  
  #1 tercera edad - 0 no es de tercera edad
  data[, tercera_edad := ifelse(edad >= 60, 1, 0)]
  
  final_df <- data[, .(DPTO, informal, edad, soltero, analfabeta, nivel_educativo, ingreso, 
                       vivienda_propia, jefe_hogar, empleado_empresa_particular, 
                       empleado_gobierno, empleado_domestico, independiente, empleador, 
                       familiar_sin_remuneracion, empresa_sin_remuneracion, jornalero, 
                       horas_trabajo, desea_cambiar_trabajo, rural, dinero_ahorrado, 
                       tiempo_desempleado, tercera_edad)]
  
  df_not_na <- na.omit(final_df)
  df_not_na <- df_not_na[ingreso > 0, ]
  
  fwrite(df_not_na, "variables_to_model.csv")
}

calculate_variables()
















