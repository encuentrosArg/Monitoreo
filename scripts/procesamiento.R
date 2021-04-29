#library(data.table)
#library(dplyr)
datos <- fread(unzip("data/Covid19Casos2021 reducido.zip", "Covid19Casos2021 reducido.csv"), encoding = "UTF-8", stringsAsFactors = TRUE)
#datos <- fread("data/Covid19Casos2021 reducido.csv", encoding = "UTF-8", stringsAsFactors = TRUE)

#Borro columnas que no nos interesan:
#datos <- datos[, -c("residencia_pais_nombre",
#      "residencia_provincia_nombre",
#      "residencia_departamento_nombre",
#      "carga_provincia_id",
#      "carga_provincia_nombre",
#      "sepi_apertura",
#      "origen_financiamiento",
#      "clasificacion",
#      "fecha_diagnostico",
#      "ultima_actualizacion",
#      "cuidado_intensivo",
#      "fallecido",
#      "asistencia_respiratoria_mecanica")]

#Modifico otras variables ----

#Convierto meses y edad a edad todo en años
datos$edad <- round(fifelse(datos$edad_años_meses == "Meses", datos$edad/12, as.numeric(datos$edad)),2)
datos <- datos[, -c("edad_años_meses")]

#Agrego rangos de edad
datos[, r_edad := fcase(edad <= 17 , "0-17",
                            edad > 17 & edad <= 39, "18-39",
                            edad > 39 & edad <= 59, "40-59",
                            edad > 59, "60 o más",
                            default = "N/R")]

#Codificacion provincia-id y depto id -----
cod_prov_depto <- fread("data/codigo_prov_depto.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
#Cuando se necesite se joinea con esta base

