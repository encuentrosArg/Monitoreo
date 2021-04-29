library(data.table)
#Preprocesamiento:
temp <- tempfile()
download.file("https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip",temp)
datos <- fread(unzip(temp, "Covid19Casos.csv"), encoding = "UTF-8", stringsAsFactors = TRUE)
unlink(temp)
file.remove("Covid19Casos.csv")
#Borro columnas que no nos interesan:
datos <- datos[, -c("residencia_pais_nombre",
      "residencia_provincia_nombre",
      "residencia_departamento_nombre",
      "carga_provincia_nombre",
      "sepi_apertura",
      "origen_financiamiento",
      "clasificacion",
      "fecha_diagnostico",
      "ultima_actualizacion",
      "cuidado_intensivo",
      "fallecido",
      "asistencia_respiratoria_mecanica",
      "fecha_cui_intensivo",
      "fecha_internacion")]

#todos los que no especificaron la provincia se las cambio por provincia de carga
nuevo <- fifelse(datos$residencia_provincia_id == 99, datos$carga_provincia_id, datos$residencia_provincia_id)
datos$residencia_provincia_id <- nuevo

#fecha_min es la fecha de inicio de sintomas y si no la tiene es la de apertura del caso
datos$fecha_min <- fifelse(is.na(datos$fecha_inicio_sintomas), datos$fecha_apertura, datos$fecha_inicio_sintomas)
datos <- datos[fecha_min >= as.Date("2021-01-01")]
#Saco las variables que use recien:
datos <- datos[, -c("carga_provincia_id", "fecha_inicio_sintomas",
                    "fecha_apertura")]

#Lo escribo:
zipped.csv <- function(df, zippedfile) {
   # init temp csv
   temp <- tempfile(fileext=".csv")
   # write temp csv
   fwrite(df, file=temp)
   # zip temp csv
   zip(zippedfile,temp)
   # delete temp csv
   unlink(temp)
}

temp <- tempfile(fileext=".csv")
fwrite(datos, file=temp)
zip("data/Covid19Casos2021 reducido.zip",temp)
unlink(temp)

