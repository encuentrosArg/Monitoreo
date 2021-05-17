#Elegir "test" o "produccion"


donde <- "produccion" #"test" o "produccion"



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

#edad
datos$edad <- round(fifelse(datos$edad_años_meses == "Meses", datos$edad/12, as.numeric(datos$edad)),2)
datos <- datos[, -c("edad_años_meses")]

#fecha_min es la fecha de inicio de sintomas y si no la tiene es la de apertura del caso
datos$fecha_min <- fifelse(is.na(datos$fecha_inicio_sintomas), datos$fecha_apertura, datos$fecha_inicio_sintomas)
datos <- datos[fecha_min >= as.Date("2021-01-01")]
#Saco las variables que use recien:
datos <- datos[, -c("carga_provincia_id", "fecha_inicio_sintomas",
                    "fecha_apertura")]

#Saco repetidos
datos <- unique(datos)

#Saco la variable id:
datos <- datos[, -c("id_evento_caso")]

#Veo si la base esta bien de tamaño:
datos.viejos <- fread("data/Covid19Casos2021 reducido.csv", encoding = "UTF-8", stringsAsFactors = TRUE)

if(object.size(datos) >= object.size(datos.viejos)){
   #Lo escribo:
   fwrite(datos, file = "data/Covid19Casos2021 reducido.csv")
   
   #Deployment
   library(rsconnect)
   
   if(donde == "test"){
      rsconnect::setAccountInfo(name='matiaspoullain', token='CD201C53D9896E152625E9307A9A3213', secret='BcYqvbQG/EuI82eRf5Bm8kyApvLg3n49ZUE1yzUA')   
      
      deployApp(appName = "Monitoreo-COVID-educacion",
                appFiles = c("app.R",
                             "data/codigo_prov_depto.csv",
                             "data/Covid19Casos2021 reducido.csv",
                             "data/poblacion_deptos.csv",
                             "scripts/funciones de analisis.R",
                             "scripts/graficos.R",
                             "scripts/procesamiento.R",
                             "www/logo_ctera.png"),
                launch.browser = TRUE,
                forceUpdate = TRUE)
   }else if(donde == "produccion"){
      rsconnect::setAccountInfo(name='encuentrosarg', token='AAEF46145BDCBC5B450BAFEC11DFCEB9', secret='rvQ+sj9/NWp1RNFRBqM9JW0oJNq3PA7syjeCBK8O')
      
      deployApp(appName = "Monitoreo-COVID-CTERA",
                appFiles = c("app.R",
                             "data/codigo_prov_depto.csv",
                             "data/Covid19Casos2021 reducido.csv",
                             "data/poblacion_deptos.csv",
                             "scripts/funciones de analisis.R",
                             "scripts/graficos.R",
                             "scripts/procesamiento.R",
                             "www/logo_ctera.png"),
                launch.browser = TRUE,
                forceUpdate = TRUE)
   }
   
   
   
}

