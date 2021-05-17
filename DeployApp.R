#Subir App automaticamente:

library(rsconnect)

rsconnect::setAccountInfo(name='encuentrosarg', token='AAEF46145BDCBC5B450BAFEC11DFCEB9', secret='rvQ+sj9/NWp1RNFRBqM9JW0oJNq3PA7syjeCBK8O')

deployApp(appName = "Monitoreo-COVID-CTERA",
          appFiles = c("app.R",
                       "data/codigo_prov_depto.csv",
                       "data/Covid19Casos2021 reducido.csv",
                       "scripts/funciones de analisis.R",
                       "scripts/graficos.R",
                       "scripts/procesamiento.R",
                       "www/logo_ctera.png"),
          launch.browser = FALSE,
          forceUpdate = TRUE)