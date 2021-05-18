#library(tidyverse)
#library(plotly)

#Hay que definir un tema y aplicarlo a todos los graficos
#tema_encuentros <- theme().....

#A los graficos tambien hay que hacerlos funciones, despues se aplican en el shiny mas facil

#Grafico de casos diarios:-----
#Pensado para usarse con la funcion confirmados()
plot_confirmados <- function(base, eje_y = "Casos confirmados diarios"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {confirmados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = confirmados, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de casos diarios por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad()
plot_confirmados_r_edad <- function(base, eje_y = "Casos confirmados diarios"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {confirmados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = confirmados, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de confirmados acumulados:-----
#Pensado para usarse con la funcion confirmados_acumulados()
plot_confirmados_acumulados <- function(base, eje_y = "Casos confirmados acumulados desde fecha inicial"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br>><b>Acumulado</b>: {confirmados_acumulados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = confirmados_acumulados, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de confirmados acumulados por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad_acumulados()
plot_confirmados_r_edad_acumulados <- function(base, eje_y = "Casos confirmados acumulados desde fecha inicial"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Acumulado</b>: {confirmados_acumulados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = confirmados_acumulados, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de confirmados acumulados ultimos 14 dias:-----
#pensado para usarse con la funcion confirmados_acumulados_14()
plot_confirmados_acumulados_14 <- function(base, eje_y = "Casos confirmados acumulado últimos 14 días"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Acumulado últimos 14 días</b>: {confirmados_14}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = confirmados_14, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de confirmados acumulados ultimos 14 dias por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad_acumulados_14()
plot_confirmados_r_edad_acumulados_14 <- function(base, eje_y = "Casos confirmados acumulado últimos 14 días"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Acumulado últimos 14 días</b>: {confirmados_14}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = confirmados_14, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}



################################################################################



#Grafico de casos posibles diarios:-----
#Pensado para usarse con la funcion casos_posibles()
plot_casos_posibles <- function(base, eje_y = "Casos posibles diarios"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {casos_posibles}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = casos_posibles, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
}

#Grafico de casos posibles diarios por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad()
plot_casos_posibles_r_edad <- function(base, eje_y = "Casos posibles diarios"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {casos_posibles}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = casos_posibles, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de casos_posibles acumulados:-----
#Pensado para usarse con la funcion casos_posibles_acumulados()
plot_casos_posibles_acumulados <- function(base, eje_y = "Casos posibles acumulados desde fecha inicial"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Acumulado</b>: {casos_posibles_acumulados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = casos_posibles_acumulados, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de casos_posibles acumulados por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad_acumulados()
plot_casos_posibles_r_edad_acumulados <- function(base, eje_y = "Casos posibles acumulados desde fecha inicial"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Acumulado</b>: {casos_posibles_acumulados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = casos_posibles_acumulados, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de casos_posibles acumulados ultimos 14 dias-----
#pensado para usarse con la funcion casos_posibles_acumulados_14()
plot_casos_posibles_acumulados_14 <- function(base, eje_y = "Casos posibles acumulado últimos 14 días"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Acumulado últimos 14 días</b>: {casos_posibles_14}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = casos_posibles_14, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de casos_posibles acumulados ultimos 14 dias por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad_acumulados_14()
plot_casos_posibles_r_edad_acumulados_14 <- function(base, eje_y = "Casos posibles acumulado últimos 14 días"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Acumulado últimos 14 días</b>: {casos_posibles_14}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = casos_posibles_14, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}




################################################################################




#Grafico de fallecidos diarios:-----
#Pensado para usarse con la funcion fallecidos()
plot_fallecidos <- function(base, eje_y = "Fallecidos diarios"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {fallecidos}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = fallecidos, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad()
plot_fallecidos_r_edad <- function(base, eje_y = "Fallecidos diarios"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {fallecidos}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = fallecidos, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos acumulados:-----
#Pensado para usarse con la funcion fallecidos_acumulados()
plot_fallecidos_acumulados <- function(base, eje_y = "Fallecidos acumulados desde fecha inicial"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Acumulado</b>: {fallecidos_acumulados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = fallecidos_acumulados, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de fallecidos acumulados por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad_acumulados()
plot_fallecidos_r_edad_acumulados <- function(base, eje_y = "Fallecidos acumulados desde fecha inicial"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Acumulado</b>: {fallecidos_acumulados}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = fallecidos_acumulados, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos acumulados ultimos 14 dias-----
#pensado para usarse con la funcion fallecidos_acumulados_14()
plot_fallecidos_acumulados_14 <- function(base, eje_y = "Fallecidos acumulado últimos 14 días"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Acumulado últimos 14 días</b>: {fallecidos_14}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = fallecidos_14, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos acumulados ultimos 14 dias por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad_acumulados_14()
plot_fallecidos_r_edad_acumulados_14 <- function(base, eje_y = "Fallecidos acumulado últimos 14 días"){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Acumulado últimos 14 días</b>: {fallecidos_14}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = fallecidos_14, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}




################################################################################


#Grafico de positividad:-----
#Pensado para usarse con la funcion positividad()
plot_positividad <- function(base){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Positividad</b>: {paste0(round(positividad,2), '%')}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = positividad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Positividad (% de casos confirmados por casos posibles)") +
    theme(axis.title.y = element_text(size = 10))
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion positividad_r_edad()
plot_positividad_r_edad <- function(base){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Positividad</b>: {paste0(round(positividad,2), '%')}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = positividad, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Positividad (% de casos confirmados por casos posibles)") +
    scale_color_discrete(name = "Rango de edades") +
    theme(axis.title.y = element_text(size = 10))
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de letalidad:-----
#Pensado para usarse con la funcion letalidad()
plot_letalidad <- function(base){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Letalidad</b>: {paste0(round(letalidad, 2), '%')}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = letalidad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Letalidad (% de fallecidos por casos confirmados)") +
    theme(axis.title.y = element_text(size = 10))
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion letalidad_r_edad()
plot_letalidad_r_edad <- function(base){
  
  g <- base %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Letalidad</b>: {paste0(round(letalidad, 2), '%')}<br>") %>% 
             map(htmltools::HTML)) %>%
    ggplot(aes(x = fecha_min, y = letalidad, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Letalidad (% de fallecidos por casos confirmados)") +
    scale_color_discrete(name = "Rango de edades") +
    theme(axis.title.y = element_text(size = 10))
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}