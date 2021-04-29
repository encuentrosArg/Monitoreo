#library(tidyverse)
#library(plotly)

#Hay que definir un tema y aplicarlo a todos los graficos
#tema_encuentros <- theme().....

#A los graficos tambien hay que hacerlos funciones, despues se aplican en el shiny mas facil

#Grafico de casos diarios:-----
#Pensado para usarse con la funcion confirmados()
plot_confirmados <- function(base, eje_y = "Casos confirmados diarios"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de casos diarios por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad()
plot_confirmados_r_edad <- function(base, eje_y = "Casos confirmados diarios"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo, col = r_edad, text = label)) +
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
    ggplot(aes(x = fecha_min, y = acumulado, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de confirmados acumulados por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad_acumulados()
plot_confirmados_r_edad_acumulados <- function(base, eje_y = "Casos confirmados acumulados desde fecha inicial"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = acumulado, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de confirmados acumulados ultimos 14 dias:-----
#pensado para usarse con la funcion confirmados_acumulados_14()
plot_confirmados_acumulados_14 <- function(base, eje_y = "Casos confirmados acumulados últimos 14 días"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = acumulado_14, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de confirmados acumulados ultimos 14 dias por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad_acumulados_14()
plot_confirmados_r_edad_acumulados_14 <- function(base, eje_y = "Casos confirmados acumulados últimos 14 días"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = acumulado_14, col = r_edad, text = label)) +
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
    ggplot(aes(x = fecha_min, y = conteo, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
}

#Grafico de casos posibles diarios por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad()
plot_casos_posibles_r_edad <- function(base, eje_y = "Casos posibles diarios"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo, col = r_edad, text = label)) +
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
    ggplot(aes(x = fecha_min, y = acumulado, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de casos_posibles acumulados por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad_acumulados()
plot_casos_posibles_r_edad_acumulados <- function(base, eje_y = "Casos posibles acumulados desde fecha inicial"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = acumulado, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de casos_posibles acumulados ultimos 14 dias-----
#pensado para usarse con la funcion casos_posibles_acumulados_14()
plot_casos_posibles_acumulados_14 <- function(base, eje_y = "Casos posibles acumulados últimos 14 días"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = acumulado_14, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de casos_posibles acumulados ultimos 14 dias por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad_acumulados_14()
plot_casos_posibles_r_edad_acumulados_14 <- function(base, eje_y = "Casos posibles acumulados últimos 14 días"){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = acumulado_14, col = r_edad, text = label)) +
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
    ggplot(aes(x = fecha_fallecimiento, y = conteo, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad()
plot_fallecidos_r_edad <- function(base, eje_y = "Fallecidos diarios"){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = conteo, col = r_edad, text = label)) +
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
    ggplot(aes(x = fecha_fallecimiento, y = acumulado, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de fallecidos acumulados por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad_acumulados()
plot_fallecidos_r_edad_acumulados <- function(base, eje_y = "Fallecidos acumulados desde fecha inicial"){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = acumulado, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y) +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos acumulados ultimos 14 dias-----
#pensado para usarse con la funcion fallecidos_acumulados_14()
plot_fallecidos_acumulados_14 <- function(base, eje_y = "Fallecidos acumulados últimos 14 días"){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = acumulado_14, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = eje_y)
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos acumulados ultimos 14 dias por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad_acumulados_14()
plot_fallecidos_r_edad_acumulados_14 <- function(base, eje_y = "Fallecidos acumulados últimos 14 días"){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = acumulado_14, col = r_edad, text = label)) +
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
    ggplot(aes(x = fecha_min, y = positividad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Positividad (casos confirmado por cada caso posible)")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion positividad_r_edad()
plot_positividad_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = positividad, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Positividad (casos confirmado por cada caso posible)") +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de letalidad:-----
#Pensado para usarse con la funcion letalidad()
plot_letalidad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = letalidad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Letalidad (fallecidos por cada caso confirmado)")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion letalidad_r_edad()
plot_letalidad_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = letalidad, col = r_edad, text = label)) +
    geom_line() +
    labs(x = "Fecha", y = "Letalidad (fallecidos por cada caso confirmado)") +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}