#library(tidyverse)
#library(plotly)

#Hay que definir un tema y aplicarlo a todos los graficos
#tema_encuentros <- theme().....

#A los graficos tambien hay que hacerlos funciones, despues se aplican en el shiny mas facil

#Grafico de casos diarios:-----
#Pensado para usarse con la funcion confirmados()
plot_confirmados <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo)) +
    geom_line() +
    labs(x = "Fecha", y = "Casos confirmados")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}


#Grafico de casos diarios por rango de edad:-----
#pensado para usarse con la funcion confirmados_r_edad()
plot_confirmados_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo, col = r_edad)) +
    geom_line() +
    labs(x = "Fecha", y = "Casos confirmados") +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de casos posibles diarios:-----
#Pensado para usarse con la funcion casos_posibles()
plot_casos_posibles <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo)) +
    geom_line() +
    labs(x = "Fecha", y = "Casos posibles")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
}

#Grafico de casos posibles diarios por rango de edad:-----
#pensado para usarse con la funcion casos_posibles_r_edad()
plot_casos_posibles_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = conteo, col = r_edad)) +
    geom_line() +
    labs(x = "Fecha", y = "Casos posibles") +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios:-----
#Pensado para usarse con la funcion fallecidos()
plot_fallecidos <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = conteo)) +
    geom_line() +
    labs(x = "Fecha", y = "Fallecidos")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion fallecidos_r_edad()
plot_fallecidos_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = conteo, col = r_edad)) +
    geom_line() +
    labs(x = "Fecha", y = "Fallecidos") +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de positividad:-----
#Pensado para usarse con la funcion positividad()
plot_positividad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = positividad)) +
    geom_line() +
    labs(x = "Fecha", y = "Positividad (casos confirmado por cada caso posible)")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion positividad_r_edad()
plot_positividad_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_min, y = positividad, col = r_edad)) +
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
    ggplot(aes(x = fecha_fallecimiento, y = letalidad)) +
    geom_line() +
    labs(x = "Fecha", y = "Letalidad (fallecidos por cada caso confirmado)")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}

#Grafico de fallecidos diarios por rango de edad:-----
#pensado para usarse con la funcion letalidad_r_edad()
plot_letalidad_r_edad <- function(base){
  
  g <- base %>%
    ggplot(aes(x = fecha_fallecimiento, y = letalidad, col = r_edad)) +
    geom_line() +
    labs(x = "Fecha", y = "Letalidad (fallecidos por cada caso confirmado)") +
    scale_color_discrete(name = "Rango de edades")
  
  ggplotly(g, tooltip = 'text', dynamicTicks = TRUE) %>%
    plotly::config(locale = "es")
  
}