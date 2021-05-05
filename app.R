
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(plotly)
library(shinycssloaders)
library(dashboardthemes)

options(encoding = 'UTF-8')
options(spinner.color = "#000000", spinner.type = 6, spinner.color.background = "#ffffff", spinner.size = 0.5)

source("scripts/procesamiento.R", encoding = "UTF-8")
source("scripts/funciones de analisis.R", encoding = "UTF-8")
source("scripts/graficos.R", encoding = "UTF-8")


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


tabBox_todos <- function(titulo_tab_box){
  tabBox(width = 12,
         title = titulo_tab_box,
         tabPanel("Casos confirmados", 
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_confirmados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_confirmados_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                      align = "center",
                    "El gráfico de serie de tiempo muestra los casos confirmados por día. Al aumentar la curva se está detectando un aumento de casos para esa fecha en relación a la fecha anterior.")
                    ), br(),
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_confirmados_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_confirmados_r_edad_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "La curva va a presentar un comportamiento creciente a medida que los casos confirmados diarios aumenten, mientras que se tornará horizontal a medida que los casos confirmados comiencen a disminuir.")
                  ), br(),
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_confirmados_acumulados_14_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_confirmados_r_edad_acumulados_14_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "La curva se confecciona realizando un promedio de los nuevos casos confirmados diarios durante el lapso de tiempo de 14 días previos. Si la curva se mantiene constante o comienza a decrecer, indica que el número de individuos positivos detectados está disminuyendo.")
                  )
         ),
         tabPanel("Casos posibles", 
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_casos_posibles_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_casos_posibles_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "El gráfico muestra todos los posibles casos, sin importar el diagnóstico, por día. Si la curva aumenta, indica un aumento de los posibles casos para esa fecha en relación a la fecha anterior.")
                  ), br(),
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_casos_posibles_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_casos_posibles_r_edad_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)) , 
                    column(width = 12,
                           align = "center",
                           "La curva va a presentar un comportamiento creciente a medida que los casos posibles diarios aumenten, mientras que se tornará horizontal a medida que los casos posibles comiencen a disminuir.")
                  ), br(),
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_casos_posibles_acumulados_14_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_casos_posibles_r_edad_acumulados_14_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "La curva se confecciona realizando un promedio de los nuevos casos posibles diarios durante el lapso de tiempo de 14 días previos. Si la curva se mantiene constante o comienza a decrecer, indica que el número de casos posibles detectados está disminuyendo.")
                  )
         ),
         tabPanel("Fallecidos", 
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_fallecidos_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_fallecidos_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "El gráfico muestra la cantidad de fallecidos por día. Un aumento de la curva se traduce en una mayor cantidad de fallecidos para esa fecha en relación a la fecha anterior.")
                  ), br(),
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_fallecidos_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_fallecidos_r_edad_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "La curva va a presentar un comportamiento creciente a medida que los fallecidos diarios aumenten, mientras que se tornará horizontal a medida que los fallecidos comiencen a disminuir.")
                  ), br(),
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_fallecidos_acumulados_14_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_fallecidos_r_edad_acumulados_14_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "La curva se confecciona realizando un promedio de los nuevos fallecidos diarios durante el lapso de tiempo de 14 días previos. Si la curva se mantiene constante o comienza a decrecer, indica que el número de fallecidos registrados está disminuyendo.")
                  )
         ),
         tabPanel("Positividad", 
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_positividad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_positividad_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "Es la cantidad de casos positivos sobre el número de diagnósticos. Un valor alto (mayor al 20%) indicaría una tendencia al subdiagnóstico (se registran sólo una porción de los casos reales).")
                  )
         ),
         tabPanel("Letalidad", 
                  fluidRow(
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_letalidad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                    column(width = 6,
                           align = "center",
                           plotly::plotlyOutput(paste0('plot_letalidad_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),  
                    column(width = 12,
                           align = "center",
                           "La curva nos muestra la proporción de fallecidos acumulados respecto de los casos confirmados acumulados. Una tendencia creciente nos indicaría que el número de fallecidos está aumentando más rápido que el de casos confirmados o que hay subdiagnóstico.")
                  )
         )
  )}




header <- dashboardHeader(title = "Monitoreo COVID-19 CTERA",
                          dropdownMenu(
                            type = "messages",
                            messageItem(
                              from = "Admin",
                              message = "Página en desarrollo constante"
                            )
                          )
)



sidebar <- dashboardSidebar(
  tags$style(HTML(".datepicker {z-index:99999 !important;}")),
  sidebarMenu(id = "leftsidebar",
              menuItem("Argentina", tabName = "tab_argentina"#, icon = icon("argentina"),
              ),
              menuItem("Provincias", tabName = "tab_provincias" #icon = icon("argentina-dividida"),
              ),
              #menuItem("AMBA", tabName = "tab_amba" #,icon = icon("amba"),
              #         ),
              menuItem("Departamentos", tabName = "tab_departamentos" #icon = icon("departamentos"),
              ),
              
              dateRangeInput(inputId = 'input_fechas',
                             label = 'Seleccione rango de fechas',
                             min = min(datos$fecha_min, na.rm = TRUE),
                             max = lubridate::today(),
                             separator = 'a',
                             start = as.Date("2021-01-01"),
                             end = lubridate::today(),
                             language = "es",
                             weekstart = 1),
              menuItem("Información", tabName = "tab_info")
  )
)




body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  tabItems(
    tabItem(tabName = "tab_argentina",
            tabBox_todos("Argentina")
    ),
    tabItem(tabName = "tab_provincias",
            fluidRow(
              column(
                width = 12,
                align = "center",
                pickerInput("tab_prov_picker", "Seleccionar provincia", choices = as.character(sort(unique(cod_prov_depto$prov_name))), options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE)
              )
            ),
            tabBox_todos("Provincias")
    ),
    # tabItem(tabName = "tab_amba",
    #        tabBox_todos("AMBA")
    # ),
    tabItem(tabName = "tab_departamentos",
            fluidRow(
              column(
                width = 6,
                align = "center",
                pickerInput("tab_depto_prov_picker", "Seleccionar provincia", choices = as.character(sort(unique(cod_prov_depto$prov_name))), options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE),
              ),
              column(
                width = 6,
                align = "center",
                uiOutput('input_departamento')
              )
            ),
            tabBox_todos("Departamentos")
    ),
    tabItem(tabName = "tab_info",
            
            
            h2("Información sobre cálculos realizados"),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Casos confirmados:'),'Número de registros diarios con un diagnóstico positivo.'),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Casos posibles:'),'Número de registros diarios totales, sin importar el diagnóstico.'),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Fallecidos:'),'Número de registros diarios que presentaron una fecha de fallecimiento.'),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Acumulados desde fecha incial:'),'Suma de los registros diarios (confirmados, posibles o fallecidos) de todas las fechas anteriores desde una fecha inicial (elegida por el usuario en el selector de fechas) hasta una fecha posterior.'),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Acumulados últimos 14 días:'),'Suma de los registros diarios (confirmados, posibles o fallecidos) de los 14 días anteriores a la fecha de interés inclusive.'),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Positividad:'),HTML('Proporción del número de registros confirmados acumulados hasta la fecha en el número de registros totales acumulados hasta la fecha:<br> Casos confirmados acumulados / Casos posibles acumulados.')),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Letalidad:'),HTML('Proporción del número de registros fallecidos acumulados hasta la fecha del número de registros confirmados acumulados hasta la fecha:<br> Fallecidos acumulados / Casos confirmados acumulados.')),
            p(style = 'padding-left:15px; line-height: 1.2em;', strong('Rangos de edad:'),'Además, todas las métricas previamente mencionadas fueron calculadas para los rangos de edades de 0-17, 18-39, 40-59 y 60 o más años. Aquellos individuos clasificados como "N/R" no presentaron información sobre su edad')
            
            
    )
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      img(src='logo_ctera.png', align = "center")
    )
  ),
  fluidRow(
    column(
      width = 12,
      align = "left",
      p(style = 'padding-left:15px; line-height: 1.7em;', strong('Información:'),'Toda la información fue extraída de el Sistema Integrado de Información Sanatiaria Argentino (SISA), Ministerio de Salud de la Nación.'),
      p(style = 'padding-left:15px; line-height: 1.7em;','Siendo que la carga de datos al sistema puede demorar varios días, es posible que la información de los últimos 10 días a la fecha actual no esté actualizada.'),
      p(style = 'padding-left:15px; line-height: 1.7em;','La página está en desarrollo contínuo y es posible que se experimenten errores inesperados.')
    )
  )
)







ui <- dashboardPage(header, sidebar, body, title = "Monitoreo COVID-19 CTERA")


server <- function(input, output, session){
  
  #Defino los codigos de provincia y depto elegidos ----
  
  cod_prov_filtrado_tab_prov <- reactive({
    cod_prov_depto[prov_name == input$tab_prov_picker,] %>%
      arrange()
  })
  
  prov_code_tab_prov <- reactive({
    
    cod_prov_filtrado_tab_prov()$prov_code[1]
    
  })
  
  
  cod_prov_filtrado <- reactive({
    cod_prov_depto[prov_name == input$tab_depto_prov_picker,] %>%
      arrange()
  })
  
  prov_code <- reactive({
    
    cod_prov_filtrado()$prov_code[1]
    
  })
  
  output$input_departamento = renderUI({
    
    pickerInput("departamento", "Seleccionar departamento", choices = sort(as.character(cod_prov_filtrado()$name)), options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE)
    
  })
  
  depto_code <- reactive({
    
    cod_prov_filtrado()[name == input$departamento, codigo]
    
  })
  
  datos_reactive <- reactive({
    datos %>%
      filtro_fecha(fecha_inicial = as.Date(input$input_fechas[1]), fecha_final = as.Date(input$input_fechas[2]))
  })
  
  datos_fallecidos_reactive <- reactive({
    datos %>%
      filtro_fecha_fallecimiento(fecha_inicial = as.Date(input$input_fechas[1]), fecha_final = as.Date(input$input_fechas[2]))
  })
  
  # Hago reactivas las funciones definidas en procesamiento.R -----
  
  confirmados_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    if(input$leftsidebar == "tab_argentina"){
      datos_reactive() %>%
        confirmados()
    }else if(input$leftsidebar == "tab_provincias"){
      datos_reactive() %>%
        filtro_prov(prov = prov_code_tab_prov()) %>%
        confirmados()
    }else if(input$leftsidebar == "tab_amba"){
      datos_reactive() %>%
        filtro_region(base_codigos = cod_prov_depto, region_name = "AMBA") %>%
        confirmados()
    }else if(input$leftsidebar == "tab_departamentos"){
      validate(need(input$departamento != "", ""))
      datos_reactive() %>%
        filtro_prov(prov = prov_code()) %>%
        filtro_depto(depto = depto_code()) %>%
        confirmados()
    }
    
  })
  
  confirmados_acumulados_reactive <- reactive({
    confirmados_reactive() %>%
      confirmados_acumulados()
  })
  
  confirmados_acumulados_14_reactive <- reactive({
    confirmados_reactive() %>%
      confirmados_acumulados_14()
  })
  
  confirmados_r_edad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    if(input$leftsidebar == "tab_argentina"){
      datos_reactive() %>%
        confirmados_r_edad()
    }else if(input$leftsidebar == "tab_provincias"){
      datos_reactive() %>%
        filtro_prov(prov = prov_code_tab_prov()) %>%
        confirmados_r_edad()
    }else if(input$leftsidebar == "tab_amba"){
      datos_reactive() %>%
        filtro_region( base_codigos = cod_prov_depto, region_name = "AMBA") %>%
        confirmados_r_edad()
    }else if(input$leftsidebar == "tab_departamentos"){
      validate(need(input$departamento != "", ""))
      datos_reactive() %>%
        filtro_prov(prov = prov_code()) %>%
        filtro_depto(depto = depto_code()) %>%
        confirmados_r_edad()
    }
    
  })
  
  confirmados_r_edad_acumulados_reactive <- reactive({
    confirmados_r_edad_reactive() %>%
      confirmados_r_edad_acumulados()
  })
  
  confirmados_r_edad_acumulados_14_reactive <- reactive({
    confirmados_r_edad_reactive() %>%
      confirmados_r_edad_acumulados_14()
  })
  
  casos_posibles_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    if(input$leftsidebar == "tab_argentina"){
      datos_reactive() %>%
        casos_posibles()
    }else if(input$leftsidebar == "tab_provincias"){
      datos_reactive() %>%
        filtro_prov(prov = prov_code_tab_prov()) %>%
        casos_posibles()
    }else if(input$leftsidebar == "tab_amba"){
      datos_reactive() %>%
        filtro_region( base_codigos = cod_prov_depto, region_name = "AMBA") %>%
        casos_posibles()
    }else if(input$leftsidebar == "tab_departamentos"){
      validate(need(input$departamento != "", ""))
      datos_reactive() %>%
        filtro_prov(prov = prov_code()) %>%
        filtro_depto(depto = depto_code()) %>%
        casos_posibles()
    }
    
  })
  
  casos_posibles_acumulados_reactive <- reactive({
    casos_posibles_reactive() %>%
      casos_posibles_acumulados()
  })
  
  casos_posibles_acumulados_14_reactive <- reactive({
    casos_posibles_reactive() %>%
      casos_posibles_acumulados_14()
  })
  
  casos_posibles_r_edad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    if(input$leftsidebar == "tab_argentina"){
      datos_reactive() %>%
        casos_posibles_r_edad()
    }else if(input$leftsidebar == "tab_provincias"){
      datos_reactive() %>%
        filtro_prov(prov = prov_code_tab_prov()) %>%
        casos_posibles_r_edad()
    }else if(input$leftsidebar == "tab_amba"){
      datos_reactive() %>%
        filtro_region( base_codigos = cod_prov_depto, region_name = "AMBA") %>%
        casos_posibles_r_edad()
    }else if(input$leftsidebar == "tab_departamentos"){
      validate(need(input$departamento != "", ""))
      datos_reactive() %>%
        filtro_prov(prov = prov_code()) %>%
        filtro_depto(depto = depto_code()) %>%
        casos_posibles_r_edad()
    }
    
  })
  
  casos_posibles_r_edad_acumulados_reactive <- reactive({
    casos_posibles_r_edad_reactive() %>%
      casos_posibles_r_edad_acumulados()
  })
  
  casos_posibles_r_edad_acumulados_14_reactive <- reactive({
    casos_posibles_r_edad_reactive() %>%
      casos_posibles_r_edad_acumulados_14()
  })
  
  fallecidos_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    if(input$leftsidebar == "tab_argentina"){
      datos_fallecidos_reactive() %>%
        fallecidos()
    }else if(input$leftsidebar == "tab_provincias"){
      datos_fallecidos_reactive() %>%
        filtro_prov(prov = prov_code_tab_prov()) %>%
        fallecidos()
    }else if(input$leftsidebar == "tab_amba"){
      datos_fallecidos_reactive() %>%
        filtro_region( base_codigos = cod_prov_depto, region_name = "AMBA") %>%
        fallecidos()
    }else if(input$leftsidebar == "tab_departamentos"){
      validate(need(input$departamento != "", ""))
      datos_fallecidos_reactive() %>%
        filtro_prov(prov = prov_code()) %>%
        filtro_depto(depto = depto_code()) %>%
        fallecidos()
    }
    
  })
  
  fallecidos_acumulados_reactive <- reactive({
    fallecidos_reactive() %>%
      fallecidos_acumulados()
  })
  
  fallecidos_acumulados_14_reactive <- reactive({
    fallecidos_reactive() %>%
      fallecidos_acumulados_14()
  })
  
  fallecidos_r_edad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    if(input$leftsidebar == "tab_argentina"){
      datos_fallecidos_reactive() %>%
        fallecidos_r_edad()
    }else if(input$leftsidebar == "tab_provincias"){
      datos_fallecidos_reactive() %>%
        filtro_prov(prov = prov_code_tab_prov()) %>%
        fallecidos_r_edad()
    }else if(input$leftsidebar == "tab_amba"){
      datos_fallecidos_reactive() %>%
        filtro_region( base_codigos = cod_prov_depto, region_name = "AMBA") %>%
        fallecidos_r_edad()
    }else if(input$leftsidebar == "tab_departamentos"){
      validate(need(input$departamento != "", ""))
      datos_fallecidos_reactive() %>%
        filtro_depto(depto = depto_code()) %>%
        fallecidos_r_edad()
    }
    
  })
  
  fallecidos_r_edad_acumulados_reactive <- reactive({
    fallecidos_r_edad_reactive() %>%
      fallecidos_r_edad_acumulados()
  })
  
  fallecidos_r_edad_acumulados_14_reactive <- reactive({
    fallecidos_r_edad_reactive() %>%
      fallecidos_r_edad_acumulados_14()
  })
  
  positividad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    positividad(confirmados_reactive(), casos_posibles_reactive())
    
  })
  
  positividad_r_edad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    positividad_r_edad(confirmados_r_edad_reactive(), casos_posibles_r_edad_reactive())
    
  })
  
  letalidad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    letalidad(fallecidos_reactive(), confirmados_reactive())
    
  })
  
  letalidad_r_edad_reactive <- reactive({
    
    req(input$input_fechas)
    
    shiny::validate(
      need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
           "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
    )
    
    letalidad_r_edad(fallecidos_r_edad_reactive(), confirmados_r_edad_reactive())
    
  })
  
  
  ##############################################################################
  #################### GRAFICOS ################################################
  #############################################################################
  
  # Hago reactivos los graficos definidos en graficos.R ----
  
  ######################### Argentina #########################################  
  
  #### Confirmados ####
  
  output$plot_confirmados_Argentina <- renderPlotly({
    confirmados_reactive() %>%
      plot_confirmados()
  })
  
  output$plot_confirmados_r_edad_Argentina <- renderPlotly({
    confirmados_r_edad_reactive() %>%
      plot_confirmados_r_edad()
  })
  
  output$plot_confirmados_acumulados_Argentina <- renderPlotly({
    confirmados_acumulados_reactive() %>%
      plot_confirmados_acumulados()
  })
  
  output$plot_confirmados_r_edad_acumulados_Argentina <- renderPlotly({
    confirmados_r_edad_acumulados_reactive() %>%
      plot_confirmados_r_edad_acumulados()
  })
  
  output$plot_confirmados_acumulados_14_Argentina <- renderPlotly({
    confirmados_acumulados_14_reactive() %>%
      plot_confirmados_acumulados_14()
  })
  
  output$plot_confirmados_r_edad_acumulados_14_Argentina <- renderPlotly({
    confirmados_r_edad_acumulados_14_reactive() %>%
      plot_confirmados_r_edad_acumulados_14()
  })
  
  #### Casos posibles ####
  
  output$plot_casos_posibles_Argentina <- renderPlotly({
    casos_posibles_reactive() %>%
      plot_casos_posibles()
  })
  
  output$plot_casos_posibles_r_edad_Argentina <- renderPlotly({
    casos_posibles_r_edad_reactive() %>%
      plot_casos_posibles_r_edad()
  })
  
  output$plot_casos_posibles_acumulados_Argentina <- renderPlotly({
    casos_posibles_acumulados_reactive() %>%
      plot_casos_posibles_acumulados()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_Argentina <- renderPlotly({
    casos_posibles_r_edad_acumulados_reactive() %>%
      plot_casos_posibles_r_edad_acumulados()
  })
  
  output$plot_casos_posibles_acumulados_14_Argentina <- renderPlotly({
    casos_posibles_acumulados_14_reactive() %>%
      plot_casos_posibles_acumulados_14()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_14_Argentina <- renderPlotly({
    casos_posibles_r_edad_acumulados_14_reactive() %>%
      plot_casos_posibles_r_edad_acumulados_14()
  })
  
  #### Fallecidos ####
  
  output$plot_fallecidos_Argentina <- renderPlotly({
    fallecidos_reactive() %>%
      plot_fallecidos()
  })
  
  output$plot_fallecidos_r_edad_Argentina <- renderPlotly({
    fallecidos_r_edad_reactive() %>%
      plot_fallecidos_r_edad()
  })
  
  output$plot_fallecidos_acumulados_Argentina <- renderPlotly({
    fallecidos_acumulados_reactive() %>%
      plot_fallecidos_acumulados()
  })
  
  output$plot_fallecidos_r_edad_acumulados_Argentina <- renderPlotly({
    fallecidos_r_edad_acumulados_reactive() %>%
      plot_fallecidos_r_edad_acumulados()
  })
  
  output$plot_fallecidos_acumulados_14_Argentina <- renderPlotly({
    fallecidos_acumulados_14_reactive() %>%
      plot_fallecidos_acumulados_14()
  })
  
  output$plot_fallecidos_r_edad_acumulados_14_Argentina <- renderPlotly({
    fallecidos_r_edad_acumulados_14_reactive() %>%
      plot_fallecidos_r_edad_acumulados_14()
  })
  
  #### Positividad ####
  
  output$plot_positividad_Argentina <- renderPlotly({
    positividad_reactive() %>%
      plot_positividad()
  })
  
  output$plot_positividad_r_edad_Argentina <- renderPlotly({
    positividad_r_edad_reactive() %>%
      plot_positividad_r_edad()
  })
  
  #### Letalidad ####
  
  output$plot_letalidad_Argentina <- renderPlotly({
    letalidad_reactive() %>%
      plot_letalidad()
  })
  
  output$plot_letalidad_r_edad_Argentina <- renderPlotly({
    letalidad_r_edad_reactive() %>%
      plot_letalidad_r_edad()
  })
  
  ######################### Provincias #########################################  
  
  #### Confirmados ####
  
  output$plot_confirmados_Provincias <- renderPlotly({
    confirmados_reactive() %>%
      plot_confirmados()
  })
  
  output$plot_confirmados_r_edad_Provincias <- renderPlotly({
    confirmados_r_edad_reactive() %>%
      plot_confirmados_r_edad()
  })
  
  output$plot_confirmados_acumulados_Provincias <- renderPlotly({
    confirmados_acumulados_reactive() %>%
      plot_confirmados_acumulados()
  })
  
  output$plot_confirmados_r_edad_acumulados_Provincias <- renderPlotly({
    confirmados_r_edad_acumulados_reactive() %>%
      plot_confirmados_r_edad_acumulados()
  })
  
  output$plot_confirmados_acumulados_14_Provincias <- renderPlotly({
    confirmados_acumulados_14_reactive() %>%
      plot_confirmados_acumulados_14()
  })
  
  output$plot_confirmados_r_edad_acumulados_14_Provincias <- renderPlotly({
    confirmados_r_edad_acumulados_14_reactive() %>%
      plot_confirmados_r_edad_acumulados_14()
  })
  
  #### Casos posibles ####
  
  output$plot_casos_posibles_Provincias <- renderPlotly({
    casos_posibles_reactive() %>%
      plot_casos_posibles()
  })
  
  output$plot_casos_posibles_r_edad_Provincias <- renderPlotly({
    casos_posibles_r_edad_reactive() %>%
      plot_casos_posibles_r_edad()
  })
  
  output$plot_casos_posibles_acumulados_Provincias <- renderPlotly({
    casos_posibles_acumulados_reactive() %>%
      plot_casos_posibles_acumulados()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_Provincias <- renderPlotly({
    casos_posibles_r_edad_acumulados_reactive() %>%
      plot_casos_posibles_r_edad_acumulados()
  })
  
  output$plot_casos_posibles_acumulados_14_Provincias <- renderPlotly({
    casos_posibles_acumulados_14_reactive() %>%
      plot_casos_posibles_acumulados_14()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_14_Provincias <- renderPlotly({
    casos_posibles_r_edad_acumulados_14_reactive() %>%
      plot_casos_posibles_r_edad_acumulados_14()
  })
  
  #### Fallecidos ####
  
  output$plot_fallecidos_Provincias <- renderPlotly({
    fallecidos_reactive() %>%
      plot_fallecidos()
  })
  
  output$plot_fallecidos_r_edad_Provincias <- renderPlotly({
    fallecidos_r_edad_reactive() %>%
      plot_fallecidos_r_edad()
  })
  
  output$plot_fallecidos_acumulados_Provincias <- renderPlotly({
    fallecidos_acumulados_reactive() %>%
      plot_fallecidos_acumulados()
  })
  
  output$plot_fallecidos_r_edad_acumulados_Provincias <- renderPlotly({
    fallecidos_r_edad_acumulados_reactive() %>%
      plot_fallecidos_r_edad_acumulados()
  })
  
  output$plot_fallecidos_acumulados_14_Provincias <- renderPlotly({
    fallecidos_acumulados_14_reactive() %>%
      plot_fallecidos_acumulados_14()
  })
  
  output$plot_fallecidos_r_edad_acumulados_14_Provincias <- renderPlotly({
    fallecidos_r_edad_acumulados_14_reactive() %>%
      plot_fallecidos_r_edad_acumulados_14()
  })
  
  #### Positividad ####
  
  output$plot_positividad_Provincias <- renderPlotly({
    positividad_reactive() %>%
      plot_positividad()
  })
  
  output$plot_positividad_r_edad_Provincias <- renderPlotly({
    positividad_r_edad_reactive() %>%
      plot_positividad_r_edad()
  })
  
  #### Letalidad ####
  
  output$plot_letalidad_Provincias <- renderPlotly({
    letalidad_reactive() %>%
      plot_letalidad()
  })
  
  output$plot_letalidad_r_edad_Provincias <- renderPlotly({
    letalidad_r_edad_reactive() %>%
      plot_letalidad_r_edad()
  })
  
  ######################### AMBA #########################################  
  
  #### Confirmados ####
  
  output$plot_confirmados_AMBA <- renderPlotly({
    confirmados_reactive() %>%
      plot_confirmados()
  })
  
  output$plot_confirmados_r_edad_AMBA <- renderPlotly({
    confirmados_r_edad_reactive() %>%
      plot_confirmados_r_edad()
  })
  
  output$plot_confirmados_acumulados_AMBA <- renderPlotly({
    confirmados_acumulados_reactive() %>%
      plot_confirmados_acumulados()
  })
  
  output$plot_confirmados_r_edad_acumulados_AMBA <- renderPlotly({
    confirmados_r_edad_acumulados_reactive() %>%
      plot_confirmados_r_edad_acumulados()
  })
  
  output$plot_confirmados_acumulados_14_AMBA <- renderPlotly({
    confirmados_acumulados_14_reactive() %>%
      plot_confirmados_acumulados_14()
  })
  
  output$plot_confirmados_r_edad_acumulados_14_AMBA <- renderPlotly({
    confirmados_r_edad_acumulados_14_reactive() %>%
      plot_confirmados_r_edad_acumulados_14()
  })
  
  #### Casos posibles ####
  
  output$plot_casos_posibles_AMBA <- renderPlotly({
    casos_posibles_reactive() %>%
      plot_casos_posibles()
  })
  
  output$plot_casos_posibles_r_edad_AMBA <- renderPlotly({
    casos_posibles_r_edad_reactive() %>%
      plot_casos_posibles_r_edad()
  })
  
  output$plot_casos_posibles_acumulados_AMBA <- renderPlotly({
    casos_posibles_acumulados_reactive() %>%
      plot_casos_posibles_acumulados()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_AMBA <- renderPlotly({
    casos_posibles_r_edad_acumulados_reactive() %>%
      plot_casos_posibles_r_edad_acumulados()
  })
  
  output$plot_casos_posibles_acumulados_14_AMBA <- renderPlotly({
    casos_posibles_acumulados_14_reactive() %>%
      plot_casos_posibles_acumulados_14()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_14_AMBA <- renderPlotly({
    casos_posibles_r_edad_acumulados_14_reactive() %>%
      plot_casos_posibles_r_edad_acumulados_14()
  })
  
  #### Fallecidos ####
  
  output$plot_fallecidos_AMBA <- renderPlotly({
    fallecidos_reactive() %>%
      plot_fallecidos()
  })
  
  output$plot_fallecidos_r_edad_AMBA <- renderPlotly({
    fallecidos_r_edad_reactive() %>%
      plot_fallecidos_r_edad()
  })
  
  output$plot_fallecidos_acumulados_AMBA <- renderPlotly({
    fallecidos_acumulados_reactive() %>%
      plot_fallecidos_acumulados()
  })
  
  output$plot_fallecidos_r_edad_acumulados_AMBA <- renderPlotly({
    fallecidos_r_edad_acumulados_reactive() %>%
      plot_fallecidos_r_edad_acumulados()
  })
  
  output$plot_fallecidos_acumulados_14_AMBA <- renderPlotly({
    fallecidos_acumulados_14_reactive() %>%
      plot_fallecidos_acumulados_14()
  })
  
  output$plot_fallecidos_r_edad_acumulados_14_AMBA <- renderPlotly({
    fallecidos_r_edad_acumulados_14_reactive() %>%
      plot_fallecidos_r_edad_acumulados_14()
  })
  
  #### Positividad ####
  
  output$plot_positividad_AMBA <- renderPlotly({
    positividad_reactive() %>%
      plot_positividad()
  })
  
  output$plot_positividad_r_edad_AMBA <- renderPlotly({
    positividad_r_edad_reactive() %>%
      plot_positividad_r_edad()
  })
  
  #### Letalidad ####
  
  output$plot_letalidad_AMBA <- renderPlotly({
    letalidad_reactive() %>%
      plot_letalidad()
  })
  
  output$plot_letalidad_r_edad_AMBA <- renderPlotly({
    letalidad_r_edad_reactive() %>%
      plot_letalidad_r_edad()
  })
  
  ######################### Departamentos #########################################  
  
  #### Confirmados ####
  
  output$plot_confirmados_Departamentos <- renderPlotly({
    confirmados_reactive() %>%
      plot_confirmados()
  })
  
  output$plot_confirmados_r_edad_Departamentos <- renderPlotly({
    confirmados_r_edad_reactive() %>%
      plot_confirmados_r_edad()
  })
  
  output$plot_confirmados_acumulados_Departamentos <- renderPlotly({
    confirmados_acumulados_reactive() %>%
      plot_confirmados_acumulados()
  })
  
  output$plot_confirmados_r_edad_acumulados_Departamentos <- renderPlotly({
    confirmados_r_edad_acumulados_reactive() %>%
      plot_confirmados_r_edad_acumulados()
  })
  
  output$plot_confirmados_acumulados_14_Departamentos <- renderPlotly({
    confirmados_acumulados_14_reactive() %>%
      plot_confirmados_acumulados_14()
  })
  
  output$plot_confirmados_r_edad_acumulados_14_Departamentos <- renderPlotly({
    confirmados_r_edad_acumulados_14_reactive() %>%
      plot_confirmados_r_edad_acumulados_14()
  })
  
  #### Casos posibles ####
  
  output$plot_casos_posibles_Departamentos <- renderPlotly({
    casos_posibles_reactive() %>%
      plot_casos_posibles()
  })
  
  output$plot_casos_posibles_r_edad_Departamentos <- renderPlotly({
    casos_posibles_r_edad_reactive() %>%
      plot_casos_posibles_r_edad()
  })
  
  output$plot_casos_posibles_acumulados_Departamentos <- renderPlotly({
    casos_posibles_acumulados_reactive() %>%
      plot_casos_posibles_acumulados()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_Departamentos <- renderPlotly({
    casos_posibles_r_edad_acumulados_reactive() %>%
      plot_casos_posibles_r_edad_acumulados()
  })
  
  output$plot_casos_posibles_acumulados_14_Departamentos <- renderPlotly({
    casos_posibles_acumulados_14_reactive() %>%
      plot_casos_posibles_acumulados_14()
  })
  
  output$plot_casos_posibles_r_edad_acumulados_14_Departamentos <- renderPlotly({
    casos_posibles_r_edad_acumulados_14_reactive() %>%
      plot_casos_posibles_r_edad_acumulados_14()
  })
  
  #### Fallecidos ####
  
  output$plot_fallecidos_Departamentos <- renderPlotly({
    fallecidos_reactive() %>%
      plot_fallecidos()
  })
  
  output$plot_fallecidos_r_edad_Departamentos <- renderPlotly({
    fallecidos_r_edad_reactive() %>%
      plot_fallecidos_r_edad()
  })
  
  output$plot_fallecidos_acumulados_Departamentos <- renderPlotly({
    fallecidos_acumulados_reactive() %>%
      plot_fallecidos_acumulados()
  })
  
  output$plot_fallecidos_r_edad_acumulados_Departamentos <- renderPlotly({
    fallecidos_r_edad_acumulados_reactive() %>%
      plot_fallecidos_r_edad_acumulados()
  })
  
  output$plot_fallecidos_acumulados_14_Departamentos <- renderPlotly({
    fallecidos_acumulados_14_reactive() %>%
      plot_fallecidos_acumulados_14()
  })
  
  output$plot_fallecidos_r_edad_acumulados_14_Departamentos <- renderPlotly({
    fallecidos_r_edad_acumulados_14_reactive() %>%
      plot_fallecidos_r_edad_acumulados_14()
  })
  
  #### Positividad ####
  
  output$plot_positividad_Departamentos <- renderPlotly({
    positividad_reactive() %>%
      plot_positividad()
  })
  
  output$plot_positividad_r_edad_Departamentos <- renderPlotly({
    positividad_r_edad_reactive() %>%
      plot_positividad_r_edad()
  })
  
  #### Letalidad ####
  
  output$plot_letalidad_Departamentos <- renderPlotly({
    letalidad_reactive() %>%
      plot_letalidad()
  })
  
  output$plot_letalidad_r_edad_Departamentos <- renderPlotly({
    letalidad_r_edad_reactive() %>%
      plot_letalidad_r_edad()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

