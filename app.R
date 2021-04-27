
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
                                         plotly::plotlyOutput(paste0('plot_confirmados_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                ),
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_confirmados_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_confirmados_r_edad_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                )
                       ),
                       tabPanel("Casos posibles", 
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_casos_posibles_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_casos_posibles_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                ),
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_casos_posibles_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_casos_posibles_r_edad_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                )
                       ),
                       tabPanel("Fallecidos", 
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_fallecidos_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_fallecidos_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                ),
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_fallecidos_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_fallecidos_r_edad_acumulados_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                )
                       ),
                       tabPanel("Positividad", 
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_positividad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_positividad_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                )
                       ),
                       tabPanel("Letalidad", 
                                fluidRow(
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_letalidad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE)),
                                  column(width = 6,
                                         align = "center",
                                         plotly::plotlyOutput(paste0('plot_letalidad_r_edad_', titulo_tab_box))%>% withSpinner(hide.ui = FALSE))  
                                )
                       )
)}




header <- dashboardHeader(title = "Monitoreo COVID-19 hasta donde llega este texto quiero saber",
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Admin",
            message = "Proximamente se agregarán otros"
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
                       weekstart = 1)
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
            )
    ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      img(src='logo_cotera.png', align = "center")
      )
    )
  )
                
                
                                
                       



ui <- dashboardPage(header, sidebar, body, title = "Monitoreo COVID-19 COTERA")


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
    
    # Hago reactivos los graficos definidos en graficos.R ----
    #Para argentina:
    
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
        plot_confirmados(eje_y = "Casos confirmados acumulados")
    })
    
    output$plot_confirmados_r_edad_acumulados_Argentina <- renderPlotly({
      confirmados_r_edad_acumulados_reactive() %>%
        plot_confirmados_r_edad(eje_y = "Casos confirmados acumulados")
    })
    
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
        plot_casos_posibles(eje_y = "Casos posibles acumulados")
    })
    
    output$plot_casos_posibles_r_edad_acumulados_Argentina <- renderPlotly({
      casos_posibles_r_edad_acumulados_reactive() %>%
        plot_casos_posibles_r_edad(eje_y = "Casos posibles acumulados")
    })
    
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
        plot_fallecidos(eje_y = "Fallecidos acumulados")
    })
    
    output$plot_fallecidos_r_edad_acumulados_Argentina <- renderPlotly({
      fallecidos_r_edad_acumulados_reactive() %>%
        plot_fallecidos_r_edad(eje_y = "Fallecidos acumulados")
    })
    
    output$plot_positividad_Argentina <- renderPlotly({
      positividad_reactive() %>%
        plot_positividad()
    })
    
    output$plot_positividad_r_edad_Argentina <- renderPlotly({
      positividad_r_edad_reactive() %>%
        plot_positividad_r_edad()
    })
    
    output$plot_letalidad_Argentina <- renderPlotly({
      letalidad_reactive() %>%
        plot_letalidad()
    })
    
    output$plot_letalidad_r_edad_Argentina <- renderPlotly({
      letalidad_r_edad_reactive() %>%
        plot_letalidad_r_edad()
    })
    
    #Para provincias:
    
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
        plot_confirmados()
    })
    
    output$plot_confirmados_r_edad_acumulados_Provincias <- renderPlotly({
      confirmados_r_edad_acumulados_reactive() %>%
        plot_confirmados_r_edad()
    })
    
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
        plot_casos_posibles()
    })
    
    output$plot_casos_posibles_r_edad_acumulados_Provincias <- renderPlotly({
      casos_posibles_r_edad_acumulados_reactive() %>%
        plot_casos_posibles_r_edad()
    })
    
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
        plot_fallecidos()
    })
    
    output$plot_fallecidos_r_edad_acumulados_Provincias <- renderPlotly({
      fallecidos_r_edad_acumulados_reactive() %>%
        plot_fallecidos_r_edad()
    })
    
    output$plot_positividad_Provincias <- renderPlotly({
      positividad_reactive() %>%
        plot_positividad()
    })
    
    output$plot_positividad_r_edad_Provincias <- renderPlotly({
      positividad_r_edad_reactive() %>%
        plot_positividad_r_edad()
    })
    
    output$plot_letalidad_Provincias <- renderPlotly({
      letalidad_reactive() %>%
        plot_letalidad()
    })
    
    output$plot_letalidad_r_edad_Provincias <- renderPlotly({
      letalidad_r_edad_reactive() %>%
        plot_letalidad_r_edad()
    })
    
    #Para AMBA:
    
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
        plot_confirmados()
    })
    
    output$plot_confirmados_r_edad_acumulados_AMBA <- renderPlotly({
      confirmados_r_edad_acumulados_reactive() %>%
        plot_confirmados_r_edad()
    })
    
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
        plot_casos_posibles()
    })
    
    output$plot_casos_posibles_r_edad_acumulados_AMBA <- renderPlotly({
      casos_posibles_r_edad_acumulados_reactive() %>%
        plot_casos_posibles_r_edad()
    })
    
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
        plot_fallecidos()
    })
    
    output$plot_fallecidos_r_edad_acumulados_AMBA <- renderPlotly({
      fallecidos_r_edad_acumulados_reactive() %>%
        plot_fallecidos_r_edad()
    })
    
    output$plot_positividad_AMBA <- renderPlotly({
      positividad_reactive() %>%
        plot_positividad()
    })
    
    output$plot_positividad_r_edad_AMBA <- renderPlotly({
      positividad_r_edad_reactive() %>%
        plot_positividad_r_edad()
    })
    
    output$plot_letalidad_AMBA <- renderPlotly({
      letalidad_reactive() %>%
        plot_letalidad()
    })
    
    output$plot_letalidad_r_edad_AMBA <- renderPlotly({
      letalidad_r_edad_reactive() %>%
        plot_letalidad_r_edad()
    })
    
    #Para deptos:
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
        plot_confirmados()
    })
    
    output$plot_confirmados_r_edad_acumulados_Departamentos <- renderPlotly({
      confirmados_r_edad_acumulados_reactive() %>%
        plot_confirmados_r_edad()
    })
    
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
        plot_casos_posibles()
    })
    
    output$plot_casos_posibles_r_edad_acumulados_Departamentos <- renderPlotly({
      casos_posibles_r_edad_acumulados_reactive() %>%
        plot_casos_posibles_r_edad()
    })
    
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
        plot_fallecidos()
    })
    
    output$plot_fallecidos_r_edad_acumulados_Departamentos <- renderPlotly({
      fallecidos_r_edad_acumulados_reactive() %>%
        plot_fallecidos_r_edad()
    })
    
    output$plot_positividad_Departamentos <- renderPlotly({
      positividad_reactive() %>%
        plot_positividad()
    })
    
    output$plot_positividad_r_edad_Departamentos <- renderPlotly({
      positividad_r_edad_reactive() %>%
        plot_positividad_r_edad()
    })
    
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

