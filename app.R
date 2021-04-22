
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(plotly)

options(encoding = 'UTF-8')

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







header <- dashboardHeader(
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Admin",
            message = "Proximamente se agregarán otros"
        )
    )
)


sidebar <- dashboardSidebar(
    sidebarMenu(
        pickerInput("provincia", "Seleccionar provincia", choices = as.character(sort(unique(cod_prov_depto$prov_name))), options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE),
        uiOutput('input_departamento'),
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
        
        
       # convertMenuItem(tabName = "tab1", menuItem("Concurrencias", tabName = "concurrencias", icon = icon("glass-cheers"),
        #                                           pickerInput("ciudad", "Seleccionar ciudad", choices = c("Pinamar", "Ushuaia"), selected = "Pinamar", options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE),
         #                                          uiOutput('pickerLugar'),
          #                                         uiOutput('checkDia'),
           #                                        sliderTextInput(inputId = "hora", label = "Seleccionar intervalo de horas del día", choices = 0:23, selected = c(0, 23)))
        #),
        


body <- dashboardBody(
                tabBox(width = 12,
                       title = "Nombre tabBox",
                       tabPanel("Casos confirmados", 
                                fluidRow(
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_confirmados')),
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_confirmados_r_edad'))  
                                )
                       ),
                       tabPanel("Casos posibles", 
                                fluidRow(
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_casos_posibles')),
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_casos_posibles_r_edad'))  
                                )
                        ),
                       tabPanel("Fallecidos", 
                                fluidRow(
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_fallecidos')),
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_fallecidos_r_edad'))  
                                )
                       ),
                       tabPanel("Positividad", 
                                fluidRow(
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_positividad')),
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_positividad_r_edad'))  
                                )
                       ),
                       tabPanel("Letalidad", 
                                fluidRow(
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_letalidad')),
                                    column(width = 6,
                                           align = "center",
                                           plotly::plotlyOutput('plot_letalidad_r_edad'))  
                                )
                       )
                       )
                )
                                
                       



ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session){
    
    #Defino los codigos de provincia y depto elegidos ----
    
    cod_prov_filtrado <- reactive({
        
        cod_prov_depto[prov_name == input$provincia,] %>%
            arrange(name)
        
    })
    
    prov_code <- reactive({
        
        cod_prov_filtrado()$prov_code[1]
        
    })
    
    output$input_departamento = renderUI({
        
        pickerInput("departamento", "Seleccionar departamento", choices = as.character(cod_prov_filtrado()$name), options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE)
        
    })
    
    depto_code <- reactive({
      
        
        cod_prov_filtrado()[name == input$departamento, codigo]
        
    })
    
    # Hago reactivas las funciones definidas en procesamiento.R -----
    
    confirmados_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        confirmados(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
          
    })
    
    confirmados_r_edad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        confirmados_r_edad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    casos_posibles_reactive <- reactive({
        
        req(datos,
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        casos_posibles(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    casos_posibles_r_edad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        casos_posibles_r_edad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    fallecidos_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        fallecidos(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    fallecidos_r_edad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        fallecidos_r_edad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    positividad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        positividad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    positividad_r_edad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        positividad_r_edad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    letalidad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        letalidad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    letalidad_r_edad_reactive <- reactive({
        
        req(
            prov_code(),
            depto_code(),
            input$input_fechas
        )
        
        shiny::validate(
            need(as.Date(input$input_fechas[1]) <= as.Date(input$input_fechas[2]),
                 "La fecha final no puede ser anterior a la fecha inicial\nSeleccione un rango de fechas válido")
        )
        
        letalidad_r_edad(datos, prov_code(), depto_code(),  as.Date(input$input_fechas[1]), as.Date(input$input_fechas[2]))
        
    })
    
    # Hago reactivos los graficos definidos en graficos.R ----
    
    output$plot_confirmados <- renderPlotly({
        confirmados_reactive() %>%
            plot_confirmados()
    })
    
    output$plot_confirmados_r_edad <- renderPlotly({
        confirmados_r_edad_reactive() %>%
            plot_confirmados_r_edad()
    })
    
    output$plot_casos_posibles <- renderPlotly({
        casos_posibles_reactive() %>%
            plot_casos_posibles()
    })
    
    output$plot_casos_posibles_r_edad <- renderPlotly({
        casos_posibles_r_edad_reactive() %>%
            plot_casos_posibles_r_edad()
    })
    
    output$plot_fallecidos <- renderPlotly({
        fallecidos_reactive() %>%
            plot_fallecidos()
    })
    
    output$plot_fallecidos_r_edad <- renderPlotly({
        fallecidos_r_edad_reactive() %>%
            plot_fallecidos_r_edad()
    })
    
    output$plot_positividad <- renderPlotly({
        positividad_reactive() %>%
            plot_positividad()
    })
    
    output$plot_positividad_r_edad <- renderPlotly({
        positividad_r_edad_reactive() %>%
            plot_positividad_r_edad()
    })
    
    output$plot_letalidad <- renderPlotly({
        letalidad_reactive() %>%
            plot_letalidad()
    })
    
    output$plot_letalidad_r_edad <- renderPlotly({
        letalidad_r_edad_reactive() %>%
            plot_letalidad_r_edad()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

