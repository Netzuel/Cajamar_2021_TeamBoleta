# TeamBoleta! returns

library(shiny)
library(shiny.router)
library(geojsonio)
library(highcharter)
library(shinyjs)
library(tidyverse)
library(shinycssloaders)
library(shinyBS)
library(r2d3)
library(shinyWidgets)
library(jsonlite)
library(taRifx)

comment_title <- function(...) {
  span(class="comment-title", ...)
}

comentario <- function(...) {
  HTML(...)
}

map_spain <- geojson_read("data/simple_spain.geojson")

mapa_europa <- read_json("data/europe.geo.json")

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

load("data/mercas_data.RData")

dataset4 <- read_csv("data/dataset4.csv")
dataset5 <- read_csv("data/dataset5.csv")

spain <- highchart(type="map") %>%
    hc_add_series_map(map_spain, 
                      data.frame(ccaa=c("Andalucía"), casos = c(1)), 
                      "casos", c("NAME", "ccaa"), 
                      nullColor="#F0F0F0", borderColor="#fff",
                      borderWidth=2) %>%
    hc_tooltip(headerFormat="<b>{point.point.NAME}</b><br>",
               pointFormat="Casos: <b>{point.value}</b>")

europe <- highchart(type="map") %>%
  hc_add_series_map(mapa_europa, 
                    data.frame(pais=c("Denmark"), casos = c(1)), 
                    "casos", c("name", "pais"), 
                    nullColor="#F0F0F0", borderColor="#fff",
                    borderWidth=2) %>%
  hc_tooltip(headerFormat="<b>{point.point.name}</b><br>",
             pointFormat="Casos: <b>{point.value}</b>")

valueBox <- function(value, subtitle, color) {
  div(class = "col-lg-12 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = ("col-xs-11 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          value
                      ),
                      div(subtitle)
                  )
              )
          )
      )
  )
}

valueBox2 <- function(value, subtitle, color) {
  div(class = "col-lg-12 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = ("col-xs-11 text-right"),
                      div(style = ("font-size: 39px; font-weight: bold;"),
                          value
                      ),
                      div(subtitle)
                  )
              )
          )
      )
  )
}

main_container <- function(...){
    div(class="main-container",
        ...
    )
}

eco_toggle <- function(id){
    HTML(sprintf('<label class="toggleButton">
            <input id="%s" type="checkbox">
            <div>
                <svg viewBox="0 0 44 44">
                    <path d="M14,24 L21,31 L39.7428882,11.5937758 C35.2809627,
                    6.53125861 30.0333333,4 24,4 C12.95,4 4,12.95 4,24 C4,35.05 
                    12.95,44 24,44 C35.05,44 44,35.05 44,24 C44,19.3 42.5809627,
                    15.1645919 39.7428882,11.5937758" 
                    transform="translate(-2.000000, -2.000000)"></path>
                </svg>
            </div>
        </label>
     ', id)
    )
}

eco_tooltip <- bsTooltip("eco_box", 
                         'Modo <b>ECO</b>: Todas las visualizaciones y cálculos que presenten información <b>únicamente</b> sobre agricultura ecológica, se mostrarán junto con el icono: 🍃</i>',
                         "left", 
                         "hover", 
                         options = NULL)

fluidRow()

home_page <- div(class="container_text",
                 HTML("<h1>¡Bienvenid@ al <strong>Reto Cajamar Agro Analysis</strong>!</h1><br><p>En esta aplicación web tenemos como propósito realizar un análisis del precio de las <strong>hortalizas</strong> y <strong>frutas</strong> a nivel nacional y ver cómo este ha sido afectado por la pandemia del <strong>COVID-19</strong>.</p><p>Para realizar este estudio disponemos de distintas bases de datos proporcionadas desde diferentes fuentes. Por ello, y antes de que explores más nuestra web, te aconsejamos que pierdas unos minutos en tener unas primeras pinceladas con los datos que hemos utilizado. Para ello hemos habilitado un desplegable justo aquí abajo.</p><p><strong>¡Empecemos!</strong></p>"),
                 tags$br(),
                 fluidRow(
                   column(4, offset = 4,
                          selectInput(inputId = "dataset_info", label = "", choices = c("- - -", "Ministerio de Pesca, Agricultura y Alimentación", "Junta de Andalucía", "MercaMadrid y MercaBarna", "Comercio exterior"), selected = "- - -", width = "100%")
                   )
                 ),
                 shinyjs::hidden(
                   div(id = "bloque_AED",
                       fluidRow(
                         column(12,
                                uiOutput("output_AED")
                         )
                       )
                   )
                 )
)

exterior_page <- div(class="container_text",
                     tags$h1("Datos sobre Comercio Exterior"),
                     HTML("<p>En esta pestaña nuestro propósito es mostrar, de forma interactiva, todo lo relacionado con la <strong>exportación</strong> e <strong>importación</strong> del resto de países de <strong>Europa</strong> con <strong>España</strong>.</p>"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              selectInput(inputId = "dataset4_producto_input", label = "Producto:", choices = unique(dataset4$Producto), selected = unique(dataset4$Producto)[1], width = "100%")
                       ),
                       column(2,
                              radioButtons(inputId = "exportacion_importacion", label = "Tipo:", choices = c("Importación", "Exportación"), inline = FALSE)
                       ),
                       column(2,
                              radioButtons(inputId = "cantidad_valor", label = "Variable a visualizar:", choices = c("Cantidad", "Valor (€)"), inline = FALSE)
                       )
                     ),
                     tags$br(),
                     tabsetPanel(
                       tabPanel("Visualización anual",
                        tags$br(),
                        fluidRow(
                          div(id = "mostrar_por_year",
                              column(1,
                                     selectInput(inputId = "dataset4_producto_year_input", label = "Año:", choices = unique(dataset4$Year), selected = unique(dataset4$Year)[1], width = "100%")
                              )
                          ),
                          div(id = "mostrar_por_mes",
                              column(2,
                                     selectInput(inputId = "dataset4_producto_month_input", label = "Mes:", choices = meses, selected = meses[1], width = "100%")
                              )
                          )
                        ),
                        tags$br(),
                        fluidRow(
                          column(2,
                                 checkboxInput("mostrar_year", "Mostrar por año", FALSE)
                          )
                        ),
                        tags$br(),
                        fluidRow(
                          div(id = "mapa_exterior",
                              column(6,
                                     highchartOutput("hc_exterior") %>% withSpinner(type = 6, color = "green")
                              ),
                              column(6,
                                     highchartOutput("hc_covid") %>% withSpinner(type = 6, color = "green")
                              )
                          )
                        )
                       ),
                       tabPanel("Comparativa anual",
                                tags$br(),
                                fluidRow(
                                  column(4,
                                         highchartOutput("hc_exterior_2018") %>% withSpinner(type = 6, color = "green")
                                  ),
                                  column(4,
                                         highchartOutput("hc_exterior_2019") %>% withSpinner(type = 6, color = "green")
                                  ),
                                  column(4,
                                         highchartOutput("hc_exterior_2020") %>% withSpinner(type = 6, color = "green")
                                  )
                                ),
                                tags$br(),
                                fluidRow(
                                  column(4,
                                         uiOutput("box_2018")
                                  ),
                                  column(4,
                                         uiOutput("box_2019")
                                  ),
                                  column(4,
                                         uiOutput("box_2020")
                                  )
                                )
                       )
                     )
                     
)

exterior_server <- function(input, output) {
  
  dataset4_color <- reactiveValues(color = "#0070FF")
  
  output$box_2018 <- renderUI({
    valueBox2(value = textOutput("text_2018"),
              subtitle = uiOutput("subtext_2018"),
              color = dataset4_color$color)
  })
  
  output$box_2019 <- renderUI({
    valueBox2(value = textOutput("text_2019"),
              subtitle = uiOutput("subtext_2019"),
              color = dataset4_color$color)
  })
  
  output$box_2020 <- renderUI({
    valueBox2(value = textOutput("text_2020"),
              subtitle = uiOutput("subtext_2020"),
              color = dataset4_color$color)
  })
  
  observeEvent(input$modo_eco, {
    if(input$modo_eco == TRUE){
      dataset4_color$color <- "#6ab04c"
    }
    else{
      dataset4_color$color <- "#0070FF"
    }
  }, once = FALSE)
  
  observeEvent(input$mostrar_year, {
    if(input$mostrar_year == TRUE){
      shinyjs::hide("mostrar_por_mes")
    }
    else{
      shinyjs::show("mostrar_por_mes")
    }
  }, once = FALSE)
  
  output$text_2018 <- renderText({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2018")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    data$Cantidad <- data$Cantidad/1000
    data$pais_es <- data$Emisor
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    
    data <- data[-which(data$Emisor == "EU"),]
    
    if(input$cantidad_valor == "Cantidad"){
      return(paste0(data$pais_es[which.max(data$Cantidad)]))
    }
    else{
      return(paste0(data$pais_es[which.max(data$Valor)]))
    }
  })
  
  output$text_2019 <- renderText({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2019")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    data$Cantidad <- data$Cantidad/1000
    data$pais_es <- data$Emisor
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    
    data <- data[-which(data$Emisor == "EU"),]
    
    if(input$cantidad_valor == "Cantidad"){
      return(paste0(data$pais_es[which.max(data$Cantidad)]))
    }
    else{
      return(paste0(data$pais_es[which.max(data$Valor)]))
    }
  })
  
  output$text_2020 <- renderText({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2020")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    data$Cantidad <- data$Cantidad/1000
    data$pais_es <- data$Emisor
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    
    data <- data[-which(data$Emisor == "EU"),]
    
    if(input$cantidad_valor == "Cantidad"){
      return(paste0(data$pais_es[which.max(data$Cantidad)]))
    }
    else{
      return(paste0(data$pais_es[which.max(data$Valor)]))
    }
  })
  
  output$subtext_2018 <- renderText({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2018")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    data$Cantidad <- data$Cantidad/1000
    data$pais_es <- data$Emisor
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    
    data <- data[-which(data$Emisor == "EU"),]
    
    if(input$cantidad_valor == "Cantidad"){
      if(input$exportacion_importacion == "Importación"){
        return(paste0("ha sido, en 2018, el país con mayor volumen de importaciones con España y <strong>", data$pais_es[which.min(data$Cantidad)], "</strong> el que menos."))
      }
      else{
        return(paste0("ha sido, en 2018, el país con mayor volumen de exportaciones con España y <strong>", data$pais_es[which.min(data$Cantidad)], "</strong> el que menos."))
      }
    }
    else{
      if(input$exportacion_importacion == "Exportación"){
        return(paste0("ha sido, en 2018, el país con mayor valor en € en importaciones con España y <strong>", data$pais_es[which.min(data$Valor)], "</strong> el que menos."))
      }
      else{
        return(paste0("ha sido, en 2018, el país con mayor valor en € en exportaciones con España y <strong>", data$pais_es[which.min(data$Valor)], "</strong> el que menos."))
      }
    }
  })
  
  output$subtext_2019 <- renderText({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2019")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    data$Cantidad <- data$Cantidad/1000
    data$pais_es <- data$Emisor
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    
    data <- data[-which(data$Emisor == "EU"),]
    
    if(input$cantidad_valor == "Cantidad"){
      if(input$exportacion_importacion == "Importación"){
        return(paste0("ha sido, en 2019, el país con mayor volumen de importaciones con España y <strong>", data$pais_es[which.min(data$Cantidad)], "</strong> el que menos."))
      }
      else{
        return(paste0("ha sido, en 2019, el país con mayor volumen de exportaciones con España y <strong>", data$pais_es[which.min(data$Cantidad)], "</strong> el que menos."))
      }
    }
    else{
      if(input$exportacion_importacion == "Exportación"){
        return(paste0("ha sido, en 2019, el país con mayor valor en € en importaciones con España y <strong>", data$pais_es[which.min(data$Valor)], "</strong> el que menos."))
      }
      else{
        return(paste0("ha sido, en 2019, el país con mayor valor en € en exportaciones con España y <strong>", data$pais_es[which.min(data$Valor)], "</strong> el que menos."))
      }
    }
  })
  
  output$subtext_2020 <- renderText({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2020")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    data$Cantidad <- data$Cantidad/1000
    data$pais_es <- data$Emisor
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    
    data <- data[-which(data$Emisor == "EU"),]
    
    if(input$cantidad_valor == "Cantidad"){
      if(input$exportacion_importacion == "Importación"){
        return(paste0("ha sido, en 2020, el país con mayor volumen de importaciones con España y <strong>", data$pais_es[which.min(data$Cantidad)], "</strong> el que menos."))
      }
      else{
        return(paste0("ha sido, en 2020, el país con mayor volumen de exportaciones con España y <strong>", data$pais_es[which.min(data$Cantidad)], "</strong> el que menos."))
      }
    }
    else{
      if(input$exportacion_importacion == "Exportación"){
        return(paste0("ha sido, en 2020, el país con mayor valor en € en importaciones con España y <strong>", data$pais_es[which.min(data$Valor)], "</strong> el que menos."))
      }
      else{
        return(paste0("ha sido, en 2020, el país con mayor valor en € en exportaciones con España y <strong>", data$pais_es[which.min(data$Valor)], "</strong> el que menos."))
      }
    }
  })
  
  output$hc_exterior <- renderHighchart({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    req(input$dataset4_producto_year_input)
    
    if(input$mostrar_year == FALSE){
      data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == input$dataset4_producto_year_input, Month == input$dataset4_producto_month_input)
      
      pointFormat <- paste0("{point.value}", " €")
    }
    else{
      req(input$dataset4_producto_month_input)
      data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == input$dataset4_producto_year_input)
      data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
      
      pointFormat <- paste0("{point.value}", " €")
    }
    
    if(input$modo_eco == TRUE){
      minColor <- "#83FF83"
      maxColor <- "#0B780B"
    }
    else{
      minColor <- "#3CA5F7"
      maxColor <- "#020290"
    }
    
    if(input$cantidad_valor == "Cantidad"){
      data <- data.frame(pais = data$Emisor, cantidad = data$Cantidad/1000)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "cantidad", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat = "<b>{point.point.pais_es}</b><br>",
                   pointFormat = paste0("{point.value}", " t")) %>%
        hc_title(text = "Comercio exterior")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
    else{
      data <- data.frame(pais = data$Emisor, valor = data$Valor)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "valor", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat="<b>{point.point.pais_es}</b><br>",
                   pointFormat = pointFormat) %>%
        hc_title(text = "Comercio exterior")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
  })
  
  output$hc_covid <- renderHighchart({
    data <- dataset5 %>% dplyr::filter(year == "2020")
    data <- data %>% dplyr::select(c("countriesAndTerritories", "cases"))
    data <- data %>% group_by(countriesAndTerritories) %>% dplyr::summarize(value = sum(cases))
    data <- data.frame(pais = data$countriesAndTerritories, value = data$value)
    
    unique_countries <- c()
    for(i in seq_along(mapa_europa$features)){
      unique_countries <- c(unique_countries, mapa_europa$features[[i]]$properties$name)
    }
    
    data <- data[data$pais %in% unique_countries,]
    
    data$pais_es <- data$pais
    for(i in seq_len(nrow(data))){
      if(data$pais[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$pais[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$pais[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$pais[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$pais[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$pais[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$pais[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$pais[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$pais[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$pais[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$pais[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$pais[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$pais[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$pais[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$pais[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$pais[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$pais[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$pais[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$pais[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$pais[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$pais[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
      else if(data$pais[i] == "Spain"){
        data$pais_es[i] <- "España"
      }
      else if(data$pais[i] == "Turkey"){
        data$pais_es[i] <- "Turquía"
      }
      else if(data$pais[i] == "Ukraine"){
        data$pais_es[i] <- "Ucrania"
      }
      else if(data$pais[i] == "Switzerland"){
        data$pais_es[i] <- "Suiza"
      }
      else if(data$pais[i] == "Iceland"){
        data$pais_es[i] <- "Islandia"
      }
      else if(data$pais[i] == "Norway"){
        data$pais_es[i] <- "Noruega"
      }
      else if(data$pais[i] == "Russia"){
        data$pais_es[i] <- "Rusia"
      }
    }
    
    mapa <- highchart(type="map") %>%
      hc_add_series_map(mapa_europa, 
                        data, 
                        "value", c("name", "pais"), 
                        nullColor="#868a86", borderColor="#fff",
                        borderWidth=2) %>%
      hc_tooltip(headerFormat = "<b>{point.point.pais_es}</b><br>",
                 pointFormat = paste0("{point.value}", " casos")) %>%
      hc_title(text = paste0("Casos totales desde ", min(dataset5$dateRep), " hasta ", max(dataset5$dateRep)))
    
    mapa <- hc_colorAxis(mapa, minColor = "#FFB2B2", maxColor = "#FF0000")
  })
  
  output$hc_exterior_2018 <- renderHighchart({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2018")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    
    pointFormat <- paste0("{point.value}", " €")
    
    if(input$modo_eco == TRUE){
      minColor <- "#83FF83"
      maxColor <- "#0B780B"
    }
    else{
      minColor <- "#3CA5F7"
      maxColor <- "#020290"
    }
    
    if(input$cantidad_valor == "Cantidad"){
      data <- data.frame(pais = data$Emisor, cantidad = data$Cantidad/1000)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "cantidad", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat = "<b>{point.point.pais_es}</b><br>",
                   pointFormat = paste0("{point.value}", " t")) %>%
        hc_title(text = "2018")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
    else{
      data <- data.frame(pais = data$Emisor, valor = data$Valor)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "valor", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat="<b>{point.point.pais_es}</b><br>",
                   pointFormat = pointFormat) %>%
        hc_title(text = "2018")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
  })
  
  output$hc_exterior_2019 <- renderHighchart({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2019")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    
    pointFormat <- paste0("{point.value}", " €")
    
    if(input$modo_eco == TRUE){
      minColor <- "#83FF83"
      maxColor <- "#0B780B"
    }
    else{
      minColor <- "#3CA5F7"
      maxColor <- "#020290"
    }
    
    if(input$cantidad_valor == "Cantidad"){
      data <- data.frame(pais = data$Emisor, cantidad = data$Cantidad/1000)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "cantidad", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat = "<b>{point.point.pais_es}</b><br>",
                   pointFormat = paste0("{point.value}", " t")) %>%
        hc_title(text = "2019")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
    else{
      data <- data.frame(pais = data$Emisor, valor = data$Valor)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "valor", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat="<b>{point.point.pais_es}</b><br>",
                   pointFormat = pointFormat) %>%
        hc_title(text = "2019")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
  })
  
  output$hc_exterior_2020 <- renderHighchart({
    req(input$dataset4_producto_input)
    req(input$exportacion_importacion)
    req(input$cantidad_valor)
    
    data <- dataset4 %>% dplyr::filter(Producto == input$dataset4_producto_input, Tipo == input$exportacion_importacion, Year == "2020")
    data <- data %>% group_by(Emisor) %>% dplyr::summarize(Cantidad = sum(Cantidad), Valor = sum(Valor))
    
    pointFormat <- paste0("{point.value}", " €")
    
    if(input$modo_eco == TRUE){
      minColor <- "#83FF83"
      maxColor <- "#0B780B"
    }
    else{
      minColor <- "#3CA5F7"
      maxColor <- "#020290"
    }
    
    if(input$cantidad_valor == "Cantidad"){
      data <- data.frame(pais = data$Emisor, cantidad = data$Cantidad/1000)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "cantidad", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat = "<b>{point.point.pais_es}</b><br>",
                   pointFormat = paste0("{point.value}", " t")) %>%
        hc_title(text = "2020")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
    else{
      data <- data.frame(pais = data$Emisor, valor = data$Valor)
      data$pais_es <- data$pais
      for(i in seq_len(nrow(data))){
        if(data$pais[i] == "Belgium"){
          data$pais_es[i] <- "Bélgica"
        }
        else if(data$pais[i] == "Croatia"){
          data$pais_es[i] <- "Croacia"
        }
        else if(data$pais[i] == "Cyprus"){
          data$pais_es[i] <- "Chipre"
        }
        else if(data$pais[i] == "Czechia"){
          data$pais_es[i] <- "Chequia"
        }
        else if(data$pais[i] == "Denmark"){
          data$pais_es[i] <- "Dinamarca"
        }
        else if(data$pais[i] == "Finland"){
          data$pais_es[i] <- "Finlandia"
        }
        else if(data$pais[i] == "France"){
          data$pais_es[i] <- "Francia"
        }
        else if(data$pais[i] == "Germany"){
          data$pais_es[i] <- "Alemania"
        }
        else if(data$pais[i] == "Greece"){
          data$pais_es[i] <- "Grecia"
        }
        else if(data$pais[i] == "Hungary"){
          data$pais_es[i] <- "Hungría"
        }
        else if(data$pais[i] == "Ireland"){
          data$pais_es[i] <- "Irlanda"
        }
        else if(data$pais[i] == "Italy"){
          data$pais_es[i] <- "Italia"
        }
        else if(data$pais[i] == "Latvia"){
          data$pais_es[i] <- "Letonia"
        }
        else if(data$pais[i] == "Lithuania"){
          data$pais_es[i] <- "Lituania"
        }
        else if(data$pais[i] == "Luxembourg"){
          data$pais_es[i] <- "Luxemburgo"
        }
        else if(data$pais[i] == "Netherlands"){
          data$pais_es[i] <- "Países Bajos"
        }
        else if(data$pais[i] == "Poland"){
          data$pais_es[i] <- "Polonia"
        }
        else if(data$pais[i] == "Romania"){
          data$pais_es[i] <- "Rumanía"
        }
        else if(data$pais[i] == "Slovakia"){
          data$pais_es[i] <- "Eslovaquia"
        }
        else if(data$pais[i] == "Slovenia"){
          data$pais_es[i] <- "Eslovenia"
        }
        else if(data$pais[i] == "Sweden"){
          data$pais_es[i] <- "Suecia"
        }
      }
      
      mapa <- highchart(type="map") %>%
        hc_add_series_map(mapa_europa, 
                          data, 
                          "valor", c("name", "pais"), 
                          nullColor="#868a86", borderColor="#fff",
                          borderWidth=2) %>%
        hc_tooltip(headerFormat="<b>{point.point.pais_es}</b><br>",
                   pointFormat = pointFormat) %>%
        hc_title(text = "2020")
      
      mapa <- hc_colorAxis(mapa, minColor = minColor, maxColor = maxColor)
      
      return(mapa)
    }
  })
  
}

consumo_page <- div(class="container_text",
                    tags$h1("Datos del MAPA"),
                    HTML("<p>En esta parte se hará uso de los datos proporcionados por el <strong>MAPA</strong> (Ministerio de Agricultura, Pesca y Alimentación) para llevar a cabo una pequeña exploratoria de los datos, así como una interfaz interactiva para visualizar, tanto mediante gráficas ordinarias como mediante mapas, las diferentes variables cuantitativas que tenemos a nuestra disposición.</p>"),
                    tags$br(),
                    tabsetPanel(
                      tabPanel("Gráfica",
                               tags$br(),
                               fluidRow(
                                 div(id = "column1_consumo",
                                     column(4,
                                       uiOutput("box_1")
                                     )
                                 ),
                                 
                                 div(id = "column2_consumo",
                                     column(4,
                                       uiOutput("box_2")
                                     )
                                 ),
                                 
                                 div(id = "column3_consumo",
                                     column(4,
                                       uiOutput("box_3")
                                     )
                                 )
                                 
                               ),
                               fluidRow(
                                 div(id = "column7_consumo",
                                     column(3,
                                            checkboxInput("agruparyears", "¿Agrupar por años?", FALSE)
                                     )
                                 )
                               ),
                               fluidRow(
                                 div(id = "column4_consumo",
                                     column(3,
                                            uiOutput("select_producto")
                                     )
                                 ),
                                 div(id = "column5_consumo",
                                     column(3,
                                            uiOutput("select_ccaa")
                                     )
                                 ),
                                 div(id = "column6_consumo",
                                     column(3,
                                            uiOutput("select_variable")
                                     )
                                 )
                               ),
                               fluidRow(
                                 div(id = "grafico_consumo",
                                     column(12,
                                            highchartOutput("highchartconsumo") %>% withSpinner(type = 6, color = "green")
                                     )
                                 )
                               ),
                               tags$br(),
                               fluidRow(
                                 shinyjs::hidden(
                                   div(id = "texto_consumo",
                                     HTML("<p>Se puede observar que para los productos en general se presenta una <strong>cierta estacionalidad</strong> en los diferentes años. Además, en líneas generales la serie correspondiente a <strong>2020</strong> suele estar por encima respecto a los valores de <strong>2018</strong> y <strong>2019</strong>. Por tanto, se puede concluir que la pandemia ha provocado un aumento generalizado del consumo de diferentes <strong>frutas</strong> y <strong>hortalizas</strong> en nuestro país.</p>")
                                   )
                                 )
                               )
                      ),
                      tabPanel("Mapa",
                               tags$br(),
                               fluidRow(
                                 div(id = "column8_consumo",
                                     column(3,
                                            uiOutput("select_fecha_mapas")
                                     )
                                 ),
                                 div(id = "column9_consumo",
                                     column(3,
                                            uiOutput("select_producto_mapas")
                                     )
                                 ),
                                 div(id = "column10_consumo",
                                     column(3,
                                            uiOutput("select_variable_mapas")
                                     )
                                 )
                               ),
                               tags$br(),
                               fluidRow(
                                 div(id = "column11_consumo",
                                     column(12,
                                            highchartOutput("highchart_mapa") %>% withSpinner(type = 6, color = "green")
                                     )
                                 )
                               )
                      )
                    )
)

mercas_page <- div(id = "mercas_page", class = "container_text",
                   tags$h1("Datos de MercaMadrid y MercaBarna"),
                   HTML("<p><strong>MercaMadrid</strong> y <strong>MercaBarna</strong> son dos de los mercados mayoristas más importantes en España, los cuales hemos querido estudiar en esta parte. Se ha realizado un cálculo del <strong>volumen</strong> de toneladas comercializadas, tanto de forma global como centrándonos en el periodo de <strong>pandemia</strong>.</p>"),
                   tags$br(),
  tabsetPanel(type="pills",
              tabPanel("Serie completa",
                       div(style="width: 50%; display: block;",
                           d3Output("mercas_streamgraph", height="450px"),
                           comment_title("¿Cómo ha evolucionado el volumen de productos en las mercas?"),
                           comentario("<p class='comment'>En 2020 se observa un apreciable ensanchamiento del volumen de productos 
                           hortofrutícolas comercializados en MercaMadrid. <b>¿Efecto COVID?</b></p>")
                       ),
                       div(style="width: 10%;"),
                       div(style="width: 40%; display: block; height: 525px; margin-top: 3%",
                           class="mercas-variedad",
                           uiOutput("mercas_variedad_ui"),
                           uiOutput("mercas_ui_year"),
                           uiOutput("mercas_ui_season"),
                           d3Output("mercas_map", height="300px")
                       )
              ),
              tabPanel("Pandemia",
                       selectInput("mercas_pandemia_variedad", "Producto",
                                   choices=mercas_variedades_list),
                       radioGroupButtons("mercas_pandemia_variable",
                                         "Variable",
                                         choices=c("Volumen" = "volumen",
                                                   "Precio" = "price")),
                       highchartOutput("mercas_pandemia_lines", height="200px"),
                       highchartOutput("mercas_pandemia_var", height="200px")
              )
  )
)

junta_andalucia <- div(class = "container_text",
    tags$h1("Datos de la Junta de Andalucía"),
    tags$p("En este apartado se utilizan los datos agroalimentarios otorgados por la Junta de Andalucía para poder estudiar la evolución del precio del kilogramo (€/kg) de los respectivos productos."),
    tags$br(),
    fluidRow(
        div(id = "column1",
            column(2,
                uiOutput("select_sector")
            )
        ),
        div(id = "column2",
            column(3,
                uiOutput("select_subsector")
            )
        ),
        div(id = "column3",
            column(2,
                   uiOutput("select_product")
            )
        ),
        div(id = "column6",
            column(3,
                   uiOutput("selectinput_1")
            )
        ),
        shinyjs::hidden(
            div(id = "column4",
                column(2,
                    uiOutput("emoji")
                )
            )
        )
    ),
    fluidRow(
      shinyjs::hidden(
        div(id = "column16",
            column(2,
                   uiOutput("select_sector2")
            )
        )
      ),
      shinyjs::hidden(
        div(id = "column17",
            column(3,
                   uiOutput("select_subsector2")
            )
        )
      ),
      shinyjs::hidden(
        div(id = "column18",
            column(2,
                   uiOutput("select_product2")
            )
        )
      ),
      shinyjs::hidden(
        div(id = "column19",
            column(3,
                   uiOutput("selectinput_2")
            )
        )
      ),
      shinyjs::hidden(
        div(id = "column20",
            column(2,
                   uiOutput("emoji2")
            )
        )
      )
    ),
    fluidRow(
      div(id = "column8",
          column(2,
                 checkboxInput("checkbox_sector", "Promediar por sector", value = FALSE, width = NULL)
          )
      ),
      div(id = "column9",
          column(3,
                 checkboxInput("checkbox_subsector", "Promediar por subsector", value = FALSE, width = NULL)
          )
      ),
      div(id = "column14",
          column(3,
                 actionButton("add_second", "Añadir línea")
          )
      ),
      shinyjs::hidden(
        div(id = "column15",
            column(3,
                   actionButton("delete_second", "Quitar línea")
            )
        )
      )
    ),
    fluidRow(
      shinyjs::hidden(
        div(id = "column7",
            column(12,
                   HTML("<h4 style='color:red'>Para mostrar por <strong>Agricultor</strong> y <strong>Subasta</strong> se ha realizado un promedio semanal.</h4>")
            )
        )
      )
    ),
    tags$br(),
    fluidRow(
        tabsetPanel(
          tabPanel("Visualización global",
                   tags$br(),
                   div(id = "column5",
                       column(12,
                              highchartOutput("grafica_1") %>% withSpinner(type = 6, color = "green")
                       )
                   )
          ),
          tabPanel("Comparativa anual",
                   tags$br(),
                   fluidRow(
                     div(id = "column11",
                         column(2,
                                selectInput(inputId = "series_1", label = "Serie 1:", choices = c("2018", "2019", "2020"), width = "100%", selected = "2020")
                         )
                     ),
                     div(id = "column12",
                         column(2,
                                selectInput(inputId = "series_2", label = "Serie 2:", choices = c("- - -", "2018", "2019", "2020"), width = "100%")
                         )
                     ),
                     div(id = "column13",
                         column(2,
                                selectInput(inputId = "series_3", label = "Serie 3:", choices = c("- - -", "2018", "2019", "2020"), width = "100%")
                         )
                     )
                   ),
                   fluidRow(
                     div(id = "column21",
                         column(2,
                                selectInput(inputId = "fecha_1_input", label = "Desde:", choices = c("- - -", meses), width = "100%")
                         ),
                         column(2,
                                uiOutput("fecha_2")
                         ),
                         shinyjs::hidden(
                           div(id = "column22",
                               column(2,
                                      HTML("<h4 style='color:red'><strong>Al seleccionar el mismo mes para el principio y el final del periodo se presentan únicamente los valores para dicho mes.</strong></h4>")
                               )
                           )
                         )
                     )
                   ),
                   fluidRow(
                     div(id = "column23",
                         column(3,
                                checkboxInput("checkbox_pandemia", "Centrar en meses de pandemia", value = FALSE, width = NULL)
                         )
                     )
                   ),
                   fluidRow(
                     div(id = "column24",
                         column(12,
                                HTML("<h4 style='color:black'>El periodo de <strong>pandemia</strong> se define como el periodo transcurrido entre <strong>01/03/2020</strong> y el <strong>30/11/2020</strong>. No obstante será posible realizar la comparación entre años para estos meses en concreto.</h4>")
                         )
                     )
                   ),
                   fluidRow(
                     div(id = "column_analisis",
                         column(12,
                                uiOutput("texto_analisis")
                         )
                     )
                   ),
                   tags$br(),
                   div(id = "column10",
                       column(12,
                              highchartOutput("grafica_2") %>% withSpinner(type = 6, color = "green")
                       )
                   )
          ),
          tabPanel("Correlación pandemia",
                   tags$br(),
                   fluidRow(
                     column(2,
                            uiOutput("variable_pandemia")
                     ),
                     column(2,
                            uiOutput("desde_select")
                     ),
                     column(2,
                            uiOutput("hasta_select")
                     )
                   ),
                   fluidRow(
                     column(12,
                            uiOutput("texto_correlacion")
                     )
                   ),
                   tags$br(),
                   fluidRow(
                     column(6,
                            highchartOutput("grafica_precio_pandemia") %>% withSpinner(type = 6, color = "green")
                     ),
                     column(6,
                            highchartOutput("grafica_pandemia") %>% withSpinner(type = 6, color = "green")
                     )
                   )
          )
        )
    ),
    tags$br()
)

home_server <- function(input, output, session) {
  hchart.cor <- function(object, ...) {
    
    df <- as.data.frame(object)
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round, 2)
    dist <- NULL
    
    x <- y <- names(df)
    
    df <- tbl_df(cbind(x = y, df)) %>% 
      gather(y, dist, -x) %>% 
      mutate(x = as.character(x),
             y = as.character(y)) %>% 
      left_join(data_frame(x = y,
                           xid = seq(length(y)) - 1), by = "x") %>% 
      left_join(data_frame(y = y,
                           yid = seq(length(y)) - 1), by = "y")
    
    ds <- df %>% 
      select_("xid", "yid", "dist") %>% 
      list_parse2()
    
    fntltp <- JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                         this.series.yAxis.categories[this.point.y] + ': <b>' +
                         Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
    cor_colr <- dataset1_color$list_color
    
    highchart() %>% 
      hc_chart(type = "heatmap") %>% 
      hc_xAxis(categories = y, title = NULL) %>% 
      hc_yAxis(categories = y, title = "") %>% 
      hc_add_series(data = ds) %>% 
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )) %>% 
      hc_tooltip(formatter = fntltp) %>% 
      hc_legend(align = "right", layout = "vertical",
                margin = 0, verticalAlign = "top",
                y = 25, symbolHeight = 280) %>% 
      hc_colorAxis(  stops= cor_colr,min=-1,max=1) %>%
      hc_title(text = "Correlaciones entre las variables")
  }
  
  dataset1 <- read_csv("data/dataset1.csv")
  
  dataset2 <- read.csv("data/dataset2.csv")
  
  dataset1_color <- reactiveValues(color = "#0070FF")
  dataset1_color$list_color <- list( list(0, '#FF5733'),
                                     list(0.5, '#F8F5F5'),
                                     list(1, '#2E86C1')
  )
  
  dataset2_grupo <- read_csv("data/dataset2_grupo.csv")
  dataset2_grupo_color <- reactiveValues(list_color = c("#0070FF", "#4ecef5"))
  
  dataset2_sector <- read_csv("data/dataset2_sector.csv")
  dataset2_sector_color <- reactiveValues(list_color = c("#0070FF", "#4ecef5", "#2297ba", "#2686e0"))
  
  dataset2_subsector <- read_csv("data/dataset2_subsector.csv")
  dataset2_subsector_color <- reactiveValues(list_color = c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb"))
  
  dataset2_producto <- read_csv("data/dataset2_producto.csv")
  colores_producto_base <- c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb", "#234663")
  colores_producto <- c()
  
  dataset2_posicion <- read_csv("data/dataset2_posicion.csv")
  dataset2_posicion_color <- reactiveValues(list_colores = c("#0070FF", "#4ecef5", "#2297ba"))
  
  for(i in seq_along(unique(dataset2$producto))){
    colores_producto <- c(colores_producto, sample(colores_producto_base, 1))
  }
  dataset2_producto_color <- reactiveValues(list_color = colores_producto)
  
  dataset3_a <- read_csv("data/dataset3_a.csv")
  
  dataset3_a_familia <- read_csv("data/dataset3_a_familia.csv")
  dataset3_a_familia_color <- reactiveValues(list_color = c("#0070FF", "#4ecef5", "#2297ba", "#2686e0"))
  
  dataset3_b <- read_csv("data/dataset3_b.csv")
  
  dataset3_b_familia <- read_csv("data/dataset3_b_familia.csv")
  
  colores_producto <- c()
  for(i in seq_along(unique(dataset3_b$Familia))){
    colores_producto <- c(colores_producto, sample(colores_producto_base, 1))
  }
  
  dataset3_b_familia_color <- reactiveValues(list_color = colores_producto)
  
  output$output_AED <- renderUI({
    if(input$dataset_info == "Ministerio de Pesca, Agricultura y Alimentación"){
      div(id = "dataset_1",
          tags$br(),
          fluidRow(
            column(12,
                   HTML("<p>Esta base de datos proviene del <strong>Ministerio de Agricultura, Pesca y Alimentación</strong> y contiene información mensual sobre el consumo alimentario en nuestro país, tanto a nivel de comunidad autónoma como nacional. Todos los datos recogidos en ella han sido recogidos a partir de encuestas realizadas entre los consumidores y los responsables de compras. De esta manera disponemos mensualmente de los datos del <strong>volumen en miles de kg</strong> y su <strong>precio medio por kg</strong>, del <strong>valor en miles de €</strong>, entre otros. Estos datos se reparten, además, tanto a nivel nacional como a nivel de comunidad autónoma.</p>")
            )
          ),
          tags$br(),
          fluidRow(
            column(3,
                   selectInput(inputId = "select_dataset1", label = "Variable a explorar:", choices = names(dataset1)[-c(1,2,3,4)])
            ),
            column(9,
                   fluidRow(
                     column(4,
                            valueBox2(value = textOutput("text_max"),
                                     subtitle = uiOutput("subtext_max"),
                                     color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("text_min"),
                                     subtitle = uiOutput("subtext_min"),
                                     color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("text_media"),
                                      subtitle = uiOutput("subtext_media"),
                                      color = dataset1_color$color)
                     )
                   ),
                   fluidRow(
                     column(4,
                            valueBox2(value = textOutput("text_mediana"),
                                     subtitle = uiOutput("subtext_mediana"),
                                     color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("text_na"),
                                     subtitle = uiOutput("subtext_na"),
                                     color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("text_filas"),
                                      subtitle = uiOutput("subtext_filas"),
                                      color = dataset1_color$color)
                     )
                   )
            )
          ),
          tags$br(),
          fluidRow(
            column(12,
                   highchartOutput("highchart_correlacion") %>% withSpinner(type = 6, color = "green")
            )
          )
      )
    }
    else if(input$dataset_info == "Junta de Andalucía"){
      div(id = "dataset_2",
          tags$br(),
          fluidRow(
            column(12,
                   HTML("<p>Esta base de datos es cortesía del <strong>Observatorio de Precios de los Alimentos de la Junta de Andalucía</strong>. Gracias a estos datos objetivos hemos tenido acceso al precio (€/kg) de gran cantidad de <strong>productos</strong>, así como <strong>sectores</strong> y <strong>subsectores</strong>. Además, esta base de datos contiene información separada sobre agricultura ordinaria y agricultura ecológica (<strong>agricultura ECO</strong>), razón por la cual se ha podido considerar un estudio separado de ambas.</p>")
            )
          ),
          tags$br(),
          fluidRow(
            column(12,
                   fluidRow(
                     column(4,
                            valueBox2(value = textOutput("precio_max"),
                                      subtitle = uiOutput("subprecio_max"),
                                      color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("precio_min"),
                                      subtitle = uiOutput("subprecio_min"),
                                      color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("precio_media"),
                                      subtitle = uiOutput("subprecio_media"),
                                      color = dataset1_color$color)
                     )
                   ),
                   fluidRow(
                     column(4,
                            valueBox2(value = textOutput("precio_mediana"),
                                      subtitle = uiOutput("subprecio_mediana"),
                                      color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("precio_na"),
                                      subtitle = uiOutput("subprecio_na"),
                                      color = dataset1_color$color)
                     ),
                     column(4,
                            valueBox2(value = textOutput("text_filas2"),
                                      subtitle = uiOutput("subtext_filas2"),
                                      color = dataset1_color$color)
                     )
                   )
            )
          ),
          tags$br(),
          fluidRow(
            column(6,
                   highchartOutput("highchart_grupo") %>% withSpinner(type = 6, color = "green")
            ),
            column(6,
                   highchartOutput("highchart_sector") %>% withSpinner(type = 6, color = "green")
            )
          ),
          tags$br(),
          fluidRow(
            column(12,
                   highchartOutput("highchart_subsector") %>% withSpinner(type = 6, color = "green")
            )
          ),
          tags$br(),
          fluidRow(
            column(6,
                   highchartOutput("highchart_product") %>% withSpinner(type = 6, color = "green")
            ),
            column(6,
                   highchartOutput("highchart_posicion") %>% withSpinner(type = 6, color = "green")
            )
          )
      )
    }
    else if(input$dataset_info == "MercaMadrid y MercaBarna"){
      div(id = "dataset_3",
          tags$br(),
          HTML("<p>Esta base de datos contiene información sobre <strong>MercaMadrid</strong> y <strong>MercaBarna</strong>, los cuales son dos de los <strong>mercados mayoristas</strong> más importantes de nuestro país. Tenemos a nuestra disposición datos sobre el precio (€/kg) y el volumen (miles de kg) de productos comercializados en sendos mercados, así como información sobre su <strong>variedad</strong> y su origen.</p>"),
          tags$br(),
          tabsetPanel(
            tabPanel("MercaMadrid",
              tags$br(),
              fluidRow(
                column(3,
                  selectInput(inputId = "select_dataset3_a", label = "Variable numérica a explorar:", choices = c("Precio medio", "Precio mínimo", "Precio máximo", "Volumen"))
                ),
                column(9,
                       fluidRow(
                         column(4,
                                valueBox2(value = textOutput("text_max_3a"),
                                          subtitle = uiOutput("subtext_max_3a"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_min_3a"),
                                          subtitle = uiOutput("subtext_min_3a"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_media_3a"),
                                          subtitle = uiOutput("subtext_media_3a"),
                                          color = dataset1_color$color)
                         )
                       ),
                       fluidRow(
                         column(4,
                                valueBox2(value = textOutput("text_mediana_3a"),
                                          subtitle = uiOutput("subtext_mediana_3a"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_na_3a"),
                                          subtitle = uiOutput("subtext_na_3a"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_filas_3a"),
                                          subtitle = uiOutput("subtext_filas_3a"),
                                          color = dataset1_color$color)
                         )
                       )
                )
              ),
              tags$br(),
              fluidRow(
                column(6,
                       selectInput(inputId = "select_dataset3a_producto", label = "Producto a explorar:", choices = unique(dataset3_a$Producto))
                ),
                column(6,
                       uiOutput("select_dataset3a_variedad")
                )
              ),
              tags$br(),
              fluidRow(
                column(6,
                  highchartOutput("highchart_origen_producto") %>% withSpinner(type = 6, color = "green")
                ),
                column(6,
                  highchartOutput("highchart_origen_variedad") %>% withSpinner(type = 6, color = "green")
                )
              ),
              tags$br(),
              fluidRow(
                column(6,
                       valueBox2(value = textOutput("text_max_origen"),
                                 subtitle = uiOutput("subtext_max_origen"),
                                 color = dataset1_color$color)
                ),
                column(6,
                       valueBox2(value = textOutput("text_max_variedad"),
                                 subtitle = uiOutput("subtext_max_variedad"),
                                 color = dataset1_color$color)
                )
              ),
              tags$br(),
              fluidRow(
                column(12,
                       highchartOutput("highchart_familia") %>% withSpinner(type = 6, color = "green")
                )
              )
            ),
            tabPanel("MercaBarna",
              tags$br(),
              fluidRow(
                column(3,
                       selectInput(inputId = "select_dataset3_b", label = "Variable numérica a explorar:", choices = c("Precio medio", "Volumen"))
                ),
                column(9,
                       fluidRow(
                         column(4,
                                valueBox2(value = textOutput("text_max_3b"),
                                          subtitle = uiOutput("subtext_max_3b"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_min_3b"),
                                          subtitle = uiOutput("subtext_min_3b"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_media_3b"),
                                          subtitle = uiOutput("subtext_media_3b"),
                                          color = dataset1_color$color)
                         )
                       ),
                       fluidRow(
                         column(4,
                                valueBox2(value = textOutput("text_mediana_3b"),
                                          subtitle = uiOutput("subtext_mediana_3b"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_na_3b"),
                                          subtitle = uiOutput("subtext_na_3b"),
                                          color = dataset1_color$color)
                         ),
                         column(4,
                                valueBox2(value = textOutput("text_filas_3b"),
                                          subtitle = uiOutput("subtext_filas_3b"),
                                          color = dataset1_color$color)
                         )
                       )
                )
              ),
              tags$br(),
              fluidRow(
                column(6,
                       selectInput(inputId = "select_dataset3b_producto", label = "Producto a explorar:", choices = unique(dataset3_b$Producto))
                )
              ),
              tags$br(),
              fluidRow(
                column(6,
                       highchartOutput("highchart_origen_producto_3b") %>% withSpinner(type = 6, color = "green")
                ),
                column(6,
                       highchartOutput("highchart_familia_3b") %>% withSpinner(type = 6, color = "green")
                )
              ),
              tags$br(),
              fluidRow(
                column(6,
                       valueBox2(value = textOutput("text_max_origen_3b"),
                                 subtitle = uiOutput("subtext_max_origen_3b"),
                                 color = dataset1_color$color)
                ),
                column(6,
                       valueBox2(value = textOutput("text_max_familia_3b"),
                                 subtitle = uiOutput("subtext_max_familia_3b"),
                                 color = dataset1_color$color)
                )
              ),
            )
          )
      )
    }
    else if(input$dataset_info == "Comercio exterior"){
      div(id = "dataset_4",
        tags$br(),
        HTML("<p>Esta base de datos tiene como objetivo recoger todos los datos referentes al <strong>comercio exterior</strong>, lo cual nos servirá para poder realizar comparativas anuales tanto del <strong>volumen</strong> (o cantidad) de kilogramos comercializados así como del <strong>valor</strong> importado o exportado por los diferentes países con respecto a España.</p>"),
        tags$br(),
        fluidRow(
          column(2,
                 selectInput(inputId = "select_tipo", label = "Tipo de comercio:", choices = c("Importación", "Exportación"), selected = "Importación", width = "100%")
          ),
          column(1,
                 selectInput(inputId = "select_year_tipo", label = "Año:", choices = c("2018", "2019", "2020"))
          )
        ),
        fluidRow(
          column(6,
                 highchartOutput("pie_exterior") %>% withSpinner(type = 6, color = "green")
          ),
          column(4, offset = 2,
                 valueBox2(value = sum(is.na(dataset4)),
                           subtitle = "valores perdidos",
                           color = color()),
                 valueBox2(value = nrow(dataset4),
                           subtitle = "instancias únicas",
                           color = color()),
                 valueBox2(value = ncol(dataset4),
                           subtitle = "variables diferentes",
                           color = color())
          )
        )
      )
    }
  })
  
  color <- reactive({
    if(input$modo_eco == TRUE){
      color <- "#6ab04c"
    }
    else{
      color <- "#0070FF"
    }
    
    return(color)
  })
  
  dataset3_a_filtrado <- reactive({
    req(input$select_dataset3a_variedad_input)
    
    data  <- dataset3_a %>% dplyr::filter(Variedad == input$select_dataset3a_variedad_input)
  })
  
  colores_pie <- reactive({
    if(input$modo_eco == TRUE){
      colores <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33", "#31d477", "#308554", "#6dc267", "#7ac722", "#466441")
    }
    else{
      colores <- c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb", "#234663")
    }
  })
  
  output$pie_exterior <- renderHighchart({
    req(input$select_tipo)
    req(input$select_year_tipo)
    
    data <- dataset4 %>% dplyr::filter(Year == input$select_year_tipo, Tipo == input$select_tipo)
    data <- as.data.frame(table(data$Emisor))
    data$Freq <- round(100*data$Freq/sum(data$Freq), 2)
    data$pais_es <- data$Var1
    names(data) <- c("Emisor", "value", "pais_es")
    data <- remove.factors(data)
    
    for(i in seq_len(nrow(data))){
      if(data$Emisor[i] == "Belgium"){
        data$pais_es[i] <- "Bélgica"
      }
      else if(data$Emisor[i] == "Croatia"){
        data$pais_es[i] <- "Croacia"
      }
      else if(data$Emisor[i] == "Cyprus"){
        data$pais_es[i] <- "Chipre"
      }
      else if(data$Emisor[i] == "Czechia"){
        data$pais_es[i] <- "Chequia"
      }
      else if(data$Emisor[i] == "Denmark"){
        data$pais_es[i] <- "Dinamarca"
      }
      else if(data$Emisor[i] == "Finland"){
        data$pais_es[i] <- "Finlandia"
      }
      else if(data$Emisor[i] == "France"){
        data$pais_es[i] <- "Francia"
      }
      else if(data$Emisor[i] == "Germany"){
        data$pais_es[i] <- "Alemania"
      }
      else if(data$Emisor[i] == "Greece"){
        data$pais_es[i] <- "Grecia"
      }
      else if(data$Emisor[i] == "Hungary"){
        data$pais_es[i] <- "Hungría"
      }
      else if(data$Emisor[i] == "Ireland"){
        data$pais_es[i] <- "Irlanda"
      }
      else if(data$Emisor[i] == "Italy"){
        data$pais_es[i] <- "Italia"
      }
      else if(data$Emisor[i] == "Latvia"){
        data$pais_es[i] <- "Letonia"
      }
      else if(data$Emisor[i] == "Lithuania"){
        data$pais_es[i] <- "Lituania"
      }
      else if(data$Emisor[i] == "Luxembourg"){
        data$pais_es[i] <- "Luxemburgo"
      }
      else if(data$Emisor[i] == "Netherlands"){
        data$pais_es[i] <- "Países Bajos"
      }
      else if(data$Emisor[i] == "Poland"){
        data$pais_es[i] <- "Polonia"
      }
      else if(data$Emisor[i] == "Romania"){
        data$pais_es[i] <- "Rumanía"
      }
      else if(data$Emisor[i] == "Slovakia"){
        data$pais_es[i] <- "Eslovaquia"
      }
      else if(data$Emisor[i] == "Slovenia"){
        data$pais_es[i] <- "Eslovenia"
      }
      else if(data$Emisor[i] == "Sweden"){
        data$pais_es[i] <- "Suecia"
      }
    }
    data <- data[-which(data$Emisor == "EU"),]
    
    colores <- c()
    for(i in seq_along(unique(data$pais_es))){
      colores <- c(colores, sample(colores_pie(), 1))
    }
    
    hc <- data %>%
      hchart(
        "pie", hcaes(x = pais_es, y = value),
        name = "Origen de producto"
      ) %>%
      hc_plotOptions(pie = list(colors = colores_producto)) %>%
      hc_tooltip(headerFormat = "<b>{point.point.pais_es}</b><br>",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = paste0("<b>Proporción de países: </b>", input$select_tipo, " en ", input$select_year_tipo))
  })
  
  output$highchart_origen_producto <- renderHighchart({
    req(input$select_dataset3a_producto)
    data_raw <- dataset3_a %>% dplyr::filter(Producto == input$select_dataset3a_producto)
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    data <- remove.factors(data)
    data$name[data$value <= 4] <- "Otros"
    sum_otros <- sum(data$value[data$name == "Otros"])
    data <- data[data$name != "Otros",]
    data[nrow(data)+1,] <- c("Otros", sum_otros)
    data$name <- factor(data$name)
    row.names(data) <- as.character(1:nrow(data))
    data$value <- as.numeric(data$value)
    
    if(input$modo_eco == TRUE){
      colores_producto_base <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33", "#31d477", "#308554", "#6dc267", "#7ac722", "#466441")
    }
    else{
      colores_producto_base <- c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb", "#234663")
    }
    
    colores_producto <- c()
    
    for(i in seq_along(unique(data$name))){
      colores_producto <- c(colores_producto, sample(colores_producto_base, 1))
    }
    
    hc <- data %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Origen de producto"
      ) %>%
      hc_plotOptions(pie = list(colors = colores_producto)) %>%
      hc_tooltip(headerFormat = "<b>{point.point.name}</b><br>",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = paste0("<b>Origen de producto: ", input$select_dataset3a_producto, "</b>"))
    
  })
  
  output$highchart_origen_producto_3b <- renderHighchart({
    req(input$select_dataset3b_producto)
    data_raw <- dataset3_b %>% dplyr::filter(Producto == input$select_dataset3b_producto)
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    data <- remove.factors(data)
    data$name[data$value <= 4] <- "Otros"
    sum_otros <- sum(data$value[data$name == "Otros"])
    data <- data[data$name != "Otros",]
    data[nrow(data)+1,] <- c("Otros", sum_otros)
    data$name <- factor(data$name)
    row.names(data) <- as.character(1:nrow(data))
    data$value <- as.numeric(data$value)
    
    if(input$modo_eco == TRUE){
      colores_producto_base <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33", "#31d477", "#308554", "#6dc267", "#7ac722", "#466441")
    }
    else{
      colores_producto_base <- c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb", "#234663")
    }
    
    colores_producto <- c()
    
    for(i in seq_along(unique(data$name))){
      colores_producto <- c(colores_producto, sample(colores_producto_base, 1))
    }
    
    hc <- data %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Origen de producto"
      ) %>%
      hc_plotOptions(pie = list(colors = colores_producto)) %>%
      hc_tooltip(headerFormat = "<b>{point.point.name}</b><br>",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = paste0("<b>Origen de producto: ", input$select_dataset3b_producto, "</b>"))
    
  })
  
  output$highchart_origen_variedad <- renderHighchart({
    req(input$select_dataset3a_variedad_input)
    data_raw <- dataset3_a_filtrado()
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    data <- remove.factors(data)
    data$name[data$value <= 4] <- "Otros"
    sum_otros <- sum(data$value[data$name == "Otros"])
    data <- data[data$name != "Otros",]
    data[nrow(data)+1,] <- c("Otros", sum_otros)
    data$name <- factor(data$name)
    row.names(data) <- as.character(1:nrow(data))
    data$value <- as.numeric(data$value)
    
    if(input$modo_eco == TRUE){
      colores_producto_base <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33", "#31d477", "#308554", "#6dc267", "#7ac722", "#466441")
    }
    else{
      colores_producto_base <- c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb", "#234663")
    }
    
    colores_producto <- c()
    
    for(i in seq_along(unique(data$name))){
      colores_producto <- c(colores_producto, sample(colores_producto_base, 1))
    }
    
    hc <- data %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Origen de variedad"
      ) %>%
      hc_plotOptions(pie = list(colors = colores_producto)) %>%
      hc_tooltip(headerFormat = "<b>{point.point.name}</b><br>",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = paste0("<b>Origen de variedad: ", input$select_dataset3a_variedad_input, "</b>"))
    
  })
  
  output$select_dataset3a_variedad <- renderUI({
    req(input$select_dataset3a_producto)
    data <- dataset3_a %>% dplyr::filter(Producto == input$select_dataset3a_producto)
    selectInput(inputId = "select_dataset3a_variedad_input", label = "Variedad a explorar:", choices = unique(data$Variedad))
  })
  
  output$highchart_correlacion <- renderHighchart({
    data <- na.omit(dataset1 %>% dplyr::select(-c(1,2,3,4)))
    hchart.cor(cor(data, method = "pearson"))
  })
  
  output$highchart_grupo <- renderHighchart({
    hc <- dataset2_grupo %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Grupo de agricultura"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset2_grupo_color$list_color)) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Grupo de agricultura</b>")
      
  })
  
  output$highchart_sector <- renderHighchart({
    hc <- dataset2_sector %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Sector de agricultura"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset2_sector_color$list_color)) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Sector de agricultura</b>")
    
  })
  
  output$highchart_subsector <- renderHighchart({
    hc <- dataset2_subsector %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Subsector de agricultura"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset2_subsector_color$list_color)) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Subsector de agricultura</b>")
    
  })
  
  output$highchart_product <- renderHighchart({
    hc <- dataset2_producto %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Productos"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset2_producto_color$list_color)) %>%
      hc_tooltip(headerFormat = "<b>{point.point.name}</b><br>",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Producto de agricultura</b>")
    
  })
  
  output$highchart_posicion <- renderHighchart({
    hc <- dataset2_posicion %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Posición en el mercado"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset2_posicion_color$list_color)) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Posición en el mercado</b>")
    
  })
  
  output$highchart_familia <- renderHighchart({
    hc <- dataset3_a_familia %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Familia de producto"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset3_a_familia_color$list_color)) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Familia de producto</b>")
  })
  
  output$highchart_familia_3b <- renderHighchart({
    hc <- dataset3_b_familia %>%
      hchart(
        "pie", hcaes(x = name, y = value),
        name = "Familia de producto"
      ) %>%
      hc_plotOptions(pie = list(colors = dataset3_b_familia_color$list_color)) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = paste0("{point.value}", " %")) %>%
      hc_title(text = "<b>Familia de producto</b>")
  })
  
  output$text_max_origen <- renderText({
    req(input$select_dataset3a_producto)
    data_raw <- dataset3_a %>% dplyr::filter(Producto == input$select_dataset3a_producto)
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    max <- data$name[which.max(data$value)]
    return(paste0(max))
  })
  
  output$text_max_familia_3b <- renderText({
    data <- dataset3_b_familia
    max <- data$name[which.max(data$value)]
    return(paste0(max))
  })
  
  output$subtext_max_familia_3b <- renderText({
    data <- dataset3_b_familia
    min <- data$name[which.min(data$value)]
    return(paste0("como la familia con más productos y <strong>", min, "</strong> como la familia más pequeña."))
  })
  
  output$text_max_origen_3b <- renderText({
    req(input$select_dataset3b_producto)
    data_raw <- dataset3_b %>% dplyr::filter(Producto == input$select_dataset3b_producto)
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    max <- data$name[which.max(data$value)]
    return(paste0(max))
  })
  
  output$subtext_max_origen <- renderText({
    req(input$select_dataset3a_producto)
    data_raw <- dataset3_a %>% dplyr::filter(Producto == input$select_dataset3a_producto)
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    min <- data$name[which.min(data$value)]
    paste0("como origen de mayor procedencia y <strong>", min, "</strong> como el de menor.")
  })
  
  output$subtext_max_origen_3b <- renderText({
    req(input$select_dataset3b_producto)
    data_raw <- dataset3_b %>% dplyr::filter(Producto == input$select_dataset3b_producto)
    data <- as.data.frame(table(data_raw$Origen))
    data$Freq <- round(data$Freq/sum(data$Freq)*100, 2)
    names(data) <- c("name", "value")
    min <- data$name[which.min(data$value)]
    paste0("como origen de mayor procedencia y <strong>", min, "</strong> como el de menor.<br><br>")
  })
  
  data_raw <- reactive({
    req(input$select_dataset3a_producto)
    req(input$select_dataset3a_variedad_input)
    data_raw <- dataset3_a %>% dplyr::filter(Producto == input$select_dataset3a_producto) %>% dplyr::filter(Variedad == input$select_dataset3a_variedad_input)
    data_raw <- as.data.frame(table(data_raw$Origen))
    data_raw$Freq <- round(data_raw$Freq/sum(data_raw$Freq)*100, 2)
    return(data_raw)
  })
  
  output$text_max_variedad <- renderText({
    req(input$select_dataset3a_producto)
    req(input$select_dataset3a_variedad_input)
    data <- data_raw()
    return(paste0(data$Var1[which.max(data$Freq)]))
  })
  
  output$subtext_max_variedad <- renderText({
    req(input$select_dataset3a_producto)
    req(input$select_dataset3a_variedad_input)
    data <- data_raw()
    paste0("como origen de mayor procedencia y <strong>", data$Var1[which.min(data$Freq)], "</strong> como el de menor.")
  })
  
  output$text_max <- renderText({
    req(input$select_dataset1)
    data <- dataset1 %>% dplyr::select(input$select_dataset1)
    names(data) <- "var"
    round(max(data$var, na.rm = TRUE), 2)
  })
  
  output$subtext_max <- renderUI({
    req(input$select_dataset1)
    HTML(paste0("<h4>Máximo de <strong>", input$select_dataset1, "<strong></h4>"))
  })
  
  output$text_min <- renderText({
    req(input$select_dataset1)
    data <- dataset1 %>% dplyr::select(input$select_dataset1)
    names(data) <- "var"
    round(min(data$var, na.rm = TRUE), 2)
  })
  
  output$subtext_min <- renderUI({
    req(input$select_dataset1)
    HTML(paste0("<h4>Mínimo de <strong>", input$select_dataset1, "<strong></h4>"))
  })
  
  output$text_media <- renderText({
    req(input$select_dataset1)
    data <- dataset1 %>% dplyr::select(input$select_dataset1)
    names(data) <- "var"
    round(mean(data$var, na.rm = TRUE), 2)
  })
  
  output$subtext_media <- renderUI({
    req(input$select_dataset1)
    HTML(paste0("<h4>Media de <strong>", input$select_dataset1, "<strong></h4>"))
  })
  
  output$text_na <- renderText({
    req(input$select_dataset1)
    data <- dataset1 %>% dplyr::select(input$select_dataset1)
    names(data) <- "var"
    sum(is.na(data$var))
  })
  
  output$subtext_na <- renderUI({
    req(input$select_dataset1)
    HTML(paste0("<h4>NAs en <strong>", input$select_dataset1, "<strong></h4>"))
  })
  
  output$text_mediana <- renderText({
    req(input$select_dataset1)
    data <- dataset1 %>% dplyr::select(input$select_dataset1)
    names(data) <- "var"
    round(median(data$var, na.rm = TRUE), 2)
  })
  
  output$subtext_mediana <- renderUI({
    req(input$select_dataset1)
    HTML(paste0("<h4>Mediana de <strong>", input$select_dataset1, "<strong></h4>"))
  })
  
  output$text_filas <- renderText({
    req(input$select_dataset1)
    data <- dataset1 %>% dplyr::select(input$select_dataset1)
    names(data) <- "var"
    nrow(data)
  })
  
  output$subtext_filas <- renderUI({
    req(input$select_dataset1)
    HTML(paste0("<h4>Instancias <strong>totales</strong>.<br><br></h4>"))
  })
  
  output$precio_max <- renderText({
    req(input$dataset_info)
    data <- dataset2 %>% dplyr::select("precio")
    names(data) <- "var"
    paste0(round(max(data$var, na.rm = TRUE), 2), " €/kg")
  })
  
  output$subprecio_max <- renderUI({
    req(input$dataset_info)
    HTML(paste0("<h4>Máximo de <strong>Precio<strong></h4>"))
  })
  
  output$precio_min <- renderText({
    req(input$dataset_info)
    data <- dataset2 %>% dplyr::select("precio")
    names(data) <- "var"
    paste0(round(min(data$var, na.rm = TRUE), 2), " €/kg")
  })
  
  output$subprecio_min <- renderUI({
    req(input$dataset_info)
    HTML(paste0("<h4>Mínimo de <strong>Precio<strong></h4>"))
  })
  
  output$precio_media <- renderText({
    req(input$dataset_info)
    data <- dataset2 %>% dplyr::select("precio")
    names(data) <- "var"
    paste0(round(mean(data$var, na.rm = TRUE), 2), " €/kg")
  })
  
  output$subprecio_media <- renderUI({
    req(input$dataset_info)
    HTML(paste0("<h4>Media de <strong>Precio<strong></h4>"))
  })
  
  output$precio_mediana <- renderText({
    req(input$dataset_info)
    data <- dataset2 %>% dplyr::select("precio")
    names(data) <- "var"
    paste0(round(median(data$var, na.rm = TRUE), 2), " €/kg")
  })
  
  output$subprecio_mediana <- renderUI({
    req(input$dataset_info)
    HTML(paste0("<h4>Mediana de <strong>Precio<strong></h4>"))
  })
  
  output$precio_na <- renderText({
    req(input$dataset_info)
    data <- dataset2 %>% dplyr::select("precio")
    names(data) <- "var"
    sum(is.na(data$var))
  })
  
  output$subprecio_na <- renderUI({
    req(input$dataset_info)
    HTML(paste0("<h4>NAs en <strong>Precio<strong></h4>"))
  })
  
  output$text_filas2 <- renderText({
    req(input$dataset_info)
    data <- dataset2 %>% dplyr::select("precio")
    names(data) <- "var"
    nrow(data)
  })
  
  output$subtext_filas2 <- renderUI({
    req(input$dataset_info)
    HTML(paste0("<h4>Instancias <strong>totales</strong>.</h4>"))
  })
  
  output$text_max_3a <- renderText({
    req(input$select_dataset3_a)
    data <- dataset3_a %>% dplyr::select(input$select_dataset3_a)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_a, "Precio")){
      paste0(round(max(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(max(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_max_3a <- renderUI({
    req(input$select_dataset3_a)
    HTML(paste0("<h4>Máximo de <strong>", input$select_dataset3_a, "<strong></h4>"))
  })
  
  output$text_media_3a <- renderText({
    req(input$select_dataset3_a)
    data <- dataset3_a %>% dplyr::select(input$select_dataset3_a)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_a, "Precio")){
      paste0(round(mean(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(mean(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_media_3a <- renderUI({
    req(input$select_dataset3_a)
    HTML(paste0("<h4>Media de <strong>", input$select_dataset3_a, "<strong></h4>"))
  })
  
  output$text_min_3a <- renderText({
    req(input$select_dataset3_a)
    data <- dataset3_a %>% dplyr::select(input$select_dataset3_a)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_a, "Precio")){
      paste0(round(min(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(min(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_min_3a <- renderUI({
    req(input$select_dataset3_a)
    HTML(paste0("<h4>Mínimo de <strong>", input$select_dataset3_a, "<strong></h4>"))
  })
  
  output$text_mediana_3a <- renderText({
    req(input$select_dataset3_a)
    data <- dataset3_a %>% dplyr::select(input$select_dataset3_a)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_a, "Precio")){
      paste0(round(median(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(median(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_mediana_3a <- renderUI({
    req(input$select_dataset3_a)
    HTML(paste0("<h4>Mediana de <strong>", input$select_dataset3_a, "<strong></h4>"))
  })
  
  output$text_na_3a <- renderText({
    req(input$select_dataset3_a)
    data <- dataset3_a %>% dplyr::select(input$select_dataset3_a)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_a, "Precio")){
      paste0(sum(is.na(data$var)), " €/kg")
    }
    else{
      paste0(sum(is.na(data$var)), " kg")
    }
  })
  
  output$subtext_na_3a <- renderUI({
    req(input$select_dataset3_a)
    HTML(paste0("<h4>NAs en <strong>", input$select_dataset3_a, "<strong></h4>"))
  })
  
  output$text_filas_3a <- renderText({
    req(input$select_dataset3_a)
    data <- dataset3_a %>% dplyr::select(input$select_dataset3_a)
    names(data) <- "var"
    nrow(data)
  })
  
  output$subtext_filas_3a <- renderUI({
    req(input$select_dataset3_a)
    HTML(paste0("<h4>Instancias <strong>totales</strong>.</h4>"))
  })
  
  output$text_max_3b <- renderText({
    req(input$select_dataset3_b)
    data <- dataset3_b %>% dplyr::select(input$select_dataset3_b)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_b, "Precio")){
      paste0(round(max(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(max(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_max_3b <- renderUI({
    req(input$select_dataset3_b)
    HTML(paste0("<h4>Máximo de <strong>", input$select_dataset3_b, "<strong></h4>"))
  })
  
  output$text_media_3b <- renderText({
    req(input$select_dataset3_b)
    data <- dataset3_b %>% dplyr::select(input$select_dataset3_b)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_b, "Precio")){
      paste0(round(mean(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(mean(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_media_3b <- renderUI({
    req(input$select_dataset3_b)
    HTML(paste0("<h4>Media de <strong>", input$select_dataset3_b, "<strong></h4>"))
  })
  
  output$text_min_3b <- renderText({
    req(input$select_dataset3_b)
    data <- dataset3_b %>% dplyr::select(input$select_dataset3_b)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_b, "Precio")){
      paste0(round(min(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(min(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_min_3b <- renderUI({
    req(input$select_dataset3_b)
    HTML(paste0("<h4>Mínimo de <strong>", input$select_dataset3_b, "<strong></h4>"))
  })
  
  output$text_mediana_3b <- renderText({
    req(input$select_dataset3_b)
    data <- dataset3_b %>% dplyr::select(input$select_dataset3_b)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_b, "Precio")){
      paste0(round(median(data$var, na.rm = TRUE), 2), " €/kg")
    }
    else{
      paste0(round(median(data$var, na.rm = TRUE), 2), " kg")
    }
  })
  
  output$subtext_mediana_3b <- renderUI({
    req(input$select_dataset3_b)
    HTML(paste0("<h4>Mediana de <strong>", input$select_dataset3_b, "<strong></h4>"))
  })
  
  output$text_na_3b <- renderText({
    req(input$select_dataset3_b)
    data <- dataset3_b %>% dplyr::select(input$select_dataset3_b)
    names(data) <- "var"
    if(str_detect(input$select_dataset3_b, "Precio")){
      paste0(sum(is.na(data$var)), " €/kg")
    }
    else{
      paste0(sum(is.na(data$var)), " kg")
    }
  })
  
  output$subtext_na_3b <- renderUI({
    req(input$select_dataset3_b)
    HTML(paste0("<h4>NAs en <strong>", input$select_dataset3_b, "<strong></h4>"))
  })
  
  output$text_filas_3b <- renderText({
    req(input$select_dataset3_b)
    data <- dataset3_b %>% dplyr::select(input$select_dataset3_b)
    names(data) <- "var"
    nrow(data)
  })
  
  output$subtext_filas_3b <- renderUI({
    req(input$select_dataset3_b)
    HTML(paste0("<h4>Instancias <strong>totales</strong>.</h4>"))
  })
  
  observeEvent(input$dataset_info, {
    if(input$dataset_info != "- - -"){
      shinyjs::show("bloque_AED")
    }
    else{
      shinyjs::hide("bloque_AED")
    }
  }, once = FALSE)
  
  observeEvent(input$modo_eco, {
    if(input$modo_eco == TRUE){
      dataset1_color$color <- "#6ab04c"
      
      dataset1_color$list_color <- list( list(0, '#FF5733'),
                                         list(0.5, '#F8F5F5'),
                                         list(1, '#6ab04c')
      )
      
      dataset2_grupo_color$list_color <- c("#6ab04c", "#56d121")
      
      dataset2_sector_color$list_color <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33")
      
      dataset2_subsector_color$list_color <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33", "#31d477", "#308554", "#6dc267", "#7ac722", "#466441")
      
      colores_producto_base <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33", "#31d477", "#308554", "#6dc267", "#7ac722", "#466441")
      colores_producto <- c()
      
      for(i in seq_along(unique(dataset2$producto))){
        colores_producto <- c(colores_producto, sample(colores_producto_base))
      }
      dataset2_producto_color$list_color <- colores_producto
      
      dataset2_posicion_color$list_color <- c("#6ab04c", "#56d121", "#14e37f")
      
      dataset3_a_familia_color$list_color <- c("#6ab04c", "#56d121", "#14e37f", "#4c9c33")
    }
    else{
      dataset1_color$color <- "#0070FF"
      
      dataset1_color$list_color <- list( list(0, '#FF5733'),
                                         list(0.5, '#F8F5F5'),
                                         list(1, '#2E86C1')
      )
      
      dataset2_grupo_color$list_color <- c("#0070FF", "#4ecef5")
      
      dataset2_sector_color$list_color <- c("#0070FF", "#4ecef5", "#2297ba", "#2686e0")
      
      dataset2_subsector_color$list_color <- c("#90f0f5", "#4ecef5", "#2297ba", "#2686e0", "#0070FF", "#12abb3", "#3d68e0", "#8fa9eb")
      
      dataset2_producto_color$list_color <- colores_producto
      
      dataset2_posicion_color$list_color <- c("#0070FF", "#4ecef5", "#2297ba")
      
      dataset3_a_familia_color$list_color <- c("#0070FF", "#4ecef5", "#2297ba", "#2686e0")
    }
  }, once = FALSE)
}

mercas_server <- function(input, output) {
  
  output$mercas_streamgraph <- renderD3({
    r2d3(
      list(
        toJSON(streamdata, na="null"),
        color_book
      ),
      script = "www/d3/mystreamgraph.js"
    )
  })
  
  streamgraph_click <- reactive({
    click <- toupper(input$mercas_streamgraph_click)
    click <- str_replace_all(click, "NN", "Ñ")
    click
  })
  
  observeEvent(input$mercas_streamgraph_click, {
    print(streamgraph_click())
  })
  
  output$mercas_variedad_ui <- renderUI({
    req(streamgraph_click())
    choices <-  as.character(variedades_madrid[[streamgraph_click()]])
    selectInput(
      "mercas_variedad_select",
      "Variedad",
      choices = choices,
      selected = choices[1]
    )
  })
  
  mercas_product_data <- reactive({
    req(input$mercas_variedad_select)
    df <- madrid_by_year %>% filter(variedad==input$mercas_variedad_select)
    years <- sort(unique(df$year))
    list(data=df, years=years)
  })
  
  output$mercas_ui_year <- renderUI({
    if(identical(streamgraph_click(), character(0))){
      HTML("<br><br><br><p><strong>¡Haz click en un producto en la gráfica de la izquierda para ver más!</strong></p>")
    }
    else{
      radioGroupButtons(
        "mercas_select_year", 
        "Año",
        choices = mercas_product_data()[["years"]],
        selected = max(mercas_product_data()[["years"]])
      )
    }
  })
  
  mercas_year_data <- reactive({
    
    df <- mercas_product_data()[["data"]] %>% filter(year==input$mercas_select_year)
    seasons <- sort(unique(df$estacion))
    
    list(data=df, seasons=seasons)
  })
  
  output$mercas_ui_season <- renderUI({
    req(streamgraph_click())
    req(input$mercas_select_year)
    print(mercas_year_data()[["seasons"]])
    radioGroupButtons(
      "mercas_select_season", 
      "Estación",
      choices = as.character(mercas_year_data()[["seasons"]])
    )
  })
  
  mercas_month_data <- reactive({
    print(mercas_year_data()[["data"]])
    mercas_year_data()[["data"]] %>% filter(estacion==input$mercas_select_season) %>%
      select(-c(product, variedad, year, estacion))
  })
  
  mercas_map_data <- reactive({
    prov.points %>% inner_join(mercas_month_data(), by=c("prov"="origen")) %>%
      filter(prov != "MADRID")
  })
  
  output$mercas_map <- renderD3({
    req(input$mercas_select_year, input$mercas_select_season, req(streamgraph_click()))
    print(mercas_map_data())
    r2d3(list(spain.topojson, madrid.point, 
              toJSON(mercas_map_data()), input$mercas_variedad_select),
         script="www/d3/mercas_map.js",
         dependencies = "www/js/topojson.min.js"
    )
  })
  
  observeEvent(input$mercas_prov_clicked, {
    print(input$mercas_prov_clicked)
  })
  
  output$mercas_pandemia_lines <- renderHighchart({
    data <- madrid_pandemia %>% filter(variedad == input$mercas_pandemia_variedad)
    data$meses <- rep(NA, nrow(data))
    for(i in seq_len(nrow(data))){
      data$meses[i] <- meses[data$month[i]]
    }
    if (input$mercas_pandemia_variable == "volumen") {
      highchart() %>% 
        hc_xAxis(categories = data$meses) %>%
        hc_add_series(data, "line", hcaes(x=meses, y=volumen20),
                      name="2020") %>%
        hc_add_series(data, "line", hcaes(x=meses, y=volumen19),
                      name="2019") %>%
        hc_add_series(data, "line", hcaes(x=meses, y=volumen18),
                      name="2018") %>%
        hc_yAxis(
          title = list(text = "Volumen comercializado en kg")
        )
    } else {
      highchart() %>% 
        hc_xAxis(categories = data$meses) %>%
        hc_add_series(data, "line", hcaes(x=meses, y=price_mean20),
                      name="2020") %>%
        hc_add_series(data, "line", hcaes(x=meses, y=price_mean19),
                      name="2019") %>%
        hc_add_series(data, "line", hcaes(x=meses, y=price_mean18),
                      name="2018") %>%
        hc_yAxis(
          title = list(text = "Precio medio en €/kg")
        )
    }
    
  })
  
  output$mercas_pandemia_var <- renderHighchart({
    data <- madrid_pandemia %>% filter(variedad == input$mercas_pandemia_variedad)
    data$meses <- rep(NA, nrow(data))
    for(i in seq_len(nrow(data))){
      data$meses[i] <- meses[data$month[i]]
    }
    if (input$mercas_pandemia_variable == "volumen") {
      highchart() %>% 
        hc_xAxis(categories = data$meses) %>%
        hc_add_series(data, "line", hcaes(x=meses, y=100*volumen_var19),
                      name="variación 2020-2019") %>%
        hc_add_series(data, "line", hcaes(x=meses, y=100*volumen_var18),
                      name="variación 2020-2018") %>%
        hc_yAxis(
          title = list(text = "Variación de volumen en %")
        )
    } else {
      highchart() %>% 
        hc_xAxis(categories = data$meses) %>%
        hc_add_series(data, "line", hcaes(x=meses, y=100*price_mean_var19),
                      name="variación 2020-2019") %>%
        hc_add_series(data, "line", hcaes(x=meses, y=100*price_mean_var18),
                      name="variación 2020-2018") %>%
        hc_yAxis(
          title = list(text = "Variación de precio en %")
        )
    }
  })
  
}

consumo_pagina_server <- function(input, output, session) {
  color_cajas <- reactive({
    if(input$modo_eco == TRUE){
      color <- "#6ab04c"
    }
    else{
      color <- "#0070FF"
    }
  })
  
  output$box_1 <- renderUI({
    valueBox(value = textOutput("textmaximo"),
             subtitle = textOutput("subtextmaximo"),
             color = color_cajas())
  })
  
  output$box_2 <- renderUI({
    valueBox(value = textOutput("textminimo"),
             subtitle = textOutput("subtextminimo"),
             color = color_cajas())
  })
  
  output$box_3 <- renderUI({
    valueBox(value = textOutput("textmedia"),
             subtitle = textOutput("subtextmedia"),
             color = color_cajas())
  })
  
  dataset_consumo <- read_csv("data/dataset1.csv")
  
  dataset_consumo_grouped <- reactive({  
    if (input$agruparyears == TRUE) {
      dataset_consumo
    } else {
      unite(dataset_consumo, Mes, c(Mes, Year), remove=FALSE, sep="/")
    }
  })
  
  observeEvent(input$agruparyears, {
    if(input$agruparyears == TRUE){
      shinyjs::show("texto_consumo")
    }
    else{
      shinyjs::hide("texto_consumo")
    }
  }, once = FALSE)
  
  dataset_consumo_filter <- reactive({dataset_consumo_grouped() %>% dplyr::filter(Producto == input$selectprod)})
  dataset_consumo_filter2 <- reactive({dataset_consumo_filter() %>% dplyr::filter(CCAA == input$selectccaa)})
  dataset_consumo_filter3 <- reactive({dataset_consumo_filter2() %>% dplyr::select(c("Year", "Mes", input$selectvar))})
  
  #select para seleccionar el producto
  output$select_producto <- renderUI({})
  outputOptions(output, "select_producto")
  output$select_producto <- renderUI({selectInput("selectprod", "Producto:", unique(dataset_consumo_grouped()$Producto))})
  
  #select para seleccionar el producto
  output$select_producto <- renderUI({})
  outputOptions(output, "select_producto")
  output$select_producto <- renderUI({selectInput("selectprod", "Producto:", unique(dataset_consumo_grouped()$Producto))}) 
  
  #select para seleccionar la ccaa
  output$select_ccaa <- renderUI({})
  outputOptions(output, "select_ccaa")
  output$select_ccaa <- renderUI({selectInput("selectccaa", "CCAA/Total Nacional:", unique(dataset_consumo_grouped()$CCAA))}) 
  
  #select para seleccionar que variable representar
  output$select_variable <- renderUI({})
  outputOptions(output, "select_variable")
  output$select_variable <- renderUI({selectInput("selectvar", "Variable:", colnames(dataset_consumo_grouped()[-c(1,2,3,4)]))}) 
  
  output$subtextmaximo <- reactive({paste0("Máximo de ", input$selectvar)})
  output$subtextminimo <- reactive({paste0("Mínimo de ", input$selectvar)})
  output$subtextmedia <- reactive({paste0("Media de ", input$selectvar)})
  
  output$textmaximo <- renderText({
    req(input$selectprod)
    req(input$selectccaa)
    req(input$selectvar)
    data <- dataset_consumo_filter3()
    names(data) <- c("year", "mes", "variable")
    round(max(data$variable, na.rm = TRUE), 2)
  })
  
  output$textminimo <- renderText({
    req(input$selectprod)
    req(input$selectccaa)
    req(input$selectvar)
    data <- dataset_consumo_filter3()
    names(data) <- c("year", "mes", "variable")
    round(min(data$variable, na.rm = TRUE), 2)
  })
  
  output$textmedia <- renderText({
    req(input$selectprod)
    req(input$selectccaa)
    req(input$selectvar)
    data <- dataset_consumo_filter3()
    names(data) <- c("year", "mes", "variable")
    round(mean(data$variable, na.rm = TRUE), 2)
  })
  
  
  output$highchartconsumo <- renderHighchart({
    req(input$selectprod)
    req(input$selectccaa)
    req(input$selectvar)
    
    if (input$selectvar == "Penetración (%)") {
      text <- paste0("Analizando la ", input$selectvar, " de ",input$selectprod , " en ",input$selectccaa, " por años")
    } else {
      text <- paste0("Analizando el ", input$selectvar, " de ",input$selectprod , " en ",input$selectccaa, " por años")
    }
    if (input$agruparyears == TRUE) {
      data <- dataset_consumo_filter3()
      names(data) <- c("year", "mes", "plot")
      
      dataset2018 <- data %>% dplyr::filter(year == 2018)
      dataset2019 <- data %>% dplyr::filter(year == 2019)
      dataset2020 <- data %>% dplyr::filter(year == 2020)
      
      highchart() %>%
        hc_xAxis(categories = unique(data$mes)) %>% 
        hc_add_series(name = "2018", data = dataset2018$plot, color = "green") %>% 
        hc_add_series(name = "2019", data = dataset2019$plot, color = "blue") %>% 
        hc_add_series(name = "2020", data = dataset2020$plot, color = "red") %>%
        hc_title(text = text)
    } else {
      data <- dataset_consumo_filter3()
      names(data) <- c("year", "mes", "plot")
      highchart() %>%
        hc_xAxis(categories = data$mes
        ) %>%
        hc_add_series(showInLegend = FALSE, name = input$selectvar, data = data$plot, color = color_cajas()) %>%
        hc_title(text = text) %>% 
        hc_xAxis(
          plotLines = list(list(
            value = (which(data$mes == "Marzo/2020") - 1),
            color = '#ff0000',
            width = 3,
            zIndex = 4,
            label = list(text = "COMIENZO PANDEMIA COVID19",
                         style = list( color = '#ff0000', fontWeight = 'bold' )
            )
          ), 
          list(
            value = (which(data$mes == "Junio/2018") - 1),
            color = '#6ab04c',
            width = 3,
            zIndex = 4,
            label = list(text = "Luis Planas nuevo ministro de agricultura",
                         style = list( color = 'black')
            )
          ),
          list(
            value = (which(data$mes == "Octubre/2019") - 1),
            color = '#6ab04c',
            width = 3,
            zIndex = 4,
            label = list(text = "Aranceles de Trump a productos Europeos",
                         style = list( color = 'black')
            )
          )
          )
        )
      
    }
  })
  
  dataset_consumo_mapa <- read_csv("data/dataset1.csv")
  
  dataset_consumo_mapa <- unite(dataset_consumo_mapa, Mes, c(Mes, Year), remove=TRUE, sep="/")
  
  dataset_consumo_mapa <- dataset_consumo_mapa[-which(dataset_consumo_mapa$CCAA == "Total Nacional"),]
  
  dataset_consumo_mapa_filtrado <- reactive({dataset_consumo_mapa %>% dplyr::filter(Mes == input$selectfechamap)})
  dataset_consumo_mapa_filtrado2 <- reactive({dataset_consumo_mapa_filtrado() %>% dplyr::filter(Producto == input$selectprodmap)})
  
  dataset_consumo_mapa_select <- reactive({dataset_consumo_mapa_filtrado2() %>% dplyr::select(c("CCAA", input$selectvarmap))})

  output$select_fecha_mapas <- renderUI({selectInput("selectfechamap", "Fecha:", unique(dataset_consumo_mapa$Mes))}) 
  
  output$select_producto_mapas <- renderUI({selectInput("selectprodmap", "Producto:", unique(dataset_consumo_mapa$Producto))})   
  
  output$select_variable_mapas <- renderUI({selectInput("selectvarmap", "Variable:", names(dataset_consumo_mapa)[-c(1,2,3)])})
  
  observeEvent(input$modo_eco, {
    if(input$modo_eco == TRUE){
      output$highchart_mapa <- renderHighchart({
        req(input$selectfechamap)
        req(input$selectprodmap)
        req(input$selectvarmap)
        data <- dataset_consumo_mapa_select()
        names(data) <- c("CCAA", "dato")
        spain_consumo <- highchart(type="map") %>%
          hc_add_series_map(map_spain, 
                            data, 
                            "dato", c("NAME", "CCAA"), 
                            nullColor="#00FF00", borderColor="#fff",
                            borderWidth=2) %>%
          hc_tooltip(headerFormat="<b>{point.point.NAME}</b><br>",
                     pointFormat= paste0(input$selectvarmap, ": <b>{point.value}</b>")) %>%
          hc_title(text = paste0("Analizando el/la ", input$selectvarmap, " de ",input$selectprodmap , " en ",input$selectfechamap))
        hc_colorAxis(spain_consumo, minColor = "#83FF83", maxColor = "#0B780B")})
    }
    else{
      output$highchart_mapa <- renderHighchart({
        req(input$selectfechamap)
        req(input$selectprodmap)
        req(input$selectvarmap)
        data <- dataset_consumo_mapa_select()
        names(data) <- c("CCAA", "dato")
        spain_consumo <- highchart(type="map") %>%
          hc_add_series_map(map_spain, 
                            data, 
                            "dato", c("NAME", "CCAA"), 
                            nullColor="#00FF00", borderColor="#fff",
                            borderWidth=2) %>%
          hc_tooltip(headerFormat="<b>{point.point.NAME}</b><br>",
                     pointFormat= paste0(input$selectvarmap, ": <b>{point.value}</b>")) %>%
          hc_title(text = paste0("Analizando el/la ", input$selectvarmap, " de ",input$selectprodmap , " en ",input$selectfechamap))
        hc_colorAxis(spain_consumo, minColor = "#3CA5F7", maxColor = "#020290")})
    }
  }, once = FALSE)
}

junta_andalucia_server <- function(input, output, session) {
    dataset2 <- reactiveValues(datos = read_csv("data/dataset2.csv"))
    dataset2$covid_total <- read_csv("data/covid_total.csv")
    dataset2$second_line <- FALSE
    
    observeEvent(input$modo_eco, {
        if(input$modo_eco == TRUE){
            dataset2$datos <- read_csv("data/dataset2.csv")
            dataset2$datos <- dataset2$datos %>% dplyr::filter(grupo == "Agricultura ECO")
            dataset2$color <- "green"
            dataset2$veces_filtrado <- 0
        }
        else{
            dataset2$datos <- read_csv("data/dataset2.csv")
            dataset2$datos <- dataset2$datos %>% dplyr::filter(grupo == "Agricultura")
            dataset2$color <- "blue"
            dataset2$veces_filtrado <- 0
        }
    }, once = FALSE)
    
    output$select_sector <- renderUI({
        selectInput(inputId = "select_sector_input", label = "Sector:", choices = unique(dataset2$datos$sector), width = "100%")
    })
    
    output$select_sector2 <- renderUI({
        selectInput(inputId = "select_sector2_input", label = "Sector:", choices = unique(dataset2$datos$sector), width = "100%")
    })
    
    observeEvent(input$select_sector_input, {
        dataset2$filtrados_sector <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input)
    }, once = FALSE)
    
    observeEvent(input$select_sector2_input, {
      dataset2$filtrados_sector2 <- dataset2$datos %>% dplyr::filter(sector == input$select_sector2_input)
    }, once = FALSE)
    
    output$select_subsector <- renderUI({
        selectInput(inputId = "select_subsector_input", label = "Subsector:", choices = unique(dataset2$filtrados_sector$subsector), width = "100%")
    })
    
    output$select_subsector2 <- renderUI({
      choices <- unique(dataset2$filtrados_sector2$subsector)
      if(is.null(choices)){
        choices <- unique(dataset2$filtrados_sector$subsector)
      }
      selectInput(inputId = "select_subsector2_input", label = "Subsector:", choices = choices, width = "100%")
    })
    
    observeEvent(input$select_subsector_input, {
      dataset2$filtrados_subsector <- dataset2$filtrados_sector %>% dplyr::filter(subsector == input$select_subsector_input)
    }, once = FALSE)
    
    observeEvent(input$select_subsector2_input, {
      req(input$select_subsector2_input)
      if(is.null(dataset2$filtrados_sector2)){
        dataset2$filtrados_sector2 <- dataset2$filtrados_sector
      }
      dataset2$filtrados_subsector2 <- dataset2$filtrados_sector2 %>% dplyr::filter(subsector == input$select_subsector2_input)
    }, once = FALSE)
    
    output$select_product <- renderUI({
        selectInput(inputId = "select_product_input", label = "Producto:", choices = unique(dataset2$filtrados_subsector$producto), width = "100%")
    })
    
    output$select_product2 <- renderUI({
      selectInput(inputId = "select_product2_input", label = "Producto:", choices = unique(dataset2$filtrados_subsector2$producto), width = "100%")
    })

    output$emoji <- renderUI({
        req(input$select_product_input)
        if(input$select_product_input == "Limón"){
          img(src="images/lemon.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Mandarina"){
          img(src="images/tangerine.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Naranja"){
          img(src="images/orange.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Pomelo"){
          img(src="images/grapefruit.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Aguacate"){
          img(src="images/avocado.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Chirimoya"){
          img(src="images/chirimoya.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Mango"){
          img(src="images/mango.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Frambuesa"){
          img(src="images/raspberry.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Fresa"){
          img(src="images/fresa.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Ajo"){
          img(src="images/ajo.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Cebolla"){
          img(src="images/cebolla.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Espárrago"){
          img(src="images/asparagus.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Lechuga"){
          img(src="images/lettuce.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Zanahoria"){
          img(src="images/carrot.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Berenjena"){
          img(src="images/eggplant.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Calabacín"){
          img(src="images/calabacin.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Judía verde"){
          img(src="images/judia.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Melón"){
          img(src="images/melon.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Pepino"){
          img(src="images/pepino.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Pimiento"){
          img(src="images/pimiento.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Sandía"){
          img(src="images/sandia.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Tomate"){
          img(src="images/tomate.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Almendra"){
          img(src="images/almendra.svg", class="myicon", width="40px", style="margin-top:25px")
        }
        else if(input$select_product_input == "Arándano"){
          img(src="images/blueberry.svg", class="myicon", width="40px", style="margin-top:25px")
        }
    })
    
    output$emoji2 <- renderUI({
      req(input$select_product2_input)
      if(input$select_product2_input == "Limón"){
        img(src="images/lemon.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Mandarina"){
        img(src="images/tangerine.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Naranja"){
        img(src="images/orange.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Pomelo"){
        img(src="images/grapefruit.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Aguacate"){
        img(src="images/avocado.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Chirimoya"){
        img(src="images/chirimoya.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Mango"){
        img(src="images/mango.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Frambuesa"){
        img(src="images/raspberry.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Fresa"){
        img(src="images/fresa.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Ajo"){
        img(src="images/ajo.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Cebolla"){
        img(src="images/cebolla.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Espárrago"){
        img(src="images/asparagus.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Lechuga"){
        img(src="images/lettuce.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Zanahoria"){
        img(src="images/carrot.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Berenjena"){
        img(src="images/eggplant.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Calabacín"){
        img(src="images/calabacin.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Judía verde"){
        img(src="images/judia.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Melón"){
        img(src="images/melon.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Pepino"){
        img(src="images/pepino.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Pimiento"){
        img(src="images/pimiento.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Sandía"){
        img(src="images/sandia.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Tomate"){
        img(src="images/tomate.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Almendra"){
        img(src="images/almendra.svg", class="myicon", width="40px", style="margin-top:25px")
      }
      else if(input$select_product2_input == "Arándano"){
        img(src="images/blueberry.svg", class="myicon", width="40px", style="margin-top:25px")
      }
    })
    
    observeEvent(input$select_product_input, {
        shinyjs::show("column4")
        dataset2$filtrados_producto <- dataset2$filtrados_subsector %>% dplyr::filter(producto == input$select_product_input)
    }, once = FALSE)
    
    observeEvent(input$select_product2_input, {
      req(input$select_product2_input)
      shinyjs::show("column20")
      dataset2$filtrados_producto2 <- dataset2$filtrados_subsector2 %>% dplyr::filter(producto == input$select_product2_input)
    }, once = FALSE)
    
    output$selectinput_1 <- renderUI({
      if(input$checkbox_sector == TRUE){
        data <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input)
        choices <- unique(data$posicion)
      }
      else if(input$checkbox_subsector == TRUE){
        data <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input)
        choices <- unique(data$posicion)
      }
      else{
        choices <- unique(dataset2$filtrados_producto$posicion)
      }
      selectInput(inputId = "selectinput_1_input", label = "Mostrar según:", choices = choices, width = "100%")
    })
    
    output$selectinput_2 <- renderUI({
      if(input$checkbox_sector == TRUE){
        req(input$select_sector2_input)
        data <- dataset2$datos %>% dplyr::filter(sector == input$select_sector2_input)
        choices <- unique(data$posicion)
      }
      else if(input$checkbox_subsector == TRUE){
        req(input$select_subsector2_input)
        data <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector2_input)
        choices <- unique(data$posicion)
      }
      else{
        choices <- unique(dataset2$filtrados_producto2$posicion)
      }
      selectInput(inputId = "selectinput_2_input", label = "Mostrar según:", choices = choices, width = "100%")
    })
    
    hc <- reactive({
      req(input$select_sector_input)
      req(input$select_subsector_input)
      req(input$select_product_input)
      req(input$selectinput_1_input)
      if(dataset2$second_line == FALSE){
        if(input$selectinput_1_input == "Mercas"){
          if(input$checkbox_sector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else if(input$checkbox_subsector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else{
            dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input)
          }
        }
        else{
          if(input$checkbox_sector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else if(input$checkbox_subsector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else{
            dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
          }
        }
        hc <- dataset2$filtrado_posicion %>%
          hchart('line', hcaes(x = inicio, y = precio), color = "green") %>%
          hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
          hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
          hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg") %>%
          hc_legend(align = "left", verticalAlign = "top", layout = "vertical", x = 0, y = 100)
        
        if(input$modo_eco == TRUE){
          hc <- hc %>% hc_title(text = "🍃")
        }
        else{
          hc <- hc
        }
      }
      else{
        req(input$selectinput_2_input)
        if(input$selectinput_1_input == "Mercas"){
          if(input$checkbox_sector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(sector == input$select_sector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else if(input$checkbox_subsector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else{
            dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input)
            dataset2$filtrado_posicion2 <- dataset2$filtrados_producto2 %>% dplyr::filter(posicion == input$selectinput_2_input)
          }
        }
        else{
          if(input$checkbox_sector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(sector == input$select_sector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else if(input$checkbox_subsector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else{
            dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
            dataset2$filtrado_posicion2 <- dataset2$filtrados_producto2 %>% dplyr::filter(posicion == input$selectinput_2_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
          }
        }
        
        if(input$checkbox_sector == TRUE){
          hc <- dataset2$filtrado_posicion %>%
            hchart(name = input$select_sector_input, 'line', hcaes(x = inicio, y = precio), color = "green") %>%
            hc_plotOptions(series = list(showInLegend = TRUE)) %>%
            hc_add_series(data = dataset2$filtrado_posicion2, hcaes(x = inicio, y = precio), type = "line", name = input$select_sector2_input) %>%
            hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
            hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
            hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
        }
        else if(input$checkbox_subsector == TRUE){
          hc <- dataset2$filtrado_posicion %>%
            hchart(name = input$select_subsector_input, 'line', hcaes(x = inicio, y = precio), color = "green") %>%
            hc_plotOptions(series = list(showInLegend = TRUE)) %>%
            hc_add_series(data = dataset2$filtrado_posicion2, hcaes(x = inicio, y = precio), type = "line", name = input$select_subsector2_input) %>%
            hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
            hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
            hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
        }
        else{
          hc <- dataset2$filtrado_posicion %>%
            hchart(name = input$select_product_input, 'line', hcaes(x = inicio, y = precio), color = "green") %>%
            hc_plotOptions(series = list(showInLegend = TRUE)) %>%
            hc_add_series(data = dataset2$filtrado_posicion2, hcaes(x = inicio, y = precio), type = "line", name = input$select_product2_input) %>%
            hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
            hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
            hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
        }
        
        if(input$modo_eco == TRUE){
          hc <- hc %>% hc_title(text = "🍃")
        }
        else{
          hc <- hc
        }
      }
    })
    
    hc2 <- reactive({
      req(input$fecha_1_input)
      req(input$fecha_2_input)
      if(dataset2$second_line == FALSE){
        if(input$series_2 == "- - -" & input$series_3 == "- - -"){
          if(input$selectinput_1_input == "Mercas"){
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1)
            }
          }
          else{
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
            }
          }
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              hc <- dataset2$filtrado_posicion %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Marzo")
            index_end <- which(meses == "Noviembre")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart('line', hcaes(x = week, y = precio, group = year)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
              hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                         pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
          dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.</h4>")
        }
        else if(input$series_3 == "- - -"){
          if(input$selectinput_1_input == "Mercas"){
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1$precio) >= mean(dataset2$filtrado_year2$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_1, "</strong> en comparación con el año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1$precio)-mean(dataset2$filtrado_year2$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_2, "</strong> en comparación con el año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2$precio)-mean(dataset2$filtrado_year1$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
              
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1$precio) >= mean(dataset2$filtrado_year2$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_1, "</strong> en comparación con el año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1$precio)-mean(dataset2$filtrado_year2$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_2, "</strong> en comparación con el año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2$precio)-mean(dataset2$filtrado_year1$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2)
              
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1$precio) >= mean(dataset2$filtrado_year2$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el año <strong>", input$series_1, "</strong> en comparación con el año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1$precio)-mean(dataset2$filtrado_year2$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el año <strong>", input$series_2, "</strong> en comparación con el año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2$precio)-mean(dataset2$filtrado_year1$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
          }
          else{
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1$precio) >= mean(dataset2$filtrado_year2$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_1, "</strong> en comparación con el año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1$precio)-mean(dataset2$filtrado_year2$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_2, "</strong> en comparación con el año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2$precio)-mean(dataset2$filtrado_year1$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1$precio) >= mean(dataset2$filtrado_year2$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_1, "</strong> en comparación con el año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1$precio)-mean(dataset2$filtrado_year2$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el año <strong>", input$series_2, "</strong> en comparación con el año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2$precio)-mean(dataset2$filtrado_year1$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
              
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2 <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1$precio) >= mean(dataset2$filtrado_year2$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el año <strong>", input$series_1, "</strong> en comparación con el año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1$precio)-mean(dataset2$filtrado_year2$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_producto_input, "</strong> es, en promedio, superior en el año <strong>", input$series_2, "</strong> en comparación con el año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2$precio)-mean(dataset2$filtrado_year1$precio))/mean(c(mean(dataset2$filtrado_year1$precio), mean(dataset2$filtrado_year2$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
          }
          
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              
              if(input$checkbox_sector == TRUE){
                if(length(unique(c(input$series_1, input$series_2))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else if(input$checkbox_subsector == TRUE){
                if(length(unique(c(input$series_1, input$series_2))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else{
                if(length(unique(c(input$series_1, input$series_2))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              if(input$checkbox_sector == TRUE){
                if(length(unique(c(input$series_1, input$series_2))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else if(input$checkbox_subsector == TRUE){
                if(length(unique(c(input$series_1, input$series_2))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else{
                if(length(unique(c(input$series_1, input$series_2))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              
              hc <- dataset2$filtrado_posicion %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Marzo")
            index_end <- which(meses == "Noviembre")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            
            if(input$checkbox_sector == TRUE){
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              if(length(unique(c(input$series_1, input$series_2))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart('line', hcaes(x = week, y = precio, group = year)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
              hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                         pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
        }
        else if(input$series_2 == "- - -"){
          if(input$selectinput_1_input == "Mercas"){
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_3)
              
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
          }
          else{
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year1_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year2_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year1_fecha$precio) >= mean(dataset2$filtrado_year2_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year1_fecha$precio)-mean(dataset2$filtrado_year2_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year2_fecha$precio)-mean(dataset2$filtrado_year1_fecha$precio))/mean(c(mean(dataset2$filtrado_year1_fecha$precio), mean(dataset2$filtrado_year2_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
            }
          }
          
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              
              if(input$checkbox_sector == TRUE){
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else if(input$checkbox_subsector == TRUE){
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else{
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              if(input$checkbox_sector == TRUE){
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else if(input$checkbox_subsector == TRUE){
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else{
                if(length(unique(c(input$series_1, input$series_3))) == 2){
                  dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                  else{
                    dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              
              hc <- dataset2$filtrado_posicion %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Noviembre")
            index_end <- which(meses == "Marzo")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            
            if(input$checkbox_sector == TRUE){
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              if(length(unique(c(input$series_1, input$series_3))) == 2){
                dataset2$filtrado_year3_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year4_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year3_fecha$precio) >= mean(dataset2$filtrado_year4_fecha$precio)){
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year3_fecha$precio)-mean(dataset2$filtrado_year4_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
                else{
                  dataset2$texto_analisis <- HTML(paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_3, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year4_fecha$precio)-mean(dataset2$filtrado_year3_fecha$precio))/mean(c(mean(dataset2$filtrado_year3_fecha$precio), mean(dataset2$filtrado_year4_fecha$precio)))*100, 2)), "%</strong>.</h4>"))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart('line', hcaes(x = week, y = precio, group = year)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
              hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                         pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
        }
        else{
          if(input$selectinput_1_input == "Mercas"){
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year5_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year6_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year7_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year5_fecha$precio) >= mean(dataset2$filtrado_year6_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year5_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year5_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year6_fecha$precio) >= mean(dataset2$filtrado_year7_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year7_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year7_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year5_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year6_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year7_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year5_fecha$precio) >= mean(dataset2$filtrado_year6_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year5_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year5_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year6_fecha$precio) >= mean(dataset2$filtrado_year7_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year7_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year7_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2 | year == input$series_3)
              
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year5_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year6_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year7_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year5_fecha$precio) >= mean(dataset2$filtrado_year6_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year5_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year5_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year6_fecha$precio) >= mean(dataset2$filtrado_year7_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year7_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year7_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
          }
          else{
            if(input$checkbox_sector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year5_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year6_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year7_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year5_fecha$precio) >= mean(dataset2$filtrado_year6_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year5_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year5_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year6_fecha$precio) >= mean(dataset2$filtrado_year7_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year7_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year7_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
              
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year5_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year6_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year7_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year5_fecha$precio) >= mean(dataset2$filtrado_year6_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year5_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year5_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year6_fecha$precio) >= mean(dataset2$filtrado_year7_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year7_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year7_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1 | year == input$series_2 | year == input$series_3) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
              
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year5_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year6_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year7_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year5_fecha$precio) >= mean(dataset2$filtrado_year6_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year5_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year5_fecha$precio))/mean(c(mean(dataset2$filtrado_year5_fecha$precio), mean(dataset2$filtrado_year6_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year6_fecha$precio) >= mean(dataset2$filtrado_year7_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year6_fecha$precio)-mean(dataset2$filtrado_year7_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year7_fecha$precio)-mean(dataset2$filtrado_year6_fecha$precio))/mean(c(mean(dataset2$filtrado_year6_fecha$precio), mean(dataset2$filtrado_year7_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
          }
          
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              
              if(input$checkbox_sector == TRUE){
                if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                  dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                  dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                    texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  else{
                    texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  
                  if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                  else{
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else if(input$checkbox_subsector == TRUE){
                if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                  dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                  dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                    texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  else{
                    texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  
                  if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                  else{
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else{
                if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                  dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                  dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                    texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  else{
                    texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  
                  if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                  else{
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              if(input$checkbox_sector == TRUE){
                if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                  dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                  dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                    texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  else{
                    texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  
                  if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                  else{
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else if(input$checkbox_subsector == TRUE){
                if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                  dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                  dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                    texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  else{
                    texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  
                  if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                  else{
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              else{
                if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                  dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_1)
                  dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_2)
                  dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion %>% dplyr::filter(year == input$series_3)
                  if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                    texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  else{
                    texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(texto1)
                  }
                  
                  if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                  else{
                    texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                    dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                  }
                }
                else{
                  dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
                }
              }
              
              hc <- dataset2$filtrado_posicion %>%
                hchart('line', hcaes(x = week, y = precio, group = year)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
                hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                           pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Noviembre")
            index_end <- which(meses == "Marzo")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            
            if(input$checkbox_sector == TRUE){
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo de pandemia del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del sector <strong>", input$select_sector_input, "</strong> es, en general, mayor en el periodo de pandemia del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else if(input$checkbox_subsector == TRUE){
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo de pandemia del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del subsector <strong>", input$select_subsector_input, "</strong> es, en general, mayor en el periodo de pandemia del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            else{
              if(length(unique(c(input$series_1, input$series_2, input$series_3))) == 3){
                dataset2$filtrado_year8_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_1)
                dataset2$filtrado_year9_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_2)
                dataset2$filtrado_year10_fecha <- dataset2$filtrado_posicion_mes %>% dplyr::filter(year == input$series_3)
                if(mean(dataset2$filtrado_year8_fecha$precio) >= mean(dataset2$filtrado_year9_fecha$precio)){
                  texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_1, "</strong> en comparación con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year8_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                else{
                  texto1 <- paste0("<h4>El precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en promedio, superior en el periodo de pandemia del año <strong>", input$series_2, "</strong> en comparación con el del año <strong>", input$series_1, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year8_fecha$precio))/mean(c(mean(dataset2$filtrado_year8_fecha$precio), mean(dataset2$filtrado_year9_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(texto1)
                }
                
                if(mean(dataset2$filtrado_year9_fecha$precio) >= mean(dataset2$filtrado_year10_fecha$precio)){
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_2, "</strong> y <strong>", input$series_3, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo de pandemia del año <strong>", input$series_2, "</strong> en contraste con el del año <strong>", input$series_3, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year9_fecha$precio)-mean(dataset2$filtrado_year10_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
                else{
                  texto2 <- paste0("<h4>En cuanto a la relacion entre los años <strong>", input$series_3, "</strong> y <strong>", input$series_2, "</strong>, el precio (€/kg) del producto <strong>", input$select_product_input, "</strong> es, en general, mayor en el periodo de pandemia del año <strong>", input$series_3, "</strong> en contraste con el del año <strong>", input$series_2, "</strong>, con una diferencia de aproximadamente un <strong>", as.character(round((mean(dataset2$filtrado_year10_fecha$precio)-mean(dataset2$filtrado_year9_fecha$precio))/mean(c(mean(dataset2$filtrado_year9_fecha$precio), mean(dataset2$filtrado_year10_fecha$precio)))*100, 2)), "%</strong>.</h4>")
                  dataset2$texto_analisis <- HTML(paste0(texto1, texto2))
                }
              }
              else{
                dataset2$texto_analisis <- HTML("<h4>Por favor, seleccione más de un año diferente en los <strong>selectores de arriba</strong> para poder realizar una comparativa anual.")
              }
            }
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart('line', hcaes(x = week, y = precio, group = year)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = "Semana"), allowDecimals = FALSE) %>%
              hc_tooltip(headerFormat = "<b>Semana {point.x}</b><br>",
                         pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
        }
      }
      else{
        req(input$selectinput_2_input)
        if(input$selectinput_1_input == "Mercas"){
          if(input$checkbox_sector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(sector == input$select_sector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else if(input$checkbox_subsector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else{
            dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1)
            dataset2$filtrado_posicion2 <- dataset2$filtrados_producto2 %>% dplyr::filter(posicion == input$selectinput_2_input) %>% dplyr::filter(year == input$series_1)
          }
        }
        else{
          if(input$checkbox_sector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(sector == input$select_sector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else if(input$checkbox_subsector == TRUE){
            dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
            dataset2$filtrado_posicion2 <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector2_input) %>% dplyr::filter(posicion == input$selectinput_2_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          }
          else{
            dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
            dataset2$filtrado_posicion2 <- dataset2$filtrados_producto2 %>% dplyr::filter(posicion == input$selectinput_2_input) %>% dplyr::filter(year == input$series_1) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
          }
        }
          
        if(input$checkbox_sector == TRUE){
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              dataset2$filtrado_posicion_mes2 <- dataset2$filtrado_posicion2 %>% dplyr::filter(month %in% meses_filtrado)
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart(name = paste(input$select_sector_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
                hc_plotOptions(series = list(showInLegend = TRUE)) %>%
                hc_add_series(data = dataset2$filtrado_posicion_mes2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_sector2_input, input$series_1)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
                hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              hc <- dataset2$filtrado_posicion %>%
                hchart(name = paste(input$select_sector_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
                hc_plotOptions(series = list(showInLegend = TRUE)) %>%
                hc_add_series(data = dataset2$filtrado_posicion2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_sector2_input, input$series_1)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
                hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Noviembre")
            index_end <- which(meses == "Marzo")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            dataset2$filtrado_posicion_mes2 <- dataset2$filtrado_posicion2 %>% dplyr::filter(month %in% meses_filtrado)
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart(name = paste(input$select_sector_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
              hc_plotOptions(series = list(showInLegend = TRUE)) %>%
              hc_add_series(data = dataset2$filtrado_posicion_mes2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_sector2_input, input$series_1)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
              hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
        }
        else if(input$checkbox_subsector == TRUE){
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              dataset2$filtrado_posicion_mes2 <- dataset2$filtrado_posicion2 %>% dplyr::filter(month %in% meses_filtrado)
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart(name = paste(input$select_subsector_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
                hc_plotOptions(series = list(showInLegend = TRUE)) %>%
                hc_add_series(data = dataset2$filtrado_posicion_mes2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_subsector2_input, input$series_1)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
                hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              hc <- dataset2$filtrado_posicion %>%
                hchart(name = paste(input$select_subsector_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
                hc_plotOptions(series = list(showInLegend = TRUE)) %>%
                hc_add_series(data = dataset2$filtrado_posicion2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_subsector2_input, input$series_1)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
                hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Noviembre")
            index_end <- which(meses == "Marzo")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            dataset2$filtrado_posicion_mes2 <- dataset2$filtrado_posicion2 %>% dplyr::filter(month %in% meses_filtrado)
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart(name = paste(input$select_subsector_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
              hc_plotOptions(series = list(showInLegend = TRUE)) %>%
              hc_add_series(data = dataset2$filtrado_posicion_mes2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_subsector2_input, input$series_1)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
              hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
        }
        else{
          if(input$checkbox_pandemia == FALSE){
            if(input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
              index_start <- which(meses == input$fecha_1_input)
              index_end <- which(meses == input$fecha_2_input)
              meses_filtrado <- meses[index_start:index_end]
              
              dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
              dataset2$filtrado_posicion_mes2 <- dataset2$filtrado_posicion2 %>% dplyr::filter(month %in% meses_filtrado)
              
              hc <- dataset2$filtrado_posicion_mes %>%
                hchart(name = paste(input$select_product_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
                hc_plotOptions(series = list(showInLegend = TRUE)) %>%
                hc_add_series(data = dataset2$filtrado_posicion_mes2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_product2_input, input$series_1)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
                hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
            else{
              hc <- dataset2$filtrado_posicion %>%
                hchart(name = paste(input$select_product_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
                hc_plotOptions(series = list(showInLegend = TRUE)) %>%
                hc_add_series(data = dataset2$filtrado_posicion2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_product2_input, input$series_1)) %>%
                hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
                hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
                hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
            }
          }
          else{
            index_start <- which(meses == "Noviembre")
            index_end <- which(meses == "Marzo")
            meses_filtrado <- meses[index_start:index_end]
            
            dataset2$filtrado_posicion_mes <- dataset2$filtrado_posicion %>% dplyr::filter(month %in% meses_filtrado)
            dataset2$filtrado_posicion_mes2 <- dataset2$filtrado_posicion2 %>% dplyr::filter(month %in% meses_filtrado)
            
            hc <- dataset2$filtrado_posicion_mes %>%
              hchart(name = paste(input$select_product_input, input$series_1), 'line', hcaes(x = week, y = precio, group = year), color = "green") %>%
              hc_plotOptions(series = list(showInLegend = TRUE)) %>%
              hc_add_series(data = dataset2$filtrado_posicion_mes2, hcaes(x = week, y = precio, group = year), type = "line", name = paste(input$select_product2_input, input$series_1)) %>%
              hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
              hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
              hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg")
          }
        }
      }
      
      if(input$modo_eco == TRUE){
        hc <- hc %>% hc_title(text = "🍃")
      }
      else{
        hc <- hc
      }
    })
    
    output$grafica_1 <- renderHighchart({
      req(input$selectinput_1_input)
      req(input$select_product_input)
      req(input$select_subsector_input)
      req(input$select_sector_input)
      
      hc()
    })
    
    output$grafica_2 <- renderHighchart({
      req(input$selectinput_1_input)
      req(input$select_product_input)
      req(input$select_subsector_input)
      req(input$select_sector_input)
      
      hc2()
    })
    
    observeEvent(input$selectinput_1_input, {
      if(input$selectinput_1_input == "Agricultor" | input$selectinput_1_input == "Subasta"){
        shinyjs::show("column7")
      }
      else{
        shinyjs::hide("column7")
      }
    }, once = FALSE)
    
    observeEvent(input$checkbox_sector, {
      if(input$checkbox_sector == TRUE){
        shinyjs::hide("column2")
        shinyjs::hide("column3")
        shinyjs::hide("column4")
        shinyjs::hide("column9")
        
        if(dataset2$second_line == TRUE){
          shinyjs::hide("column17")
          shinyjs::hide("column18")
          shinyjs::hide("column20")
        }
      }
      else{
        shinyjs::show("column2")
        shinyjs::show("column3")
        shinyjs::show("column4")
        shinyjs::show("column9")
        
        if(dataset2$second_line == TRUE){
          shinyjs::show("column17")
          shinyjs::show("column18")
          shinyjs::show("column20")
        }
      }
    }, once = FALSE)
    
    observeEvent(input$checkbox_subsector, {
      if(input$checkbox_subsector == TRUE){
        shinyjs::hide("column1")
        shinyjs::hide("column3")
        shinyjs::hide("column4")
        shinyjs::hide("column8")
        
        if(dataset2$second_line == TRUE){
          shinyjs::hide("column16")
          shinyjs::hide("column18")
          shinyjs::hide("column20")
        }
      }
      else{
        shinyjs::show("column1")
        shinyjs::show("column3")
        shinyjs::show("column4")
        shinyjs::show("column8")
        
        if(dataset2$second_line == TRUE){
          shinyjs::show("column16")
          shinyjs::show("column18")
          shinyjs::show("column20")
        }
      }
    }, once = FALSE)
    
    observeEvent(input$add_second, {
      shinyjs::show("column15")
      shinyjs::hide("column14")
      shinyjs::show("column16")
      shinyjs::show("column17")
      shinyjs::show("column18")
      shinyjs::show("column19")
      shinyjs::hide("column12")
      shinyjs::hide("column13")
      
      if(input$checkbox_sector == TRUE){
        shinyjs::hide("column17")
        shinyjs::hide("column18")
        shinyjs::hide("column20")
      }
      
      if(input$checkbox_subsector == TRUE){
        shinyjs::hide("column16")
        shinyjs::hide("column18")
        shinyjs::hide("column20")
      }
      
      dataset2$second_line <- TRUE
    }, once = FALSE)
    
    observeEvent(input$delete_second, {
      shinyjs::hide("column15")
      shinyjs::show("column14")
      shinyjs::hide("column16")
      shinyjs::hide("column17")
      shinyjs::hide("column18")
      shinyjs::hide("column19")
      shinyjs::hide("column20")
      shinyjs::show("column12")
      shinyjs::show("column13")
      
      dataset2$second_line <- FALSE
    }, once = FALSE)
    
    output$fecha_2 <- renderUI({
      req(input$fecha_1_input)
      choices <- c("- - -", meses)
      
      if(input$fecha_1_input == "- - -"){
        choices <- c("- - -")
      }
      else{
        index_month  <- which(choices == input$fecha_1_input)
        choices <- choices[index_month:length(choices)]
      }
      selectInput(inputId = "fecha_2_input", label = "Hasta:", choices = choices, width = "100%", selected = choices[length(choices)])
    })
    
    observeEvent(input$fecha_2_input, {
      if(input$fecha_2_input == input$fecha_1_input & input$fecha_1_input != "- - -" & input$fecha_2_input != "- - -"){
        print(input$fecha_1_input)
        print(input$fecha_2_input)
        shinyjs::show("column22")
      }
      else{
        shinyjs::hide("column22")
      }
    }, once = FALSE)
    
    observeEvent(input$checkbox_pandemia, {
      if(input$checkbox_pandemia == TRUE){
        shinyjs::hide("column21")
        shinyjs::hide("column22")
        shinyjs::show("column24")
      }
      else{
        shinyjs::show("column21")
        shinyjs::hide("column24")
      }
    }, once = FALSE)
    
    output$desde_select <- renderUI({
      dateInput(inputId = "desde_select_input", list(icon("calendar-alt"), "Desde:"), value = unique(dataset2$covid_total$Fecha)[1], min = unique(dataset2$covid_total$Fecha)[1], max = unique(dataset2$covid_total$Fecha)[nrow(dataset2$covid_total)], format = "yyyy-mm-dd", language = "es")
    })
    
    output$hasta_select <- renderUI({
      dateInput(inputId = "hasta_select_input", list(icon("calendar-alt"), "Hasta:"), value = unique(dataset2$covid_total$Fecha)[nrow(dataset2$covid_total)], min = unique(dataset2$covid_total$Fecha)[1], max = unique(dataset2$covid_total$Fecha)[nrow(dataset2$covid_total)], format = "yyyy-mm-dd", language = "es")
    })
    
    output$variable_pandemia <- renderUI({
      selectInput(inputId = "variable_pandemia_input", label = "Variable COVID", choices = names(dataset2$covid_total)[-1], selected = names(dataset2$covid_total)[-1][1], width = "100%")
    })
    
    hc_pandemia <- reactive({
      req(input$select_sector_input)
      req(input$select_subsector_input)
      req(input$select_product_input)
      req(input$selectinput_1_input)
      req(input$desde_select_input)
      req(input$hasta_select_input)
      req(input$variable_pandemia_input)
      
      dataset2$covid_total_filtrados <- dataset2$covid_total %>% dplyr::filter(Fecha >= input$desde_select_input & Fecha <= input$hasta_select_input)
      
      if(input$variable_pandemia_input == "Casos"){
        dataset2$covid_variable <- "Casos"
        hc_pandemia <- dataset2$covid_total_filtrados %>%
          hchart('line', hcaes(x = Fecha, y = Casos), color = "red") %>%
          hc_yAxis(title = list(text = "Casos"), allowDecimals = FALSE) %>%
          hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
          hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
          hc_tooltip(headerFormat = "<b>{point.x:%Y-%m-%d}</b><br>", pointFormat = "<b>Casos:</b> {point.Casos}") %>%
          hc_legend(align = "left", verticalAlign = "top", layout = "vertical", x = 0, y = 100) %>%
          hc_title(text = "Casos de infección")
      }
      else if(input$variable_pandemia_input == "IA14"){
        dataset2$covid_variable <- "IA14"
        hc_pandemia <- dataset2$covid_total_filtrados %>%
          hchart('line', hcaes(x = Fecha, y = IA14), color = "red") %>%
          hc_yAxis(title = list(text = "IA14"), allowDecimals = FALSE) %>%
          hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
          hc_tooltip(headerFormat = "<b>{point.x:%Y-%m-%d}</b><br>", pointFormat = "<b>IA14:</b> {point.IA14}") %>%
          hc_legend(align = "left", verticalAlign = "top", layout = "vertical", x = 0, y = 100) %>%
          hc_title(text = "Incidencia Acumulada 14 días / 100k hab.")
      }
      else{
        dataset2$covid_variable <- "Fallecidos"
        hc_pandemia <- dataset2$covid_total_filtrados %>%
          hchart('line', hcaes(x = Fecha, y = Fallecidos), color = "red") %>%
          hc_yAxis(title = list(text = "Fallecidos"), allowDecimals = FALSE) %>%
          hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
          hc_tooltip(headerFormat = "<b>{point.x:%Y-%m-%d}</b><br>", pointFormat = "<b>Fallecidos:</b> {point.Fallecidos}") %>%
          hc_legend(align = "left", verticalAlign = "top", layout = "vertical", x = 0, y = 100) %>%
          hc_title(text = "Fallecidos")
      }
      
      hc_pandemia <- hc_pandemia %>%
        hc_xAxis(
          plotLines = list(
            list(
              label = list(text = "Se acaba el stock de papel higiénico"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
            ),
            list(
              label = list(text = "Horarios para salir a la calle"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
            ),
            list(
              label = list(text = "Apertura primeros bares"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
            ),
            list(
              label = list(text = "Fin primer estado de alarma"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
            ),
            list(
              label = list(text = "Segunda ola"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
            ),
            list(
              label = list(text = "Aprobación primera vacuna"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
            ),
            list(
              label = list(text = "Primera vacunación"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-12-28", tz = "UTC"))
            ),
            list(
              label = list(text = "Tercera ola"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2021-01-07", tz = "UTC"))
            )
          )
        )
    })
    
    hc_pandemia2 <- reactive({
      req(input$select_sector_input)
      req(input$select_subsector_input)
      req(input$select_product_input)
      req(input$selectinput_1_input)
      req(input$desde_select_input)
      req(input$hasta_select_input)
      req(input$variable_pandemia_input)
      
      if(input$selectinput_1_input == "Mercas"){
        if(input$checkbox_sector == TRUE){
          dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          titulo <- paste("€/kg del sector:", input$select_sector_input)
        }
        else if(input$checkbox_subsector == TRUE){
          dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          titulo <- paste("€/kg del subsector:", input$select_subsector_input)
        }
        else{
          dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input)
          titulo <- paste("€/kg del producto:", input$select_product_input)
        }
      }
      else{
        if(input$checkbox_sector == TRUE){
          dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(sector == input$select_sector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          titulo <- paste("€/kg del sector:", input$select_sector_input)
        }
        else if(input$checkbox_subsector == TRUE){
          dataset2$filtrado_posicion <- dataset2$datos %>% dplyr::filter(subsector == input$select_subsector_input) %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE), 2))
          titulo <- paste("€/kg del subsector:", input$select_subsector_input)
        }
        else{
          dataset2$filtrado_posicion <- dataset2$filtrados_producto %>% dplyr::filter(posicion == input$selectinput_1_input) %>% group_by(inicio) %>% dplyr::mutate(precio = round(mean(precio, na.rm = TRUE),2))
          titulo <- paste("€/kg del producto:", input$select_product_input)
        }
      }
      
      dataset2$filtrado_posicion_pandemia <- dataset2$filtrado_posicion %>% dplyr::filter(inicio >= input$desde_select_input & fin <= input$hasta_select_input)
      
      hc <- dataset2$filtrado_posicion_pandemia %>%
        hchart('line', hcaes(x = inicio, y = precio), color = "green") %>%
        hc_yAxis(title = list(text = "Precio (€/kg)"), allowDecimals = FALSE) %>%
        hc_xAxis(title = list(text = ""), allowDecimals = FALSE) %>%
        hc_tooltip(pointFormat="<b>Precio:</b> {point.precio} €/kg") %>%
        hc_legend(align = "left", verticalAlign = "top", layout = "vertical", x = 0, y = 100) %>%
        hc_title(text = titulo) %>%
        hc_xAxis(
          plotLines = list(
            list(
              label = list(text = "Se acaba el stock de papel higiénico"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
            ),
            list(
              label = list(text = "Horarios para salir a la calle"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
            ),
            list(
              label = list(text = "Apertura primeros bares"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
            ),
            list(
              label = list(text = "Fin primer estado de alarma"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
            ),
            list(
              label = list(text = "Segunda ola"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
            ),
            list(
              label = list(text = "Aprobación primera vacuna"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
            ),
            list(
              label = list(text = "Nueva cepa COVID"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
            ),
            list(
              label = list(text = "Primera vacunación"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2020-12-28", tz = "UTC"))
            ),
            list(
              label = list(text = "Tercera ola"),
              color = "#000000",
              width = 1,
              value = datetime_to_timestamp(as.Date("2021-01-07", tz = "UTC"))
            )
          )
        )
      
      if(input$modo_eco == TRUE){
        hc <- hc %>% hc_title(text = "🍃")
      }
      else{
        hc <- hc
      }
    })
    
    output$grafica_pandemia <- renderHighchart({
      req(input$selectinput_1_input)
      req(input$select_product_input)
      req(input$select_subsector_input)
      req(input$select_sector_input)
      req(input$desde_select_input)
      req(input$hasta_select_input)
      req(input$variable_pandemia_input)
      
      hc_pandemia()
    })
    
    output$grafica_precio_pandemia <- renderHighchart({
      req(input$selectinput_1_input)
      req(input$select_product_input)
      req(input$select_subsector_input)
      req(input$select_sector_input)
      req(input$desde_select_input)
      req(input$hasta_select_input)
      req(input$variable_pandemia_input)
      
      hc_pandemia2()
    })
    
    output$texto_correlacion <- renderUI({
      req(input$selectinput_1_input)
      req(input$select_product_input)
      req(input$select_subsector_input)
      req(input$select_sector_input)
      req(input$desde_select_input)
      req(input$hasta_select_input)
      req(input$variable_pandemia_input)
      
      x <- dataset2$filtrado_posicion_pandemia$precio
      y <- dataset2$covid_total$Casos
      N <- min(length(x), length(y))
      
      dataset2$texto_correlacion <- ""
      
      HTML(dataset2$texto_correlacion)
    })
    
    output$texto_analisis <- renderUI({
      req(input$series_1)
      req(input$series_2)
      req(input$series_3)
      req(input$fecha_1_input)
      req(input$fecha_2_input)
      
      dataset2$texto_analisis
    })
}

router <- make_router(
    route("/", home_page, home_server),
    route("mercas", mercas_page, mercas_server),
    route("consumo", consumo_page, consumo_pagina_server),
    route("junta_andalucia", junta_andalucia, junta_andalucia_server),
    route("comercio_exterior", exterior_page, exterior_server)
)

menu_button <- function(link = "/", ...) {
    a(class="button-guay",
      href=route_link(link),
      span(class="button-text", ...)
    )
}

ui <- fluidPage(
    shinyjs::useShinyjs(),
    title = "Cajamar TeamBoleta!",
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"),
        HTML('<script src="https://kit.fontawesome.com/260fafe623.js" crossorigin="anonymous"></script>')
    ),
    div(class="big-container",
        div(id="dashboard-header",
            div(class="mini-header",
                menu_button(link="/", "Inicio"),
                menu_button(link="consumo", "Consumo en España"),
                menu_button(link="junta_andalucia", "Junta de Andalucía"),
                menu_button(link="mercas", "Mercas"),
                menu_button(link="comercio_exterior", "Comercio exterior")
            ),
            div(id="mini-header",
                div(id="eco_box",
                    eco_toggle("modo_eco"),
                    HTML("&nbsp&nbsp&nbsp <b>Modo ECO</b>"),
                    eco_tooltip
                )
            )
        ),
        router$ui
    ),
    div(id="footer",
        div(
        HTML("Cajamar UniversityHack 2021 - Reto Cajamar Agro Analysis")
        ),
        div(
        HTML('<i class="fas fa-code"></i>&nbsp; with &nbsp;<i class="fas fa-heart"></i>&nbsp; by &nbsp;<b>TeamBoleta!</b>')
        )
    )
)
server <- function(input, output, session) {
  router$server(input, output, session)
}
shinyApp(ui, server)
