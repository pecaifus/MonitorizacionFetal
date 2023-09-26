library(shiny)
library(shinydashboard)
library(shinyalert)
library(tidyverse)
library(shinyFiles)
library(scales)
library(zoo)
library(shinythemes)
library(plotly)


ui_1 <- dashboardPage(skin = "black",

  dashboardHeader(
    title = "Visualizador"
  ),

  dashboardSidebar(

    tags$h3("Zona de menús", align = "left", class = "subrayado",
            style = "font-size: 20px; font-family:Verdana; color: #F0FFFF; margin-left: 25px"),

    hr(),

    sidebarMenu(
      tags$head(
        tags$style(
          HTML("
                .subrayado {
                text-decoration: underline;}"
             )
          )
        ),

      actionButton("guia", "Guía de uso de la app",
                   style = "color: #005A47; background-color: #fff; border-color: #fff"),
      br(),

      shinyDirButton('folder',
                     'Seleccionar archivo',
                     'Seleccione el registro que desea visualizar:', FALSE,
                     style = "color: #005A47; background-color: #fff; border-color: #fff"),
      br(),

      checkboxInput("Eliminar", "Eliminar Outliers"),
      br(),

      uiOutput("slide"),
      tags$strong("Para volver a la vista general arrastra la barra hasta 0"),
      br(),
      
      strong("Leyenda del gráfico"),
      HTML("<ul>
              <li style=color:red;>HR1</li>
              <li style=color:green;>HR2</li>
              <li style=color:blue;>MHR</li>
              <li style=color:brown;>TOCO</li>
           </ul>"),
      br(),
      
      checkboxInput("VCP", "Ocultar cambios de VCP"),
      checkboxInput("SPO2", "Ocultar cambios de SPO2"),
      checkboxInput("Pmedia", "Ocultar cambios de Pmedia")
    )
  ),
  dashboardBody(
    h3("Gráfico sobre las frecuencias cardíacas y actividad uterina",
       align = "center"),
    plotlyOutput("Graf"),
    dataTableOutput("tabla")
  )
)

ui_2 <- fluidPage(

theme = shinytheme("lumen"),
tags$head(
  tags$style(HTML("
  .subrayado {
    text-decoration: underline;
  }"))),


    fluidRow(
      style = "background-color:#00B48E",

      tags$h2(tags$strong("Visualizador de monitorización fetal"), align = "center",
             style = "font-size: 45px; font-family: Verdana; color: #FFFFFF")),

    fluidRow(
      style = "background-color:#00B48E;",

      tags$h3("Zona de menús", align = "left", class = "subrayado",
              style = "font-size: 20px; font-family:Verdana; color: #F0FFFF; margin-left: 25px"),
      
      
      br(),
      column(3, actionButton("guia", "Guía de uso de la app",
                             style = "color: #005A47; background-color: #fff; border-color: #fff")),

      column(3, shinyDirButton('folder',
                               'Seleccionar archivo',
                               'Seleccione el registro que desea visualizar:', FALSE,
                               style = "color: #005A47; background-color: #fff; border-color: #fff")),

      column(2, checkboxInput("Eliminar", "Eliminar Outliers")),
  
      column(4, uiOutput("slide"),
                tags$strong("Para volver a la vista general arrastra la barra hasta 0"))
    ),

    fluidRow(
      style = "background-color:#00B48E",
      column(3, strong("Leyenda del gráfico"),
             HTML("<ul>
              <li style=color:red;>HR1</li>
              <li style=color:green;>HR2</li>
              <li style=color:blue;>MHR</li>
              <li style=color:brown;>TOCO</li>
           </ul>")),
      
      column(3, checkboxInput("VCP", "Ocultar cambios de VCP"),
             checkboxInput("SPO2", "Ocultar cambios de SPO2"),
             checkboxInput("Pmedia", "Ocultar cambios de Pmedia"))
    ),

    fluidRow(
      h3("Gráfico sobre las frecuencias cardíacas y actividad uterina",
         align = "center"),
      plotlyOutput("Graf"),
      dataTableOutput("tabla")
    )

)

server <- function(input, output, session) {
  
  MetodoDesviaciones <- function(vector){
    media <- mean(vector)
    des <- sd(vector)
    lim_inf <- media - 3 * des
    lim_sup <- media + 3 * des
    
    return(vector < lim_inf | vector > lim_sup)
  }
  
  MetodoDerivadas <- function(vector, umbral){
    res <- diff(vector)
    out <- c(0, res)
    return(abs(out) > umbral)
  }
  
  cambios <- function(vector){
    res <- c(0, diff(vector))
    return(res > 0)
  }
  
  observeEvent(input$guia, {
    shinyalert(
      title = "Guía de uso de la aplicación",
      text = HTML("<p>
      <u> Esta es una aplicación en pruebas. Dispones de una serie de 
      botones para configurar el gráfico a tu gusto. </u> </br> </br> 
      
      <strong> 1. </strong> Primero debes indicar el directorio en el que se encunetran
      los archivos mediante el botón. </br> </br> 
      
      <strong> 2. </strong> También tienes un botón que al pulsarlo
      se eliminan los ouliers </br></br>
      
      <strong> 3. </strong> En tercer lugar, tras cargar los archivo se
      muestra una línea temporal con la que te puedes situar en un punto
      concreto de la señal. Además, como se indica debajo, para volver a
      la vista general debes arrastrar la barra al 0. </br></br>
      
      <strong> 4. </strong> A continuación, se indica una leyenda del gráfico
      mediante tres colores que indican las señales que se representan. 
      </br></br>
                  
      <strong> 5. </strong> Por último, tienes tres marcadores con los que
      puedes ocultar los cambios de cada una de las señales indicadas en
      el gráfico para manejar el gráfico a tu antojo. 
                  </p>"),
      html = T,
      showCancelButton = FALSE,
      showConfirmButton = TRUE
    )
  })
  
  ## CARGA DE DATOS ##
  
  # volumes <- getVolumes()()
  volumes <- c(MonitorizacionFetal = "../")
  
  shinyDirChoose(input, 'folder', roots=volumes)
  
  directorio <- eventReactive(input$folder, {
    dir_path <- parseDirPath(volumes, input$folder)
    ficheros <- list.files(dir_path)
    return(list(dir_path = dir_path, ficheros = ficheros))
  })
  
  output$slide <- renderUI({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    hr <- as.data.frame(datosDig())
    hr <- hr %>% dplyr::select(HR1, HR2, MHR)
    
    cabecera <- cabeceraDigital()
    inicio <- hms(paste(cabecera[4], cabecera[5], cabecera[6], collapse = ":"))
    
    seg <- nrow(hr) / 4
    sliderInput("momento", "¿En qué momento temporal te quieres situar?",
                0, seg, 0, animate = TRUE)
  })

  # Cargamos los datos del fichero digital
  datosDig <- reactive({
    
    fichero <- directorio()$ficheros[4]
    dir_path <- directorio()$dir_path
    datapath <- paste(dir_path, fichero, sep = "/")
    
    filename <- file(
      description = datapath,
      open = "rb")
    
    data <- readBin(filename, integer(),
                            n = file.info(datapath)$size,
                            size = 1, signed = FALSE)
    
    HR1 <- data[seq(1, length(data), by = 9)]
    HR2 <- data[seq(2, length(data), by = 9)]
    MHR <- data[seq(3, length(data), by = 9)]
    TOCO <- data[seq(4, length(data), by = 9)]
    SPO2 <- data[seq(5, length(data), by = 9)]
    VCP <- data[seq(6, length(data), by = 9)]
    Psistolica <- data[seq(7, length(data), by = 9)]
    Pdiastolica <- data[seq(8, length(data), by = 9)]
    Pmedia <- data[seq(9, length(data), by = 9)]
    
    data <- cbind(HR1, HR2, MHR, TOCO, SPO2, VCP,
                  Psistolica, Pdiastolica, Pmedia)
    
    return(data)
  })
  
  ## CREACIÓN DE GRÁFICOS ##
  
  dataHR <- reactive({
    
    hr <- as.data.frame(datosDig())
    hr <- hr %>% dplyr::select(HR1, HR2, MHR, VCP)
    
    seg <- nrow(hr) / 4
    hr$x <- seq(0, seg, length.out = nrow(hr))
    
    cambioVCP <- cambios(hr$VCP)
    
    hr[!cambioVCP, 4] <- NA
    
    if (input$VCP){
      hr[cambioVCP, 4] <- NA
    }
    
    if (input$Eliminar){
      out1 <- MetodoDesviaciones(hr$HR1)
      out2 <- MetodoDerivadas(hr$HR2, 30)
      out3 <- MetodoDerivadas(hr$MHR, 30)
      
      hr[out1, 1] <- NA
      hr[out2, 2] <- NA
      hr[out3, 3] <- NA 
    }
    
    if (input$momento){
      lim_inf <- input$momento - 50
      lim_sup <- input$momento + 50
      
      hr <- hr %>% filter(x > lim_inf & x < lim_sup)
    }
    
    return(hr)
  })
  
  dataTC <- reactive({
    TC <- as.data.frame(datosDig())
    TC <- TC %>% dplyr::select(TOCO, SPO2, Pmedia)
    
    seg <- nrow(TC) / 4
    TC$x <- seq(0, seg, length.out = nrow(TC))
    
    cambioSPO2 <- cambios(TC$SPO2)
    cambioVCP <- cambios(TC$VCP)
    cambioPM <- cambios(TC$Pmedia)

    TC[!cambioSPO2, 2] <- NA
    TC[!cambioPM, 3] <- NA
    
    if (input$SPO2){
      TC[cambioSPO2, 2] <- NA
    }
    
    if (input$Pmedia){
      TC[cambioPM, 3] <- NA
    }
    
    
    if (input$Eliminar){
      umbral <- 20
      out <- MetodoDerivadas(TC$TOCO, umbral)
      TC[out, 1] <- NA 
    }
    
    if (input$momento){
      lim_inf <- input$momento - 50
      lim_sup <- input$momento + 50

      TC <- TC %>% filter(x > lim_inf & x < lim_sup)
    }
    
    return(TC)
  })
  
  # Creamos el gráfico de HR1, HR2 y MHR
  output$Graf <- renderPlotly({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    g <- subplot(G1(dataHR()), G2(dataTC()), nrows = 2, titleY = TRUE, heights = c(0.6, 0.4)) %>%
      layout(autosize = TRUE)
    
    g
  })
  

  ###############  
  ## CABECERAS ##
  ###############
  
  # Cargamos los datos del fichero analógico
  datosAna <- reactive({
    
    fichero <- directorio()$ficheros[3]
    dir_path <- directorio()$dir_path
    datapath <- paste(dir_path, fichero, sep = "/")
    
    filename <- file(
      description = datapath,
      open = "rb")
    
    data <- readBin(filename, integer(),
                    n = file.info(datapath)$size,
                    size = 2, signed = TRUE)
    return(data)
  })
    
  # Cargamos la cabecera del fichero digital
  cabeceraDigital <- reactive({
    
    fichero <- directorio()$ficheros[2]
    dir_path <- directorio()$dir_path
    datapath <- paste(dir_path, fichero, sep = "/")
    
    filename <- file(
      description = datapath,
      open = "rb")
    
    data <- readBin(filename, integer(),
                    n = file.info(datapath)$size,
                    size = 1, signed = FALSE)
    return(data)
  })
  
  # Mostramos la cabecera digital
  output$cabeceraDigi <- renderPrint({
    validate(
      need(input$folder, 
           message = "Esperando cabecera del fichero digital")
    )
    
    datos <- cabeceraDigital()
    datos
    print(paste("La frecuencia de muestreo es: ", datos[7]))
    print(paste("El número de canales es: ", datos[8]))
    print(datos[1:6])
  })
  
  # Cargamos la cabecera del fichero analógico
  cabeceraAnalogico <- reactive({
    
    fichero <- directorio()$ficheros[1]
    dir_path <- directorio()$dir_path
    datapath <- paste(dir_path, fichero, sep = "/")
    
    filename <- file(
      description = datapath,
      open = "rb")
    
    data <- readBin(filename, raw(),
                    n = file.info(datapath)$size, signed = TRUE)
    
    return(data)
  })
  
  # Mostramos la cabecera analógica
  output$cabeceraAna <- renderPrint({
    validate(
      need(input$folder, 
           message = "Esperando cabecera del fichero analógico")
    )
    
    datos <- cabeceraAnalogico()
    fecha <- as.numeric(datos[1:6])
    resolucion <- as.numeric(datos[7])
    print(paste("La fecha es: ", fecha))
    print(paste("La resolucion es: ", resolucion))
  })

}

G1 <- function(df){
  
  g <- ggplot(df, aes(x = x, y = HR1)) + 
    geom_line(color = "red") +
    geom_line(data = df, aes(x, HR2), color = "green") +
    geom_line(data = df, aes(x, MHR), color = "blue") +
    geom_text(data = df, aes(x = x, y = VCP + 60, label = paste("VCP:", VCP))) +
    xlab("Tiempo (s)") + ylab("Latidos por minuto") + ylim(c(60, 220)) +
    scale_y_continuous(breaks = seq(60, 220, 20), limits = c(60, 200)) +
    scale_x_continuous(breaks = seq(0, 5000, 30)) +
    geom_hline(yintercept = 120, color = "red", linetype = "solid", linewidth = 0.75) +
    geom_hline(yintercept = 160, color = "red", linetype = "solid", linewidth = 0.75) +
    theme(panel.grid.major.y = element_line(color = "#EE6363", size = 0.2, linetype = 1), 
          panel.grid.major.x = element_line(color = "#EE6363", size = 0.2, linetype = 1),
          panel.background = element_rect(fill = 'ivory'))
  
  g1 <- ggplotly(g)
  
  return(g1)
}

G2 <- function(TC){
  
  g <- ggplot(TC, aes(x, TOCO)) + geom_line(color = "brown") +  
    geom_text(data = TC, aes(x = x, y = SPO2, label = paste("SPO2:", SPO2))) +
    geom_text(data = TC, aes(x = x, y = Pmedia, label = paste("Pmed:", Pmedia))) +
    xlab("Tiempo (s)") + ylab("Valor (mmHg)") + ylim(c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 120, 20)) +
    scale_x_continuous(breaks = seq(0, 5000, 30)) +
    theme(panel.grid.major.y = element_line(color = "#EE6363", size = 0.2, linetype = 1), 
          panel.grid.major.x = element_line(color = "#EE6363", size = 0.2, linetype = 1),
          panel.background = element_rect(fill = 'ivory'))
  
  g1 <- ggplotly(g)
  return(g1)
}

# Run the application 
shinyApp(ui = ui_1, server = server)
