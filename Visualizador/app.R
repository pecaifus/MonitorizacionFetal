library(shiny)
library(ggplot2)
library(tidyverse)
library(shinyFiles)
library(zoo)
library(shinythemes)
library(plotly)

ui <- fluidPage(
    theme = shinytheme("darkly"),    
    titlePanel("Visualizador de monitorización fetal"),

    sidebarLayout(
        sidebarPanel(
            shinyDirButton('folder', 'Select a folder',
                           'Please select a folder', FALSE),
            hr(),
            checkboxInput("Eliminar", "Eliminar Outliers")
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("HR1, MHR y TOCO",
                     h1("Gráfico sobre las frecuencias cardiacas
                        actividad uterina"),
                     plotlyOutput("HR")),
            tabPanel("SPO2",
                     h1("Gráfico sobre el oxígeno en sangre"),
                     plotlyOutput("SPO2")),
            tabPanel("Presion",
                     h1("Gráfico sobre las diferentes presiones"),
                     plotlyOutput("PRESION")),
            tabPanel("Datos analógicos", 
                     plotOutput("ana"))
          )
        )
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
  
  ## CARGA DE DATOS ##
  
  # volumes <- getVolumes()()
  volumes <- c(MonitorizacionFetal = "../")
  
  shinyDirChoose(input, 'folder', roots=volumes)
  
  directorio <- eventReactive(input$folder, {
    dir_path <- parseDirPath(volumes, input$folder)
    ficheros <- list.files(dir_path)
    return(list(dir_path = dir_path, ficheros = ficheros))
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
  
  ## CREACIÓN DE GRÁFICOS ##
  
  # Creamos el gráfico de HR1 y MHR
  output$HR <- renderPlotly({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(HR1, HR2, MHR)
    
    cabecera <- cabeceraDigital()
    inicio <- hms(paste(cabecera[4], cabecera[5], cabecera[6], collapse = ":"))

    
    seg <- nrow(df) / 4
    df$x <- seq(0, seg, length.out = nrow(df))
    
    if (input$Eliminar){
      out1 <- MetodoDesviaciones(df$HR1)
      out2 <- MetodoDerivadas(df$HR2, 30)
      out3 <- MetodoDerivadas(df$MHR, 30)
      
      df[out1, 1] <- NA
      df[out2, 2] <- NA
      df[out3, 3] <- NA 
    }

    df <- df %>% pivot_longer(names_to = "Variable",
                              values_to = "Valor", cols = !x)

    
    g1 <- plot_ly(data = df, type = "scatter", mode = "lines") %>%
      add_lines(x = ~ x, y = ~ Valor, color = ~ Variable, colors = "blacks") %>% 
      layout(yaxis = list(title = "Latidos por minuto",
                          gridcolor = "#fd0000"),
             xaxis = list(title = "Tiempo (s)",
                          gridcolor = "#fd0000")) %>%
      config(scrollZoom = TRUE)

    
    TC <- as.data.frame(datosDig())
    TC <- TC %>% dplyr::select(TOCO)
    seg <- nrow(TC) / 4
    TC$x <- seq(0, seg, length.out = nrow(TC))
    
    if (input$Eliminar){
      umbral <- 20
      out <- MetodoDerivadas(TC$TOCO, umbral)
      TC[out, 1] <- NA 
    }
    
    g2 <- plot_ly(data = TC, type = "scatter", 
                  mode = "lines") %>% 
      add_lines(x = ~ x, y = ~ TOCO,
                line = list(color = "black"), name = "TOCO") %>% 
      config(scrollZoom = TRUE) %>% 
      layout(yaxis = list(title = "Valor (mmHg)",
                          gridcolor = "#fd0000"),
             xaxis = list(title = "Tiempo (s)",
                          gridcolor = "#fd0000"))
    
    g <- subplot(g1, g2, nrows = 2, titleY = TRUE, heights = c(0.6, 0.4)) %>%
      layout(title = "Actividad uterina",
             plot_bgcolor = "#ffd8d8", autosize = TRUE)
    
    g
  })
  
  # Creamos el gráfico del SPO2
  output$SPO2 <- renderPlotly({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(SPO2)
    
    seg <- nrow(df) / 4
    df$x <- seq(0, seg, length.out = nrow(df))
    
    df <- df %>% filter(SPO2 > 0)
    
    
    # ggplot(df, aes(x = x, y = SPO2)) +
    #   geom_line(color = "blue") + labs(x = "Tiempo (s)", y = "Porcentaje %")
    
    g1 <- plot_ly(data = df, type = "scatter", mode = "lines") %>% 
      add_trace(x = ~ x, y = ~ SPO2, color = "green") %>% 
      layout(yaxis = list(title = "Porcentaje %"),
             xaxis = list(title = "Tiempo (s)")) %>% 
      config(scrollZoom = TRUE)
    
    g1
  })
  
  # Creamos el gráfico de las presiones
  output$PRESION <- renderPlotly({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(Pmedia, Pdiastolica, Psistolica)
    
    seg <- nrow(df) / 4000
    df$x <- seq(0, seg, length.out = nrow(df))
    
    df_l <- df %>% 
      pivot_longer(names_to = "Variable", values_to = "Valor", cols = !x)
    
    # ggplot(data = df_l ,aes(x = x, y = Valor, color = Variable)) +
    #   geom_line() +
    #   labs(x = "Tiempo (s)", y = "Valor mmHg")
    
    g1 <- plot_ly(data = df_l, type = "scatter", mode = "lines") %>% 
      add_trace(x = ~ x, y = ~ Valor, color = ~ Variable) %>% 
      config(scrollZoom = TRUE) %>% 
      layout(yaxis = list(title = "Valor mmHg"),
             xaxis = list(title = "Tiempo (s)"))
    
    g1
  })
  
  # Creamos el gráfico de las señales a representar
  output$digi <- renderPlot({
    
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    validate(
      need(input$señal, message = "Esperando señal a representar")
    )
    
    # Trabajamos los datos para poder representar las variables juntas
    señal <- data.frame(digitalesFiltrados())
    x <- seq(1, dim(señal)[1])
    datos <- cbind(x, señal)
    datos <- datos %>% 
      pivot_longer(cols = -x, names_to = "señal", values_to = "valor")
    
    # Creamos el gráfico
    ggplot(data = datos, aes(x = x, y = valor)) + 
      geom_line(aes(color = señal)) + facet_wrap(~señal) + theme_bw()
    
  })
  
  # Creamos el gráfico del fichero analógico
  output$ana <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero analógico")
    )
    
    datosAna <- datosAna()
    plot(tail(datosAna, 500))
  })

  ###############  
  ## CABECERAS ##
  ###############
    
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

# Run the application 
shinyApp(ui = ui, server = server)
