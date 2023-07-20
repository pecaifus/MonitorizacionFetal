library(shiny)
library(ggplot2)
library(tidyverse)
library(shinyFiles)
library(zoo)

ui <- fluidPage(

    titlePanel("Visualizador de monitorización fetal"),

    sidebarLayout(
        sidebarPanel(
            shinyDirButton('folder', 'Select a folder',
                           'Please select a folder', FALSE)
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Datos digitales",
                     h1(textOutput("nombre_señal")),
                     plotOutput("g1"),
                     textOutput("cabeceraDigi")),
            tabPanel("Datos analógicos", 
                     plotOutput("ana"),
                     textOutput("cabeceraAna"))
          )
        )
    )
)


server <- function(input, output) {
  
  # volumes <- getVolumes()() # this makes the directory at the base of your computer.
  volumes <- c(MonitorizacionFetal = "C:/Users/perem/OneDrive/Escritorio/Practicas_2023/MonitorizacionFetal")
  
  shinyDirChoose(input, 'folder', roots=volumes)
  
  directorio <- eventReactive(input$folder, {
    dir_path <- parseDirPath(volumes, input$folder)
    ficheros <- list.files(dir_path)
    return(list(dir_path = dir_path, ficheros = ficheros))
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
  
  # Almacenamos las señales seleccionadas por el usuario
  output$señales <- renderUI({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    datosDig <- datosDig()
    selectizeInput("señal", label = "Señales dataset",
                choices = colnames(datosDig), multiple = TRUE)
  })
  
  # Mostramos los normbres de las señales seleccionadas
  output$nombre_señal <- renderText({
    return(input$señal)
  })
  
  # Filtramos los datos digitales por las señales seleccionadas
  digitalesFiltrados <- reactive({
    datosDig <- datosDig()
    seleccionada <- input$señal
    filtrado <- subset(datosDig, select = seleccionada)
    return(filtrado)
  })
  
  # Creamos el gráfico de HR1, MHR y TOCO
  output$g1 <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(HR1, MHR, TOCO)
    seg <- nrow(df)/4000
    df$x <- seq(0, seg, length.out = nrow(df))
    
    # ventana <- 100
    # df$HR1 <- rollmean(df$HR1, k = ventana, fill = NA)
    # df$MHR <- rollmean(df$MHR, k = ventana, fill = NA)
    # df$TOCO <- rollmean(df$TOCO, k = ventana, fill = NA)
    
      
    
    df <- df %>% pivot_longer(names_to = "Variable",
                              values_to = "Valor", cols = !x)
    
    df <- df %>% filter((Valor > 50 & Variable == "HR1") | 
                        (Valor > 50 & Variable == "MHR") |
                        (Variable == "TOCO"))
    
    ggplot(df, aes(x = x, y = Valor, color = Variable)) +
      geom_line() +
      labs(x = "Tiempo", y = "Valor")
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
  
  # Creamos el gráfico del fichero analógico
  output$ana <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero analógico")
    )
    
    datosAna <- datosAna()
    plot(tail(datosAna, 1500))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
