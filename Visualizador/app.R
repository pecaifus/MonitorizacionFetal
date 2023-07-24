library(shiny)
library(ggplot2)
library(tidyverse)
library(shinyFiles)
library(zoo)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("darkly"),    
    titlePanel("Visualizador de monitorización fetal"),

    sidebarLayout(
        sidebarPanel(
            shinyDirButton('folder', 'Select a folder',
                           'Please select a folder', FALSE)
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("HR1, MHR y TOCO",
                     h1("Gráfico sobre las frecuencias cardiacas
                        actividad uterina"),
                     h2("Frecuencia cardicaca fetal y materna"),
                     plotOutput("HR"),
                     h2("Actividad uterina"),
                     plotOutput("TOCO")),
            tabPanel("SPO2",
                     h1("Gráfico sobre el oxígeno en sangre"),
                     plotOutput("SPO2")),
            tabPanel("Presion",
                     h1("Gráfico sobre las diferentes presiones"),
                     plotOutput("PRESION")),
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
  
  # Creamos el gráfico de HR1 y MHR
  output$HR <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(HR1, HR2, MHR)
    
    # Revisar si está bien hecho el cambio a tiempo
    seg <- nrow(df) / 4000
    df$x <- seq(0, seg, length.out = nrow(df))
    
    df <- df %>% pivot_longer(names_to = "Variable",
                              values_to = "Valor", cols = !x)
    
    # Revisar este filtrado de datos
    # df <- df %>% filter((Valor > 50 & Variable == "HR1") |
    #                     (Valor > 50 & Variable == "MHR"))

    ventana <- 20
    df <- df %>%
          group_by(Variable) %>%
          summarise(Suavizado = rollmean(Valor, k = ventana, fill = NA),
                    Tiempo = x)
    ggplot(df, aes(x = Tiempo, y = Suavizado, color = Variable)) +
      geom_line() +
      labs(x = "Tiempo (s)", y = "Valor")
  })

  # Creamos el gráfico de TOCO
  output$TOCO <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    TC <- as.data.frame(datosDig())
    TC <- TC %>% dplyr::select(TOCO)
    seg <- nrow(TC) / 4000 # Revisar si está bien hecho el cambio a tiempo
    TC$x <- seq(0, seg, length.out = nrow(TC))
    
    ventana <- 20
    TC <- TC %>% 
      mutate(TOCO = rollmean(TOCO, k = ventana, fill = NA))
    
    ggplot(TC, aes(x = x, y = TOCO)) + 
      geom_line(color = "green") +
      labs(x = "Tiempo (s)", y = "Valor (mmHg)")
  })
  
  # Creamos el gráfico del SPO2
  output$SPO2 <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(SPO2)
    
    seg <- nrow(df) / 1000
    df$x <- seq(0, seg, length.out = nrow(df))
    
    df <- df %>% filter(SPO2 > 0)
    
    
    ggplot(df, aes(x = x, y = SPO2)) +
      geom_line(color = "blue") + labs(x = "Tiempo (s)", y = "Porcentaje %")
  })
  
  # Creamos el gráfico de las presiones
  output$PRESION <- renderPlot({
    validate(
      need(input$folder, message = "Esperando fichero digital")
    )
    
    df <- as.data.frame(datosDig())
    df <- df %>% dplyr::select(Pmedia, Pdiastolica, Psistolica)
    
    seg <- nrow(df) / 1000
    df$x <- seq(0, seg, length.out = nrow(df))
    
    df_l <- df %>% 
      pivot_longer(names_to = "Variable", values_to = "Valor", cols = !x)
    
    ggplot(data = df_l ,aes(x = x, y = Valor, color = Variable)) +
      geom_line() +
      labs(x = "Tiempo (s)", y = "Valor mmHg")
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
    plot(tail(datosAna, 500))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
