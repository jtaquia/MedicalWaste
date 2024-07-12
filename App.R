
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(readxl)
library(googleway)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(geosphere)
library(tidyverse)
library(leaflet.extras)
#library(leaflet.extras2)
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
#require(geosphere)
require(measurements)
require(maps)
library(gmapsdistance)
library(mapsapi)
library(xlsx)
library(reticulate)

# UNIVERSIDAD_DE_LIMA/IDIC/

data <- read_csv("GeolocalizacionNuevo.csv")#,sheetName = "Hoja1" )
data2<-data
data2$rownumber = 1:dim(data2)[1]
data2$c = paste("Dirección", data2$rownumber, sep="-")
#data_valores<- as.list(setNames(data2$rownumber,data2$c))
data_valores<- as.list(data$Address)
#data_valores<- list("Dirección-1","Dirección-2","Dirección-3")
#print(typeof(data_valores))
# Define UI for application that draws a histogram
data3 <- read_excel("dataset5.xlsx")#,sheetName = "Hoja1" )
data4<-data3
data4$rownumber = 1:dim(data4)[1]
data4$c = paste("Dirección", data4$rownumber, sep="-")
#data_valores<- as.list(setNames(data2$rownumber,data2$c))
data_valores2<-
  as.list(data3$Address)


ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Medical Waste :: Capacitated Vehicle Routing",
                  #tabpanel1
                  tabPanel("CVRP - Non uniform demand",
                           # Application title
                           titlePanel("Data Input"),
                           
                           # Sidebar with a slider input for number of bins 
                           #sidebarLayout(
                           sidebarPanel(
                             
                             
                             fileInput("file1", "Choose CSV File",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             actionButton("submitbutton22", "Show locations on map", class = "btn btn-primary"),
                             # Input: Checkbox if file has header ----
                             checkboxInput("header", "Header", TRUE),
                             
                             # Input: Select separator ----
                             radioButtons("sep", "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             
                             # Input: Select quotes ----
                             radioButtons("quote", "Quote",
                                          choices = c(None = "",
                                                      "Double Quote" = '"',
                                                      "Single Quote" = "'"),
                                          selected = '"'),
                             
                             # Horizontal line ----
                             tags$hr(),
                             radioButtons("disp", "Display",
                                          choices = c(Head = "head",
                                                      All = "all"),
                                          selected = "head"),
                             
                             tags$hr(),
                             
                             
                             # Button
                             downloadButton("Locations Selected", "Download"),
                             
                             hr()
                             
                             #,actionButton("submitbutton", "Submit", class = "btn btn-primary")
                             
                             #,numericInput("obs2", "Observations:", 4, min = 1, max = 100)
                             
                             # Built with Shiny by RStudio
                             ,br(), br(),
                             
                             h5("Built with",
                                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                "by",
                                img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                                
                                img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px"))
                             
                             #  h5("Elaborado por Jose Antonio Taquia Gutierrez")
                             #   ,
                             
                             #  helpText(a("www.taquiagutierrez.com",target="_blank", href="https://www.taquiagutierrez.com")
                             
                           ),
                           
                           
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             leafletOutput("mymap1"),
                             tableOutput("contents"), textOutput("filas")
                             
                             #textInput("txt", "Enter the text to display below:"),
                            # textOutput("text")
                             
                             
                             
                           )
                  ),
                  #tabpanel   2
                  tabPanel("Routing locations to visit",headerPanel('Mapping Analytics'),
                           
                          
                           
                           #req(input$file1),
                           #data<-input$file1,
                           #data_valores2<- as.list(data$Address),
                           
                           # Input values
                           sidebarPanel( HTML("<h3>Input parameters</h3>"),
                                         tags$hr(), 
                                        actionButton("submitbutton77", "Update list", class = "btn btn-primary"),
                                        checkboxGroupInput("checkGroup", label = h3("Select four directions and click Submit button"), choices = data_valores2,selected = 1)                                    
                           ), 
                                                      # Horizontal line ----
                           
                           tags$hr(),
                           
                           
                           # Button
                           
                           #uiOutput("ui"),
                           mainPanel(fluidRow(column(4),tags$label(h3('Output'),column(4),actionButton("submitbutton105", "Submit", class = "btn btn-primary") 
                           )),
                           verbatimTextOutput("value"),
                           tableOutput("values2"),               
                           # Status/Output Text Box
                           #leafletOutput("newmap2"),
                           google_mapOutput(outputId = "lima3"),
                           
                           
                           
                           )
                           
                ),
                           
                  
                  
                  
                  #tabPabel   3
                  
                  tabPanel("Algorithm  column generation approach sequencing",
                           # Input values
                           sidebarPanel( HTML("<h3>Input parameters</h3>"),
                                         tags$hr(), 
                                         wellPanel(actionButton("submitbutton78", "Start optimal search", class = "btn btn-primary")),
                                         # Input: Select separator ----
                                         wellPanel(radioButtons("resultadosVrpy", "Presenta resultados",
                                                      choiceNames = c("Best value",
                                                                  "Location sequence obtained"), choiceValues = c(0,1),
                                                      selected = "Best value")),
                                         wellPanel(sliderInput("Capacidad",
                                                     "Medical waste vehicle-Capacity Limit:",
                                                     min = 400,  max = 1000,  value = 500, step=100)),
                                         hr()
                                         
                                         #,actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                         
                                         #,numericInput("obs2", "Observations:", 4, min = 1, max = 100)
                                         
                                         # Built with Shiny by RStudio
                                         ,br(), br(),
                                         
                                         h5("Built with",
                                            img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                            "by",
                                            img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                                            
                                            img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px"))
                           ),
                           
                           mainPanel(
                             fluidRow(tags$label(column(6,h3('Output'),offset = 5))),
                             fluidRow(column(6,img(src='Buho5.jpg', align = "right"), offset=2)),
                             fluidRow(),
                             fluidRow(verbatimTextOutput("txtout")),
                             fluidRow(tableOutput("values22"))
                             #tableOutput("table22"),
                             #dataTableOutput("table22")
                             #google_mapOutput(outputId = "lima1")
                             
                             
                           )
                           
                           
                           
                  ),# End tabPanel    3
                
                #Begin tabPanel 4
                tabPanel("Routing with Mixed Integer Programming Opt",headerPanel('Gurobi optimizer '),
                sidebarPanel( wellPanel(actionButton("submitbutton233", "Run MIP", class = "btn btn-primary")),
                             wellPanel( sliderInput("Capacidad_B",
                                          "Medical waste vehicle-Capacity Limit:",
                                          min = 400,  max = 1000,  value = 500, step=100)),
                             hr()
                             
                             #,actionButton("submitbutton", "Submit", class = "btn btn-primary")
                             
                             #,numericInput("obs2", "Observations:", 4, min = 1, max = 100)
                             
                             # Built with Shiny by RStudio
                             ,br(), br(),
                             
                             h5("Built with",
                                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                "by",
                                img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                                
                                img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px"))
                              ),        
                mainPanel(
                  fluidRow(tags$label(column(6,h3('Output'),offset = 5))),
                  fluidRow(column(6,img(src='Buho3.jpg', align = "right"), offset=2)),
                  fluidRow(),
                  fluidRow(tableOutput("values22B"))
                  #fluidRow(verbatimTextOutput("values22B"))
                  #tableOutput("table22"),
                  #dataTableOutput("table22")
                  #google_mapOutput(outputId = "lima1")
                )
                  
                )
                
                )
                )# navbarPage

# fluidpage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mymap1<-renderLeaflet({
    if (input$submitbutton22>0) {
     # output$contents <- renderTable({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data3<-read.csv(file$datapath, header = input$header)
    # })
      #data3 <- read_csv("archivoShinyRepartoPanaderia.csv")#,sheetName = "Hoja1" )
      #print(typeof(data3))
      #data2<-data3
      
      #data2$rownumber = 1:dim(data2)[1]
      #data2$c = paste("Dirección", data2$rownumber, sep="-")
      #data_valores<- as.list(setNames(data2$rownumber,data2$c))
      #data_valores<- as.list(data2$Address)
      #isolate(sliderValues2()) 
      m <- leaflet(data3) %>% setView(lng = -77.0688469 , lat = -12.0739147, zoom = 14)
      m %>% addTiles()%>% addMarkers(lng = ~lng, lat = ~lat,popup = ~htmlEscape(Address))
    } else {
      
      m <- leaflet(data) %>% setView(lng = -76.997664, lat = -12.1030277, zoom = 14)
      m %>% addTiles()%>% addMarkers(popup = ~htmlEscape(Address))
      #print(beneficio)
    }
  })
  
  #datosMostrar<- data.frame(input$file1)
  #leaflet(options = leafletOptions(minZoom = 0, maxZoom = 24))
  #output$mymap1<-renderLeaflet({
  #  m <- leaflet(data) %>% setView(lng = -76.997664, lat = -12.1030277, zoom = 14)
  #  m %>% addTiles()%>% addMarkers(popup = ~htmlEscape(Address))
  #})
  
  output$value <- renderPrint({ input$checkGroup })
  
  
  
  
  slidervalues<-reactive({
    
    valores<-as.list(input$checkGroup)
    
    
  })
  # Show the values in an HTML table ----
  #output$values <- renderTable({
  #  sliderValues()
  #})
  ####################################
  # tab 4
  ######################################
  output$mymap2<-renderLeaflet({
    if (input$submitbutton23>0) {
      
    } else {
      
  
    }
  })
  
  
  output$mymap3<-renderLeaflet({
    if (input$submitbutton23>0) {
      # output$contents <- renderTable({
      file <- input$file2
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      demandaLocales <-read.csv(file$datapath, header = input$header)
      # })
      #data3 <- read_csv("archivoShinyRepartoPanaderia.csv")#,sheetName = "Hoja1" )
      #print(typeof(data3))
      #data2<-data3
      
      #data2$rownumber = 1:dim(data2)[1]
      #data2$c = paste("Dirección", data2$rownumber, sep="-")
      #data_valores<- as.list(setNames(data2$rownumber,data2$c))
      #data_valores<- as.list(data2$Address)
      #isolate(sliderValues2()) 
      #m <- leaflet(data3) %>% setView(lng = -77.0688469 , lat = -12.0739147, zoom = 14)
      #m %>% addTiles()%>% addMarkers(lng = ~lng, lat = ~lat,popup = ~htmlEscape(Address))
      
      if (nrow(demandaLocales)>25) {
        valores_nuevos<-as.list(demandaLocales$Address)
        o <- paste(unlist(valores_nuevos[1]), collapse='')
        d <- paste(unlist(valores_nuevos[length(valores_nuevos)]), collapse='')
        demandaLocales<-demandaLocales[sample(nrow(demandaLocales),23),]
        valores_nuevos<-as.list(demandaLocales$Address)
        #print(length(valores_nuevos))
        
        #print(dim(data)[1])
        l<-c()
        i=1
        while(i<=length(valores_nuevos)) {
          
          b<-paste(unlist(valores_nuevos[i]), collapse='')
          l<-c(l,b)
          i=i+1
        }
        direcciones<-as.list(l)
        
      } else {
        valores_nuevos<-as.list(demandaLocales$Address)
        o <- paste(unlist(valores_nuevos[1]), collapse='')
        d <- paste(unlist(valores_nuevos[length(valores_nuevos)]), collapse='')
        
        #print(length(valores_nuevos))
        #print(dim(data)[1])
        l<-c()
        i=1
        while(i<=length(valores_nuevos)) {
          
          b<-paste(unlist(valores_nuevos[i]), collapse='')
          l<-c(l,b)
          i=i+1
        }
        direcciones<-as.list(l)
      }
      
     
      ############ gmapsdistance #######################
      ############ gmapsdistance #######################
     
      # key	is the user's Google Maps API key
      ##################################################
      ##################################################
      ##################################################
      #Recuperar la dirección con las coordenadas
      
      direcciones<-as.list(l)
      
      map_key <- "#############################"
      api_key <- "#############################"
      
      google_map(key = map_key,location = c(-12.114467, -76.9699337) ,
                 zoom = 16,
                 search_box = TRUE, 
                 scale_control = TRUE, 
                 height = 1000) #%>% add_traffic()
      
      res<-google_directions(key=api_key ,
                             origin = o,
                             destination =  d,
                             departure_time =  Sys.time() + (24 * 60 * 60),
                             waypoints = direcciones,
                             mode = "driving",
                             alternatives = FALSE,
                             avoid = c("TOLLS", "highways"),
                             units = "imperial",
                             simplify = TRUE)
      
      df_polyline <- decode_pl(res$routes$overview_polyline$points)
      df_way <- cbind(
        res$routes$legs[[1]]$end_location,
        data.frame(address = res$routes$legs[[1]]$end_address)
      )
      
      #google_distance(origins = o,
      #                destinations = d,
      #                key = api_key,
      #                simplify = FALSE)  ## use simplify = T to simplify to a data.frame
      
      #<-data.matrix(df_way)
      
      leaflet() %>% addTiles()%>%addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
      
      leaflet(data = demandaLocales) %>%
        addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(minZoom=5, maxZoom=16))%>%
        addMiniMap(position = "bottomright")%>%
        addMarkers(demandaLocales, lng = ~lng, lat = ~lat) %>%addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
      #%>%addPolygons(data = india)
      
      #df3 <- data.frame(matrix(unlist(direcciones), nrow=length(direcciones), byrow=TRUE))
      leaflet(data = df_way)%>%
        addTiles()%>%
       # addMiniMap(position = "bottomright")%>%
        addCircleMarkers(data=df_way, lng = ~lng, lat = ~lat,radius= 20,stroke=TRUE, color="#ff0000") %>%addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
      
    } else {
      
      file <- input$file2
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      data3<-read.csv(file$datapath, header = input$header)
      
      
      m <- leaflet(data3) %>% setView(lng = -76.997664, lat = -12.1030277, zoom = 14)
      m %>% addTiles()%>% addMarkers(popup = ~htmlEscape(Address))
      #print(beneficio)
    }
  })
  
  
  output$values2 <- renderTable({
    if (input$submitbutton105>0) { 
      #isolate(sliderValues2()) 
      valores<-as.list(input$checkGroup)
      df <- data.frame(
        Name = ("DireccionSeleccionada"),
        Value = as.character(input$checkGroup),
        stringsAsFactors = FALSE)
      #write.table(df,"jat777.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      #test <- read.csv(paste("jat777", ".csv", sep=""), header = TRUE)
      
      
    } else {
      
      valores<-input$checkGroup
      #print(valores)
      beneficio <- data.frame(valores)
      #print(beneficio)
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$direccionesSeleccionadas <- downloadHandler(
    filename = function() {
      paste(as.character(input$checkGroup), ".csv", sep = ",")
    },
    content = function(file) {
      write.csv(input$checkGroup, file, row.names = FALSE)
    }
  )
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        data<-df
        data$rownumber = 1:dim(data)[1]
        data$c = paste("Dirección", data$rownumber, sep="-")
        data_valores<- as.list(setNames(data$rownumber,data$c))
        #print(dim(data)[1])
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df) # cambiar por "nrow(df)" para que muestre la cantidad de filas del df
    }
    
    
  })
  
  output$contents2 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file2)
    #data<-input$file1
    #data_valores2<- as.list(data$Address)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file2$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        data<-df
        data$rownumber = 1:dim(data)[1]
        data$c = paste("Dirección", data$rownumber, sep="-")
        data_valores2<- as.list(setNames(data$rownumber,data$c))
        print(dim(data)[1])
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df) # cambiar por "nrow(df)" para que muestre la cantidad de filas del df
    }
    
    
  })
  
  output$ui <- renderUI(
    if (input$submitbutton77>0) {
      
      test_data <- reactive({
        req(input$file1)
        inFile <- as.character(input$file1)
        print(as.character(inFile$Address))
     })
        checkboxGroupInput('test', 'checkboxes',choices = unique(test_data()$Address))
      
      } else { 
        test_data <- reactive({
        req(input$file1)
        data3 <- read_csv("archivoShinyRepartoPanaderia.csv")
        inFile <- as.character(input$file1)
        print(as.character(data3$Address))
          #checkboxGroupInput('test', 'checkboxes',choices = unique(test_data()$Address)
        checkboxGroupInput('test', 'checkboxes',choices = as.character(data3$Address))
      })
      }
  )

  
  ##################################################
  ###########  tabPanel   3    #####################
  ##################################################
  
  output$txtout <- renderText({
  #output$table22 <- renderTable({  
  #output$table22 <- renderDataTable({  
    if (input$submitbutton78>0 && input$resultadosVrpy==1) {
      Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/python35_env/Scripts/python.exe")
      library(reticulate)
      Sys.getenv('RETICULATE_PYTHON')
      reticulate::virtualenv_create("python35_env", python = "python3")
      reticulate::virtualenv_install("python35_env", packages = c("networkx", "pandas", "numpy", "vrpy", "fsspec","openpyxl"))
      reticulate::use_virtualenv("python35_env", required = TRUE)
      #reticulate::source_python("~/vacunacioncvrptimewindow.py")
      #reticulate::py_run_file("D:~/vacunacioncvrptimewindow.py")
      reticulate::source_python("vacunacioncvrptimewindowFuncionC.py")
      run_model_value(input$Capacidad)
      #print(resultado[1])
    }
  })
  #objetivo<- run_model_vrp()
  
  output$values22 <- renderTable({
    if (input$submitbutton78>0 && input$resultadosVrpy==0) { 
      Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/python35_env/Scripts/python.exe")
      library(reticulate)
      Sys.getenv('RETICULATE_PYTHON')
      reticulate::virtualenv_create("python35_env", python = "python3")
      reticulate::virtualenv_install("python35_env", packages = c("networkx", "pandas", "numpy", "vrpy", "fsspec","openpyxl"))
      reticulate::use_virtualenv("python35_env", required = TRUE)
     
      reticulate::source_python("vacunacioncvrptimewindowFuncionB.py")
      run_model_vrp(input$Capacidad)#run_model_vrp es el nombre de la función en el script del source_python
      
    } 
  })
  
  
  
  ##################################################
  ###########  tabPanel   4    #####################
  ##################################################
  output$values22B<- renderTable({
  #output$values22B <- reactive({
    if (input$submitbutton233>0) { 
      Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/python35_env/Scripts/python.exe")
      library(reticulate)
      Sys.getenv('RETICULATE_PYTHON')
      reticulate::virtualenv_create("python35_env", python = "python3")
      reticulate::virtualenv_install("python35_env", packages = c("gmplot","gurobipy","pandas", "numpy", "openpyxl"))
      reticulate::use_virtualenv("python35_env", required = TRUE)
      #reticulate::source_python("~/vacunacioncvrptimewindow.py")
      #reticulate::py_run_file("~/vacunacioncvrptimewindow.py")
      reticulate::source_python("IDIC_21_03_2024_CapacidadDinamica.py")
      reticulate::py_run_file("~/IDIC_21_03_2024.py")
      #reticulate::source_python("~/vacunacioncvrptimewindowFuncionB.py")
      resolver_VRP(input$Capacidad_B)
    } else {
      
      print("Waiting to get Vehicle routing sequence")
      
    }
  })
  
 
  
 
  #####################################################
  ########                        #####################
  ########    tabPanel  2         #####################
  ########                        #####################
  #####################################################
  datasetInput <- reactive({# render newmap 
    print("lee datos1") 
  })
  
  
  
  
  # Ejecuta el datasetInput que genera el comportamiento
  observeEvent(input$submitbutton105
               ,{
                 #print("lee datos2") 
                 valores_nuevos<-as.list(input$checkGroup)
                 #print(length(valores_nuevos))
                 o <-  paste(unlist(valores_nuevos[1]), collapse='')
                 w <- paste(unlist(valores_nuevos[2]), collapse='')
                 q <- paste(unlist(valores_nuevos[3]), collapse='')
                 d <- paste(unlist(valores_nuevos[4]), collapse='')
                 
                 print(o)
                 print(w)
                 print(q)
                 print(d)
                 
                 output$lima3 <- renderGoogle_map({
                   print("ingreso al mapa")
                   
              output$lima3 <- renderGoogle_map({
                  if (input$submitbutton>0) { 
                    isolate(datasetInput()) 
                  } else {
                    google_map(key = map_key, 
                               search_box = TRUE, 
                               scale_control = TRUE, 
                               height = 1000) %>%
                      add_traffic()
                    
                  }
                })
                   api_key <- "~"
                   google_map(key = map_key, 
                              search_box = TRUE, 
                              scale_control = TRUE, 
                              height = 1000) %>%
                     add_traffic()
                 })
                 res <- google_directions(key = "~",
                                          origin = o,
                                          waypoints = list(stop = w,
                                                           stop = q),
                                          destination = d,
                                          optimise_waypoints = TRUE,
                                          mode = "driving")
                 
                 df_route <- data.frame(route = res$routes$overview_polyline$points)
                 
                 df_way <- cbind(
                   res$routes$legs[[1]]$end_location,
                   data.frame(address = res$routes$legs[[1]]$end_address)
                 )
                 
                 df_way$order <- as.character(1:nrow(df_way))
                 
                 google_map_update(map_id = "lima3") %>%
                   clear_traffic() %>%
                   clear_polylines() %>%
                   clear_markers() %>%
                   add_traffic() %>%
                   add_polylines(data = df_route,
                                 polyline = "route",
                                 stroke_colour = "#FF33D6",
                                 stroke_weight = 7,
                                 stroke_opacity = 0.7,
                                 info_window = "New route",
                                 load_interval = 100) %>%
                   add_markers(data = df_way,
                               info_window = "end_address",
                               label = "order")
               })
  
  output$lima3 <- renderGoogle_map({
    if (input$submitbutton105>0) { 
      isolate(datasetInput()) 
    } else {
      map_key <- "~"
      api_key <- "~"
      google_map(key = map_key, 
                 search_box = TRUE, 
                 scale_control = TRUE, 
                 height = 1000) %>%
        add_traffic()
      
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

