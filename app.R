
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)

# User Interface                   
ui <- fluidPage(theme = shinytheme("flatly"),
        navbarPage("myBMI",
            tabPanel("Calculator",
             # Input values
            sidebarPanel(
                textInput("name", "Enter Name"),
                textInput("year", "Enter Year"),
                h3("Input Parameters"),
                sliderInput("height", 
                    label = "Height", 
                    value = 175, 
                    min = 40, 
                    max = 250),
                
                sliderInput("weight", 
                    label = "Weight", 
                    value = 70, 
                    min = 20, 
                    max = 100),
                                          
                actionButton("submitbutton", 
                    "Submit", 
                    class = "btn btn-primary")
                            ),
                
             mainPanel(
                    h3("Program Status: "), # Status/Output Text Box
                          verbatimTextOutput('contents'),
                          h4("The BMI is: "), 
                          tableOutput('tabledata'))), # Results table

            tabPanel("Data", DT::dataTableOutput("data")),
            
            tabPanel("Visualization",
              mainPanel(
                h4("Patient's Overall Status per Year"),
                                    
                h5("2020"),
                   plotOutput("pie2020"),
                h5("2021"),
                  plotOutput("pie2021"),
                h5("2022"),
                  plotOutput("pie2022"))),
            
            tabPanel("About", 
                titlePanel("About"), 
                div(includeMarkdown("about.md"), 
                align="justify"))))
                           

# Server                        

server <- function(input, output, session) {
      
      # Input Data
      datasetInput <- reactive({  
        
        bmi <- input$weight/( (input$height/100) * (input$height/100) )
        bmi <- data.frame(bmi)
        names(bmi) <- "BMI"
        print(bmi)
        
      })
    
      # Status/Output Text Box
      output$contents <- renderPrint({
        if (input$submitbutton>0) { 
          isolate("Calculation complete.") 
        } else {
          return("Please input patient's height & weight")
        }
      })
      
      # Prediction results table
      output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
          isolate(datasetInput()) 
        } 
      })
      
      # Data Table
      bb_data <- read.csv("BMI_data.csv", stringsAsFactors = FALSE )
      bb_data <- data.frame(bb_data)
      output$data <-DT::renderDataTable(datatable(
        bb_data[,c(-1,-13,-14,-15,-18:-35)],filter = 'top',
        colnames = c("Row", "Name", "Height", "Weight", "BMI", "Status", "Record Year")
      ))
      
      # Data Visualization
      
      # Year 2020
      output$pie2020 <- renderPlot({
        
        under2020 <- nrow(filter(bb_data, bb_data$Status == "Underweight", bb_data$Year == "2020"))
        normal2020 <- nrow(filter(bb_data, bb_data$Status == "Normal", bb_data$Year == "2020"))
        overweight2020 <- nrow(filter(bb_data, bb_data$Status == "Overweight", bb_data$Year == "2020"))
        obese2020 <- nrow(filter(bb_data, bb_data$Status == "Obese", bb_data$Year == "2020"))
        
        countStatus20 <- c(under2020, normal2020, overweight2020, obese2020)
        status <- c("Underweight", "Normal", "Overweight", "Obese")
        
        df2020 <- data.frame(countStatus20, status)
        
        ggplot(df2020, aes(x = "", y = countStatus20, fill = status)) +
          geom_col(color = "black") +
          geom_text(aes(label = countStatus20),
                    position = position_stack(vjust = 0.5)) +
          coord_polar(theta = "y") +
          scale_fill_brewer()
         })
      
      # Year 2021
      output$pie2021 <- renderPlot({
        
        under2021 <- nrow(filter(bb_data, bb_data$Status == "Underweight", bb_data$Year == "2021"))
        normal2021 <- nrow(filter(bb_data, bb_data$Status == "Normal", bb_data$Year == "2021"))
        overweight2021 <- nrow(filter(bb_data, bb_data$Status == "Overweight", bb_data$Year == "2021"))
        obese2021 <- nrow(filter(bb_data, bb_data$Status == "Obese", bb_data$Year == "2021"))
        
        countStatus21 <- c(under2021, normal2021, overweight2021, obese2021)
        status <- c("Underweight", "Normal", "Overweight", "Obese")
        
        df2021 <- data.frame(countStatus21, status)
        
        ggplot(df2021, aes(x = "", y = countStatus21, fill = status)) +
          geom_col(color = "black") +
          geom_text(aes(label = countStatus21),
                    position = position_stack(vjust = 0.5)) +
          coord_polar(theta = "y") +
          scale_fill_brewer()
      })
      
      # Year2022
      output$pie2022 <- renderPlot({
        
        under2022 <- nrow(filter(bb_data, bb_data$Status == "Underweight", bb_data$Year == "2022"))
        normal2022 <- nrow(filter(bb_data, bb_data$Status == "Normal", bb_data$Year == "2022"))
        overweight2022 <- nrow(filter(bb_data, bb_data$Status == "Overweight", bb_data$Year == "2022"))
        obese2022 <- nrow(filter(bb_data, bb_data$Status == "Obese", bb_data$Year == "2022"))
        
        countStatus22 <- c(under2022, normal2022, overweight2022, obese2022)
        status <- c("Underweight", "Normal", "Overweight", "Obese")
        
        df2022 <- data.frame(countStatus22, status)
        
        ggplot(df2022, aes(x = "", y = countStatus22, fill = status)) +
          geom_col(color = "black") +
          geom_text(aes(label = countStatus22),
                    position = position_stack(vjust = 0.5)) +
          coord_polar(theta = "y") +
          scale_fill_brewer()
      })
      
    }
    
    # Create Shiny App                 
    shinyApp(ui = ui, server = server)
