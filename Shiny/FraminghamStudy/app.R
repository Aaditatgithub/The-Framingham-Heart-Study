#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(xgboost)
library(caret)

# Define UI
ui <- fluidPage(
  titlePanel("Coronary Heart Disease Prediction"),
  theme = shinythemes::shinytheme("slate"),  # Apply Slate theme
  sidebarLayout(
    sidebarPanel(
      width = 12,  # Set width to occupy entire viewport
      tags$h4("Input Parameters", style = "color: #FFF;"),
      # Input fields
      fluidRow(
        column(4, 
               sliderInput("age", "Age", value = 60, min = 1, max = 90, step = 1, width = "100%"),
               tags$small("Enter age in years")),
        column(4, 
               sliderInput("creatinine", "Creatinine", value = 80, min = 10, max = 1050, step = 0.1, width = "100%"),
               tags$small("Enter creatinine level in mg/dL")),
        column(4, 
               sliderInput("glycohemoglobin", "Glycohemoglobin", value = 5.6, min = 3, max = 20, step = 0.1, width = "100%"),
               tags$small("Enter glycohemoglobin level in %"))
      ),
      fluidRow(
        column(4, 
               sliderInput("cholesterol", "Cholesterol", value = 5, min = 0, max = 16, step = 0.01, width = "100%"),
               tags$small("Enter cholesterol level in mg/dL")),
        column(4, 
               sliderInput("platelet_count", "Platelet Count", value = 250, min = 0, max = 1000, step = 2, width = "100%"),
               tags$small("Enter platelet count in 10^3/μL")),
        column(4, 
               sliderInput("lymphocyte", "Lymphocyte", value = 50, min = 2, max = 100, step = 0.1, width = "100%"),
               tags$small("Enter lymphocyte count in %"))
      ),
      fluidRow(
        column(4, 
               sliderInput("systolic", "Systolic", value = 120, min = 0, max = 300, step = 1, width = "100%"),
               tags$small("Enter systolic blood pressure in mmHg")),
        column(4, 
               sliderInput("uric_acid", "Uric Acid", value = 340, min = 70, max = 1050, step = 0.1, width = "100%"),
               tags$small("Enter uric acid level in mg/dL")),
        column(4, 
               sliderInput("red_cell_distribution_width", "Red Cell Distribution Width", value = 12, min = 9, max = 33, step = 1, width = "100%"),
               tags$small("Enter red cell distribution width in %"))
      ),
      fluidRow(
        column(4, 
               sliderInput("monocyte", "Monocyte", value = 5, min = 0, max = 70, step = 1, width = "100%"),
               tags$small("Enter monocyte count in %")),
        column(2,
               br(),
               actionButton("predictBtn", "Predict", style = "background-color: #007bff; color: #fff; border: none;")
              ),
        column(6.5,
               br(),
               verbatimTextOutput("predictionText")
               ),
             ),
      hr(),
      
    ),
    mainPanel(
      # Placeholder for future enhancements
    )
  )
)



# Define server logic
server <- function(input, output) {
  # Load XGBoost model
  # Load the XGBoost model
  loaded_xgb_model <- readRDS("C:/Users/Aadit/Downloads/xgb_model.rds")
  
  
  # Perform prediction
  observeEvent(input$predictBtn, {
    # Prepare input data for prediction
    input_data <- data.frame(
      Age = input$age,
      Creatinine = input$creatinine,
      Glycohemoglobin = input$glycohemoglobin,
      Cholesterol = input$cholesterol,
      Platelet.count = input$platelet_count,
      Lymphocyte = input$lymphocyte,
      Systolic = input$systolic,
      Uric.Acid = input$uric_acid,
      Red.Cell.Distribution.Width = input$red_cell_distribution_width,
      Monocyte = input$monocyte
    )
    
    # Perform prediction using the loaded XGBoost model
    prediction <- predict(loaded_xgb_model, newdata = input_data)
    
    # Display prediction result
    output$predictionText <- renderPrint({
      paste("Predicted Result:", ifelse(prediction == 1, "Heart Disease", "No Heart Disease"))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
