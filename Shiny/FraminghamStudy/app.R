library(shiny)

# Define UI with custom CSS styles
ui <- fluidPage(
  tags$style(
    HTML("
      /* Custom CSS styles */
      body {
        background-color: #ffffff; /* Set background color to slate */
      }
    ")
  ),
  titlePanel("Heart Disease Prediction"),
  fluidRow(
    column(width = 3,
           numericInput("age", "Age:", value = 50, min = 20, max = 80)
    ),
    column(width = 3,
           numericInput("sex", "Sex (0 = female, 1 = male):", value = 1, min = 0, max = 1)
    ),
    column(width = 3,
           numericInput("cp", "Chest Pain Type (1-4):", value = 1, min = 1, max = 4)
    ),
    column(width = 3,
           numericInput("trestbps", "Resting Blood Pressure (mm Hg):", value = 120, min = 90, max = 200)
    ),
    column(width = 3,
           numericInput("chol", "Serum Cholesterol (mg/dl):", value = 200, min = 100, max = 400)
    ),
    column(width = 3,
           numericInput("fbs", "Fasting Blood Sugar (> 120 mg/dl):", value = 0, min = 0, max = 1)
    ),
    column(width = 3,
           numericInput("restecg", "Resting Electrocardiographic Results (0-2):", value = 0, min = 0, max = 2)
    ),
    column(width = 3,
           numericInput("thalach", "Maximum Heart Rate Achieved:", value = 150, min = 60, max = 220)
    ),
    column(width = 3,
           numericInput("exang", "Exercise Induced Angina (0 = no, 1 = yes):", value = 0, min = 0, max = 1)
    ),
    column(width = 12,
           actionButton("predict", "Predict")
    ),
    column(width = 12,
           textOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    # Combine inputs into a data frame
    new_data <- data.frame(
      age = input$age,
      sex = input$sex,
      cp = input$cp,
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = input$fbs,
      restecg = input$restecg,
      thalach = input$thalach,
      exang = input$exang
    )
    
    # Load the logistic regression model (this is a simple example; you'd use your own trained model)
    # For demonstration, I'm using a random model without actual training
    model <- glm(target ~ ., data = mtcars, family = binomial)
    
    # Make predictions
    predictions <- predict(model, newdata = new_data, type = "response")
    
    # Output prediction
    output$prediction <- renderText({
      if(predictions >= 0.5) {
        "Prediction: Heart Disease Likely"
      } else {
        "Prediction: No Heart Disease"
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
