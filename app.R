library(shiny)
library(caret)
library(randomForest)
library(pROC)

rm(list= ls())

lg_model <- readRDS("models/lg_model.rds")
lg_step_model <- readRDS("models/lg_step_model.rds")
lg_reduced_model <- readRDS("models/lg_reduced_model.rds")
rf_model <- readRDS("models/rf_model.rds")
rf_reduced_model <- readRDS("models/rf_reduced_model.rds")

svm_model <- readRDS("models/svm_model.rds")
train_means <- readRDS("models/train_means.rds")
train_sds <- readRDS("models/train_sds.rds")

train_means
num_features <- c("SpeedI2", "SpeedI1", "SpeedFL", "TyreLife")


scale_for_svm <- function(data, means, sds, features) {
  scaled <- data
  
  for (f in features) {
    scaled[[f]] <- (data[[f]] - means[[f]]) / sds[[f]]
  }
  
  scaled
}



ui <- fluidPage(
  
  titlePanel("Top 10 Lap Prediction App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Input lap conditions"),
      
      numericInput("SpeedI2", "SpeedI2", value = 250),
      numericInput("SpeedFL", "SpeedFL", value = 260),
      numericInput("SpeedI1", "SpeedI1", value = 240),
      numericInput("TyreLife", "Tyre Life", value = 10),
      
      selectInput(
        "Stint",
        "Stint",
        choices = c("2", "3", "4")
      ),
      
      selectInput(
        "Compound",
        "Compound",
        choices = c("HARD", "MEDIUM", "SOFT")
      ),
      
      selectInput(
        "model_choice",
        "Choose model",
        choices = c(
          "Logistic Regression - Full",
          "Logistic Regression - Stepwise",
          "Logistic Regression - Reduced",
          "Random Forest - Full",
          "Random Forest - Reduced",
          "Support Vector Machine"
        )
      ),
      
      sliderInput(
        "threshold",
        "Classification threshold",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.01
      )
    ),
    
    mainPanel(
      h3("Prediction"),
      verbatimTextOutput("prediction_output"),
      
      h3("Probability of Top 10"),
      plotOutput("prob_plot"),
      
      h3("Model note"),
      textOutput("model_note"),
      
      verbatimTextOutput("debug_svm")
    )
  )
)

server <- function(input, output) {
  
  new_data <- reactive({
    data.frame(
      SpeedI2 = input$SpeedI2,
      TyreLife = input$TyreLife,
      Stint = factor(input$Stint, levels = c("2", "3", "4")),
      SpeedI1 = input$SpeedI1,
      SpeedFL = input$SpeedFL,
      Compound = factor(input$Compound, levels = c("HARD", "MEDIUM", "SOFT"))
    )
  })
  
  new_data_svm <- reactive({
    scale_for_svm(
      data = new_data(),
      means = train_means,
      sds = train_sds,
      features = num_features
    )
  })
  
  selected_model <- reactive({
    switch(
      input$model_choice,
      "Logistic Regression - Full" = lg_model,
      "Logistic Regression - Stepwise" = lg_step_model,
      "Logistic Regression - Reduced" = lg_reduced_model,
      "Random Forest - Full" = rf_model,
      "Random Forest - Reduced" = rf_reduced_model,
      "Support Vector Machine" = svm_model
    )
  })
  
  pred_prob <- reactive({
    model <- selected_model()
    data <- new_data()
    
    if (grepl("Logistic", input$model_choice)) {
      predict(model, newdata = data, type = "response")
    } else if (!grepl("Support Vector Machine", input$model_choice)) {
      predict(model, newdata = data, type = "prob")[, "TRUE"]
    } else{
      probs <- predict(model, newdata = new_data_svm(), probability = TRUE)
      probs <- attr(probs, "probabilities")[, "TRUE"]
      
      probs
    }
  })
  
  output$prediction_output <- renderPrint({
    prob <- pred_prob()
    pred_class <- ifelse(prob >= input$threshold, "TRUE", "FALSE")
    
    cat("Predicted class:", pred_class, "\n")
    cat("Probability of Top 10:", round(prob, 4), "\n")
    cat("Threshold:", input$threshold, "\n")
  })
  
  output$prob_plot <- renderPlot({
    prob <- pred_prob()
    
    barplot(
      height = c(prob, 1 - prob),
      names.arg = c("Top 10 TRUE", "Top 10 FALSE"),
      ylim = c(0, 1),
      main = "Predicted Probability",
      ylab = "Probability"
    )
  })
  
  output$model_note <- renderText({
    switch(
      input$model_choice,
      "Logistic Regression - Full" =
        "Full logistic regression uses all selected Scenario 1 predictors.",
      "Logistic Regression - Stepwise" =
        "Stepwise model uses AIC-based algorithmic feature selection.",
      "Logistic Regression - Reduced" =
        "Reduced logistic model removes statistically insignificant predictors.",
      "Random Forest - Full" =
        "Full Random Forest uses all predictors and captures nonlinear effects.",
      "Random Forest - Reduced" =
        "RFE Random Forest uses recursive feature elimination to evaluate feature subsets."
    )
  })
  
  output$debug_svm <- renderPrint({
    new_data_svm()
  })
}

shinyApp(ui = ui, server = server)

