library(shiny)
library(caret)
library(randomForest)
library(pROC)

rm(list= ls())

lg_model <- readRDS("models/lg_model.rds")
roc_lg <- readRDS("models/roc_lg.rds")

lg_step_model <- readRDS("models/lg_step_model.rds")
roc_step_lg <- readRDS("models/roc_step_lg.rds")

lg_reduced_model <- readRDS("models/lg_reduced_model.rds")
roc_reduced_lg <- readRDS("models/roc_reduced_lg.rds")

rf_model <- readRDS("models/rf_model.rds")
roc_rf <- readRDS("models/roc_rf.rds")

rf_reduced_model <- readRDS("models/rf_reduced_model.rds")
roc_reduced_rf <- readRDS("models/roc_reduced_rf.rds")

svm_model <- readRDS("models/svm_model.rds")
roc_svm <- readRDS("models/roc_svm.rds")

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
      
      h3("ROC Curve"),
      plotOutput("roc_plot"),
      
      h3("Model note"),
      textOutput("model_note")
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
  
  selected_roc <- reactive({
    switch(
      input$model_choice,
      "Logistic Regression - Full" = roc_lg,
      "Logistic Regression - Stepwise" = roc_step_lg,
      "Logistic Regression - Reduced" = roc_reduced_lg,
      "Random Forest - Full" = roc_rf,
      "Random Forest - Reduced" = roc_reduced_rf,
      "Support Vector Machine" = roc_svm
    )
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

  
  output$roc_plot <- renderPlot({
    roc_obj <- selected_roc()
    
    plot(
      roc_obj,
      main = paste("ROC Curve -", input$model_choice)
    )
    
    legend(
      "bottomright",
      legend = paste("AUC =", round(auc(roc_obj), 4)),
      bty = "n"
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
        "Reduced Random Forest removes statistically not important predictors.",
      "Support Vector Machine" =
        "Support Vector Machine uses all selected Scenario 1 predictors."
    )
  })
  
}

shinyApp(ui = ui, server = server)

