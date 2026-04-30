library(shiny)
library(caret)
library(randomForest)
library(pROC)
library(e1071)

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

train_sc1 <- readRDS("models/train_sc1.rds")
test_sc1 <-readRDS("models/test_sc1.rds")

train_svm <- readRDS("models/train_svm.rds")
test_svm <- readRDS("models/test_svm.rds")


scale_for_svm <- function(data, means, sds, features) {
  scaled <- data
  
  for (f in features) {
    scaled[[f]] <- (data[[f]] - means[[f]]) / sds[[f]]
  }
  
  scaled
}

ui <- fluidPage(
  
  titlePanel("Top 10 Lap Prediction App"),
  
  tabsetPanel(
    
    tabPanel(
      "Pretrained models",
      sidebarLayout(
        sidebarPanel(
          h4("Input lap conditions"),
          
          numericInput("SpeedI2", "SpeedI2", value = 180),
          numericInput("SpeedFL", "SpeedFL", value = 260),
          numericInput("SpeedI1", "SpeedI1", value = 190),
          numericInput("TyreLife", "Tyre Life", value = 30),
          
          selectInput("Stint", "Stint", choices = c("2", "3", "4")),
          selectInput("Compound", "Compound", choices = c("HARD", "MEDIUM", "SOFT")),
          
          selectInput(
            "model_choice",
            "Choose pretrained model",
            choices = c(
              "Logistic Regression - Full",
              "Logistic Regression - Stepwise",
              "Logistic Regression - Reduced",
              "Random Forest - Full",
              "Random Forest - Reduced",
              "Support Vector Machine"
            )
          ),
          
          sliderInput("threshold", "Classification threshold", 0, 1, 0.5, 0.01)
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
    ),
    
    tabPanel(
      "Custom model fitting",
      sidebarLayout(
        sidebarPanel(
          h4("Choose predictors"),
          
          checkboxGroupInput(
            "custom_predictors",
            "Predictors",
            choices = c("SpeedI2", "SpeedI1", "SpeedFL", "TyreLife", "Stint", "Compound"),
            selected = c("SpeedI2", "SpeedFL", "TyreLife", "Stint", "Compound")
          ),
          
          selectInput(
            "custom_model_type",
            "Model type",
            choices = c("Logistic Regression", "Random Forest", "SVM")
          ),
          
          conditionalPanel(
            condition = "input.custom_model_type == 'Random Forest'",
            numericInput("custom_ntree", "ntree", value = 500, min = 50, step = 50),
            numericInput("custom_mtry", "mtry", value = 2, min = 1, step = 1),
            numericInput("custom_nodesize", "nodesize", value = 3, min = 1, step = 1)
          ),
          
          conditionalPanel(
            condition = "input.custom_model_type == 'SVM'",
            numericInput("custom_cost", "cost", value = 9, min = 0.1, step = 0.1),
            numericInput("custom_gamma", "gamma", value = 0.955, min = 0.001, step = 0.001),
            selectInput("custom_kernel", "kernel", choices = c("radial", "linear", "polynomial", "sigmoid"))
          ),
          
          sliderInput("custom_threshold", "Classification threshold", 0, 1, 0.5, 0.01)
        ),
        
        mainPanel(
          h3("Custom Model Results"),
          verbatimTextOutput("custom_model_summary"),
          
          h3("Confusion Matrix"),
          verbatimTextOutput("custom_confusion"),
          
          h3("ROC Curve"),
          plotOutput("custom_roc_plot")
        )
      )
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
  
  custom_formula <- reactive({
    req(length(input$custom_predictors) > 0)
    
    as.formula(
      paste("top10 ~", paste(input$custom_predictors, collapse = " + "))
    )
  })
  
  custom_model <- reactive({
    req(length(input$custom_predictors) > 0)
    
    if (input$custom_model_type == "Logistic Regression") {
      
      glm(
        formula = custom_formula(),
        data = train_sc1,
        family = binomial
      )
    }
    else if (input$custom_model_type == "Random Forest") {
      
      randomForest(
        formula = custom_formula(),
        data = train_sc1,
        ntree = input$custom_ntree,
        mtry = input$custom_mtry,
        nodesize = input$custom_nodesize
      )
      
    } else {
      svm(
        formula = custom_formula(),
        data = train_svm,
        kernel = input$custom_kernel,
        cost = input$custom_cost,
        gamma = input$custom_gamma,
        probability = TRUE
      )
    }
  })
  
  custom_test_data <- reactive({
    if(input$custom_model_type != "SVM"){
      data <- test_sc1
    } else{
      data <- test_svm
    }
    
    data
  })
  

  custom_probs <- reactive({
    model <- custom_model()
    data <- custom_test_data()
    
    
    if (input$custom_model_type == "Logistic Regression") {
      predict(model, newdata = data, type = "response")
      
    }  else if (input$custom_model_type == "Random Forest") {
      predict(model, newdata = data, type = "prob")[, "TRUE"]
    } else {
      pred <- predict(model, newdata = data, probability = TRUE)
      attr(pred, "probabilities")[, "TRUE"]
    }
  })
  
  custom_preds <- reactive({
    if(input$custom_model_type != "SVM") {
      factor(
        ifelse(custom_probs() >= input$custom_threshold, "TRUE", "FALSE"),
        levels = levels(test_sc1$top10)
      )
    } else {
      factor(
        ifelse(custom_probs() >= input$custom_threshold, "TRUE", "FALSE"),
        levels = levels(test_svm$top10)
      )
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
  
  output$custom_model_summary <- renderPrint({
    cat("Model type:", input$custom_model_type, "\n")
    cat("Selected predictors:", paste(input$custom_predictors, collapse = ", "), "\n\n")
    
    if (input$custom_model_type == "Random Forest") {
      cat("ntree:", input$custom_ntree, "\n")
      cat("mtry:", input$custom_mtry, "\n")
      cat("nodesize:", input$custom_nodesize, "\n")
    } else {
      cat("kernel:", input$custom_kernel, "\n")
      cat("cost:", input$custom_cost, "\n")
      cat("gamma:", input$custom_gamma, "\n")
    }
  })
  
  output$custom_roc_plot <- renderPlot({
    if(input$custom_model_type != "SVM") {
      roc_obj <- roc(
        response = test_sc1$top10,
        predictor = custom_probs(),
        levels = c("FALSE", "TRUE")
      )
    } else {
      roc_obj <- roc(
        response = test_svm$top10,
        predictor = custom_probs(),
        levels = c("FALSE", "TRUE")
      )
    }
    
    plot(
      roc_obj,
      main = paste("ROC Curve - Custom", input$custom_model_type)
    )
    
    legend(
      "bottomright",
      legend = paste("AUC =", round(auc(roc_obj), 4)),
      bty = "n"
    )
  })
  
  output$custom_confusion <- renderPrint({
    req(custom_preds())
    
    if(input$custom_model_type != "SVM") {
      confusionMatrix(
        custom_preds(),
        test_sc1$top10,
        positive = "TRUE"
      )
    } else {
      confusionMatrix(
        custom_preds(),
        test_svm$top10,
        positive = "TRUE"
      )
    }
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

