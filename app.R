library(rsconnect)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(reactable)
library(tidyr)
library(dplyr)
library(htmltools)
library(shinyBS)
library(mgcv)

# Define the UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Linear and Non-Linear Models Diagnostics"),
  dashboardSidebar(
    width = 300,
    box(
      title = "Sample Specifics", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
      sliderInput("sample_size",
                  tags$span("Select Sample Size:", style = "color: gray;"),
                  min = 10, max = 600,
                  value = 100, step = 10),
      sliderInput("num_samples", tags$span("Number of Samples:", style = "color: gray;"), min = 1, max = 100, value = 10, step = 1),
      numericInput("independent_value_mean", tags$span("Independent Value Mean:", style = "color: gray;"), value = 4.5, step = 0.1),
      numericInput("independent_value_sd", tags$span("Independent Value SD:", style = "color: gray;"), value = 1, step = 0.1),
      numericInput("error_mean", tags$span("Error Mean:", style = "color: gray;"), value = 0, step = 0.1),
      numericInput("error_sd", tags$span("Error SD:", style = "color: gray;"), value = 10, step = 0.1)
    ),
    box(
      title = "Model Parameters", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
      textInput("intercept_input", tags$span("True Model Intercept:", style = "color: gray;"), value = "58"),
      textInput("slope_input", tags$span("True Model Slope (Linear Term):", style = "color: gray;"), value = "-10"),
      textInput("quadratic_input", tags$span("True Model Quadratic Term:", style = "color: gray;"), value = "0.869"),
      textInput("cubic_input", tags$span("True Model Cubic Term:", style = "color: gray;"), value = "0.1")
    ),
    box(
      title = "What happens if you violate the assumptions?", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
      checkboxInput("violate_homoscedasticity", tags$span("Violate Homoscedasticity", style = "color: gray;"), value = FALSE),
      bsTooltip("violate_homoscedasticity", 
                "This assumption means that the variance of the residuals is constant across all levels of the independent variable. The variance is modeled as: σ² = (1 + k * X). When k = 0, homoscedasticity is not violated. If k > 0, variance increases with the predictor. If k < 0, variance decreases with the predictor.", 
                "right"),
      conditionalPanel(
        condition = "input.violate_homoscedasticity == true",
        numericInput("variance_formula", tags$span("Variance Formula Multiplier:", style = "color: gray;"), value = 0.5, step = 0.1)
      ),
    ),
    box(
      title = "Graph Details", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
      textInput("x_axis_label", tags$span("X-Axis Label:", style = "color: gray;"), value = "Stability"),
      textInput("y_axis_label", tags$span("Y-Axis Label:", style = "color: gray;"), value = "UCLA Loneliness Score"),
      numericInput("x_axis_min", tags$span("X-Axis Min:", style = "color: gray;"), value = 0),
      numericInput("x_axis_max", tags$span("X-Axis Max:", style = "color: gray;"), value = 10),
      numericInput("y_axis_min", tags$span("Y-Axis Min:", style = "color: gray;"), value = 0),
      numericInput("y_axis_max", tags$span("Y-Axis Max:", style = "color: gray;"), value = 80),
      checkboxInput("show_CI", tags$span("Show Confidence Intervals", style = "color: gray;"), value = TRUE)
    ),
    box(
      title = "Prediction Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
      numericInput("x_min", tags$span("X Min:", style = "color: gray;"), value = 0),
      numericInput("x_max", tags$span("X Max:", style = "color: gray;"), value = 10),
      numericInput("x_interval", tags$span("X Interval:", style = "color: gray;"), value = 1),
      sliderInput("alpha_level",
                  tags$span("Select Confidence Interval Alpha Level:", style = "color: gray;"),
                  min = 0.01, max = 0.99,
                  value = 0.05, step = 0.01),
      selectInput("interval_type", tags$span("Interval Type:", style = "color: gray;"),
                  choices = c("Confidence Interval" = "confidence", "Prediction Interval" = "prediction"),
                  selected = "confidence")
    )
  ),
  dashboardBody(
    tabBox(
      id = "tabs", width = 12,
      tabPanel(
        title = tagList(icon("chart-line"), "Model Analysis"),
        fluidRow(
          column(4, plotOutput("cubicPlot", width = "100%", height = "400px")),
          column(4, plotOutput("quadraticPlot", width = "100%", height = "400px")),
          column(4, plotOutput("linearPlot", width = "100%", height = "400px"))
        ),
        fluidRow(
          box(
            title = "Diagnostics for One Sample", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            reactableOutput("diagnosticsTable")
          )
        ),
        fluidRow(
          box(
            title = "Model Comparison Across Samples", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            reactableOutput("comparisonTable")
          )
        )
      ),
      tabPanel(
        title = tagList(icon("clipboard-check"), "Assumption Checks"),
        box(
          title = "Linear Model Assumption Checks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, collapsed = TRUE,
          fluidRow(
            column(4, plotOutput("histogram_residuals_linear", width = "100%", height = "300px")),
            column(4, plotOutput("residual_dependence_plot_linear", width = "100%", height = "300px")),
            column(4, plotOutput("scale_location_plot_linear", width = "100%", height = "300px"))
          )
        ),
        box(
          title = "Quadratic Model Assumption Checks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, collapsed = TRUE,
          fluidRow(
            column(4, plotOutput("histogram_residuals_quadratic", width = "100%", height = "300px")),
            column(4, plotOutput("residual_dependence_plot_quadratic", width = "100%", height = "300px")),
            column(4, plotOutput("scale_location_plot_quadratic", width = "100%", height = "300px"))
          )
        ),
        box(
          title = "Cubic Model Assumption Checks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, collapsed = TRUE,
          fluidRow(
            column(4, plotOutput("histogram_residuals_cubic", width = "100%", height = "300px")),
            column(4, plotOutput("residual_dependence_plot_cubic", width = "100%", height = "300px")),
            column(4, plotOutput("scale_location_plot_cubic", width = "100%", height = "300px"))
          )
        )
      ),
      tabPanel(
        title = tagList(icon("table"), "Predictions"),
        fluidRow(
          box(
            title = "Linear Model Predictions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            reactableOutput("predictions_linear_table")
          )
        ),
        fluidRow(
          box(
            title = "Quadratic Model Predictions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            reactableOutput("predictions_quadratic_table")
          )
        ),
        fluidRow(
          box(
            title = "Cubic Model Predictions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            reactableOutput("predictions_cubic_table")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Generate all samples at the beginning
  all_samples <- reactive({
    samples <- list()
    for (i in 1:input$num_samples) {
      set.seed(NULL)  # Ensures different random seed for each iteration
      stability_sample <- rnorm(input$sample_size, mean = input$independent_value_mean, sd = input$independent_value_sd)
      
      errors <- rnorm(input$sample_size, mean = input$error_mean, sd = input$error_sd)
      
      
      if (input$violate_homoscedasticity) {
        errors <- errors * (1 + input$variance_formula * stability_sample)
      }
      
      ucla_sample <- as.numeric(input$intercept_input) +
        as.numeric(input$slope_input) * stability_sample +
        as.numeric(input$quadratic_input) * stability_sample^2 +
        as.numeric(input$cubic_input) * stability_sample^3 +
        errors
      
      samples[[i]] <- data.frame(Stability = stability_sample, UCLA = ucla_sample)
    }
    samples
  })
  
  # Use the first sample for individual plots and diagnostics
  first_sample <- reactive({
    all_samples()[[1]]
  })
  
  true_model <- reactive({
    intercept <- as.numeric(input$intercept_input)
    slope <- as.numeric(input$slope_input)
    quadratic_term <- as.numeric(input$quadratic_input)
    cubic_term <- as.numeric(input$cubic_input)
    function(x) intercept + slope * x + quadratic_term * x^2 + cubic_term * x^3
  })
  
  linear_model <- reactive({
    lm(UCLA ~ Stability, data = first_sample())
  })
  
  quadratic_model <- reactive({
    lm(UCLA ~ Stability + I(Stability^2), data = first_sample())
  })
  
  cubic_model <- reactive({
    lm(UCLA ~ Stability + I(Stability^2) + I(Stability^3), data = first_sample())
  })
  
  output$linearPlot <- renderPlot({
    data <- first_sample()
    model <- linear_model()
    conf_level <- 1 - input$alpha_level
    stability_range <- seq(input$x_axis_min, input$x_axis_max, length.out = 100)
    predicted_values <- predict(model, newdata = data.frame(Stability = stability_range), interval = "confidence", level = conf_level)
    
    p <- ggplot() +
      geom_line(aes(x = stability_range, y = true_model()(stability_range)), color = "#1f78b4", size = 1.5, linetype = "dashed") +
      geom_line(aes(x = stability_range, y = predicted_values[, "fit"]), color = "#e31a1c", size = 1.5, linetype = "dotdash") +
      geom_point(aes(x = data$Stability, y = data$UCLA), color = "#fb9a99", alpha = 0.3) +
      xlim(input$x_axis_min, input$x_axis_max) +
      ylim(input$y_axis_min, input$y_axis_max) +
      labs(title = "Linear Model vs. True Model",
           x = input$x_axis_label,
           y = input$y_axis_label) +
      theme_minimal() +
      theme(legend.position = "top")
    
    if (input$show_CI) {
      p <- p +
        geom_ribbon(aes(x = stability_range, ymin = predicted_values[, "lwr"], ymax = predicted_values[, "upr"]),
                    fill = "#fadaac", alpha = 0.2)
    }
    
    print(p)
  }, width = 400, height = 400)
  
  output$quadraticPlot <- renderPlot({
    data <- first_sample()
    model <- quadratic_model()
    conf_level <- 1 - input$alpha_level
    stability_range <- seq(input$x_axis_min, input$x_axis_max, length.out = 100)
    predicted_values <- predict(model, newdata = data.frame(Stability = stability_range), interval = "confidence", level = conf_level)
    
    p <- ggplot() +
      geom_line(aes(x = stability_range, y = true_model()(stability_range)), color = "#1f78b4", size = 1.5, linetype = "dashed") +
      geom_line(aes(x = stability_range, y = predicted_values[, "fit"]), color = "#33a02c", size = 1.5) +
      geom_point(aes(x = data$Stability, y = data$UCLA), color = "#a6cee3", alpha = 0.3) +
      xlim(input$x_axis_min, input$x_axis_max) +
      ylim(input$y_axis_min, input$y_axis_max) +
      labs(title = "Quadratic Model vs. True Model",
           x = input$x_axis_label,
           y = input$y_axis_label) +
      theme_minimal() +
      theme(legend.position = "top")
    
    if (input$show_CI) {
      p <- p +
        geom_ribbon(aes(x = stability_range, ymin = predicted_values[, "lwr"], ymax = predicted_values[, "upr"]),
                    fill = "#c6f0bb", alpha = 0.2)
    }
    
    print(p)
  }, width = 400, height = 400)
  
  output$cubicPlot <- renderPlot({
    data <- first_sample()
    model <- cubic_model()
    conf_level <- 1 - input$alpha_level
    stability_range <- seq(input$x_axis_min, input$x_axis_max, length.out = 100)
    predicted_values <- predict(model, newdata = data.frame(Stability = stability_range), interval = "confidence", level = conf_level)
    
    p <- ggplot() +
      geom_line(aes(x = stability_range, y = true_model()(stability_range)), color = "#1f78b4", size = 1.5, linetype = "dashed") +
      geom_line(aes(x = stability_range, y = predicted_values[, "fit"]), color = "#6a3d9a", size = 1.5) +
      geom_point(aes(x = data$Stability, y = data$UCLA), color = "#cab2d6", alpha = 0.3) +
      xlim(input$x_axis_min, input$x_axis_max) +
      ylim(input$y_axis_min, input$y_axis_max) +
      labs(title = "Cubic Model vs. True Model",
           x = input$x_axis_label,
           y = input$y_axis_label) +
      theme_minimal() +
      theme(legend.position = "top")
    
    if (input$show_CI) {
      p <- p +
        geom_ribbon(aes(x = stability_range, ymin = predicted_values[, "lwr"], ymax = predicted_values[, "upr"]),
                    fill = "#c2b0f7", alpha = 0.2)
    }
    
    print(p)
  }, width = 400, height = 400)
  
  # Assumption Check Plots
  output$histogram_residuals_linear <- renderPlot({
    residuals <- residuals(linear_model())
    
    ggplot(data.frame(residuals), aes(x = residuals)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#56B4E9", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      labs(title = "Histogram of Residuals (Linear Model)", x = "Residuals", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$residual_dependence_plot_linear <- renderPlot({
    residuals <- residuals(linear_model())
    fitted_values <- fitted(linear_model())
    
    plot_data <- data.frame(Fitted_Values = fitted_values, Residuals = residuals)
    
    ggplot(plot_data, aes(x = Fitted_Values, y = Residuals)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residual Plot with LOESS Smooth (Linear Model)",
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$scale_location_plot_linear <- renderPlot({
    sqrt_abs_residuals <- sqrt(abs(residuals(linear_model())))
    fitted_values <- fitted(linear_model())
    
    ggplot(data.frame(fitted = fitted_values, sqrt_abs_residuals = sqrt_abs_residuals), aes(x = fitted, y = sqrt_abs_residuals)) +
      geom_point(color = "#D55E00", alpha = 0.7) +
      geom_smooth(method = "loess", color = "blue", se = FALSE) +
      labs(title = "Scale-Location Plot (Linear Model)", x = "Fitted Values", y = "Square Root of |Residuals|") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$histogram_residuals_quadratic <- renderPlot({
    residuals <- residuals(quadratic_model())
    
    ggplot(data.frame(residuals), aes(x = residuals)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#56B4E9", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      labs(title = "Histogram of Residuals (Quadratic Model)", x = "Residuals", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$residual_dependence_plot_quadratic <- renderPlot({
    residuals <- residuals(quadratic_model())
    fitted_values <- fitted(quadratic_model())
    
    plot_data <- data.frame(Fitted_Values = fitted_values, Residuals = residuals)
    
    ggplot(plot_data, aes(x = Fitted_Values, y = Residuals)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residual Plot with LOESS Smooth (Quadratic Model)",
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$scale_location_plot_quadratic <- renderPlot({
    sqrt_abs_residuals <- sqrt(abs(residuals(quadratic_model())))
    fitted_values <- fitted(quadratic_model())
    
    ggplot(data.frame(fitted = fitted_values, sqrt_abs_residuals = sqrt_abs_residuals), aes(x = fitted, y = sqrt_abs_residuals)) +
      geom_point(color = "#D55E00", alpha = 0.7) +
      geom_smooth(method = "loess", color = "blue", se = FALSE) +
      labs(title = "Scale-Location Plot (Quadratic Model)", x = "Fitted Values", y = "Square Root of |Residuals|") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$histogram_residuals_cubic <- renderPlot({
    residuals <- residuals(cubic_model())
    
    ggplot(data.frame(residuals), aes(x = residuals)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#56B4E9", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      labs(title = "Histogram of Residuals (Cubic Model)", x = "Residuals", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  output$residual_dependence_plot_cubic <- renderPlot({
    residuals <- residuals(cubic_model())
    fitted_values <- fitted(cubic_model())
    
    plot_data <- data.frame(Fitted_Values = fitted_values, Residuals = residuals)
    
    ggplot(plot_data, aes(x = Fitted_Values, y = Residuals)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residual Plot with LOESS Smooth (Cubic Model)",
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }, width = 400, height = 300)
  
  # Predictions for the new tab
  new_data <- reactive({
    data.frame(Stability = seq(input$x_min, input$x_max, by = input$x_interval))
  })
  
  output$predictions_linear_table <- renderReactable({
    interval_type <- input$interval_type
    predictions <- predict(linear_model(), newdata = new_data(), interval = interval_type, level = input$alpha_level)
    true_values <- true_model()(new_data()$Stability)
    
    prediction_table <- data.frame(
      Stability = new_data()$Stability,
      true_value = round(true_values, 2),
      fit = round(predictions[, "fit"], 2),
      lwr = round(predictions[, "lwr"], 2),
      upr = round(predictions[, "upr"], 2)
    )
    
    prediction_table <- prediction_table %>%
      mutate(
        highlight = ifelse(true_value >= lwr & true_value <= upr, "Yes", "No")
      )
    
    reactable(
      prediction_table,
      bordered = TRUE,
      columns = list(
        Stability = colDef(name = "Stability"),
        true_value = colDef(name = "True Value"),
        fit = colDef(name = "Fit"),
        lwr = colDef(name = paste0("Lower ", ifelse(interval_type == "confidence", "CI", "PI"))),
        upr = colDef(name = paste0("Upper ", ifelse(interval_type == "confidence", "CI", "PI"))),
        highlight = colDef(show = FALSE)
      ),
      rowStyle = function(index) {
        if (prediction_table$highlight[index] == "Yes") {
          list(background = "#d9f0d3", fontWeight = "bold")
        } else {
          list()
        }
      }
    )
  })
  
  output$predictions_quadratic_table <- renderReactable({
    interval_type <- input$interval_type
    predictions <- predict(quadratic_model(), newdata = new_data(), interval = interval_type, level = input$alpha_level)
    true_values <- true_model()(new_data()$Stability)
    
    prediction_table <- data.frame(
      Stability = new_data()$Stability,
      true_value = round(true_values, 2),
      fit = round(predictions[, "fit"], 2),
      lwr = round(predictions[, "lwr"], 2),
      upr = round(predictions[, "upr"], 2)
    )
    
    prediction_table <- prediction_table %>%
      mutate(
        highlight = ifelse(true_value >= lwr & true_value <= upr, "Yes", "No")
      )
    
    reactable(
      prediction_table,
      bordered = TRUE,
      columns = list(
        Stability = colDef(name = "Stability"),
        true_value = colDef(name = "True Value"),
        fit = colDef(name = "Fit"),
        lwr = colDef(name = paste0("Lower ", ifelse(interval_type == "confidence", "CI", "PI"))),
        upr = colDef(name = paste0("Upper ", ifelse(interval_type == "confidence", "CI", "PI"))),
        highlight = colDef(show = FALSE)
      ),
      rowStyle = function(index) {
        if (prediction_table$highlight[index] == "Yes") {
          list(background = "#d9f0d3", fontWeight = "bold")
        } else {
          list()
        }
      }
    )
  })
  
  output$predictions_cubic_table <- renderReactable({
    interval_type <- input$interval_type
    predictions <- predict(cubic_model(), newdata = new_data(), interval = interval_type, level = input$alpha_level)
    true_values <- true_model()(new_data()$Stability)
    
    prediction_table <- data.frame(
      Stability = new_data()$Stability,
      true_value = round(true_values, 2),
      fit = round(predictions[, "fit"], 2),
      lwr = round(predictions[, "lwr"], 2),
      upr = round(predictions[, "upr"], 2)
    )
    
    prediction_table <- prediction_table %>%
      mutate(
        highlight = ifelse(true_value >= lwr & true_value <= upr, "Yes", "No")
      )
    
    reactable(
      prediction_table,
      bordered = TRUE,
      columns = list(
        Stability = colDef(name = "Stability"),
        true_value = colDef(name = "True Value"),
        fit = colDef(name = "Fit"),
        lwr = colDef(name = paste0("Lower ", ifelse(interval_type == "confidence", "CI", "PI"))),
        upr = colDef(name = paste0("Upper ", ifelse(interval_type == "confidence", "CI", "PI"))),
        highlight = colDef(show = FALSE)
      ),
      rowStyle = function(index) {
        if (prediction_table$highlight[index] == "Yes") {
          list(background = "#d9f0d3", fontWeight = "bold")
        } else {
          list()
        }
      }
    )
  })
  
  diagnostics <- reactive({
    data <- first_sample()
    linear_model <- lm(UCLA ~ Stability, data = data)
    summary_model_1 <- summary(linear_model)
    quadratic_model <- lm(UCLA ~ Stability + I(Stability^2), data = data)
    summary_model_2 <- summary(quadratic_model)
    cubic_model <- lm(UCLA ~ Stability + I(Stability^2) + I(Stability^3), data = data)
    summary_model_3 <- summary(cubic_model)
    
    # Calculate Bayes Factor and Prediction Accuracy
    bf_linear_quadratic <- BIC(linear_model) - BIC(quadratic_model)
    bf_quadratic_cubic <- BIC(quadratic_model) - BIC(cubic_model)
    prediction_accuracy <- function(model) {
      predictions <- predict(model, newdata = data.frame(Stability = data$Stability))
      mean((data$UCLA - predictions)^2)
    }
    
    decimal_point <-4
    diagnostics_table <- data.frame(
      Model = c("Linear", "Quadratic", "Cubic"),
      Residual_Standard_Error = round(c(summary_model_1$sigma, summary_model_2$sigma, summary_model_3$sigma), decimal_point),
      Multiple_R_Squared = round(c(summary_model_1$r.squared, summary_model_2$r.squared, summary_model_3$r.squared), decimal_point),
      Adjusted_R_Squared = round(c(summary_model_1$adj.r.squared, summary_model_2$adj.r.squared, summary_model_3$adj.r.squared), decimal_point),
      F_Statistic = round(c(summary_model_1$fstatistic[1], summary_model_2$fstatistic[1], summary_model_3$fstatistic[1]), decimal_point),
      p_value = round(c(
        pf(summary_model_1$fstatistic[1], summary_model_1$fstatistic[2], summary_model_1$fstatistic[3], lower.tail = FALSE),
        pf(summary_model_2$fstatistic[1], summary_model_2$fstatistic[2], summary_model_2$fstatistic[3], lower.tail = FALSE),
        pf(summary_model_3$fstatistic[1], summary_model_3$fstatistic[2], summary_model_3$fstatistic[3], lower.tail = FALSE)
      ), decimal_point),
      AIC = round(c(AIC(linear_model), AIC(quadratic_model), AIC(cubic_model)), decimal_point),
      BIC = round(c(BIC(linear_model), BIC(quadratic_model), BIC(cubic_model)), decimal_point),
      Bayes_Factor = round(c(bf_linear_quadratic, bf_quadratic_cubic, NA), decimal_point),
      Prediction_Accuracy = round(c(prediction_accuracy(linear_model), prediction_accuracy(quadratic_model), prediction_accuracy(cubic_model)), decimal_point)
    )
    
    diagnostics_table
  })
  
  output$diagnosticsTable <- renderReactable({
    reactable(
      diagnostics(),
      bordered = TRUE,
      columns = list(
        AIC = colDef(style = function(value) {
          if (value == min(diagnostics()$AIC)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        }),
        BIC = colDef(style = function(value) {
          if (value == min(diagnostics()$BIC)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        }),
        Residual_Standard_Error = colDef(style = function(value) {
          if (value == min(diagnostics()$Residual_Standard_Error)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        }),
        Multiple_R_Squared = colDef(style = function(value) {
          if (value == max(diagnostics()$Multiple_R_Squared)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        }),
        Adjusted_R_Squared = colDef(style = function(value) {
          if (value == max(diagnostics()$Adjusted_R_Squared)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        }),
        Bayes_Factor = colDef(style = function(value) {
          if (!is.na(value) && value == max(diagnostics()$Bayes_Factor, na.rm = TRUE)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        }),
        Prediction_Accuracy = colDef(style = function(value) {
          if (value == min(diagnostics()$Prediction_Accuracy)) {
            list(background = "#d9f0d3", fontWeight = "bold")
          }
        })
      )
    )
  })
  
  output$comparisonTable <- renderReactable({
    linear_model_diagnostics <- list()
    quadratic_model_diagnostics <- list()
    cubic_model_diagnostics <- list()
    
    count_better_linear <- list(
      Residual_Standard_Error = 0,
      Multiple_R_Squared = 0,
      Adjusted_R_Squared = 0,
      F_Statistic = 0,
      p_value = 0,
      Standard_Error = 0,
      AIC = 0,
      BIC = 0,
      Bayes_Factor = 0,
      Prediction_Accuracy = 0
    )
    
    count_better_quadratic <- count_better_linear
    
    samples_list <- all_samples()
    for (i in 1:input$num_samples) {
      data <- samples_list[[i]]
      Stability_Loneliness <- lm(UCLA ~ Stability, data = data)
      summary_model_1 <- summary(Stability_Loneliness)
      
      Stability_Loneliness_2 <- lm(UCLA ~ Stability + I(Stability^2), data = data)
      summary_model_2 <- summary(Stability_Loneliness_2)
      
      Stability_Loneliness_3 <- lm(UCLA ~ Stability + I(Stability^2) + I(Stability^3), data = data)
      summary_model_3 <- summary(Stability_Loneliness_3)
      
      linear_model_diagnostics[[i]] <- list(
        Residual_Standard_Error = summary_model_1$sigma,
        Multiple_R_Squared = summary_model_1$r.squared,
        Adjusted_R_Squared = summary_model_1$adj.r.squared,
        F_Statistic = summary_model_1$fstatistic[1],
        p_value = pf(summary_model_1$fstatistic[1], 
                     summary_model_1$fstatistic[2], 
                     summary_model_1$fstatistic[3], 
                     lower.tail = FALSE),
        Standard_Error = summary_model_1$coefficients[2, "Std. Error"],
        AIC = AIC(Stability_Loneliness),
        BIC = BIC(Stability_Loneliness),
        Bayes_Factor = BIC(Stability_Loneliness) - BIC(Stability_Loneliness_2),
        Prediction_Accuracy = mean((data$UCLA - predict(Stability_Loneliness, newdata = data.frame(Stability = data$Stability)))^2)
      )
      
      quadratic_model_diagnostics[[i]] <- list(
        Residual_Standard_Error = summary_model_2$sigma,
        Multiple_R_Squared = summary_model_2$r.squared,
        Adjusted_R_Squared = summary_model_2$adj.r.squared,
        F_Statistic = summary_model_2$fstatistic[1],
        p_value = pf(summary_model_2$fstatistic[1], 
                     summary_model_2$fstatistic[2], 
                     summary_model_2$fstatistic[3], 
                     lower.tail = FALSE),
        Standard_Error = summary_model_2$coefficients[2, "Std. Error"],
        AIC = AIC(Stability_Loneliness_2),
        BIC = BIC(Stability_Loneliness_2),
        Bayes_Factor = BIC(Stability_Loneliness_2) - BIC(Stability_Loneliness_3),
        Prediction_Accuracy = mean((data$UCLA - predict(Stability_Loneliness_2, newdata = data.frame(Stability = data$Stability)))^2)
      )
      
      cubic_model_diagnostics[[i]] <- list(
        Residual_Standard_Error = summary_model_3$sigma,
        Multiple_R_Squared = summary_model_3$r.squared,
        Adjusted_R_Squared = summary_model_3$adj.r.squared,
        F_Statistic = summary_model_3$fstatistic[1],
        p_value = pf(summary_model_3$fstatistic[1], 
                     summary_model_3$fstatistic[2], 
                     summary_model_3$fstatistic[3], 
                     lower.tail = FALSE),
        Standard_Error = summary_model_3$coefficients[2, "Std. Error"],
        AIC = AIC(Stability_Loneliness_3),
        BIC = BIC(Stability_Loneliness_3),
        Bayes_Factor = NA,
        Prediction_Accuracy = mean((data$UCLA - predict(Stability_Loneliness_3, newdata = data.frame(Stability = data$Stability)))^2)
      )
      
      if (linear_model_diagnostics[[i]]$Residual_Standard_Error < quadratic_model_diagnostics[[i]]$Residual_Standard_Error) {
        count_better_linear$Residual_Standard_Error <- count_better_linear$Residual_Standard_Error + 1
      } else if (quadratic_model_diagnostics[[i]]$Residual_Standard_Error < cubic_model_diagnostics[[i]]$Residual_Standard_Error) {
        count_better_quadratic$Residual_Standard_Error <- count_better_quadratic$Residual_Standard_Error + 1
      }
      
      if (linear_model_diagnostics[[i]]$Multiple_R_Squared > quadratic_model_diagnostics[[i]]$Multiple_R_Squared) {
        count_better_linear$Multiple_R_Squared <- count_better_linear$Multiple_R_Squared + 1
      } else if (quadratic_model_diagnostics[[i]]$Multiple_R_Squared > cubic_model_diagnostics[[i]]$Multiple_R_Squared) {
        count_better_quadratic$Multiple_R_Squared <- count_better_quadratic$Multiple_R_Squared + 1
      }
      
      if (linear_model_diagnostics[[i]]$Adjusted_R_Squared > quadratic_model_diagnostics[[i]]$Adjusted_R_Squared) {
        count_better_linear$Adjusted_R_Squared <- count_better_linear$Adjusted_R_Squared + 1
      } else if (quadratic_model_diagnostics[[i]]$Adjusted_R_Squared > cubic_model_diagnostics[[i]]$Adjusted_R_Squared) {
        count_better_quadratic$Adjusted_R_Squared <- count_better_quadratic$Adjusted_R_Squared + 1
      }
      
      if (linear_model_diagnostics[[i]]$F_Statistic > quadratic_model_diagnostics[[i]]$F_Statistic) {
        count_better_linear$F_Statistic <- count_better_linear$F_Statistic + 1
      } else if (quadratic_model_diagnostics[[i]]$F_Statistic > cubic_model_diagnostics[[i]]$F_Statistic) {
        count_better_quadratic$F_Statistic <- count_better_quadratic$F_Statistic + 1
      }
      
      if (linear_model_diagnostics[[i]]$p_value < quadratic_model_diagnostics[[i]]$p_value) {
        count_better_linear$p_value <- count_better_linear$p_value + 1
      } else if (quadratic_model_diagnostics[[i]]$p_value < cubic_model_diagnostics[[i]]$p_value) {
        count_better_quadratic$p_value <- count_better_quadratic$p_value + 1
      }
      
      if (linear_model_diagnostics[[i]]$Standard_Error < quadratic_model_diagnostics[[i]]$Standard_Error) {
        count_better_linear$Standard_Error <- count_better_linear$Standard_Error + 1
      } else if (quadratic_model_diagnostics[[i]]$Standard_Error < cubic_model_diagnostics[[i]]$Standard_Error) {
        count_better_quadratic$Standard_Error <- count_better_quadratic$Standard_Error + 1
      }
      
      if (linear_model_diagnostics[[i]]$AIC < quadratic_model_diagnostics[[i]]$AIC) {
        count_better_linear$AIC <- count_better_linear$AIC + 1
      } else if (quadratic_model_diagnostics[[i]]$AIC < cubic_model_diagnostics[[i]]$AIC) {
        count_better_quadratic$AIC <- count_better_quadratic$AIC + 1
      }
      
      if (linear_model_diagnostics[[i]]$BIC < quadratic_model_diagnostics[[i]]$BIC) {
        count_better_linear$BIC <- count_better_linear$BIC + 1
      } else if (quadratic_model_diagnostics[[i]]$BIC < cubic_model_diagnostics[[i]]$BIC) {
        count_better_quadratic$BIC <- count_better_quadratic$BIC + 1
      }
      
      if (linear_model_diagnostics[[i]]$Prediction_Accuracy < quadratic_model_diagnostics[[i]]$Prediction_Accuracy) {
        count_better_linear$Prediction_Accuracy <- count_better_linear$Prediction_Accuracy + 1
      } else if (quadratic_model_diagnostics[[i]]$Prediction_Accuracy < cubic_model_diagnostics[[i]]$Prediction_Accuracy) {
        count_better_quadratic$Prediction_Accuracy <- count_better_quadratic$Prediction_Accuracy + 1
      }
    }
    
    percentage_better_linear <- sapply(count_better_linear, function(x) (x / input$num_samples) * 100)
    percentage_better_quadratic <- sapply(count_better_quadratic, function(x) (x / input$num_samples) * 100)
    
    decimal_point = 4
    comparison_table <- data.frame(
      Model = c("Linear", "Quadratic", "Cubic"),
      Residual_Standard_Error = round(c(percentage_better_linear["Residual_Standard_Error"], percentage_better_quadratic["Residual_Standard_Error"], 100 - sum(percentage_better_linear["Residual_Standard_Error"], percentage_better_quadratic["Residual_Standard_Error"])), decimal_point),
      Multiple_R_Squared = round(c(percentage_better_linear["Multiple_R_Squared"], percentage_better_quadratic["Multiple_R_Squared"], 100 - sum(percentage_better_linear["Multiple_R_Squared"], percentage_better_quadratic["Multiple_R_Squared"])), decimal_point),
      Adjusted_R_Squared = round(c(percentage_better_linear["Adjusted_R_Squared"], percentage_better_quadratic["Adjusted_R_Squared"], 100 - sum(percentage_better_linear["Adjusted_R_Squared"], percentage_better_quadratic["Adjusted_R_Squared"])), decimal_point),
      F_Statistic = round(c(percentage_better_linear["F_Statistic"], percentage_better_quadratic["F_Statistic"], 100 - sum(percentage_better_linear["F_Statistic"], percentage_better_quadratic["F_Statistic"])), decimal_point),
      p_value = round(c(percentage_better_linear["p_value"], percentage_better_quadratic["p_value"], 100 - sum(percentage_better_linear["p_value"], percentage_better_quadratic["p_value"])), decimal_point),
      Standard_Error = round(c(percentage_better_linear["Standard_Error"], percentage_better_quadratic["Standard_Error"], 100 - sum(percentage_better_linear["Standard_Error"], percentage_better_quadratic["Standard_Error"])), decimal_point),
      AIC = round(c(percentage_better_linear["AIC"], percentage_better_quadratic["AIC"], 100 - sum(percentage_better_linear["AIC"], percentage_better_quadratic["AIC"])), decimal_point),
      BIC = round(c(percentage_better_linear["BIC"], percentage_better_quadratic["BIC"], 100 - sum(percentage_better_linear["BIC"], percentage_better_quadratic["BIC"])), decimal_point),
      Bayes_Factor = round(c(percentage_better_linear["Bayes_Factor"], percentage_better_quadratic["Bayes_Factor"], 100 - sum(percentage_better_linear["Bayes_Factor"], percentage_better_quadratic["Bayes_Factor"])), 2),
      Prediction_Accuracy = round(c(percentage_better_linear["Prediction_Accuracy"], percentage_better_quadratic["Prediction_Accuracy"], 100 - sum(percentage_better_linear["Prediction_Accuracy"], percentage_better_quadratic["Prediction_Accuracy"])), decimal_point)
    )
    
    reactable(comparison_table, 
              bordered = TRUE,
              columns = list(
                Model = colDef(name = "Model"),
                Residual_Standard_Error = colDef(name = "Residual Std. Error (%)",
                                                 style = function(value) {
                                                   if (value == max(comparison_table$Residual_Standard_Error)) {
                                                     list(background = "#d9f0d3", fontWeight = "bold")
                                                   }
                                                 }),
                Multiple_R_Squared = colDef(name = "Multiple R-Squared (%)",
                                            style = function(value) {
                                              if (value == max(comparison_table$Multiple_R_Squared)) {
                                                list(background = "#d9f0d3", fontWeight = "bold")
                                              }
                                            }),
                Adjusted_R_Squared = colDef(name = "Adjusted R-Squared (%)",
                                            style = function(value) {
                                              if (value == max(comparison_table$Adjusted_R_Squared)) {
                                                list(background = "#d9f0d3", fontWeight = "bold")
                                              }
                                            }),
                F_Statistic = colDef(name = "F-Statistic (%)",
                                     style = function(value) {
                                       if (value == max(comparison_table$F_Statistic)) {
                                         list(background = "#d9f0d3", fontWeight = "bold")
                                       }
                                     }),
                p_value = colDef(name = "p-Value (%)",
                                 style = function(value) {
                                   if (value == max(comparison_table$p_value)) {
                                     list(background = "#d9f0d3", fontWeight = "bold")
                                   }
                                 }),
                Standard_Error = colDef(name = "Standard Error (%)",
                                        style = function(value) {
                                          if (value == max(comparison_table$Standard_Error)) {
                                            list(background = "#d9f0d3", fontWeight = "bold")
                                          }
                                        }),
                AIC = colDef(name = "AIC (%)",
                             style = function(value) {
                               if (value == max(comparison_table$AIC)) {
                                 list(background = "#d9f0d3", fontWeight = "bold")
                               }
                             }),
                BIC = colDef(name = "BIC (%)",
                             style = function(value) {
                               if (value == max(comparison_table$BIC)) {
                                 list(background = "#d9f0d3", fontWeight = "bold")
                               }
                             }),
                Bayes_Factor = colDef(name = "Bayes Factor (%)",
                                      style = function(value) {
                                        if (value == max(comparison_table$Bayes_Factor)) {
                                          list(background = "#d9f0d3", fontWeight = "bold")
                                        }
                                      }),
                Prediction_Accuracy = colDef(name = "Prediction Accuracy (%)",
                                             style = function(value) {
                                               if (value == max(comparison_table$Prediction_Accuracy)) {
                                                 list(background = "#d9f0d3", fontWeight = "bold")
                                               }
                                             })
              ))
  })
}

shinyApp(ui = ui, server = server)

rsconnect::deployApp()

