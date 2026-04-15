#Server side code for rendering plots

function(input, output, session) {
  
  # Reactive Filter from shinyapp to create data for EDA visuals that
  # is updated when the inputs are changed
  filtered_data <- reactive({
    req(input$eda_group)
    req(input$eda_yvar)
    req(input$eda_group %in% names(taxi_data))
    req(input$eda_yvar %in% names(taxi_data))
    
    taxi_data %>%
      dplyr::filter(
        !is.na(.data[[input$eda_group]]),
        !is.na(.data[[input$eda_yvar]])
      )
  })
  
  # EDA boxplot
  output$eda_boxplot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = .data[[input$eda_group]],
        y = .data[[input$eda_yvar]]
      )
    ) +
      geom_boxplot(fill = "lightblue", outlier.shape = NA, na.rm = TRUE) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      stat_summary(
        fun = median,
        geom = "text",
        aes(label = paste0("$", round(..y.., 2))),
        vjust = -1.5,
        size = 4,
        fontface = "bold"
      ) +
      labs(
        title = paste("Distribution of", input$eda_yvar, "by", input$eda_group),
        subtitle = "Box = middle 50%; line = median; red dot = mean",
        x = paste(input$eda_group, "(Category)"),
        y = paste(input$eda_yvar, "(Continuous Measure)"),
        caption = "Outliers removed from visualization for clarity"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    if (input$eda_yvar == "tip_pct") {
      p <- p + coord_cartesian(ylim = c(0, 100))
    }
    
    p
  })
  
  # I found this in a shinyapp widget and it counts the number of rows (trips)
  # in the reactive dataset that is created with each change of input.
  # This allows the viewer to see how many trips were included in this evaluation.
  output$eda_n <- renderText({
    req(nrow(filtered_data()) > 0)
    paste("Trips in current view:", nrow(filtered_data()))
  })
  
  # Populate the pickup zone selector with the 5 used for EDA and modeling
  observe({
    updateSelectInput(
      session,
      "reg_zone",
      choices = levels(taxi_data$PULocationID),
      selected = levels(taxi_data$PULocationID)[1]
    )
  })
  
  # This code is designed to create the model
  new_trip <- reactive({
    req(input$reg_fare)
    req(input$reg_duration)
    req(input$reg_payment)
    req(input$reg_zone)
    req(input$reg_hour)
    
    data.frame(
      log_fare = log(input$reg_fare + 1),
      log_duration = log(input$reg_duration + 1),
      payment_type = as.numeric(input$reg_payment),
      PULocationID = factor(
        input$reg_zone,
        levels = levels(taxi_data$PULocationID)
      ),
      pickup_hour = factor(
        input$reg_hour,
        levels = levels(taxi_data$pickup_hour)
      )
    )
  })
  
  # Warn user if they select cash. I found this on a shinyapp widget
  # and it creates a temporary box. Since cash tips were not seperately
  # noted it was important to let the user know why.
  observeEvent(input$reg_payment, {
    if (input$reg_payment == 2) {
      showNotification(
        "Cash tips are not fully captured and predictions may be unreliable.",
        type = "warning"
      )
    }
  })
  
  # Predicted tip output
  output$predicted_tip <- renderText({
    pred_log <- predict(taxi_model_log, newdata = new_trip())
    pred_tip <- exp(pred_log) - 1
    
    paste0("Estimated Tip Amount: $", round(pred_tip, 2))
  })
  
  # Full plot dataset
  plot_data <- reactive({
    taxi_data %>% sample_n(min(5000,nrow(taxi_data)))
  })
  
  # Fare vs predicted tip plot
  output$actual_pred_plot <- renderPlot({
    pred_vals <- exp(predict(taxi_model_log, newdata = plot_data())) - 1
    new_pred  <- exp(predict(taxi_model_log, newdata = new_trip())) - 1
    
    ggplot(
      data.frame(
        fare = plot_data()$fare_amount,
        predicted = pred_vals
      ),
      aes(x = fare, y = predicted)
    ) +
      geom_point(alpha = 0.2) +
      
      geom_point(
        data = data.frame(
          fare = input$reg_fare,
          predicted = new_pred
        ),
        color = "red",
        size = 6
      ) +
      
      coord_cartesian(ylim = c(0, 20)) +
      
      labs(
        title = "Fare vs Predicted Tip Amount",
        subtitle = "Red point = your selected trip",
        x = "Fare Amount",
        y = "Predicted Tip"
      ) +
      theme_minimal()
  })
  
  # What drives tip amount. This is static and grouped.
  output$feature_impact_plot <- renderPlot({
    
    coef_df <- as.data.frame(summary(taxi_model_log)$coefficients)
    coef_df$Predictor <- rownames(coef_df)
    
    impact_df <- coef_df %>%
      dplyr::filter(Predictor != "(Intercept)") %>%
      dplyr::mutate(
        group = dplyr::case_when(
          grepl("^log_fare$", Predictor) ~ "Fare Amount",
          grepl("^log_duration$", Predictor) ~ "Trip Duration",
          grepl("^payment_type", Predictor) ~ "Payment Type",
          grepl("^PULocationID", Predictor) ~ "Pickup Zone",
          grepl("^pickup_hour", Predictor) ~ "Pickup Hour",
          TRUE ~ "Other"
        ),
        impact = abs(Estimate)
      ) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        impact = sum(impact),
        .groups = "drop"
      ) %>%
      dplyr::arrange(impact)
    
    ggplot(impact_df, aes(x = reorder(group, impact), y = impact, fill = group)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "What Drives Tip Amount",
        subtitle = "Grouped relative impact of variables in the model",
        x = NULL,
        y = "Total absolute coefficient impact"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10)
      )
  })
  
  # Ask tab retrieval is a proof of concept for asking questions
  # on the data and the model
  qa_result <- eventReactive(input$ask_btn, {
    req(input$user_question)
    
    question <- tolower(trimws(input$user_question))
    req(nchar(question) > 0)
    
    question_words <- unlist(strsplit(question, "\\s+"))
    
    kb_scored <- project_kb %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        score = sum(stringr::str_detect(
          question_words,
          stringr::regex(
            paste(strsplit(keywords, "\\s+")[[1]], collapse = "|"),
            ignore_case = TRUE
          )
        ))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(score))
    
    top_hits <- kb_scored %>%
      dplyr::filter(score >= 2) %>%
      dplyr::slice_head(n = 3)
    
    if (nrow(top_hits) == 0) {
      return(list(
        answer = "I do not have enough context. Try asking about the model, boxplot, or predictions.",
        sources = data.frame(topic = "No match", score = 0)
      ))
    }
    
    list(
      answer = paste(top_hits$answer, collapse = "\n\n"),
      sources = top_hits %>% dplyr::select(topic, id, score)
    )
  })
  
  output$qa_answer <- renderText({
    req(qa_result())
    qa_result()$answer
  })
  
  output$qa_sources <- renderTable({
    req(qa_result())
    qa_result()$sources
  })
}