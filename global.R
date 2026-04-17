library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(tibble)
library(purrr)

set.seed(123)

# Load taxi dataset, keep key variables, and remove invalid rows
taxi_data <- readRDS("cleaned_data.rds") %>%
  dplyr::select(
    RatecodeID,
    payment_type,
    trip_distance,
    fare_amount,
    tip_amount,
    PULocationID,
    pickup_hour,
    trip_duration
  ) %>%
  dplyr::filter(
    fare_amount > 0,
    tip_amount >= 0,
    trip_duration > 0
  )

# Sample data down to a manageable size for faster app performance.
# Larger datasets would not process in rshiny
taxi_data <- dplyr::slice_sample(
  taxi_data,
  n = min(30000, nrow(taxi_data))
)

# Created derived fields in an app
taxi_data <- taxi_data %>%
  dplyr::mutate(
    # Treat pickup zone as categorical for modeling
    PULocationID = factor(PULocationID),
    pickup_hour = factor(pickup_hour),
    # Derived metrics
    tip_pct = (tip_amount / fare_amount) * 100,
    log_tip = log(tip_amount + 1),
    log_fare = log(fare_amount + 1),
    log_duration = log(trip_duration + 1),
    
    payment_type_label = dplyr::recode(
      as.character(payment_type),
      "1" = "Credit Card",
      "2" = "Cash",
      .default = "Other"
    ),
    
    payment_type_label = factor(
      payment_type_label,
      levels = c("Credit Card", "Cash", "Other")
    ),
    
    payment_type_plot = dplyr::case_when(
      payment_type == 2 & tip_amount == 0 ~ NA_character_,
      TRUE ~ as.character(payment_type_label)
    ),
    
    payment_type_plot = factor(
      payment_type_plot,
      levels = c("Credit Card", "Cash", "Other")
    ),
    
    # Keep RatecodeID ordered numerically for plotting.
    # Originally they were in alphabetical order
    RatecodeID = factor(
      RatecodeID,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "1 - Standard",
        "2 - JFK",
        "3 - Newark",
        "4 - Nassau/Westchester",
        "5 - Negotiated"
      )
    ),
    
    # Ordered distance groups for display
    distance_group = case_when(
      trip_distance < 2 ~ "Under 2 miles",
      trip_distance < 5 ~ "2 to <5 miles",
      trip_distance < 10 ~ "5 to <10 miles",
      trip_distance < 20 ~ "10 to <20 miles",
      TRUE ~ "20+ miles"
    ),
    
    distance_group = factor(
      distance_group,
      levels = c(
        "Under 2 miles",
        "2 to <5 miles",
        "5 to <10 miles",
        "10 to <20 miles",
        "20+ miles"
      ),
      ordered = TRUE
    )
  ) %>%
  dplyr::filter(
    tip_pct >= 0,
    tip_pct <= 100
  )

# Fit regression model used in the app. used the same model as
# used for the linear regression model
taxi_model_log <- lm(
  log_tip ~ log_fare + log_duration + payment_type + PULocationID + pickup_hour,
  data = taxi_data
)

# Lightweight knowledge base for Ask RAG
# Questions that can be answered:
#. What is the difference between tip amount and tip percentage?
#  Why do credit card trips look like they tip more?
#  Do airport trips tip differently?
#  Do long trips always tip more?
#  Why did we use log transformation?
#  Why doesn’t the coefficient table change?
#  What is this dashboard supposed to do?
#  How accurate is the model at predicting tip amounts?

project_kb <- tibble::tibble(
  id = c(
    "model_overview",
    "model_variables",
    "payment_type",
    "distance_group",
    "ratecode",
    "boxplot_help",
    "residuals",
    "coefficients",
    "prediction",
    "limitations",
    "tip_amount_vs_tip_pct",
    "cash_vs_card",
    "airport_trips",
    "long_trips",
    "short_trips",
    "pickup_hour",
    "why_log_transform",
    "outliers",
    "dashboard_purpose",
    "model_not_retrained",
    "model_accuracy"
  ),
  topic = c(
    "model",
    "model",
    "insight",
    "insight",
    "insight",
    "visual",
    "visual",
    "model",
    "prediction",
    "limitations",
    "insight",
    "insight",
    "insight",
    "insight",
    "insight",
    "insight",
    "model",
    "visual",
    "dashboard",
    "model",
    "model"
  ),
  keywords = c(
    "model regression predict prediction purpose",
    "variables predictors fare duration payment type zone location hour",
    "payment type credit card cash tipping tip behavior",
    "distance group trip distance miles tip percentage amount",
    "ratecode rate code airport jfk newark zone",
    "boxplot mean median red dot box outlier chart",
    "residual residuals fitted plot error assumptions",
    "coefficient coefficients estimate table regression effect",
    "predicted tip estimate selected trip user input",
    "limitations limit bias cash missing tips recorded data",
    "tip amount tip percentage difference compare",
    "cash card credit payment recorded tips underreported",
    "airport jfk newark laguardia ratecode airport trips",
    "long trips longer trip high fare tipping distance",
    "short trips shorter trip local tipping",
    "pickup hour time of day hour tipping patterns",
    "log transform log transformed why skewed skewness",
    "outliers extreme values unusual trips removed",
    "dashboard purpose what does this app do",
    "coefficients update change retrain fixed model",
    "accuracy rmse mae error model performance predicting tip amounts"
  ),
  answer = c(
    "This project uses a linear regression model to predict taxi tip amount. The model is designed to support both prediction and interpretation of tipping patterns across trip conditions.",
    
    "The regression model uses log-transformed fare amount and trip duration, along with payment type, pickup location, and pickup hour, to estimate tip amount.",
    
    "Payment type is important because recorded tip behavior often differs between credit card and cash trips. Credit card tips are more directly captured in the dataset, while cash tips may be under-recorded.",
    
    "Distance group helps compare tipping patterns across shorter and longer trips. Distance may influence tip amount and tip percentage differently, so both measures should be interpreted carefully.",
    
    "RatecodeID can help identify whether tipping patterns differ across special fare structures, such as airport-related trips or other coded trip types. It is useful for comparing tipping behavior across trip contexts.",
    
    "In the boxplot, the box shows the middle 50 percent of values, the line inside the box is the median, and the red dot marks the mean. This helps compare typical tipping behavior and skew across categories.",
    
    "The residual plot is used to check whether model errors are centered around zero and whether there are visible patterns the model is missing. A good residual plot should not show a strong systematic shape.",
    
    "The coefficient table shows the estimated effect of each predictor in the regression model, holding the other variables constant. These coefficients come from the fixed model and do not change when a user changes trip inputs.",
    
    "The predicted tip section estimates the tip for a user-selected trip based on the fitted regression model. Changing the input values changes the prediction, but it does not retrain the model.",
    
    "This project has some limitations. Recorded cash tips may be incomplete, some rider behavior is unobserved, and the model is meant to support useful prediction rather than explain every factor that affects tipping.",
    
    "Tip amount and tip percentage are related but not identical. A longer or more expensive trip may produce a higher dollar tip, while the percentage tipped may remain moderate. This is why the dashboard allows both outcomes to be explored.",
    
    "A key taxi-data insight is that credit card trips usually show higher recorded tips than cash trips. This does not necessarily mean cash riders tip less in reality; it may also reflect that cash tips are not always fully captured in the trip record.",
    
    "Airport-related trips can show different tipping patterns because they often involve different fare structures, longer travel distances, and distinct rider contexts. RateCodeID helps identify whether those airport or special-fare trips behave differently from standard trips.",
    
    "Longer trips often have higher tip amounts in dollars because fares are larger, but that does not automatically imply a much higher tip percentage. This distinction is important when interpreting trip distance effects.",
    
    "Shorter trips may have lower total tip amounts simply because the fare base is smaller. However, short trips can still have meaningful or even relatively high tip percentages depending on rider behavior.",
    
    "Pickup hour is included because tipping behavior may vary across times of day. Differences by hour can reflect commuter traffic, airport demand, nightlife activity, or changes in trip purpose.",
    
    "Log transformations were used because fare amount, trip duration, and tip amount are often right-skewed. Using logs helps stabilize extreme values and can improve model fit for linear regression.",
    
    "Outliers were handled carefully because taxi data can contain unusually high fares, durations, or tips. Reducing the visual effect of extreme values makes category comparisons easier while still preserving the main structure of the data.",
    
    "This dashboard is designed to do two things: communicate key insights about NYC taxi tipping behavior and provide a prediction tool for estimated tip amount based on trip characteristics.",
    
    "The coefficient table does not update when a user changes the inputs because the model is fixed after training. User selections change the prediction for a new trip, but they do not retrain the regression model.",
    
    "Model accuracy can be evaluated using RMSE and MAE. In this model, RMSE is approximately 1.89 and MAE is around 0.85. This means predictions are typically off by about 0.85 to 1.89 dollars. RMSE penalizes larger errors more heavily, while MAE shows the average absolute prediction error."
  )
)