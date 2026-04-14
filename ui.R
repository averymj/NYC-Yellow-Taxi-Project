#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# bslib provides themes and additional items to polish the look of rshiny apps
library(bslib)
#shinycssloaders - gives me a visual while the plot is rendering. So that it doesn't look like my code isn't working.
library(shinycssloaders)

#This line in the code assigns an adjustable web page to the object ui
#navset_tab was used to create individual tabs for each topic
#nav_panel creates the ui for each tab
ui <- fluidPage(
  
  # Application title
  titlePanel("NYC Taxi Tipping Insights Dashboard"),
      navset_tab(
        id = "tab",
        
        nav_panel(
          "Overview",
          h3("Overview"),
          p(strong("Understanding tipping behavior across trip conditions and travel patterns.")),
          br(),
          p(strong("Prepared by:")),
          p("Avery John"),
          p('Diane Roberts'),
          p("Spring 2026")
        ),
        
        nav_panel(
          "Explore Patterns",
          
          h3("Explore Patterns"),
          
          p("Explore the key patterns identified in the data, including differences in tipping behavior across trip conditions, payment characteristics, and trip distance."),
          
          br(),
          
          strong(textOutput("eda_n")),
          
          fluidRow(
            column(
              width = 4,
              
              selectInput(
                "eda_group",
                "Compare by category:",
                choices = c(
                  "RateCodeID" = "RatecodeID",
                  "Payment Type" = "payment_type_plot",
                  "Distance Group" = "distance_group"
                ),
                selected = "RatecodeID"
              ),
              
              selectInput(
                "eda_yvar",
                "Select outcome:",
                choices = c(
                  "Tip Amount" = "tip_amount",
                  "Tip Percentage" = "tip_pct"
                ),
                selected = "tip_pct"
              )
            ),
            
            column(
              width = 8,
              withSpinner(plotOutput("eda_boxplot", height = "350px"))
            )
          )
        ),
        
        nav_panel(
          "Model Insights",
          h3("Model Insights"),
          
          p("This section demonstrates the linear regression model used to estimate taxi tip amount based on trip characteristics. The model uses log-transformed variables to better handle skewed data and improve model fit."),
          
          br(),
          
          fluidRow(
            column(
              width = 4,
              
              numericInput(
                "reg_fare",
                "Fare Amount ($):",
                value = 20,
                min = 1,
                max = 200
              ),
              
              numericInput(
                "reg_duration",
                "Trip Duration (mins):",
                value = 15,
                min = 1,
                max = 300
              ),
              
              selectInput(
                "reg_payment",
                "Payment Type:",
                choices = c(
                  "Credit Card" = 1,
                  "Cash" = 2
                ),
                selected = 1
              ),
              
              selectInput(
                "reg_zone",
                "Pickup Zone ID:",
                choices = NULL
              ),
              
              sliderInput(
                "reg_hour",
                "Pickup Hour:",
                min = 0,
                max = 23,
                value = 12,
                step = 1
              )
            ),
            
            column(
              width = 8,
              
              h4("Predicted Tip"),
              verbatimTextOutput("predicted_tip"),
              
              br(),
 # Added a spinner based on code found on the Shiny Apps widget site so that if the rendeering
 # takes a long time there is a spinner that indicates that work is being done. Not that there is an
 # issue with the dashboard
              withSpinner(plotOutput("actual_pred_plot", height = "300px")),
              
              h4("What Drives Tip Amount"),
              plotOutput("feature_impact_plot", height = "300px")
            )
          )
        ),
        
          nav_panel(
          "Ask",
          h3("Ask About the Taxi Project"),
          p("Ask a question about the model, tipping patterns, or dashboard visuals."),
    
          textAreaInput(
            "user_question",
            "Your question:",
            rows = 4,
            placeholder = "Example: How does payment type affect tipping?"
          ),
  ##Due to limitations with Colab tools being active when a RAG is run, I chose to do an internal knowledge base
  ## which serves as a RAG using natural language processing to demonstrate a proof of concept
  
          actionButton("ask_btn", "Ask"),
    
          br(), br(),
    
          h4("Answer"),
          verbatimTextOutput("qa_answer"),
    
          h4("Retrieved Knowledge"),
          tableOutput("qa_sources")
        )
      )
)
