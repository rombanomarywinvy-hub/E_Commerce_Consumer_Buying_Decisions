
library(shiny)
library(ggplot2)

# LOAD DATA
ecommerce_data <- read.csv("ecommerce_consumer_buying.csv")

# PREMIUM DARK DASHBOARD UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #101827;
        color: #e5e7eb;
        font-family: 'Segoe UI', Arial, sans-serif;
      }

      .container-fluid {
        padding: 25px;
      }

      .dashboard-header {
        background: linear-gradient(120deg, #123c46, #087f8c);
        padding: 35px;
        border-radius: 18px;
        margin-bottom: 25px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.25);
      }

      .dashboard-header h1 {
        color: #ffffff;
        font-size: 30px;
        font-weight: 800;
      }

      .dashboard-header p {
        color: #c9f4ed;
        font-size: 15px;
      }

      .sidebar {
        background: #1b2638;
        padding: 22px;
        border-radius: 16px;
        border: 1px solid #334155;
      }

      .sidebar h3 {
        color: #5eead4;
        font-weight: bold;
      }

      .control-label {
        color: #d1d5db;
      }

      .form-control {
        background-color: #273449;
        color: white;
        border: 1px solid #475569;
        border-radius: 8px;
      }

      .form-control option {
        background: #273449;
        color: white;
      }

      .metric-card {
        background: #1b2638;
        padding: 23px;
        border-radius: 15px;
        margin-bottom: 20px;
        min-height: 125px;
        border: 1px solid #334155;
        box-shadow: 0 5px 18px rgba(0,0,0,0.2);
      }

      .metric-card.green {
        border-top: 4px solid #2dd4bf;
      }

      .metric-card.blue {
        border-top: 4px solid #38bdf8;
      }

      .metric-card.orange {
        border-top: 4px solid #fbbf24;
      }

      .metric-title {
        color: #94a3b8;
        font-size: 13px;
        font-weight: bold;
        text-transform: uppercase;
      }

      .metric-value {
        color: #5eead4;
        font-size: 27px;
        font-weight: bold;
        margin-top: 15px;
      }

      .panel-card {
        background: #1b2638;
        padding: 22px;
        border-radius: 16px;
        margin-bottom: 22px;
        border: 1px solid #334155;
        box-shadow: 0 5px 18px rgba(0,0,0,0.2);
      }

      .panel-card h3 {
        color: #5eead4;
        font-weight: bold;
        margin-top: 0;
        margin-bottom: 20px;
      }

      .btn-primary {
        background: #0d9488;
        color: white;
        border: none;
        border-radius: 8px;
        font-weight: bold;
      }

      .btn-primary:hover {
        background: #14b8a6;
        color: white;
      }

      .table {
        color: #e5e7eb;
      }

      .table > thead > tr > th {
        background: #0f766e;
        color: white;
        border-color: #334155;
      }

      .table > tbody > tr > td {
        background: #273449;
        color: #e5e7eb;
        border-color: #475569;
      }

      .footer {
        color: #94a3b8;
        text-align: center;
        padding: 20px;
      }
    "))
  ),
  
  # HEADER
  div(
    class = "dashboard-header",
    h1("E-COMMERCE ANALYTICS"),
    h3("Consumer Usage and Buying Decisions"),
    p("A data-driven overview of shopping preferences, spending habits, and consumer satisfaction.")
  ),
  
  sidebarLayout(
    
    # SIDEBAR
    sidebarPanel(
      class = "sidebar",
      
      h3("FILTER CONTROL"),
      p("Customize your dashboard view."),
      
      selectInput(
        "platform",
        "Preferred Platform:",
        choices = c("All", sort(unique(
          ecommerce_data$Preferred_Platform
        )))
      ),
      
      selectInput(
        "frequency",
        "Shopping Frequency:",
        choices = c("All", sort(unique(
          ecommerce_data$Shopping_Frequency
        )))
      ),
      
      selectInput(
        "category",
        "Product Category:",
        choices = c("All", sort(unique(
          ecommerce_data$Preferred_Product_Category
        )))
      ),
      
      actionButton(
        "reset",
        "RESET FILTERS",
        class = "btn-primary",
        width = "100%"
      )
    ),
    
    # MAIN CONTENT
    mainPanel(
      
      # KPI CARDS
      fluidRow(
        column(
          4,
          div(
            class = "metric-card green",
            div(class = "metric-title", "Total Respondents"),
            div(class = "metric-value",
                textOutput("total_respondents"))
          )
        ),
        
        column(
          4,
          div(
            class = "metric-card blue",
            div(class = "metric-title", "Average Spending"),
            div(class = "metric-value",
                textOutput("average_spending"))
          )
        ),
        
        column(
          4,
          div(
            class = "metric-card orange",
            div(class = "metric-title", "Satisfaction Score"),
            div(class = "metric-value",
                textOutput("average_satisfaction"))
          )
        )
      ),
      
      # GRAPH 1
      div(
        class = "panel-card",
        h3("01 | Preferred E-Commerce Platforms"),
        plotOutput("platform_plot", height = "320px")
      ),
      
      # GRAPH 2
      div(
        class = "panel-card",
        h3("02 | Monthly Spending by Shopping Frequency"),
        plotOutput("spending_plot", height = "320px")
      ),
      
      # GRAPH 3
      div(
        class = "panel-card",
        h3("03 | Consumer Buying Decision Factors"),
        plotOutput("factor_plot", height = "320px")
      ),
      
      # DATA TABLE
      div(
        class = "panel-card",
        h3("Consumer Information Records"),
        tableOutput("consumer_table")
      )
    )
  ),
  
  div(
    class = "footer",
    "E-Commerce Usage and Consumer Buying Decisions | Quantitative Methods"
  )
)

# SERVER
server <- function(input, output, session) {
  
  # FILTER DATA
  filtered_data <- reactive({
    
    data <- ecommerce_data
    
    if (input$platform != "All") {
      data <- data[
        data$Preferred_Platform == input$platform, ]
    }
    
    if (input$frequency != "All") {
      data <- data[
        data$Shopping_Frequency == input$frequency, ]
    }
    
    if (input$category != "All") {
      data <- data[
        data$Preferred_Product_Category == input$category, ]
    }
    
    data
  })
  
  # RESET FILTERS
  observeEvent(input$reset, {
    updateSelectInput(session, "platform", selected = "All")
    updateSelectInput(session, "frequency", selected = "All")
    updateSelectInput(session, "category", selected = "All")
  })
  
  # KPI CARDS
  output$total_respondents <- renderText({
    nrow(filtered_data())
  })
  
  output$average_spending <- renderText({
    data <- filtered_data()
    
    if (nrow(data) == 0) return("No data")
    
    paste0("₱", format(
      round(mean(data$Average_Spending_PHP), 2),
      big.mark = ",", nsmall = 2
    ))
  })
  
  output$average_satisfaction <- renderText({
    data <- filtered_data()
    
    if (nrow(data) == 0) return("No data")
    
    paste0(
      round(mean(data$Buying_Satisfaction_Score), 2),
      " / 5"
    )
  })
  
  # GRAPH 1
  output$platform_plot <- renderPlot({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records."))
    
    ggplot(data, aes(
      x = Preferred_Platform,
      fill = Preferred_Platform
    )) +
      geom_bar(width = 0.7) +
      scale_fill_manual(values = c(
        "#2dd4bf", "#38bdf8", "#a78bfa", "#fbbf24"
      )) +
      labs(x = "Platform", y = "Respondents") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(
          fill = "#1b2638", color = NA
        ),
        panel.background = element_rect(
          fill = "#1b2638", color = NA
        ),
        panel.grid.major = element_line(color = "#334155"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#e5e7eb"),
        axis.title = element_text(color = "#5eead4"),
        legend.position = "none"
      )
  })
  
  # GRAPH 2
  output$spending_plot <- renderPlot({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records."))
    
    ggplot(data, aes(
      x = Shopping_Frequency,
      y = Average_Spending_PHP,
      fill = Shopping_Frequency
    )) +
      geom_boxplot(alpha = 0.9) +
      scale_fill_manual(values = c(
        "#2dd4bf", "#38bdf8", "#a78bfa", "#fbbf24"
      )) +
      labs(
        x = "Shopping Frequency",
        y = "Monthly Spending (PHP)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(
          fill = "#1b2638", color = NA
        ),
        panel.background = element_rect(
          fill = "#1b2638", color = NA
        ),
        panel.grid.major = element_line(color = "#334155"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#e5e7eb"),
        axis.title = element_text(color = "#5eead4"),
        legend.position = "none"
      )
  })
  
  # GRAPH 3
  output$factor_plot <- renderPlot({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records."))
    
    factor_data <- as.data.frame(
      table(data$Purchase_Decision_Factor)
    )
    
    names(factor_data) <- c("Factor", "Respondents")
    
    ggplot(factor_data, aes(
      x = reorder(Factor, Respondents),
      y = Respondents,
      fill = Factor
    )) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c(
        "#2dd4bf", "#38bdf8", "#a78bfa",
        "#fbbf24", "#fb7185"
      )) +
      labs(
        x = "Buying Decision Factor",
        y = "Respondents"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(
          fill = "#1b2638", color = NA
        ),
        panel.background = element_rect(
          fill = "#1b2638", color = NA
        ),
        panel.grid.major = element_line(color = "#334155"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#e5e7eb"),
        axis.title = element_text(color = "#5eead4"),
        legend.position = "none"
      )
  })
  
  # DATA TABLE
  output$consumer_table <- renderTable({
    filtered_data()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
}

# RUN APP
shinyApp(ui = ui, server = server)
