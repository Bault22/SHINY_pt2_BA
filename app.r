library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f0f5; /* Light gray background */
      }
      .btn-primary, .btn-primary:hover {
        background-color: #4CAF50; /* Green button */
        border-color: #4CAF50;
      }
      .form-control, .form-control:focus {
        border-color: #4CAF50; /* Green form input */
        box-shadow: none;
      }
      .panel-default > .panel-heading {
        background-color: #4CAF50; /* Green panel heading */
        border-color: #4CAF50;
        color: #fff; /* White text */
      }
      .panel-body {
        background-color: #fff; /* White panel body */
      }
      .plot-container {
        border: 2px solid #4CAF50; /* Green border around plots */
        border-radius: 5px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  # Application title
  titlePanel("Brian's Linear Model Dashboard"),
  
  # Sidebar with file input and action button
  sidebarLayout(
    sidebarPanel(
      
      # Input: Select a file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line
      tags$hr(),
      
      # Action button to run linear model
      actionButton("go", "Run Model"),
      
      # Input: Select number of rows to display
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      div(class = "plot-container",
          plotOutput("origPlot", width = "100%", height = "400px")),
      div(class = "plot-container",
          plotOutput("lmPlot", width = "100%", height = "400px")),
      verbatimTextOutput("model_summary"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw plots and model data
server <- function(input, output) {
  
  # Reactive values to store data and model
  dataInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  lm_model <- reactiveVal(NULL)
  
  # Update the linear model when 'Run Model' button is clicked
  observeEvent(input$go, {
    lm_fit <- lm(y ~ x, data = dataInput())
    lm_model(lm_fit)
  })
  
  # Render the original scatter plot
  output$origPlot <- renderPlot({
    req(dataInput())
    
    plot(dataInput()$x, dataInput()$y,
         main = "Original Data",
         xlab = "X", ylab = "Y",
         col = "#4CAF50",  # Green points
         pch = 16          # Solid circles
    )
  })
  
  # Render the scatter plot with linear regression line
  output$lmPlot <- renderPlot({
    req(dataInput(), lm_model())
    
    # Scatter plot of original data
    plot(dataInput()$x, dataInput()$y,
         main = "Linear Model Overlay",
         xlab = "X", ylab = "Y",
         col = "#4CAF50",  # Green points
         pch = 16          # Solid circles
    )
    
    # Add linear regression line
    abline(lm_model(), col = "#FF5733", lwd = 2)  # Orange line
  })
  
  # Render model summary text with slope, intercept, correlation
  output$model_summary <- renderPrint({
    req(lm_model())
    
    # Extract coefficients
    coef_summary <- coef(lm_model())
    
    # Return formatted summary
    cat("Linear Model Summary:\n")
    cat(paste("Slope (Coefficient for x):", round(coef_summary[2], 3), "\n"))
    cat(paste("Intercept:", round(coef_summary[1], 3), "\n"))
    cat(paste("Correlation (R-squared):", round(summary(lm_model())$r.squared, 3), "\n"))
  })
  
  # Render table of data
  output$contents <- renderTable({
    req(dataInput())
    
    if (input$disp == "head") {
      return(head(dataInput()))
    } else {
      return(dataInput())
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
