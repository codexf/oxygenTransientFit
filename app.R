# To run the app, you will need to have R and the following packages installed:
# install.packages(c("shiny", "tidyverse", "shinythemes", "DT", "nls.multstart"))

library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(nls.multstart)
source("fit_exponential_decay_one_phase.R")
source("fit_exponential_decay_two_phase.R")

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "oxygenTransientFit",
    tabPanel("Upload Data",
             icon = icon("table"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 # Define file input
                 fileInput(
                   inputId = "file",
                   label = "Choose a CSV file",
                   accept = c("csv/text", ".csv", "text/comma-separated-values"),
                   multiple = FALSE,
                   buttonLabel = "Browse",
                   placeholder = "No file selected"
                 ),
                 p("Note: the CSV file being uploaded must contain a header row"),
                 br(),
                 # Define checkbox for using default data
                 checkboxInput("load_demo", strong("Load Demo Data"), value = FALSE),
                 br(),
                 hr(),
                 # Define select input for x variable
                 selectInput("x_var", "Select column containing time in seconds", NULL),
                 br(),
                 # Define select input for y variable
                 selectInput("y_var", "Select column containing net assimilation rate", NULL),
                 br(),
                 p("If the selected data looks good, proceed to the next tab 
                   to fit exponential models to the uploaded data.")
               ),
               
               mainPanel(
                 fluidRow(
                   splitLayout(
                     cellWidths = c("25%", "75%"),
                     # Define table output for selected data
                     tableOutput("selected_data"),
                     # Define plot output
                     plotOutput("plot_original")
                   )
                 )
               )
             )
    ),
    
    tabPanel(
      "Fitting Exponential Models to Data",
      icon = icon("wrench"),
      fluidRow(
        column(
          width = 3,
          wellPanel(
          h4("Select Data Range"),
          p("Enter the starting value and ending value of the x-axis range for the model fitting process"),
          # Define numeric input for x-axis ranges
          numericInput("range_min", "Start", value = 310),
          numericInput("range_max", "End", value = 600),
          hr(),
          br(),
          p("Subset the selected range of data and reset the x-axis to start from zero
            for fitting the exponential decay models"),
          # Define reset button
          actionButton("reset_button", "Subset and Reset X-Axis to Zero")),
          br()
        ),
        column(
          width = 9,
          h4("Plot for Original Data"),
          # Define plot output
          plotOutput("plot_with_range")
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
          h4("Choose Fitting Models"),
          p("Fit one-phase exponential decay model"),
          # Define fit button for one-phase exponential decay
          actionButton("fit_button_one_phase", "Fit One-Phase"),
          br(),
          br(),
          p("Fit two-phase exponential decay model"),
          # Define fit button for two-phase exponential decay
          actionButton("fit_button_two_phase", "Fit Two-Phase"),
          br(),
          hr(),
          br(),
          p("If the fitting is successful, the plot is updated with the fitting 
            curve and equation, and a table of fit parameters and Sum of Squared 
            Residuals (SSR) is generated. "),
          br(),
          p("If the fitting fails, try selecting a different range of the data 
            and click the Subset and Reset X-Axis to Zero button before 
            clicking the appropriate fit button again. "))
        ),
        column(
          width = 6,
          h4("Plot for Selected Data"),
          # Define plot output for selected data with reset x-axis
          plotOutput("plot_selected_reset"),
        ),
        column(
          width = 3,
          h4("Result Table"),
          # Define table output for exponential decay fit parameters
          dataTableOutput("fit_output")
        )
      )
    ),
  tabPanel(
    "About",
    icon = icon("info"),
    h4("This app is designed to help researchers to analyze leaf photosynthetic 
      responses transitioning from low to high oxygen. The app allows users 
      to upload a CSV file of their data and fit exponential decay models to 
      the net assimilation rate over time. Users can choose to fit either 
      a one-phase or a two-phase exponential decay model to their data."),
    br(),
    h4("The app includes two tabs:"),
    h4(strong("1. Upload Data"), ": This tab allows users to upload their data and 
      select the columns containing the time and net assimilation rate data.
      Users can also view a table of the selected data and a plot of the original data. 
       A demo data is provided for users to explore fitting models to data without 
       uploading their own data. Users can click on the Load Demo Data checkbox to load the demo data. 
       The demo data contains two columns, one for time in seconds and another for net assimilation rate. 
       Users can upload their own data at any time by unchecking the Load Demo Data checkbox a
       nd uploading their CSV file."),
    h4(strong("2. Fitting Exponential Models to Data"), ": This tab allows users to fit 
      one-phase or two-phase exponential decay models to their data. Users 
      can select a range of the x-axis for fitting and reset the x-axis to a 
      new range. The app generates a plot of the selected data with the fitted 
      curve and a table of the fit parameters and the sum of squared residuals (SSR).")
  )
  )




# Define server function
server <- function(input, output, session) {
  # Reactive variable to track uploaded data
  data <- reactive({
    if (input$load_demo) {
      # Default data if checkbox load_demo is checked
      data <- read.csv("demo.csv")
    } else if (!is.null(input$file)) {
      # Read uploaded file if one is provided
      data <- read.csv(input$file$datapath)
    } else {
      # No data if no file is uploaded and checkbox is unchecked
      data <- NULL
    }
    data
  })

  # Update select inputs when data is loaded
  observe({
    updateSelectInput(
      inputId = "x_var",
      #label = "Select X Variable",
      choices = names(data())
    )
    
    updateSelectInput(
      inputId = "y_var",
      #label = "Select Y Variable",
      choices = names(data())
    )
  })
  
  
  # Define reactive expression for selected data
  selected_data <- reactive({
    req(input$x_var, input$y_var) # Require x and y variable selections
    # Select x and y variables from data
    data() %>%
      select(!!input$x_var,!!input$y_var)
  })
  
  
 # Define table output for selected data
  output$selected_data <- renderTable({
    # Check if data has been loaded
    if (!is.null(selected_data())) {
      data.frame(selected_data())
    }
  })
  
  # Define plot output for original data
  output$plot_original <- renderPlot({
    # Check if x and y variables have been selected
    if (!is.null(input$x_var) && !is.null(input$y_var)) {
      # Create plot of selected data
      ggplot(selected_data(), aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
        geom_point() +
        theme_bw(base_size=20)
    }
  })
  
  # Define plot output with x-axis range highlighted
  output$plot_with_range <- renderPlot({
    # Check if x and y variables have been selected
    if (!is.null(input$x_var) && !is.null(input$y_var)) {
      # Create plot of selected data
      ggplot(selected_data(), aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
        geom_rect(
          xmin = input$range_min,
          xmax = input$range_max,
          ymin = -Inf,
          ymax = Inf,
          fill = "deepskyblue",
          alpha = 0.1
        ) +
        geom_point() +
        theme_bw(base_size=20)
    }
  })
  
  # Define reactive expression for selected data with reset x-axis
  offset_data_reset <- eventReactive(input$reset_button, {
    # Calculate offset x values with second slider range set to 0
    x_offset <- selected_data() %>%
      pull(!!sym(input$x_var)) - input$range_min
    
    # Combine offset x values with original y values
    data.frame(x = x_offset,
               y = selected_data() %>%
                 pull(!!sym(input$y_var))) %>%
      # Subset offset data based on the original x-axis range
      filter(0 < x & x <= input$range_max - input$range_min)
  })
  
  # Define reactive expression for fit results of one-phase exponential decay
  fit_results_one_phase <-
    eventReactive(input$fit_button_one_phase, {
      # Fit one-phase exponential decay function to selected data with reset x-axis
      fit_exponential_decay_one_phase(offset_data_reset())
    })
  
  # Define reactive expression for fit results of two-phase exponential decay
  fit_results_two_phase <-
    eventReactive(input$fit_button_two_phase, {
      # Fit two-phase exponential decay function to selected data with reset x-axis
      fit_exponential_decay_two_phase(offset_data_reset())
    })
  
  
  # Define observer to update table output for one-phase exponential decay
  observeEvent(input$fit_button_one_phase, {
    # Check if fitting failed
    # When the app is launched, an empty table is displayed since no initial values exist
    if (is.null(fit_results_one_phase()$model)) {
      # Create a table with error message if fitting failed
      output$fit_output <- renderDataTable({
        # The character() function is used to create an empty character vector for each column.
        data.frame(term = character(), value = character())
      },
      caption = "Fitting Failed",
      options = list(paging = FALSE, searching = FALSE, info = FALSE))
    } else {
      output$fit_output <- renderDataTable({
        # Print table of fit parameters and SSR for one-phase exponential decay
        fit_results_one_phase()$table_data
      }, caption = "Fit Parameters and SSR",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, digits = 3),
      rownames= FALSE)
    }
  })
  
  # Define observer to update table output for two-phase exponential decay
  observeEvent(input$fit_button_two_phase, {
    # Check if fitting failed
    if (is.null(fit_results_two_phase()$model)) {
      # Create a table with error message if fitting failed
      output$fit_output <- renderDataTable({
        data.frame(term = character(), value = character())
      },
      caption = "Fitting Failed",
      options = list(paging = FALSE, searching = FALSE, info = FALSE))
    } else {
      output$fit_output <- renderDataTable({
        # Print table of fit parameters and SSR for two-phase exponential decay
        fit_results_two_phase()$table_data
      }, caption = "Fit Parameters and SSR",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, digits = 3),
      rownames= FALSE)
    }
  })
  
  # Observer to update plot_selected_reset and table output
  observeEvent(input$reset_button, {
    # Reset table output
    output$fit_output <- renderDataTable({
      data.frame(term = character(), value = character())
    },
    caption = "No data available",
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))
    
    output$plot_selected_reset <- renderPlot({
      # Create plot of selected data with reset x-axis
      ggplot(offset_data_reset(), aes(x = x, y = y)) +
        geom_point()+
        theme_bw(base_size=20)
    })
  })
  
  # Observer to update plot_selected_reset for one-phase exponential decay
  observeEvent(fit_results_one_phase(), {
    # Check if fitting failed
    if (is.null(fit_results_one_phase()$model)) {
      # Create a plot with error message if fitting failed
      output$plot_selected_reset <- renderPlot({
        ggplot(offset_data_reset(), aes(x = x, y = y)) +
          geom_point() +
          ggtitle("Fitting failed") +
          theme_bw(base_size=20)
      })
    } else {
      output$plot_selected_reset <- renderPlot({
        # Create plot of selected data with reset x-axis and fitted curve for one-phase exponential decay
        ggplot(offset_data_reset(), aes(x = x, y = y)) +
          geom_point() +
          geom_line(data = data.frame(
            x = offset_data_reset()$x,
            y = predict(fit_results_one_phase()$model)
          ),
          color = "deepskyblue", size = 1.5) +
          labs(title = fit_results_one_phase()$equation,
               x = input$x_var, 
               y = input$y_var) +
          theme_bw(base_size=20) +
          theme(plot.title = element_text(size=14))
      })
    }
  })
  
  # Observer to update plot_selected_reset for two-phase exponential decay
  observeEvent(fit_results_two_phase(), {
    # Check if fitting failed
    if (is.null(fit_results_two_phase()$model)) {
      # Create a plot with error message if fitting failed
      output$plot_selected_reset <- renderPlot({
        ggplot(offset_data_reset(), aes(x = x, y = y)) +
          geom_point() +
          ggtitle("Fitting failed") +
          theme_bw(base_size=20)
      })
    } else {
      output$plot_selected_reset <- renderPlot({
        # Create plot of selected data with reset x-axis and fitted curve for two-phase exponential decay
        ggplot(offset_data_reset(), aes(x = x, y = y)) +
          geom_point() +
          geom_line(data = data.frame(
            x = offset_data_reset()$x,
            y = predict(fit_results_two_phase()$model)
          ),
          color = "deepskyblue", size = 1.5) +
          labs(title = fit_results_two_phase()$equation,
               x = input$x_var, 
               y = input$y_var)  +
          theme_bw(base_size=20) +
          theme(plot.title = element_text(size=14))
      })
    }
  })
  
  
}

shinyApp(ui = ui, server = server)