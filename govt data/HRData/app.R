
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Loading in the data
hr_data <- read.csv("HR_comma_sep.csv", stringsAsFactors = TRUE)
sales_names <- sort(unique(hr_data$sales))
salary_levels <- sort(unique(hr_data$salary))                    

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Human Resources Analytics"),
   tags$div(class="header", checked=NA,
            tags$p("Use this dashboard to analyze HR data to find trends amongst employees. 
                   You can sort by Department, Salary Level, Whether or not they were injured at work, 
                   or were promoted in the past 5 years.")
   ),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        selectInput("salesInput", "Choose Department", 
                    sales_names,
                    selected = sales_names[1]),
        br(),
        selectInput("salaryInput", "Choose Salary Level", 
                    salary_levels,
                    selected = salary_levels[1]),
        br(),
        checkboxGroupInput("accidentInput", "Accident in the Workplace?",
                     choices = list("Yes" = 1, "No" = 0),
                     selected = 1),
        br(),
        checkboxGroupInput("promoInput", "Promotion in the past 5 years?",
                           choices = list("Yes" = 1, "No" = 0),
                           selected = 1),
        plotOutput("leftplot")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("satisf_plot"),
        br(), br(),
        plotOutput("average_hours_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  output$satisf_plot <- renderPlot({
    sat_filter <- 
        hr_data %>%
        filter(sales == input$salesInput,
               salary == input$salaryInput,
               Work_accident == input$accidentInput,
               promotion_last_5years == input$promoInput
        )
    ggplot(sat_filter, aes(satisfaction_level)) + 
      geom_histogram(aes(y = ..density..)) + theme_fivethirtyeight() + 
      geom_density(fill="blue", alpha = 0.2) + 
      labs(title = "Employee Satisfaction")
  })
  output$average_hours_plot <- renderPlot({
    input_filter <- 
      hr_data %>%
      filter(sales == input$salesInput,
             salary == input$salaryInput,
             Work_accident == input$accidentInput, 
             promotion_last_5years == input$promoInput
             )
    ggplot(input_filter, aes(average_montly_hours)) + 
      geom_histogram(aes(y = ..density..), binwidth = 10) + theme_fivethirtyeight() + 
      geom_density(fill="blue", alpha = 0.2) + 
      labs(title = "Employee Average Monthly Hours")
  })
  
  output$leftplot <- renderPlot({
    left_filter <- 
      hr_data %>%
      filter(sales == input$salesInput,
             salary == input$salaryInput,
             Work_accident == input$accidentInput,
             promotion_last_5years == input$promoInput
      )
    ggplot(left_filter, aes(x=factor(1), fill=factor(left)))+
      geom_bar(width = 1) + labs(title = "Did the employee \nleave the company?", size = 16) +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_discrete(name  ="", labels=c("No", "Yes")) +
      theme(title=element_text(size = 16, face = "bold"), legend.text=element_text(size=14))
  }, bg="transparent")
  }


# Run the application 
shinyApp(ui = ui, server = server)

