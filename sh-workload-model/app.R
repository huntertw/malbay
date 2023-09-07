#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidytext)
library(ggh4x)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "spacelab"),
  titlePanel("Workforce View"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = list.files('../data/')),
      selectInput("plot", label = "Select plot", 
                  choices = c("Position.Specialty", 
                              "Position.Specialty & Current.Position",
                              "Operational.Function",
                              "Operational.Function & Current.Position")),
      sliderInput('pltheight', "Plot height", value = 15, min = 5, max = 20)
      # dataTableOutput("table")
    ),
    
    mainPanel(
      dataTableOutput("table"),
      plotOutput("plot")
     
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Prepare data
  
  main_dat <- readxl::read_xlsx('../data/sample-workload-model-060123.xlsx') %>% 
    setNames(make.names(names(.))) %>% 
    mutate(Total.Hours = round(Total.Mins.Max / 60, 2),
           period = "week",
           Current.Position = stringr::str_replace(Current.Position, "Director", "Dir"),
           Current.Position = stringr::str_replace(Current.Position, "and", "&"),
           Current.Position = stringr::str_replace(Current.Position, "Technology", "Tech"))
  
  my_pallette <- pals::kelly()[-1]
  color_dat <- unique(c(main_dat$Position.Specialty, main_dat$Operational.Function))
  my_colors = my_pallette[1:length(color_dat)]
  names(my_colors) <- color_dat
  # xlab <- unique(main_dat$period)
  
  # Reactive bits
  ht <- reactive(input$pltheight * 100)
  # gps <- reactive(input$plot)
  plot_to_display <- reactive({
    grps <- strsplit(input$plot, " & ")[[1]]
    create_data_and_plot(main_dat, groups = grps)
  })
  
  
  # renderPlot takes a function, ht, as argument to height
  output$plot <- renderPlot(plot_to_display(), height = ht, res = 96)
  # Create a reactive expression
  output$table <- renderDataTable(plot_to_display()$data %>% select(-reTask),
                                  options = list(pageLength = 5))
}

# Run the application 
shinyApp(ui = ui, server = server)

# NEXT STEP:
# Pass in vars to both funcs



