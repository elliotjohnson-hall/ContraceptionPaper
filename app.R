library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(vroom)
library(here)

Categories <- c("NumberOfPaidItems", "PaidQuantity", "BNFItemCode", "BNFItemDescription", "PaidDateMonth", "GrossIngredientCost", "ClassOfPreparationCode")

# if (file.exists(here("Data", "Data.RDS")) == TRUE) {
#   CompleteDataset <- read_rds(here("Data", "Data.RDS"))
# } else {
#   source(here("Scripts", "CKAN.R"))
# }
CompleteDataset <- read_rds(here("Data", "Data.RDS"))


ui <- fluidPage(theme = shinytheme("cyborg"),
  sidebarPanel(
    selectizeInput(
      "select_hb",
      "HB",
      choices = (unique(CompleteDataset$HBName)),
      multiple = FALSE
    ),
    uiOutput("select_gp"),
    selectInput(
      inputId = "x_attribute",
      label   = "Select attribute for x axis",
      choices = Categories
    ),
    selectInput(
      inputId  = "y_attribute",
      label    = "Select attribute for y axis",
      choices  = Categories,
      selected = Categories[2]
    )),
  mainPanel(plotOutput("ggplotPlot"))
)

server <- function(input, output, session) {
  # render the child dropdown menu
  output$select_gp <- renderUI({
    dat <- CompleteDataset %>%
      filter(HBName %in% input$select_hb)

    # get available carb values
    gp <- sort(unique(dat$GPPracticeName))

    # render selectizeInput
    selectizeInput("select_gp",
                   "GP",
                   choices = c(gp),
                   multiple = FALSE)
  })

  output$ggplotPlot <- renderPlot({
    CompleteDataset %>%
      filter(HBName == input$select_hb,
             GPPracticeName %in% input$select_gp) %>%
      ggplot(aes(x = .data[[input$x_attribute]],
                 y = .data[[input$y_attribute]])) +
      geom_point() +
      theme_minimal()
  })
}

shinyApp(ui, server)
