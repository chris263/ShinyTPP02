# This is the interface CSAR
# Corn Stunt Analysis RAting
# Chris Simoes
# chris.simoes@syngenta.com

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "CSAR", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      "Settings",
      selectInput("tpp",
                  label = "Select TPP",
                  choices = c("1-2","4-5","9",
                              "10","11","12","Brazil"),
                  selected = "Brazil")
    )
  ),
  dashboardBody()
)
server <- function(input, output) {}

shinyApp(ui = ui, server = server)
