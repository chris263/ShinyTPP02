# This is the interface CSAR
# Corn Stunt Analysis RAting
# Chris Simoes
# chris.simoes@syngenta.com

library(shiny)
library(DT)
library(shinydashboard)

# Codigos auxiliares
source("code/parseCS.R")
source("code/acuraciaCS.R")
source("code/selectionIndex.R")
source("code/graficos.R")

# Loading File
prepro() #parseCR
curation() #parseCR
getLoc() #parseCR
finalTabela2 <- preBLUP() #acuraciaCS


shinyUI(
  dashboardPage(
  dashboardHeader(title = "CSAR", titleWidth = 230),
  dashboardSidebar(
    sidebarMenu(
      selectInput("tpp",
                  label = "Select TPP",
                  choices = c("1-2","4-5","9",
                              "10","11","12","Brazil"),
                  selected = "Brazil"),
      selectInput("duasT",
                  label = "Joint Analysis Yield and CS?",
                  choices = c("Yes", "No"),
                  selected = "Yes"),
      selectInput("Stage",
                  label = "Select Stage",
                  choices= c("4","5","6.1","6.2","8"),
                  selected = "6.1"),
      selectInput("pesos",
                  label = "Select Yield Weight",
                  choices = c("10","20","30","40","50",
                              "60","70","80","90"),
                  selected="60"),
      selectInput("qControl",
                  label = "Treshold QC",
                  choices = c("0.30","0.40","0.50","0.60",
                              "0.70","0.80","0.90"),
                  selected = "0.60"),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green"),
      tags$head(tags$script(src = "message-handler.js")),
      actionButton("do", "Run Analysis")
      )
    ),
  dashboardBody(
    fluidPage(
      tabsetPanel(
        tabPanel("Flowchart",
                 mainPanel(
                   h3("Corn Stunt Analysis Rating"),
                   imageOutput("flowChart")
                 )),
        tabPanel("Score QC",
                 mainPanel(
                   h3("Score distribution per location:"),
                   plotOutput("notaBP"),
                   br(),
                   h3("Score Histogram:"), 
                   plotOutput("notaHist")
                 )),
        tabPanel("Yield QC",
                 mainPanel(
                   h3("Yield distribution per location:"),
                   plotOutput("yieldBP"),
                   br(),
                   h3("Yield Histogram:"),
                   plotOutput("yieldHist")
                 )),
        tabPanel("Class Locations",
                 mainPanel(
                   h3("Classification table:"),
                   DTOutput('tbl')
                 ))
        )
      )
  )
  )
)


