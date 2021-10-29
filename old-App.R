library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
rawPheno <- readxl::read_xlsx("data/analysis-23-10.xlsx",sheet = "Analysis")
checksCS <- readxl::read_xlsx("data/analysis-23-10.xlsx", sheet="Checks")
sinN <- read.table("data/sinonimos.txt", header = T, sep="\t")

# Criando uma coluna para ter nomes unicos para os ensaios
rawPheno$TrialYear <- paste0(rawPheno$TrialID,rawPheno$Year)

myDF <- data.frame(as.character(rawPheno$barcode),as.character(rawPheno$TPP),
                   as.character(rawPheno$TrialYear),as.character(rawPheno$genotipo),
                   as.character(rawPheno$Year),as.numeric(rawPheno$Rep),
                   as.numeric(rawPheno$Nota),as.numeric(rawPheno$yield), 
                   as.numeric(rawPheno$Stg),as.character(rawPheno$Year))

colnames(myDF)<- c("barcode","TPP","local","genotipo","Ano","Rep","Nota","yield","Stage","Year")
# Padronizando nomes
# Removendo espaços
myDF$genotipo <- gsub(" ", "", myDF$genotipo, fixed = TRUE)
myDF$local <- gsub(" ", "", myDF$local, fixed = TRUE)

# Padronizando para letras maiusculas
myDF$genotipo <- toupper(myDF$genotipo)
myDF$local <- toupper(myDF$local)

# Codigos auxiliares
source("code/parseCS.R")
source("code/acuraciaCS.R")
source("code/selectionIndex.R")
source("code/graficos.R")

# Definindo parametros

# duasT <- "yes" # Analisar yield e CS (yes = analisa os dois. or no=analisa apenas CS) 
Stage = 6 # Filtrando para stage
# pesos <- c(Yield=60, CS=40) # Pesos para o index. Essa ordem é importante
rmYC <- "no" # remover os plots que não possuem dados de yield (yes or no)

# qControl <- 0.60 # Controle de qualidade para alta pressão
limiteAlta <- 3 # Significancia do desvio para ser alta P
limiteBaixa <- (-2) # Significancia do desvio para ser baixa P
years<-unique(myDF$Year) # Anos Disponíveis
filterYear <- "yes" # Capturar dados de 2 anos?
minLoc <- 4 # Numero minimo de locais sem penalizacao na acuracia

myDFF <- filter_locais(myDF, Stage, years, filterYear, sinN) # remove locais sem dados de CS

ui <- 
  navbarPage("CSAR", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             tabPanel("Background",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("FlowChart",
                                   h5("Flowchart Rationale"),
                                   tags$img(src="TAlogo1.JPG", height = 450, width=250)),
                          tabPanel("Math")
                        ))),
             tabPanel("Quality Control",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Settings",br(),
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
                                               selected = "6"),
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
                                   mainPanel("this is a test")),
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
                                   radioButtons(inputId = "question1", 
                                                label = "Choose a question to explore",
                                                choices = c("Email/chat with library staff", 
                                                            "Find books", 
                                                            "Search for articles")))
                        ))),
             tabPanel("Analysis & Indice",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Study Habit"),
                          tabPanel("Space Preference - Mid & Final Terms"),
                          tabPanel("Space Preference - Most Days"),
                          tabPanel("Space Preference - Student Submissions")
                        ))), 
             tabPanel("Rank"),
             tabPanel("About",
                      p("CSAR - Corn Stunt Analysis Rating"))
  )

server <- function(input, output) {
  
  output$notaBP <- renderPlot({
    makeG0(myDFF,"Nota",input$tpp)
  })
  output$notaHist <- renderPlot({
    makeH0(myDFF,"Nota",input$tpp)
  })
  output$yieldBP <- renderPlot({
    makeG0(myDFF,"yield",input$tpp)
  })
  output$yieldHist <- renderPlot({
    makeH0(myDFF,"yield",input$tpp)
  })
  
}

shinyApp(ui = ui, server = server)

