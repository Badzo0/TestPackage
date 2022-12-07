library(bslib)
library(shiny)
library(reactable)
library(tidyverse)
library(readxl)
library(likert)
library(plyr)
library(naniar)
library(shinycssloaders)
library(waiter)
library(shinyWidgets)
library(shinyalert)


  navbarPage("Indicator Analysis",id = "tabs",
             theme = bs_theme(bootswatch = "minty",  secondary = "#F4DD77"),
             tabPanel("Import Data",
                      titlePanel("Importing StudentSurvey.Ie Data File"),
                      fileInput("file1", "Choose Excel File",
                                multiple = T,
                                accept = c(".xlsx")),

                      useWaitress(color = "#F4DD77"),
                      pickerInput("inSelect", "Select input",
                                  c(""), options = list(`actions-box` = TRUE,liveSearch =T),multiple = T),
                      pickerInput("selectreport", "Select Report",
                                  c("Student_Survey_2021_Indicators","Student_Survey_2022","Course_report"),multiple = F),
                      pickerInput("seleccompare", "Select Comparison",
                                  c("All StudentSurvey.ie","StudentSurvey.ie Universities","StudentSurvey.ie Technological Higher Education Institutions","StudentSurvey.ie Other Institutions"),multiple = F),
                      numericInput("obs", "Number of N-grams (Qualitative):", 4, min = 1, max = 50),
                      downloadButton("report", "Download Report"),

                      # Horizontal line ----
                      tags$hr(),
                      reactableOutput("contents"),
                      tags$br(),
                      reactableOutput("contents2"),
                      tags$br(),tags$br(),tags$br(),
                      #tags$hr(),
                      plotOutput("radarplot", height = "600px" ),
                      tags$br(),
                      fluidRow(
                        column(2),
                        column(8,
                               reactableOutput("table1"),
                             # reactableOutput("table2"),  #111111111
                               column(2))),

                      tags$br(),
                      # plotOutput("plot12", width = "70%" ),
                      tags$br(),
                      tags$br(),

                      fluidRow(
                        column(2),
                        column(8,
                               plotOutput('plot12')),
                        column(2))



             )
)
