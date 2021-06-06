#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("filtR Demo"),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(
      12,
      selectInput("mode", "Select mode:", choices = c("Simulate Data", "Upload Data")),
      tags$hr(style = "border-top: 1px solid #000000;"),

      fluidRow(
        column(
          6,
          conditionalPanel(
            "input.mode == 'Simulate Data'",
            h3("Sample Size"),
            numericInput("size", "", 100, min = 1, max = 10000),
            h3("True Effect"),
            numericInput("effect", "", 0, min = 0, max = 100),
            br(),

            actionButton("run", "Run"),
          ),
        ),

        column(
          6,
          conditionalPanel(
            "input.mode == 'Simulate Data'",
            checkboxGroupInput("checkGroup",
              h3("Options"),
              choices = list(
                "Stratification" = 2,
                "Control" = 3
              ),
              selected = 0
            ),
            h3("Distribution"),
            selectInput("distribution", "", choices = c("normal", "gamma", "beta", "neg. binomial", "binary")),
          ),
        ),
      ),

      fluidRow(
        column(
          6,
          tags$hr(style = "border-top: 1px solid #000000;"),

          tabsetPanel(
            type = "tabs",
            tabPanel("Data", DT::dataTableOutput("sim_data")),
            tabPanel(
              "Plot",
              tabsetPanel(
                type = "tabs",
                tabPanel("RESvsFIT", plotOutput("sim_plot1")),
                tabPanel("QQ", plotOutput("sim_plot2")),
                tabPanel("SCA-LOC", plotOutput("sim_plot3")),
                tabPanel("CKS", plotOutput("sim_plot4")),
                tabPanel("RESvsLEV", plotOutput("sim_plot5"))
              )
            ),
            tabPanel("Summary", verbatimTextOutput("sim_summary"))
          )
        ),
        column(
          6,
          tags$hr(style = "border-top: 1px solid #000000;"),

          tabsetPanel(
            type = "tabs",
            tabPanel("Data", DT::dataTableOutput("fil_data")),
            tabPanel(
              "Plot",
              tabsetPanel(
                type = "tabs",
                tabPanel("PVAL", plotOutput("fil_plot1")),
                tabPanel("EFFECT", plotOutput("fil_plot2"))
              )
            ),
            tabPanel("Summary", verbatimTextOutput("fil_summary"))
          )
        ),
      ),
    )
  ),
))
