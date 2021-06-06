#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(simstudy)
library(DT)
library(broom)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  observeEvent(input$run, {
    def <- defData(varname = "male", dist = "binary", formula = .5, id = "cid")
    def <- defData(def, varname = "age", dist = "uniformInt", formula = "18;100")

    dt <- genData(input$size, def)

    if (2 %in% input$checkGroup) {
      study <- trtAssign(dt,
        n = 2, balanced = TRUE, strata = c("male"),
        grpName = "treat"
      )
    } else {
      study <- trtAssign(dt, n = 2, balanced = TRUE, grpName = "treat")
    }

    formula <- paste("treat *", input$effect)

    if ("binary" %in% input$distribution) {
      outcome <- defDataAdd(
        varname = "outcome", dist = "binary", formula = formula,
        link = "logit"
      )
    } else {
      outcome <- defDataAdd(
        varname = "outcome", dist = input$distribution, formula = formula,
        variance = 1
      )
    }

    study <- addColumns(outcome, study)

    output$sim_data <- DT::renderDataTable(
      datatable(study) %>% formatRound("outcome")
    )

    if (3 %in% input$checkGroup) {
      model <- as.formula("outcome ~ treat + male + age")
    } else {
      model <- as.formula("outcome ~ treat")
    }

    fit <- lm(model, data = study)

    output$sim_plot1 <- renderPlot({
      plot(fit, which = 1)
    })

    output$sim_plot2 <- renderPlot({
      plot(fit, which = 2)
    })

    output$sim_plot3 <- renderPlot({
      plot(fit, which = 3)
    })

    output$sim_plot4 <- renderPlot({
      plot(fit, which = 4)
    })

    output$sim_plot5 <- renderPlot({
      plot(fit, which = 5)
    })

    output$sim_summary <- renderPrint({
      tidy(fit)
    })
  })
})
