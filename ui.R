# ui.R
library(shiny)
library(ggplot2)
library(reshape2)


shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("UseMarketPath",label = "Use Market Path?",value = F),
      dateInput("FirstRateRise",value = as.Date("2017-01-31"),label = "First Rate Rise: "),
      numericInput("PaceOfRateRises",label = "Pace Of Rate Rises: ",value = 1.),
      numericInput("TargetRate",label = "Target Rate: ",value = 2.),
      dateInput("FirstSale",value = as.Date("2019-08-31"),label = "First Sale: "),
      numericInput("PaceOfSales",label = "Pace Of Sales: ",value = 15.),
      numericInput("YieldImpact",label = "Yield Impact: ",value = 5.)
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4")
    )
  )
))


