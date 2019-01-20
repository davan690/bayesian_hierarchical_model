library(shiny)

shinyUI(fluidPage(

    titlePanel("Bayesian Hierarchical Modelling"),

    sidebarLayout(
        sidebarPanel(
            actionButton("flip_coins", "Flip Coins"),
            verbatimTextOutput("total_prop"),
            plotOutput("coin_props")
        ),

        mainPanel()
    )
))
