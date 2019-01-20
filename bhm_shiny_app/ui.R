library(shiny)

shinyUI(fluidPage(

    titlePanel("Bayesian Hierarchical Modelling"),

    sidebarLayout(
        sidebarPanel(
            actionButton("flip_coins", "Flip Coins"),
            actionButton("run_jags", "Run JAGS"),
            verbatimTextOutput("please_wait"),
            verbatimTextOutput("total_prop"),
            plotOutput("coin_props")
        ),

        mainPanel()
    )
))
