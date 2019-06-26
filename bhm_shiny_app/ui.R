library(shiny)

shinyUI(fluidPage(

    titlePanel("Bayesian Hierarchical Modelling"),

    sidebarLayout(
        sidebarPanel(
            h5("Observe the effects of different levels of dependency & strength of certainity in the hyperprior
               by choosing different parameter settings for the same data."),
            selectInput("model_type",
                        "Choose the model parameters:",
                        choices = c("High Dependency & Vague Hyperprior",
                                    "Low Dependency & Vague Hyperprior",
                                    "High Dependency & Informative Hyperprior",
                                    "Low Dependency & Informative Hyperprior")),
            h5("Generate the data by flipping 8 fair coins:"),
            h6("More extreme individual outcomes will yield more interesting modelling results."),
            actionButton("flip_coins", "Flip Coins"),
            h5("Use JAGS to run the MCMC process:"),
            h6("This will take ~20 seconds to complete."),
            actionButton("run_jags", "Run JAGS"),
            verbatimTextOutput("total_prop"),
            plotOutput("coin_props_plot")
        ),

        mainPanel(
            plotOutput("jags_plot", height = 800)
        )
    )
))
