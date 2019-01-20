library(shiny)
library(tidyverse)
# library(ggmcmc)
# library(rjags)

chain_length <- 100000 # chain length
n_chains <- 3 # number of chains
n_adapt <- 1000 # adaptation period
burn_in <- ceiling(0.005 * chain_length) # burn in

shinyServer(function(input, output) {

    coins_df <- eventReactive(input$flip_coins, {
        flip_coins <- function() replicate(10, runif(1, 0, 1))
        
        temp_df <- replicate(8, flip_coins()) %>% 
            as_tibble() 
        
        temp_df <- temp_df %>% 
            mutate_all(~ if_else(. > 0.5, "H", "T")) %>% 
            gather(coin, value)
        
        temp_df$coin <- temp_df$coin %>% 
            factor(labels = map_chr(1:8, ~ paste("Coin", .)))
        
        temp_df
    })
    
    output$total_prop <- renderText({
        temp_total_prop <- coins_df() %>% 
            filter(value == "H") %>% 
            summarise(prop = n() / 80) %>% 
            pull() %>% 
            round(2)
        
        paste("Total proportion of Heads:", temp_total_prop)
    })
    
    output$coin_props <- renderPlot({
        temp_total_prop <- coins_df() %>% 
            filter(value == "H") %>% 
            summarise(prop = n() / 80) 
        
        coins_df() %>% 
            group_by(coin) %>% 
            filter(value == "H") %>% 
            summarise(prop = n() / 10) %>% 
            ggplot(aes(coin, prop)) +
            geom_col(fill = "darkolivegreen3") +
            geom_hline(data = temp_total_prop,
                       aes(yintercept = prop, linetype = ""),
                       size = 1) +
            scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
            scale_linetype_manual("Total Proportion of Heads", values = "solid") +
            labs(title = "Proportion of Heads by Coin",
                 x = "Coin",
                 y = "Proportion",
                 fill = "Mint") +
            theme(legend.position = "bottom")
        
    })
    
    wait_text <- eventReactive(input$run_jags, {
        "Please wait for the MCMC process to be completed."
    })
    
    output$please_wait <- renderText({
        wait_text()
    })

})
