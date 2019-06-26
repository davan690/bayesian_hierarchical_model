library(shiny)
library(tidyverse)
library(ggmcmc)
library(rjags)

post_dist_fun <- function(x, t_p) {
    ggplot(x, aes(value, fill = fct_inorder(parameter_fill))) +
        geom_histogram(bins = 100, colour = "white", alpha = 0.8) +
        geom_vline(xintercept = t_p, size = 0.8) +
        facet_grid(Parameter ~ .) +
        scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Dark2")[c(5:6, 4)]) +
        scale_x_continuous(limits = c(0, 1),
                           breaks = seq(0, 1, by = 0.1)) +
        labs(title = "Posterior Distributions",
             x = "Value",
             y = "Count") +
        guides(fill = F) +
        theme(plot.title = element_text(size = 32),
              strip.text.y = element_text(angle = 0))
}

N <- rep(10, 10) # number of flips by coin
n_c <- 8 # number of coins

chain_length <- 100000 # chain length
n_chains <- 3 # number of chains
n_adapt <- 1000 # adaptation period
burn_in <- ceiling(0.005 * chain_length) # burn in

# high dependency & vague hyperprior model
model_hd_vh <- "
model {
  for (c in 1:n_c) {
    z[c] ~ dbin(theta[c], N[c])
    theta[c] ~ dbeta(omega * (50 - 2) + 1, (1 - omega) * (50 - 2) + 1)
  }
  omega ~ dbeta(1, 1)
}
"

model_ld_vh <- "
model {
  for (c in 1:n_c) {
    z[c] ~ dbin(theta[c], N[c])
    theta[c] ~ dbeta(omega * (5 - 2) + 1, (1 - omega) * (5 - 2) + 1)
  }
  omega ~ dbeta(1, 1)
}
"

model_hd_ih <- "
model {
  for (c in 1:n_c) {
    z[c] ~ dbin(theta[c], N[c])
    theta[c] ~ dbeta(omega * (50 - 2) + 1, (1 - omega) * (50 - 2) + 1)
  }
  omega ~ dbeta(20, 20)
}
"

model_ld_ih <- "
model {
  for (c in 1:n_c) {
    z[c] ~ dbin(theta[c], N[c])
    theta[c] ~ dbeta(omega * (5 - 2) + 1, (1 - omega) * (5 - 2) + 1)
  }
  omega ~ dbeta(20, 20)
}
"

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
    
    jags_data <- eventReactive(input$run_jags, {
        z <- coins_df() %>%  
            group_by(coin) %>% 
            summarise(z = (value == "H") %>% sum()) %>% 
            pull(z)
        
        data_list <- list(
            N = N,
            z = z,
            n_c = n_c
        )
        
        if (input$model_type == "High Dependency & Vague Hyperprior") {
          writeLines(model_hd_vh, "jagsmodel.txt")
        } else if (input$model_type == "Low Dependency & Vague Hyperprior") {
          writeLines(model_ld_vh, "jagsmodel.txt")
        } else if (input$model_type == "High Dependency & Informative Hyperprior") {
          writeLines(model_hd_ih, "jagsmodel.txt")
        } else {
          writeLines(model_ld_ih, "jagsmodel.txt")
        }
        
        # run model
        jags_model <- jags.model(
            file = "jagsmodel.txt",
            data = data_list,
            n.chains = n_chains,
            n.adapt = n_adapt
        )
        
        # update for burn in
        update(jags_model, n.iter = burn_in)

        # posterior paramaters
        thetas <- map_chr(1:8, ~ paste0("theta[", ., "]"))

        parameters <- c("omega", thetas)

        # posterior samples
        coda_samples <- coda.samples(
            jags_model,
            variable.names = parameters,
            n.iter = chain_length
        )

        post_samples <- coda_samples %>%
            ggs() %>%
            mutate(parameter_fill = c(rep("omega", 300000), rep("theta", 2400000)) %>% factor())

        post_samples
    })
    
    output$total_prop <- renderText({
        temp_total_prop <- coins_df() %>% 
            filter(value == "H") %>% 
            summarise(prop = n() / 80) %>% 
            pull() %>% 
            round(2)
        
        paste("Total proportion of Heads:", temp_total_prop)
    })
    
    output$coin_props_plot <- renderPlot({
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
    
    output$jags_plot <- renderPlot({
      
        temp_total_prop <- coins_df() %>% 
            filter(value == "H") %>% 
            summarise(prop = n() / 80) %>% 
            pull(prop)
        
        post_dist_fun(jags_data(), temp_total_prop)
    })
    
    url <- a("Bayesian Hierarchical Modelling", href = "https://turneralex.github.io/hierarchical_model.html")
      
      output$tab <- renderUI({
        tagList("More information on this type of modelling
                 can be found here:", url)
    })
    
})
