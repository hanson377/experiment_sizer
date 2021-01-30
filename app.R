## in this file, I generate a shiny app that allows users to visualize two proportions with a beta distribution
## for simplicity purposes, we assume a flat prior (jeffreys prior of beta(1/2,1/2))
## additionally, the distribution of the % difference between the two proportions from 1m simulations is visualized

library(shiny)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)


# Define UI ----
ui <- fluidPage(
  titlePanel("Bayesian Comparision of Proportions"),
  sidebarLayout(
    sidebarPanel(


      h2('Statistical Input'),
      numericInput("control_rate", "Baseline Rate", min = 0, max = 1, value = 0.2),
      numericInput("delta_from_control", "Expected Relative % Delta", min = 0, max = 1, value = 0.05),
      numericInput("credible_interval", "Credible Interval:", .99, min = .8, max = .99),

      h2('Sample Size Input'),
      numericInput("n_daily_volume", "Daily Volume:", 1000, min = 10, max = 100000),
      numericInput("n_days", "Run-time (Days)", min = 0, max = 90, value = 14),
      numericInput("n_days_alt", "Alternative Run-Time (Days)", min = 0, max = 90, value = 28),
      numericInput("sample_split", "Split to Control:", min = 0, max = 1, value = 0.5),


      h2('Simulation Input'),
      numericInput("simulation_volume", "Simulation Volume:", 500000, min = 1, max = 10000000),
    ),

    mainPanel(
      h1('First Scenario'),
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("likelihood_distributions"),plotOutput("likelihood_differential")),

      h1('Alternative Scenario'),
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("likelihood_distributions_alt"),plotOutput("likelihood_differential_alt")),

      h1('Comparision of Results for Scenarios'),
      tableOutput("table_summary"),
      tableOutput("table_summary_alt")
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {


  likelihood <- reactive({

    ## calculate input from sliders
    simulation_volume = input$simulation_volume
    total_sample_size <- input$n_daily_volume*input$n_days

    control_n <- round(total_sample_size*input$sample_split,digits=0)
    variant_n <- total_sample_size-control_n

    control_rate <- input$control_rate
    variant_rate <- input$control_rate*(1+input$delta_from_control)

    control_alpha <- round(control_n*control_rate,digits=0)
    control_beta <- control_n-control_alpha

    variant_alpha <- round(variant_n*variant_rate,digits=0)
    variant_beta <- variant_n-variant_alpha

    ## generate probability distributions for control and variant
    sample_a <- data.frame(value=rbeta(simulation_volume,control_alpha,control_beta))
    sample_a$sample <- 'Control'

    sample_b <- data.frame(value=rbeta(simulation_volume,variant_alpha,variant_beta))
    sample_b$sample <- 'Variant'

    samples <- rbind(sample_a,sample_b)
    samples
  })

  likelihood_alt <- reactive({

    ## calculate input from sliders
    simulation_volume = input$simulation_volume
    total_sample_size <- input$n_daily_volume*input$n_days_alt

    control_n <- round(total_sample_size*input$sample_split,digits=0)
    variant_n <- total_sample_size-control_n

    control_rate <- input$control_rate
    variant_rate <- input$control_rate*(1+input$delta_from_control)

    control_alpha <- round(control_n*control_rate,digits=0)
    control_beta <- control_n-control_alpha

    variant_alpha <- round(variant_n*variant_rate,digits=0)
    variant_beta <- variant_n-variant_alpha

    ## generate probability distributions for control and variant
    sample_a <- data.frame(value=rbeta(simulation_volume,control_alpha,control_beta))
    sample_a$sample <- 'Control'

    sample_b <- data.frame(value=rbeta(simulation_volume,variant_alpha,variant_beta))
    sample_b$sample <- 'Variant'

    samples <- rbind(sample_a,sample_b)
    samples
  })

  likelihood_diff <- reactive({

    ## calculate input from sliders
    simulation_volume = input$simulation_volume
    total_sample_size <- input$n_daily_volume*input$n_days

    control_n <- round(total_sample_size*input$sample_split,digits=0)
    variant_n <- total_sample_size-control_n

    control_rate <- input$control_rate
    variant_rate <- input$control_rate*(1+input$delta_from_control)

    control_alpha <- round(control_n*control_rate,digits=0)
    control_beta <- control_n-control_alpha

    variant_alpha <- round(variant_n*variant_rate,digits=0)
    variant_beta <- variant_n-variant_alpha


    samples <- data.frame(prop_a=rbeta(simulation_volume,control_alpha,control_beta),prop_b=rbeta(simulation_volume,variant_alpha,variant_beta))
    samples$diff <- (samples$prop_b/samples$prop_a)-1
    samples

  })

  likelihood_diff_alt <- reactive({

    ## calculate input from sliders
    simulation_volume = input$simulation_volume
    total_sample_size <- input$n_daily_volume*input$n_days_alt

    control_n <- round(total_sample_size*input$sample_split,digits=0)
    variant_n <- total_sample_size-control_n

    control_rate <- input$control_rate
    variant_rate <- input$control_rate*(1+input$delta_from_control)

    control_alpha <- round(control_n*control_rate,digits=0)
    control_beta <- control_n-control_alpha

    variant_alpha <- round(variant_n*variant_rate,digits=0)
    variant_beta <- variant_n-variant_alpha


    samples <- data.frame(prop_a=rbeta(simulation_volume,control_alpha,control_beta),prop_b=rbeta(simulation_volume,variant_alpha,variant_beta))
    samples$diff <- (samples$prop_b/samples$prop_a)-1
    samples

  })

  summary_results <- reactive({

  simulation_volume = input$simulation_volume

  lower <- (1-input$credible_interval)/2
  upper <- 1-lower

  ## calculate input from sliders
  simulation_volume = input$simulation_volume
  total_sample_size <- input$n_daily_volume*input$n_days

  control_n <- round(total_sample_size*input$sample_split,digits=0)
  variant_n <- total_sample_size-control_n

  control_rate <- input$control_rate
  variant_rate <- input$control_rate*(1+input$delta_from_control)

  control_alpha <- round(control_n*control_rate,digits=0)
  control_beta <- control_n-control_alpha

  variant_alpha <- round(variant_n*variant_rate,digits=0)
  variant_beta <- variant_n-variant_alpha

    samples <- data.frame(prop_a=rbeta(simulation_volume,control_alpha,control_beta),prop_b=rbeta(simulation_volume,variant_alpha,variant_beta))
    samples$diff <- (samples$prop_b/samples$prop_a)-1

    probability <- sum(samples$diff >= 0)/simulation_volume
    probability <- paste(round(probability*100,digits=1),'%',sep='')

    lower_diff <- paste(round(quantile(samples$diff,lower)*100,digits=1),'%',sep='')
    upper_diff <- paste(round(quantile(samples$diff,upper)*100,digits=1),'%',sep='')

    credible_interval <- paste(lower_diff,', ',upper_diff,sep='')

    control_value <- paste(round((control_alpha/control_n)*100,digits=1),'%',sep='')
    variant_value <- paste(round((variant_alpha/variant_n)*100,digits=1),'%',sep='')

    likelihood <- data.frame(control_value,variant_value,probability,credible_interval,control_n,variant_n)

    ## combine
    summary <- rbind(likelihood)

  })

  summary_results_alt <- reactive({

  simulation_volume = input$simulation_volume

  lower <- (1-input$credible_interval)/2
  upper <- 1-lower

  ## calculate input from sliders
  simulation_volume = input$simulation_volume
  total_sample_size <- input$n_daily_volume*input$n_days_alt

  control_n <- round(total_sample_size*input$sample_split,digits=0)
  variant_n <- total_sample_size-control_n

  control_rate <- input$control_rate
  variant_rate <- input$control_rate*(1+input$delta_from_control)

  control_alpha <- round(control_n*control_rate,digits=0)
  control_beta <- control_n-control_alpha

  variant_alpha <- round(variant_n*variant_rate,digits=0)
  variant_beta <- variant_n-variant_alpha

    samples <- data.frame(prop_a=rbeta(simulation_volume,control_alpha,control_beta),prop_b=rbeta(simulation_volume,variant_alpha,variant_beta))
    samples$diff <- (samples$prop_b/samples$prop_a)-1

    probability <- sum(samples$diff >= 0)/simulation_volume
    probability <- paste(round(probability*100,digits=1),'%',sep='')

    lower_diff <- paste(round(quantile(samples$diff,lower)*100,digits=1),'%',sep='')
    upper_diff <- paste(round(quantile(samples$diff,upper)*100,digits=1),'%',sep='')

    credible_interval <- paste(lower_diff,', ',upper_diff,sep='')

    control_value <- paste(round((control_alpha/control_n)*100,digits=1),'%',sep='')
    variant_value <- paste(round((variant_alpha/variant_n)*100,digits=1),'%',sep='')

    likelihood <- data.frame(control_value,variant_value,probability,credible_interval,control_n,variant_n)

    ## combine
    summary <- rbind(likelihood)

  })

  output$likelihood_distributions<-renderPlot({
    ggplot(likelihood(),aes(x=value,fill=sample)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Values') + ylab('Density') + scale_x_continuous(label=scales::percent) + ggtitle('Comparison of Probability Distributions')
  })

  output$likelihood_distributions_alt<-renderPlot({
    ggplot(likelihood_alt(),aes(x=value,fill=sample)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Values') + ylab('Density') + scale_x_continuous(label=scales::percent) + ggtitle('Comparison of Probability Distributions')
  })

  output$likelihood_differential<-renderPlot({
    ggplot(likelihood_diff(),aes(x=diff)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Deltas Between Proportion A and Proportion B') + ylab('Relative % Difference from Control') + scale_x_continuous(label=scales::percent) + geom_vline(xintercept=0,linetype='dashed',colour='red') + ggtitle('Distribution for Range of Possible Differences')
  })

  output$likelihood_differential_alt<-renderPlot({
    ggplot(likelihood_diff_alt(),aes(x=diff)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Deltas Between Proportion A and Proportion B') + ylab('Relative % Difference from Control') + scale_x_continuous(label=scales::percent) + geom_vline(xintercept=0,linetype='dashed',colour='red') + ggtitle('Distribution for Range of Possible Differences')
  })


  output$table_summary<-renderTable({
    ##kable(summary_data(),format = 'html', col.names = c('lower diff','upper diff'),caption = 'summary of two proportions')
    summary_results()
  })

  output$table_summary_alt<-renderTable({
    ##kable(summary_data(),format = 'html', col.names = c('lower diff','upper diff'),caption = 'summary of two proportions')
    summary_results_alt()
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
