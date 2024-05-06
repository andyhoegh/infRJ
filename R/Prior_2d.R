################################################################################
################################################################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  titlePanel("Scale Selection Prior Elicitation: 2d"),

  sidebarLayout(
    sidebarPanel(
      h4("1. Set probability of meaningful effect"),
      sliderInput("prob_effect",
                  "For variable 1 how confident are you that there is a meaningful effect?",
                  min = 0,
                  max = 100,
                  value = 50,
                  post = '%'),
      sliderInput("prob_effect2",
                  "For variable 2 how confident are you that there is a meaningful effect?",
                  min = 0,
                  max = 100,
                  value = 50,
                  post = '%'),
      hr(),
      ################################################################################
      h4("2. Create meaningful set of distances"),
      numericInput("distance", "Enter Distance", 1000, min = 0, step = 250),
      numericInput("probability", "Prior Probability (may need to be scaled) for Variable 1", .1, min = 0, max = 1, step = .1),
      numericInput("probability2", "Prior Probability (may need to be scaled) for Variable 2", .1, min = 0, max = 1, step = .1),
      actionButton("add_data", "Add Distance", width = "100%"),
      hr(),
      ################################################################################
      h4("3. Normalize your prior and interact with marginal prior plots"),
      actionButton("normalize", "Normalize Priors", width = "100%"),
      downloadButton("downloadPrior", "Download Prior", width = "100%"),
      actionButton("exclude_reset", "Reset Prior", width = "100%")
    ),
    ################################################################################
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Plot: Marginal Var 1",
                 plotOutput("PriorPlot", click = "plot_click"),
                 actionButton("exclude_reset", "Reset Prior")),
        tabPanel("Interactive Plot: Marginal Var 2",
                 plotOutput("PriorPlot2", click = "plot_click2"),
                 actionButton("exclude_reset2", "Reset Prior")),
        tabPanel("Table: Marginal Var 1", dataTableOutput('table')),
        tabPanel("Table: Marginal Var 2", dataTableOutput('table2')),
        tabPanel("Table: Joint",
                 dataTableOutput('tableJ'),
                 actionButton("create_joint", "Create Joint Prior")),
        tabPanel("Plot: Joint Prior", plotOutput("JointPrior"))
      )
    )
  ))
################################################################################
################################################################################
server <- function(input, output, session) {

  prior_table <- reactiveVal(
    tibble(`Var1: distance (km)` = 0, `Prior Probability` = .5)
  )

  prior_table2 <- reactiveVal(
    tibble(`Var2: distance (km)` = 0, `Prior Probability` = .5)
  )

  prior_tableJ <- reactiveVal(tibble(`Var1: distance (km)` = 0,
                                     `Var2: distance (km)` = 0,
                                     `Joint Probability` = .4))
  ################################################################################

  observeEvent(input$prob_effect,  {
    if (nrow(prior_table()) > 1){
      unif_dist <- prior_table() %>% slice(2:n())

      zero_val <- tibble::tibble(`Var1: distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_effect/100)

      start_dist <- zero_val %>% bind_rows(unif_dist)
      prior_table(start_dist)
      prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                        rename(`Prior Probability2` = `Prior Probability`))|>
                            mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            select(-`Prior Probability2`, -`Prior Probability`)))
    } else {
      prior_table( tibble(`Var1: distance (km)` = 0, `Prior Probability` = 1 - input$prob_effect/100))
      prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                        rename(`Prior Probability2` = `Prior Probability`))|>
                            mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            select(-`Prior Probability2`, -`Prior Probability`)))    }
  })

  observeEvent(input$prob_effect2,  {
    if (nrow(prior_table2()) > 1){
      unif_dist <- prior_table2() %>% slice(2:n())

      zero_val <- tibble::tibble(`Var2: distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_effect2/100)

      start_dist <- zero_val %>% bind_rows(unif_dist)
      prior_table2(start_dist)
      prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                        rename(`Prior Probability2` = `Prior Probability`))|>
                            mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            select(-`Prior Probability2`, -`Prior Probability`)))
    } else {
      prior_table2( tibble(`Var2: distance (km)` = 0, `Prior Probability` = 1 - input$prob_effect2/100))
      prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                        rename(`Prior Probability2` = `Prior Probability`))|>
                            mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            select(-`Prior Probability2`, -`Prior Probability`)))
    }
  })
  ################################################################################

  observeEvent(input$plot_click, {
    new_val <- nearPoints(prior_table(), input$plot_click, threshold = 1000, maxpoints = 1)
    new_row <- tibble(`Var1: distance (km)` = new_val$`Var1: distance (km)`,
                      `Prior Probability` = input$plot_click$y)
    tmp <- prior_table() %>%
      filter(`Var1: distance (km)` != new_val$`Var1: distance (km)`) %>%
      bind_rows(new_row)
    prior_table(prior_table() %>%
                  filter(`Var1: distance (km)` != new_val$`Var1: distance (km)`) %>%
                  bind_rows(new_row)|>
                  mutate(`Prior Probability` = `Prior Probability` / sum(tmp$`Prior Probability`)))
    prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                      rename(`Prior Probability2` = `Prior Probability`))|>
                          mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          select(-`Prior Probability2`, -`Prior Probability`)))
  })

  observeEvent(input$plot_click2, {
    new_val <- nearPoints(prior_table2(), input$plot_click2, threshold = 1000, maxpoints = 1)
    new_row <- tibble(`Var2: distance (km)` = new_val$`Var2: distance (km)`,
                      `Prior Probability` = input$plot_click2$y)
    tmp <- prior_table2() %>%
      filter(`Var2: distance (km)` != new_val$`Var2: distance (km)`) %>%
      bind_rows(new_row)
    prior_table2(prior_table2() %>%
                   filter(`Var2: distance (km)` != new_val$`Var2: distance (km)`) %>%
                   bind_rows(new_row)|>
                   mutate(`Prior Probability` = `Prior Probability` / sum(tmp$`Prior Probability`)))
    prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                      rename(`Prior Probability2` = `Prior Probability`))|>
                          mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          select(-`Prior Probability2`, -`Prior Probability`)))
  })
  ################################################################################
  observeEvent(input$exclude_reset, {
    prior_table( tibble(`Var1: distance (km)` = 0, `Prior Probability` = .5))
    updateSliderInput(session = session,
                      inputId = "prob_effect",
                      value = 50)
    prior_tableJ(tibble(`Var1: distance (km)` = 0,
                        `Var2: distance (km)` = 0,
                        `Joint Probability` = .4))
  })

  observeEvent(input$exclude_reset, {
    prior_table2( tibble(`Var2: distance (km)` = 0, `Prior Probability` = .5))
    updateSliderInput(session = session,
                      inputId = "prob_effect2",
                      value = 50)
    prior_tableJ(tibble(`Var1: distance (km)` = 0,
                        `Var2: distance (km)` = 0,
                        `Joint Probability` = .4))
  })
  ################################################################################
  output$PriorPlot <- renderPlot({
    prior_table() %>%
      ggplot(aes(x = `Var1: distance (km)`, y = `Prior Probability`)) +
      geom_point() +
      geom_segment( aes(x=`Var1: distance (km)`, xend=`Var1: distance (km)`,
                        y=0, yend=`Prior Probability`))+
      theme_bw() +
      xlab('distance (km)') +
      ylab('Prior Probability') +
      labs(title = 'Elicited Prior',
           subtitle = 'Note: 0 distance implies no meaningful effect',
           caption = 'Click to adjust priors. The click will recognize the nearest point.')
  })

  output$PriorPlot2 <- renderPlot({
    prior_table2() %>%
      ggplot(aes(x = `Var2: distance (km)`, y = `Prior Probability`)) +
      geom_point() +
      geom_segment( aes(x=`Var2: distance (km)`, xend=`Var2: distance (km)`,
                        y=0, yend=`Prior Probability`))+
      theme_bw() +
      xlab('distance (km)') +
      ylab('Prior Probability') +
      labs(title = 'Elicited Prior',
           subtitle = 'Note: 0 distance implies no meaningful effect',
           caption = 'Click to adjust priors. The click will recognize the nearest point.')
  })

  output$JointPrior <- renderPlot({
    prior_tableJ() %>%
      mutate(lab = round(`Joint Probability`,2)) |>
      ggplot(aes(x = `Var2: distance (km)`, y = `Var1: distance (km)`,
                 size = `Joint Probability`, color = `Joint Probability`)) +
      geom_point( size = 12) +
      scale_color_gradient(low = "yellow", high = "red", na.value = NA)   +
      geom_text(aes(label = lab), size = 5, color = "black") +
      theme_bw() +
      xlab('Variable 1') +
      ylab('Variable 2') +
      labs(title = 'Elicited Prior',
           subtitle = 'Note: 0 distance implies no meaningful effect')
  })
  ################################################################################
  output$table <- renderDataTable({prior_table()|> arrange(`Var1: distance (km)`)})
  output$table2 <- renderDataTable({prior_table2()|> arrange(`Var2: distance (km)`)})
  output$tableJ <- renderDataTable({prior_tableJ()})

  ################################################################################
  output$downloadPrior <- downloadHandler(
    filename = function() {
      paste("myprior", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(prior_tableJ(), file, row.names = FALSE)
    })
  ################################################################################
  observeEvent(input$add_data, {
    prior_table() %>%
      add_row(
        `Var1: distance (km)` = input$distance,
        `Prior Probability` = input$probability,
      ) %>%
      prior_table()
    prior_table2() %>%
      add_row(
        `Var2: distance (km)` = input$distance,
        `Prior Probability` = input$probability2,
      ) %>%
      prior_table2()
    prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                      rename(`Prior Probability2` = `Prior Probability`))|>
                          mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          select(-`Prior Probability2`, -`Prior Probability`)))
  })


  ################################################################################
  observeEvent(input$normalize, {
    prior_table(prior_table() %>%
                  mutate(`Prior Probability` = `Prior Probability` / sum(`Prior Probability`)))
    prior_table2(prior_table2() %>%
                   mutate(`Prior Probability` = `Prior Probability` / sum(`Prior Probability`)))


    prior_tableJ(tibble(expand_grid(prior_table(), prior_table2()|>
                                      rename(`Prior Probability2` = `Prior Probability`))|>
                          mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          select(-`Prior Probability2`, -`Prior Probability`)))

  })
}


################################################################################
# Run the application
shinyApp(ui = ui, server = server)
