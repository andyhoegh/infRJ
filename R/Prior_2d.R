#' Generate 2d Shiny elicitation app
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Prior_2d()
#' }
################################################################################
################################################################################
Prior_2d <- function(){
  ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("hr {border-top: 1px solid #000000;}"))
  ),
  shiny::titlePanel("Scale Selection Prior Elicitation: 2d"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4("1. Set probability of meaningful effect"),
      shiny::sliderInput("prob_effect",
                  "For variable 1 how confident are you that there is a meaningful effect?",
                  min = 0,
                  max = 100,
                  value = 50,
                  post = '%'),
      shiny::sliderInput("prob_effect2",
                  "For variable 2 how confident are you that there is a meaningful effect?",
                  min = 0,
                  max = 100,
                  value = 50,
                  post = '%'),
      shiny::hr(),
      ################################################################################
      shiny::h4("2. Create meaningful set of distances"),
      shiny::numericInput("distance", "Enter Distance", 1000, min = 0, step = 250),
      shiny::numericInput("probability", "Prior Probability (may need to be scaled) for Variable 1", .1, min = 0, max = 1, step = .1),
      shiny::numericInput("probability2", "Prior Probability (may need to be scaled) for Variable 2", .1, min = 0, max = 1, step = .1),
      shiny::actionButton("add_data", "Add Distance", width = "100%"),
      shiny::hr(),
      ################################################################################
      shiny::h4("3. Normalize your prior and interact with marginal prior plots"),
      shiny::actionButton("normalize", "Normalize Priors", width = "100%"),
      shiny::downloadButton("downloadPrior", "Download Prior", width = "100%"),
      shiny::actionButton("exclude_reset", "Reset Prior", width = "100%")
    ),
    ################################################################################
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Interactive Plot: Marginal Var 1",
                        shiny::plotOutput("PriorPlot", click = "plot_click"),
                        shiny::actionButton("exclude_reset", "Reset Prior")),
        shiny::tabPanel("Interactive Plot: Marginal Var 2",
                        shiny::plotOutput("PriorPlot2", click = "plot_click2"),
                        shiny::actionButton("exclude_reset2", "Reset Prior")),
        shiny::tabPanel("Table: Marginal Var 1", DT::DTOutput('table')),
        shiny::tabPanel("Table: Marginal Var 2", DT::DTOutput('table2')),
        shiny::tabPanel("Table: Joint",
                        DT::DTOutput('tableJ'),
                        shiny::actionButton("create_joint", "Create Joint Prior")),
        shiny::tabPanel("Plot: Joint Prior", shiny::plotOutput("JointPrior"))
      )
    )
  ))
################################################################################
################################################################################

server <- function(input, output, session) {

  prior_table <- shiny::reactiveVal(
    tibble::tibble(`Var1: distance (km)` = 0, `Prior Probability` = .5)
  )

  prior_table2 <- shiny::reactiveVal(
    tibble::tibble(`Var2: distance (km)` = 0, `Prior Probability` = .5)
  )

  prior_tableJ <- shiny::reactiveVal(tibble::tibble(`Var1: distance (km)` = 0,
                                     `Var2: distance (km)` = 0,
                                     `Joint Probability` = .4))
  ################################################################################

  shiny::observeEvent(input$prob_effect,  {
    if (nrow(prior_table()) > 1){
      unif_dist <- prior_table() |> dplyr::slice(2:dplyr::n())

      zero_val <- tibble::tibble(`Var1: distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_effect/100)

      start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
      prior_table(start_dist)
      prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                        dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                            dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            dplyr::select(-`Prior Probability2`, -`Prior Probability`)))
    } else {
      prior_table( tibble::tibble(`Var1: distance (km)` = 0, `Prior Probability` = 1 - input$prob_effect/100))
      prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                        dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                            dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            dplyr::select(-`Prior Probability2`, -`Prior Probability`)))    }
  })

  shiny::observeEvent(input$prob_effect2,  {
    if (nrow(prior_table2()) > 1){
      unif_dist <- prior_table2() |> dplyr::slice(2:dplyr::n())

      zero_val <- tibble::tibble(`Var2: distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_effect2/100)

      start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
      prior_table2(start_dist)
      prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                        dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                            dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            dplyr::select(-`Prior Probability2`, -`Prior Probability`)))
    } else {
      prior_table2( tibble::tibble(`Var2: distance (km)` = 0, `Prior Probability` = 1 - input$prob_effect2/100))
      prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                        dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                            dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                            dplyr::select(-`Prior Probability2`, -`Prior Probability`)))
    }
  })
  ################################################################################

  shiny:: observeEvent(input$plot_click, {
    new_val <- shiny::nearPoints(prior_table(), input$plot_click, threshold = 1000, maxpoints = 1)
    new_row <- tibble::tibble(`Var1: distance (km)` = new_val$`Var1: distance (km)`,
                      `Prior Probability` = input$plot_click$y)
    tmp <- prior_table() |>
      dplyr::filter(`Var1: distance (km)` != new_val$`Var1: distance (km)`) |>
      dplyr::bind_rows(new_row)
    prior_table(prior_table() |>
                  dplyr::filter(`Var1: distance (km)` != new_val$`Var1: distance (km)`) |>
                  dplyr::bind_rows(new_row)|>
                  dplyr::mutate(`Prior Probability` = `Prior Probability` / sum(tmp$`Prior Probability`)))
    prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                      dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                          dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          dplyr::select(-`Prior Probability2`, -`Prior Probability`)))
  })

  shiny::observeEvent(input$plot_click2, {
    new_val <- shiny::nearPoints(prior_table2(), input$plot_click2, threshold = 1000, maxpoints = 1)
    new_row <- tibble::tibble(`Var2: distance (km)` = new_val$`Var2: distance (km)`,
                      `Prior Probability` = input$plot_click2$y)
    tmp <- prior_table2() |>
      dplyr::filter(`Var2: distance (km)` != new_val$`Var2: distance (km)`) |>
      dplyr::bind_rows(new_row)
    prior_table2(prior_table2() |>
                   dplyr::filter(`Var2: distance (km)` != new_val$`Var2: distance (km)`) |>
                   dplyr::bind_rows(new_row)|>
                   dplyr::mutate(`Prior Probability` = `Prior Probability` / sum(tmp$`Prior Probability`)))
    prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                      dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                          dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          dplyr::select(-`Prior Probability2`, -`Prior Probability`)))
  })
  ################################################################################
  shiny::observeEvent(input$exclude_reset, {
    prior_table( tibble::tibble(`Var1: distance (km)` = 0, `Prior Probability` = .5))
    shiny::updateSliderInput(session = session,
                      inputId = "prob_effect",
                      value = 50)
    prior_tableJ(tibble::tibble(`Var1: distance (km)` = 0,
                        `Var2: distance (km)` = 0,
                        `Joint Probability` = .4))
  })

  shiny::observeEvent(input$exclude_reset, {
    prior_table2( tibble::tibble(`Var2: distance (km)` = 0, `Prior Probability` = .5))
    shiny::updateSliderInput(session = session,
                      inputId = "prob_effect2",
                      value = 50)
    prior_tableJ(tibble::tibble(`Var1: distance (km)` = 0,
                        `Var2: distance (km)` = 0,
                        `Joint Probability` = .4))
  })
  ################################################################################
  output$PriorPlot <- shiny::renderPlot({
    prior_table() |>
      ggplot2::ggplot(ggplot2::aes(x = `Var1: distance (km)`, y = `Prior Probability`)) +
      ggplot2::geom_point() +
      ggplot2::geom_segment( ggplot2::aes(x=`Var1: distance (km)`, xend=`Var1: distance (km)`,
                        y=0, yend=`Prior Probability`))+
      ggplot2::theme_bw() +
      ggplot2::xlab('distance (km)') +
      ggplot2::ylab('Prior Probability') +
      ggplot2::labs(title = 'Elicited Prior',
           subtitle = 'Note: 0 distance implies no meaningful effect',
           caption = 'Click to adjust priors. The click will recognize the nearest point.')
  })

  output$PriorPlot2 <- shiny::renderPlot({
    prior_table2() |>
      ggplot2::ggplot(ggplot2::aes(x = `Var2: distance (km)`, y = `Prior Probability`)) +
      ggplot2::geom_point() +
      ggplot2::geom_segment( ggplot2::aes(x=`Var2: distance (km)`, xend=`Var2: distance (km)`,
                        y=0, yend=`Prior Probability`))+
      ggplot2::theme_bw() +
      ggplot2::xlab('distance (km)') +
      ggplot2::ylab('Prior Probability') +
      ggplot2::labs(title = 'Elicited Prior',
           subtitle = 'Note: 0 distance implies no meaningful effect',
           caption = 'Click to adjust priors. The click will recognize the nearest point.')
  })

  output$JointPrior <- shiny::renderPlot({
    prior_tableJ() |>
      dplyr::mutate(lab = round(`Joint Probability`,2)) |>
      ggplot2::ggplot(ggplot2::aes(x = `Var2: distance (km)`, y = `Var1: distance (km)`,
                 size = `Joint Probability`, color = `Joint Probability`)) +
      ggplot2::geom_point( size = 12) +
      ggplot2::scale_color_gradient(low = "yellow", high = "red", na.value = NA)   +
      ggplot2::geom_text(ggplot2::aes(label = lab), size = 5, color = "black") +
      ggplot2::theme_bw() +
      ggplot2::xlab('Variable 1') +
      ggplot2::ylab('Variable 2') +
      ggplot2::labs(title = 'Elicited Prior',
           subtitle = 'Note: 0 distance implies no meaningful effect')
  })
  ################################################################################
  output$table <- DT::renderDT({prior_table()|> dplyr::arrange(`Var1: distance (km)`)})
  output$table2 <- DT::renderDT({prior_table2()|> dplyr::arrange(`Var2: distance (km)`)})
  output$tableJ <- DT::renderDT({prior_tableJ()|> dplyr::arrange(`Var1: distance (km)`, `Var2: distance (km)`)})

  ################################################################################
  output$downloadPrior <- shiny::downloadHandler(
    filename = function() {
      paste("myprior", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(prior_tableJ(), file, row.names = FALSE)
    })
  ################################################################################
  shiny::observeEvent(input$add_data, {
    prior_table() |>
      dplyr::add_row(
        `Var1: distance (km)` = input$distance,
        `Prior Probability` = input$probability,
      ) |>
      prior_table()
    prior_table2() |>
      dplyr::add_row(
        `Var2: distance (km)` = input$distance,
        `Prior Probability` = input$probability2,
      ) |>
      prior_table2()
    prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                      dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                          dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          dplyr::select(-`Prior Probability2`, -`Prior Probability`)))
  })


  ################################################################################
  shiny::observeEvent(input$normalize, {
    prior_table(prior_table() |>
                  dplyr::mutate(`Prior Probability` = `Prior Probability` / sum(`Prior Probability`)))
    prior_table2(prior_table2() |>
                   dplyr::mutate(`Prior Probability` = `Prior Probability` / sum(`Prior Probability`)))


    prior_tableJ(tibble::tibble(tidyr::expand_grid(prior_table(), prior_table2()|>
                                      dplyr::rename(`Prior Probability2` = `Prior Probability`))|>
                          dplyr::mutate(`Joint Probability` = `Prior Probability2` * `Prior Probability`) |>
                          dplyr::select(-`Prior Probability2`, -`Prior Probability`)))

  })
}


################################################################################
# Run the application
shiny::shinyApp(ui = ui, server = server)
}
