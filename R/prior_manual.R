#' 1D Prior Elicitation with manual features
#'
#' @return opens shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' prior_manual()
#' }

prior_manual <- function(){
  ################################################################################
  ################################################################################
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("hr {border-top: 1px solid #000000;}"))
    ),
    shiny::titlePanel("Scale Selection Prior Elicitation: Manual Option"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("1. Set probability of meaningful effect"),
        shiny::sliderInput("prob_effect",
                    "How confident are you that there is a meaningful effect?",
                    min = 0,
                    max = 100,
                    value = 50,
                    post = '%'),
        shiny::hr(),
        shiny::h4("2. Create meaningful set of distances"),
        shiny::h5("Option 2. Manually create distances"),
        shiny::numericInput("distance", "Enter Distance", 1000, min = 0, step = 250),
        shiny::numericInput("probability", "Prior Probability (may need to be scaled)", .1, min = 0, max = 1, step = .1),
        shiny::actionButton("add_data", "Add Distance", width = "100%"),
        shiny::hr(),
        shiny::actionButton("normalize", "Normalize Prior", width = "100%"),
        shiny::downloadButton("downloadPrior", "Download Prior", width = "100%"),
        shiny::actionButton("exclude_reset", "Reset Prior", width = "100%")),

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Interactive Plot",
                          shiny::plotOutput("PriorPlot", click = "plot_click"),
                          shiny::actionButton("exclude_reset", "Reset Prior")),
          shiny::tabPanel("Table", DT::DTOutput('table'))
        )
      )
    )
  )

  ################################################################################
  ################################################################################
  server <- function(input, output, session) {

    prior_table2 <- shiny::reactiveVal(
      tibble::tibble(`distance (km)` = 0, `Prior Probability` = .5)
    )

    shiny::observeEvent(input$prob_effect,  {
      if (nrow(prior_table2()) > 1){
        unif_dist <- prior_table2() |> dplyr::slice(2:dplyr::n())

        zero_val <- tibble::tibble(`distance (km)` = 0,
                                   `Prior Probability` = 1 - input$prob_effect/100)

        start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
        prior_table2(start_dist)
      } else {
        prior_table2( tibble::tibble(`distance (km)` = 0, `Prior Probability` = 1 - input$prob_effect/100))
      }
    })

    shiny::observeEvent(input$plot_click, {
      new_val <- shiny::nearPoints(prior_table2(), input$plot_click, threshold = 1000, maxpoints = 1)
      new_row <- tibble::tibble(`distance (km)` = new_val$`distance (km)`,
                        `Prior Probability` = input$plot_click$y)
      tmp <- prior_table2() |>
        dplyr::filter(`distance (km)` != new_val$`distance (km)`) |>
        dplyr::bind_rows(new_row)
      prior_table2(prior_table2() |>
                     dplyr::filter(`distance (km)` != new_val$`distance (km)`) |>
                     dplyr::bind_rows(new_row)|>
                     dplyr::mutate(`Prior Probability` = `Prior Probability` / sum(tmp$`Prior Probability`)))

    })

    shiny::observeEvent(input$exclude_reset, {
      prior_table2( tibble::tibble(`distance (km)` = 0, `Prior Probability` = .5))
      shiny::updateSliderInput(session = session,
                        inputId = "max_dist",
                        value = 10)
      shiny::updateSliderInput(session = session,
                        inputId = "min_dist",
                        value = .25)
      shiny::updateSliderInput(session = session,
                        inputId = "step_dist",
                        value = 250)
      shiny::updateSliderInput(session = session,
                        inputId = "prob_effect",
                        value = 50)
    })

    output$PriorPlot <- shiny::renderPlot({
      prior_table2() |>
        ggplot2::ggplot(ggplot2::aes(x = `distance (km)`, y = `Prior Probability`)) +
        ggplot2::geom_point() +
        ggplot2::geom_segment( ggplot2::aes(x=`distance (km)`, xend=`distance (km)`,
                          y=0, yend=`Prior Probability`))+
        ggplot2::theme_bw() +
        ggplot2::xlab('distance (km)') +
        ggplot2::ylab('Prior Probability') +
        ggplot2::labs(title = 'Elicited Prior',
             subtitle = 'Note: 0 distance implies no meaningful effect',
             caption = 'Click to adjust priors. The click will recognize the nearest point.')
    })

    output$table <- DT::renderDT({prior_table2()|> dplyr::arrange(`distance (km)`)})

    output$downloadPrior <- shiny::downloadHandler(
      filename = function() {
        paste("myprior", ".csv", sep = "")
      },
      content = function(file) {
        utils::write.csv(prior_table(), file, row.names = FALSE)
      })

    shiny::observeEvent(input$add_data, {
      prior_table2() |>
        dplyr::add_row(
          `distance (km)` = input$distance,
          `Prior Probability` = input$probability,
        ) |>
        prior_table2()
    })

    shiny::observeEvent(input$normalize, {
      prior_table2() |>
        dplyr::mutate(
          `Prior Probability` = `Prior Probability` / sum(`Prior Probability`),
        ) |>
        prior_table2()
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
