#' 1D Prior Elicitation with automatic features
#'
#' @return opens shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' prior_auto()
#' }
prior_auto <- function(){
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("hr {border-top: 1px solid #000000;}"))
    ),
    shiny::titlePanel("Scale Selection Prior Elicitation: Automatic"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("prob_noeffect",
                    "How confident are you that there is a meaningful effect?",
                    min = 0,
                    max = 100,
                    value = 50,
                    post = '%'),
        shiny::hr(),
        shiny::sliderInput("min_dist",
                    "What is the minimum distance for which you'd expect an effect?",
                    min = .25,
                    max = 10,
                    value = .25,
                    post = ' kilometers'),
        shiny::sliderInput("max_dist",
                    "What is the maximum distance for which you'd expect an effect?",
                    min = .25,
                    max = 50,
                    value = 10,
                    post = ' kilometers'),
        shiny::numericInput("step_dist",
                     "What is the minimum interval in meters (say 1000 M vs 1250 KM) that makes intuitive sense?",
                     min = 25,
                     max = 5000,
                     value = 250,
                     step = 50),
        shiny::hr(),
        shiny::h5("Adjusting any widgets will reset to uniform prior"),
        shiny::downloadButton("downloadPrior", "Download Prior")),

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Plot",
                          shiny::plotOutput("PriorPlot", click = "plot_click"),
                          shiny::actionButton("exclude_reset", "Reset Prior")),
          shiny::tabPanel("Table", DT::DTOutput('table'))
        )
      )
    )
  )

  server <- function(input, output, session) {

    dist_range = seq(from = 0.25, to = 10, by = 250 / 1000)
    unif_dist <- tibble::tibble(`distance (km)` = dist_range,
                                `Prior Probability` = (1 - .5) / length(dist_range))
    zero_val <- tibble::tibble(`distance (km)` = 0,
                               `Prior Probability` = .5)

    start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
    prior_table = shiny::reactiveVal({start_dist})

    shiny::observeEvent(input$min_dist,  {
      shiny::updateSliderInput(session = session,
                        inputId = "max_dist",
                        min = input$min_dist)
      dist_range = seq(from = input$min_dist, to = input$max_dist,
                       by = input$step_dist / 1000)

      unif_dist <- tibble::tibble(`distance (km)` = dist_range,
                                  `Prior Probability` = (input$prob_noeffect/100)
                                  / length(dist_range))

      zero_val <- tibble::tibble(`distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_noeffect/100)

      start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
      prior_table(start_dist)
    })

    shiny::observeEvent(input$max_dist,  {
      shiny::updateSliderInput(session = session,
                        inputId = "min_dist",
                        max = input$max_dist)
      shiny::updateSliderInput(session = session,
                        inputId = "max_dist",
                        min = input$min_dist)
      dist_range = seq(from = input$min_dist, to = input$max_dist,
                       by = input$step_dist / 1000)

      unif_dist <- tibble::tibble(`distance (km)` = dist_range,
                                  `Prior Probability` = ( input$prob_noeffect/100)
                                  / length(dist_range))

      zero_val <- tibble::tibble(`distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_noeffect/100)

      start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
      prior_table(start_dist)
    })

    shiny::observeEvent(input$step_dist,  {
      shiny::updateSliderInput(session = session,
                        inputId = "max_dist",
                        min = input$min_dist)
      dist_range = seq(from = input$min_dist, to = input$max_dist,
                       by = input$step_dist / 1000)

      unif_dist <- tibble::tibble(`distance (km)` = dist_range,
                                  `Prior Probability` = ( input$prob_noeffect/100)
                                  / length(dist_range))

      zero_val <- tibble::tibble(`distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_noeffect/100)

      start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
      prior_table(start_dist)
    })

    shiny::observeEvent(input$prob_noeffect,  {
      shiny::updateSliderInput(session = session,
                        inputId = "max_dist",
                        min = input$min_dist)
      dist_range = seq(from = input$min_dist, to = input$max_dist,
                       by = input$step_dist / 1000)

      unif_dist <- tibble::tibble(`distance (km)` = dist_range,
                                  `Prior Probability` = ( input$prob_noeffect/100)
                                  / length(dist_range))

      zero_val <- tibble::tibble(`distance (km)` = 0,
                                 `Prior Probability` = 1 - input$prob_noeffect/100)

      start_dist <- zero_val |> dplyr::bind_rows(unif_dist)
      prior_table(start_dist)
    })

    shiny::observeEvent(input$plot_click, {
      new_val <- shiny::nearPoints(prior_table(), input$plot_click, threshold = 1000, maxpoints = 1)
      new_row <- tibble::tibble(`distance (km)` = new_val$`distance (km)`,
                        `Prior Probability` = input$plot_click$y)
      tmp <- prior_table() |>
        dplyr::filter(`distance (km)` != new_val$`distance (km)`) |>
        dplyr::bind_rows(new_row)
      prior_table(prior_table() |>
                    dplyr::filter(`distance (km)` != new_val$`distance (km)`) |>
                    dplyr::bind_rows(new_row)|>
                    dplyr::mutate(`Prior Probability` = `Prior Probability` / sum(tmp$`Prior Probability`)))
    })

    shiny::observeEvent(input$exclude_reset, {
      prior_table(start_dist)
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
                        inputId = "prob_noeffect",
                        value = 50)
    })

    output$PriorPlot <- shiny::renderPlot({
      prior_table() |>
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

    output$table <- DT::renderDT()({prior_table()|> dplyr::arrange(`distance (km)`)})


    output$downloadPrior <- shiny::downloadHandler(
      filename = function() {
        paste("myprior", ".csv", sep = "")
      },
      content = function(file) {
        utils::write.csv(prior_table(), file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
