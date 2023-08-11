server <- function(input, output, session) {

  leaderboard_data <- shiny::eventReactive(
    eventExpr = input$update_leaderboard,
    valueExpr = predpitchscore::leaderboard_2023 |>
      dplyr::filter(pitch_type %in% input$pitch_types, !is.na(pred)) |>
      dplyr::mutate(
        split_by_pitch_type = input$split_by_pitch_type,
        pitch_type = ifelse(split_by_pitch_type == "Yes", pitch_type, "all")
      ) |>
      dplyr::group_by(pitcher_id, player_name, pitch_type) |>
      dplyr::summarize(
        stuff = round(2000 * weighted.mean(stuff, w = n, na.rm = TRUE)),
        desc = round(2000 * weighted.mean(desc, w = n, na.rm = TRUE)),
        pred = round(2000 * weighted.mean(pred, w = n, na.rm = TRUE)),
        n = sum(n),
        .groups = "drop"
      ) |>
      dplyr::filter(n >= input$range_n[1], n <= input$range_n[2]) |>
      dplyr::select(player_name, pitch_type, n, stuff, desc, pred) |>
      dplyr::arrange(pred)
  )

  output$leaderboard <- DT::renderDT(
    expr = DT::datatable(
      data = leaderboard_data(),
      colnames = c("Pitcher", "PT", "#", "Stuff", "Desc Score", "Pred Score"),
      rownames = FALSE
    )
  )

  viz_reactive <- function(slot) {
    shiny::eventReactive(
      eventExpr = input[[glue::glue("update_visualization_{slot}")]],
      valueExpr = {
        pitcher_id <- as.integer(input[[glue::glue("pitcher_id_{slot}")]])
        pitch_type <- input[[glue::glue("pitch_type_viz_{slot}")]]
        model <- predpitchscore::shiny_model_2023[[pitch_type]]
        shiny::validate(
          shiny::need(
            expr = pitcher_id %in% model$pitcher_hand$pitcher_id,
            message = glue::glue("Sorry, we didn't observe enough {pitch_type} from this pitcher.")
          )
        )
        list(
          pitcher_id = pitcher_id,
          pitch_type = pitch_type,
          plot_type = tolower(input[[glue::glue("plot_type_{slot}")]]),
          bat_side = input[[glue::glue("bat_side_{slot}")]],
          balls = as.integer(substring(input[[glue::glue("count_{slot}")]], 1, 1)),
          strikes = as.integer(substring(input[[glue::glue("count_{slot}")]], 3, 3)),
          show_data = input[[glue::glue("show_data_{slot}")]]
        )
      }
    )
  }

  viz_output <- function(viz_args) {
    shiny::renderPlot(
      expr = {
        if (viz_args()$show_data == "Yes") {
          data <- predpitchscore::shiny_data_2023 |>
            dplyr::filter(
              pitcher_id == viz_args()$pitcher_id,
              pitch_type == viz_args()$pitch_type,
              bat_side == viz_args()$bat_side,
              pre_balls == viz_args()$balls,
              pre_strikes == viz_args()$strikes
            )
        } else {
          data <- NULL
        }
        predpitchscore::visualize_pitch_distrib(
          model = predpitchscore::shiny_model_2023[[viz_args()$pitch_type]],
          pitcher_id = viz_args()$pitcher_id,
          plot_type = viz_args()$plot_type,
          data = data,
          bat_side = viz_args()$bat_side,
          pre_balls = viz_args()$balls,
          pre_strikes = viz_args()$strikes
        )
      },
      height = 450,
      width = 360
    )
  }

  viz_args_1 <- viz_reactive(1)
  viz_args_2 <- viz_reactive(2)

  output$visualization_1 <- viz_output(viz_args_1)
  output$visualization_2 <- viz_output(viz_args_2)
}