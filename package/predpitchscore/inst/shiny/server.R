server <- function(input, output, session) {

  leaderboard_data <- shiny::eventReactive(
    eventExpr = input$update_leaderboard,
    valueExpr = predpitchscore::leaderboard_2023 |>
      dplyr::filter(pitch_type %in% input$pitch_types) |>
      dplyr::mutate(
        split_by_pitch_type = input$split_by_pitch_type,
        pitch_type = ifelse(split_by_pitch_type == "Yes", pitch_type, "all")
      ) |>
      dplyr::group_by(pitcher_id, player_name, pitch_type) |>
      dplyr::summarize(
        stuff = round(2000 * weighted.mean(stuff, w = n, na.rm = TRUE)),
        desc_score = round(2000 * weighted.mean(desc_pitch_score, w = n, na.rm = TRUE)),
        pred_score = round(2000 * weighted.mean(pred_pitch_score, w = n, na.rm = TRUE)),
        n = sum(n),
        .groups = "drop"
      ) |>
      dplyr::filter(n >= input$range_n[1], n <= input$range_n[2]) |>
      dplyr::select(player_name, pitch_type, n, stuff, desc_score, pred_score) |>
      dplyr::arrange(pred_score)
  )

  output$leaderboard <- DT::renderDT(
    expr = DT::datatable(
      data = leaderboard_data(),
      colnames = c("Pitcher", "PT", "#", "Stuff", "Desc Score", "Pred Score"),
      rownames = FALSE
    )
  )

  viz_args <- shiny::eventReactive(
    eventExpr = input$update_visualization,
    valueExpr = list(
      pitcher_id = input$pitcher_id,
      pitch_type = input$pitch_type,
      bat_side = input$bat_side,
      balls = input$balls,
      strikes = input$strikes
    )
  )

  output$visualization <- shiny::renderPlot(
    expr = predpitchscore::visualize_pitch_distrib(
      model = predpitchscore::pitch_distrib_model_2023[[viz_args()$pitch_type]],
      pitcher_id = viz_args()$pitcher_id,
      data = predpitchscore::data_2023 |>
        dplyr::filter(
          pitcher_id == viz_args()$pitcher_id,
          pitch_type == viz_args()$pitch_type,
          bat_side == viz_args()$bat_side,
          pre_balls == viz_args()$balls,
          pre_strikes == viz_args()$strikes
        ),
      bat_side = viz_args()$bat_side,
      pre_balls = viz_args()$balls,
      pre_strikes = viz_args()$strikes
    )
  )
}