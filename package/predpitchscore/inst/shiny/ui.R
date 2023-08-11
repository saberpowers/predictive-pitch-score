viz_tab <- function(slot) {

  pitcher_table <- predpitchscore::leaderboard_2023 |>
    dplyr::distinct(pitcher_id, player_name)
  pitcher_id_vec <- pitcher_table$pitcher_id
  names(pitcher_id_vec) <- pitcher_table$player_name

  shiny::tabPanel(title = glue::glue("Visualization (Slot {slot})"),
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        shiny::selectInput(
          inputId = glue::glue("pitcher_id_{slot}"),
          label = "Pitcher",
          choices = pitcher_id_vec
        ),
        shiny::radioButtons(
          inputId = glue::glue("pitch_type_viz_{slot}"),
          label = "Pitch Type",
          choices = c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS"),
          selected = "FF",
          inline = TRUE
        ),
        shiny::radioButtons(
          inputId = glue::glue("plot_type_{slot}"),
          label = "Plot Type",
          choices = c("Plate", "Break"),
          selected = "Plate",
          inline = TRUE
        ),
        shiny::radioButtons(
          inputId = glue::glue("bat_side_{slot}"),
          label = "Batter Side",
          choices = c("L", "R"),
          selected = "R",
          inline = TRUE
        ),
        shiny::selectInput(
          inputId = glue::glue("count_{slot}"),
          label = "Count",
          choices = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
        ),
        shiny::radioButtons(
          inputId = glue::glue("show_data_{slot}"),
          label = "Show Actual Pitches",
          choices = c("Yes", "No"),
          selected = "Yes",
          inline = TRUE
        ),
        shiny::actionButton(inputId = glue::glue("update_visualization_{slot}"), label = "Update")
      ),
      mainPanel = shiny::mainPanel(
        shiny::plotOutput(outputId = glue::glue("visualization_{slot}"))
      )
    )
  )
}


ui <- shiny::fluidPage(
  shiny::titlePanel("2023 MLB Predictive Pitch Score"),
  shiny::tabsetPanel(
    shiny::tabPanel(title = "Leaderboard",
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::selectInput(
            inputId = "pitch_types",
            label = "Pitch Types",
            choices = c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS"),
            selected = c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS"),
            multiple = TRUE
          ),
          shiny::sliderInput(
            inputId = "range_n",
            label = "Range for # of Pitches",
            min = 0,
            max = 4000,
            value = c(0, 4000)
          ),
          shiny::radioButtons(
            inputId = "split_by_pitch_type",
            label = "Split by Pitch Type?",
            choices = c("Yes", "No"),
            selected = "Yes",
            inline = TRUE
          ),
          shiny::actionButton(inputId = "update_leaderboard", label = "Update")
        ),
        mainPanel = shiny::mainPanel(
          DT::DTOutput(outputId = "leaderboard")
        )
      )
    ),
    viz_tab(1),
    viz_tab(2)
  )
)