ui <- shiny::fluidPage(
  shiny::titlePanel("2023 MLB Predictive Pitch Score Leaderboard"),
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
    shiny::tabPanel(title = "Visualization",
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::selectInput(
            inputId = "pitcher",
            label = "Pitcher",
            choices = sort(unique(predpitchscore::leaderboard_2023$pitcher_id))
          ),
          shiny::radioButtons(
            inputId = "pitch_type_viz",
            label = "Pitch Type",
            choices = c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS"),
            selected = "FF",
            inline = TRUE
          ),
          shiny::radioButtons(
            inputId = "bat_side",
            label = "Batter Side",
            choices = c("L", "R"),
            selected = "R",
            inline = TRUE
          ),
          shiny::numericInput(
            inputId = "balls",
            label = "Balls",
            value = 0,
            min = 0,
            max = 3,
            step = 1
          ),
          shiny::numericInput(
            inputId = "strikes",
            label = "Strikes",
            value = 0,
            min = 0,
            max = 2,
            step = 1
          ),
          shiny::actionButton(inputId = "update_visualization", label = "Update")
        ),
        mainPanel = shiny::mainPanel(
          shiny::plotOutput(outputId = "visualization")
        )
      )
    )
  )
)