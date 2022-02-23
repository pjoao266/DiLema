if(!require(shiny)) install.packages('shiny');require(shiny)
if(!require(htmltools)) install.packages('htmltools');require(htmltools)
if(!require(dplyr)) install.packages('dplyr');require(dplyr)
if(!require(stringr)) install.packages('stringr');require(stringr)
if(!require(shinyjs)) install.packages('shinyjs');require(shinyjs)


shinyUI(fluidPage(
  theme = bslib::bs_theme(version = 4),
  title = "DiLema",
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$link(rel = "shortcut icon", href = "favicon.ico"),
  tags$link(rel = "shortcut icon", href = "favicon.ico"),
  tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "2.png"),
  tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "2.png"),
  tags$head(HTML("<script type='text/javascript' src='script.js'></script>")),
  div(
    class = "guesses",
    h3("DiLema"),
    uiOutput("current_guess"),
    uiOutput("endgame"),
    uiOutput("new_game_ui")
  ),
  useShinyjs(),
  uiOutput("keyboard")
  
))
