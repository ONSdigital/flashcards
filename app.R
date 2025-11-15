library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "zephyr"), #flatly and darkly are nice too
  titlePanel("Flashcards"),
  navlistPanel(
    # tabPanel(
    #   id = "cards",
    #   "Make flashcards",
    #   homeUI("id")
    # ),
    tabPanel(
      id = "play",
      "Play",
      playUI("id")
    )
  )
)

server <- function(input, output, session) {
  # cardsServer("id")
  playServer("id")
}
shinyApp(ui, server)
