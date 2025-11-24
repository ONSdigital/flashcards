library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "zephyr"), #flatly and darkly are nice too
  titlePanel("Flashcards"),
  navlistPanel(

    tabPanel(
      id = "play",
      "Play",
      playUI("id")
    ),
    tabPanel(
      id = "cards",
      "Make flashcards",
      cardsUI("id")
    )
  )
)

server <- function(input, output, session) {
  playServer("id")
  cardsServer("id")
}
shinyApp(ui, server)
