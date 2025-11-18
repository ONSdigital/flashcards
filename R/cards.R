#
# welsh_dict <- data.frame(
#   welsh = c(
#     "cymoedd",
#     "cymryd",
#     "cynnwys",
#     "digon",
#     "fel arfer",
#     "hysbyseb",
#     "o'r gorau",
#     "trefnu",
#     "yn erbyn"
#   ),
#   english = c(
#     "valleys",
#     "to take",
#     "to include",
#     "enough",
#     "usually",
#     "advert",
#     "ok, alright",
#     "to organise",
#     "against"
#   ),
#   wrong = 0,
#   right = 0,
#   prob = 0.5
# )
# write.csv(welsh_dict, "D://welsh_dict.csv", row.names = FALSE)
dict <- read.csv("D://welsh_dict.csv")

cardsUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      textInput(ns("welsh"), "Enter Welsh word"),
      textInput(ns("english"), "Enter English translation"),
      numericInput(ns("prob"),
                   "Confidence with word (0.9: v. high, 0.1: v. low):",
                   0.5,
                   min = 0.1, max = 0.9, step = 0.1),
      actionButton(ns("add_btn"), "Add to dictionary"),
    ),
    card(
      actionButton(ns("save_btn"), "Save dictionary")
    ),
    card(
      DTOutput(ns("shiny_table"))
    ),
    col_widths = c(12, 12, 12)
  )
}

cardsServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {
      dict <- reactiveVal(dict)

      observeEvent(input$add_btn, {
        t = rbind(
          data.frame(
            welsh = input$welsh,
            english = input$english,
            wrong = 0,
            right = 0,
            prob = round(1-input$prob, 1)
            ),
          dict()
          )
        dict(t)
      })

      output$shiny_table <- renderDT({
        datatable(
          dict(),
          selection = 'single',
          options = list(dom = 't')
          )
      })

      observeEvent(input$save_btn, {
        write.csv(dict(), "D://welsh_dict.csv", row.names = FALSE)
      })
    }
  )}
