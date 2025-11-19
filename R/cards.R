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
#   weight = 0.5
# )
# write.csv(welsh_dict, "D://welsh_dict.csv", row.names = FALSE)
dict <- read.csv("D://welsh_dict.csv")

cardsUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      textInput(ns("welsh"), "Enter Welsh word"),
      textInput(ns("english"), "Enter English translation"),
      numericInput(ns("weight"),
                   "Confidence with word (1: full, 0: none):",
                   0.5,
                   min = 0, max = 1, step = 0.1),
      actionButton(ns("add_btn"), "Add to dictionary"),
    ),
    card(
      actionButton(ns("save_btn"), "Save dictionary"),
      textOutput(ns("save_msg")),
      textOutput(ns("repeat_msg")),
      textOutput(ns("repeat_suggestion"))
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
        if (
          all(
            input$welsh %in% dict()$welsh == FALSE,
            input$english %in% dict()$english == FALSE
            )
        ) {
          output$repeat_msg <- renderText("")
          output$repeat_suggestion <- renderText("")
          output$save_msg <- renderText("")

          updateTextInput(session, "welsh", value = "")
          updateTextInput(session, "english", value = "")
          updateNumericInput(session, "weight", value = 0.5)

          t = rbind(
            data.frame(
              welsh = input$welsh,
              english = input$english,
              wrong = 0,
              right = 0,
              weight = ifelse(input$weight == 1,
                              round(1.1-input$weight, 1),
                              round(1-input$weight, 1))
            ),
            dict()
          )
          dict(t)

        } else {

          welsh_in <- input$welsh %in% dict()$welsh
          english_in <- input$english %in% dict()$english
          existing <- c()
          if(welsh_in) {existing <- c(existing, "Welsh")}
          if(english_in) {existing <- c(existing, "English")}
          existing_text <- paste0(existing, collapse = (", "))

          output$repeat_msg <- renderText(
            paste0("There is already an entry for the given: ", existing_text)
          )
          output$repeat_suggestion <- renderText(
            "If the word is slighly different in meaning, edit the translation to reflect the difference."
            )

        }

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
        output$save_msg <- renderText("dictionary saved in D://welsh_dict.csv")
      })

    }
  )}
