dict <- data.frame(
  welsh = NA, english = NA, gender = NA, right = NA, wrong = NA, weight = NA
  )

cardsUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      textInput(ns("dict_path"), NULL, "D://welsh.csv"),
    ),
    card(
      actionButton(ns("path_btn"), "Set filepath to get dictionary"),
      ),
    card(
      textInput(ns("welsh"), "Enter Welsh word"),
      textInput(ns("english"), "Enter English translation"),
      textInput(ns("gender"), "Enter m (masculine)/ f (feminine) for nouns"),
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
      DTOutput(ns("tab1"))
    ),
    col_widths = c(6, 6, 12, 12, 12)
  )
}

cardsServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {

      dict <- reactiveVal(dict)

      observeEvent(input$path_btn, {
        file_path <- input$dict_path
        dict <- dict(read.csv(as.character(file_path)))
      })

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
              gender = input$gender,
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

      output$tab1 <- renderDT({
        datatable(
          dict(),
          selection = 'single',
          options = list(
            pageLength = 10
          ),
          editable = TRUE
          )
      })


      observeEvent(input$tab1_cell_edit, {
        info <- input$tab1_cell_edit
        str(info)
        modified_data <- dict()
        modified_data[info$row, info$col] <- info$value
        dict(modified_data)
      })


      observeEvent(input$save_btn, {
        dict()
        write.csv(dict(), input$dict_path, row.names = FALSE)
        output$save_msg <- renderText(
          paste0("dictionary saved in '", input$dict_path, "'.")
          )
      })
    }
  )}
