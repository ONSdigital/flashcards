library(blogdown)
library(shinyFiles)
library(bslib)
library(fs)
library(stringr)
library(shinycssloaders)
library(dplyr)

current_dir <- file.path("D:/", "coding_repos", "flashcards")
dict <- read.csv("D://welsh_dict.csv")

playUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      input_switch(ns("to_welsh"), "Guess the Welsh"),
      textOutput(ns("language")),
      textOutput(ns("from_language"))
    ),
    card(
      # hit button to display the next word
      actionButton(ns("button"), "Next word"),
      actionButton(ns("button2"), "Get answer"),
      actionButton(ns("right_button"), "Right :)"),
      actionButton(ns("wrong_button"), "Wrong :("),
     ),
    card(
      textOutput(ns("curr_word")),
      textOutput(ns("translation")),

    ),
    card(
      textOutput(ns("complete")),
      DTOutput(ns("test"))
    ),
    card(
      actionButton(ns("save_btn"), "Save progress"),
      textOutput(ns("save_msg"))
    ),
    col_widths = c(6, 6, 12, 12)
  )
}

playServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {

      dynamic_dict <- reactiveVal({dict})

      curr_word <- reactiveVal(as.character())
      from_this <- reactiveVal()
      to_this <- reactiveVal()

      observeEvent(input$to_welsh, {
        if (input$to_welsh == TRUE) {
          to_this <- to_this("welsh")
          from_this <- from_this("english")

        } else  if (input$to_welsh == FALSE){
          to_this <- to_this("english")
          from_this <-from_this("welsh")
        }
        output$language <- renderText({paste0("to: ", to_this())})
      })

      # hit button for next word
      observeEvent(input$button, {
        curr_word(
          dynamic_dict()[
            sample(1:nrow(dynamic_dict()),
                   1,
                   prob = dynamic_dict()$prob), "welsh"
            ]
        )

        output$curr_word <- renderText({curr_word()})
        output$from_language <- renderText({paste("from ", from_this())})

      })

      # hit button for correct answer
      observeEvent(input$button2, {
        curr_word <- curr_word()
        translation <- dynamic_dict()[dynamic_dict()$welsh==curr_word(), "english"]
        output$translation <- renderText(translation)
        output$complete <- renderText("")
      })

      # update the dynamic_dict with a correct answer
      observeEvent( input$right_button, {
        output$curr_word <- renderText("")
        output$translation <- renderText("")

        probability <- dynamic_dict() %>%
          filter(welsh==curr_word()) %>%
          pull(prob)

        dynamic_dict(
          dynamic_dict() %>%
            mutate(right =
                     ifelse(
                       welsh == curr_word(),
                       right + 1,
                       right
                     )
            )
        )

        if (probability > 0.2) {
          dynamic_dict(
            mutate(
              dynamic_dict(),
              prob =
                ifelse(
                  welsh == curr_word(),
                  round(prob - 1/5, 1),
                  round(prob, 1)
                )
            )
          )


        } else if (probability <= 0.2) {

          dynamic_dict(
            dynamic_dict() %>%
              mutate(prob =
                       ifelse(
                         welsh == curr_word(),
                         0,
                         prob
                       )
              )
          )
        }
        if (sum(dynamic_dict()$prob) < 0.2) {
          output$complete <- renderText(
            "Done! All words marked as high confidence. Probabilities reset based on count of guesses.")
          dynamic_dict(
              mutate(dynamic_dict(),
                   guesses = right + wrong,
                   prob = round(1-right/max(guesses), 1),
                   right = 0,
                   wrong = 0) %>%
              select(-guesses)
          )
        }
        summary <- dynamic_dict() %>%
          select(-english)
        output$test <- renderDT(summary, options = list(lengthChange = FALSE))

      })

      # update the dict with a wrong answer
      observeEvent( input$wrong_button, {

        output$curr_word <- renderText("")
        output$translation <- renderText("")

        probability <- dynamic_dict() %>%
          filter(welsh==curr_word()) %>%
          pull(prob)

        dynamic_dict(
          dynamic_dict() %>%
            mutate(wrong =
                     ifelse(
                       welsh == curr_word(),
                       wrong + 1,
                       wrong
                     )
            )
        )

        dynamic_dict(
          mutate(
            dynamic_dict(),
            prob =
              ifelse(
                welsh == curr_word(),
                round(prob + 1/5, 1),
                prob
              )
          )
        )

        output$test <- renderDT(dynamic_dict(), options = list(lengthChange = FALSE))

      })
      observeEvent(input$save_btn, {
        write.csv(dynamic_dict(), "D://welsh_dict.csv", row.names = FALSE)
        output$save_msg <- renderText("dictionary saved in D://welsh_dict.csv")
      })


    }



  )
}


