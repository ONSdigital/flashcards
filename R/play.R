library(blogdown)
library(shinyFiles)
library(bslib)
library(fs)
library(stringr)
library(shinycssloaders)
library(dplyr)

current_dir <- file.path("D:/", "coding_repos", "flashcards")
dict <- read.csv("D://welsh_dict.csv")
row_count <- nrow(dict)

playUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      numericInput(
        ns("num_words"),
        "Number of least known words to practice (default is all):", row_count),
      sliderInput(
        ns("set_speed"), "how many repeats do you want? 1=few, 8=many", 1, 8, 5
      ),
      input_switch(ns("to_welsh"), "Guess the Welsh")
      ),
    card(
      # hit button to display the next word
      actionButton(ns("button"), "Next word"),
      actionButton(ns("assess_btn"), "Get answer"),
      actionButton(ns("right_button"), "Right :)"),
      actionButton(ns("wrong_button"), "Wrong :("),
     ),
    card(
      textOutput(ns("curr_word")),
      textOutput(ns("translation")),
      textOutput(ns("result"))
    ),
    card(
      textOutput(ns("timer")),
      textOutput(ns("complete")),
      DTOutput(ns("test"))
    ),
    card(
      actionButton(ns("save_btn"), "Save progress"),
      textOutput(ns("save_msg"))
    ),
    col_widths = c(12, 6, 6, 12, 12)
  )
}

playServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {

      dynamic_dict <- reactiveVal({
        distinct(dict)
        })

      curr_word <- reactiveVal(as.character())
      from_this <- reactiveVal()
      to_this <- reactiveVal()
      speed <- reactiveVal()

      observeEvent(input$num_words, {
        dynamic_dict <- dynamic_dict(
          slice_max(dynamic_dict(), weight, n = input$num_words)
        )

      })

      observeEvent(input$set_speed, {
        speed <- speed(input$set_speed + 1)
      })

      observeEvent(input$to_welsh, {
        if (input$to_welsh == TRUE) {
          to_this <- to_this("welsh")
          from_this <- from_this("english")

        } else  if (input$to_welsh == FALSE){
          to_this <- to_this("english")
          from_this <-from_this("welsh")
        }
      })

      # hit button for next word
      observeEvent(input$button, {
        output$curr_word <- renderText("")
        output$translation <- renderText("")
        output$result <- renderText("")

        curr_word(
          dynamic_dict()[
            sample(1:nrow(dynamic_dict()),
                   1,
                   prob = dynamic_dict()$weight), from_this()
            ]
        )

        output$curr_word <- renderText({curr_word()})

      })

      # hit button for correct answer
      observeEvent(input$assess_btn, {
        curr_word <- curr_word()
        translation <- dynamic_dict() %>%
          filter(!!sym(from_this()) == curr_word()) %>%
          select(!!sym(to_this())) %>%
          pull(!!sym(to_this()))
        output$translation <- renderText(translation)
        output$complete <- renderText("")
      })

      # update the dynamic_dict with a correct answer
      observeEvent(input$right_button, {
        # output$curr_word <- renderText("")
        # output$translation <- renderText("")
        output$result <- renderText("")

        probability <- dynamic_dict() %>%
          filter(!!sym(from_this())==curr_word()) %>%
          pull(weight)

        dynamic_dict(
          dynamic_dict() %>%
            mutate(right =
                     ifelse(
                       !!sym(from_this()) == curr_word(),
                       right + 1,
                       right
                     )
            )
        )

        if (probability > 1/speed()) {
          dynamic_dict(
            mutate(
              dynamic_dict(),
              weight =
                ifelse(
                  !!sym(from_this()) == curr_word(),
                  round(weight - 1/speed(), 1),
                  round(weight, 1)
                )
            )
          )


        } else {

          dynamic_dict(
            dynamic_dict() %>%
              mutate(weight =
                       ifelse(
                         !!sym(from_this()) == curr_word(),
                         0,
                         weight
                       )
              )
          )
        }
        if (sum(dynamic_dict()$weight) < 1/speed()) {
          output$complete <- renderText(
            "Done! All words marked as high confidence. Probabilities reset based on count of guesses.")

          unique_right_counts <- unique(dynamic_dict()$right)
          right_equal <- length(unique_right_counts) == 1
          unique_wrong_counts <- unique(dynamic_dict()$wrong)
          wrong_equal = length(unique_wrong_counts) == 1

          dynamic_dict(
              mutate(dynamic_dict(),
                   guesses = right + wrong,
                   weight = ifelse(all(all_equal, right_equal),
                                   0.5,
                                   round(1-right/max(guesses), 1)),
                   right = 0,
                   wrong = 0) %>%
              select(-guesses)
          )
        }
        summary <- dynamic_dict() %>%
          select(-!!sym(to_this())) #%>%
          # filter(!!sym(from_this()) == current_word())

        output$timer <- renderText(
          paste0("Timer: ", sum(dynamic_dict()$weight))
        )
        output$result <- renderText("Well done!")
        output$test <- renderDT(summary, options = list(lengthChange = FALSE))

      })

      # update the dict with a wrong answer
      observeEvent( input$wrong_button, {

        output$result <- renderText("")

        probability <- dynamic_dict() %>%
          filter(!!sym(from_this())==curr_word()) %>%
          pull(weight)

        dynamic_dict(
          dynamic_dict() %>%
            mutate(wrong =
                     ifelse(
                       !!sym(from_this()) == curr_word(),
                       wrong + 1,
                       wrong
                     )
            )
        )

        dynamic_dict(
          mutate(
            dynamic_dict(),
            weight =
              ifelse(
                !!sym(from_this()) == curr_word(),
                round(weight + 1/speed(), 1),
                weight
              )
          )
        )

        summary <- dynamic_dict() %>%
          select(-!!sym(to_this())) #%>%
          # filter(!!sym(from_this()) == current_word())

        output$timer <- renderText(
          paste0("Total left (0 = none, 1 = all): ", sum(dynamic_dict()$weight))
                 )
        output$result <- renderText("Better luck next time")
        output$test <- renderDT(summary, options = list(lengthChange = FALSE))

      })

      observeEvent(input$save_btn, {
        write.csv(dynamic_dict(), "D://welsh_dict.csv", row.names = FALSE)
        output$save_msg <- renderText("dictionary saved in D://welsh_dict.csv")
      })


    }



  )
}


