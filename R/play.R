library(blogdown)
library(shinyFiles)
library(bslib)
library(fs)
library(stringr)
library(shinycssloaders)

dict <- data.frame(
  welsh = c(
    "digon",
    "cymoedd",
    "cymryd",
    "cynnwys",
    "trefnu",
    "yn erbyn",
    "o'r gorau",
    "hysbyseb",
    "fel arfer"
  ),
  english = c(
    "enough",
    "valleys",
    "to take",
    "to include",
    "to organise",
    "against",
    "ok, alright",
    "advert",
    "usually"
  ),
  wrong = 0,
  right = 0,
  prob = 0.5
)

# guess <- 0
current_dir <- file.path("D:/", "coding_repos", "flashcards")
# source("R/flashcards.R")

playUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "2 columns",

    column(
      width = 10,
      # hit button to display the next word
      actionButton(ns("button"), "Next word"),
      textOutput(ns("word")),

      actionButton(ns("button2"), "Get answer"),
      textOutput(ns("translation")),

      fluidRow(
        # state whether your guess was correct
        radioButtons(ns("result"), "choose one:",
                     choiceNames = list(
                       "right",
                       "wrong"
                       ),
                     choiceValues = list(
                       1, -1
                     )
        ),
        textOutput(ns("value"))

        # end of fluidRow:
      )
      # end of column (width 10):
    ),
    p(""),

  )
}

playServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {

      # hit button for next word
      observeEvent(input$button, {
        word <- dict[
          sample(1:nrow(dict), 1, prob = dict$prob), "welsh"
        ]
        output$word <- renderText({word})

        # hit button for correct answer
        observeEvent(input$button2, {
          translation <- dict[dict$welsh==word, "english"]
          output$translation <- renderText(translation)
        })

        # update the dict
        observeEvent(input$result, {

          prob <- dict$prob[dict$welsh==word]
          if (result == -1) {
            dict$wrong[dict$welsh==word] <- dict$wrong[dict$welsh==word] + 1
          } else {
            dict$right[dict$welsh==word] <- dict$right[dict$welsh==word] + 1
          }

          if (prob > 0.2) {
            dict$prob[dict$welsh==word] <- prob - as.numeric(response)/5
          } else {
            dict$prob[dict$welsh==word] <- 0
          }
        })
      })

      # state whether your guess was correct
      output$value <- renderText({
        paste("You chose: ", input$result)
      })


    }

    # function() {
    #   response <- 0
    #   while (response != "stop" & sum(dict$prob) > 0) {
    #     word <- dict[sample(1:nrow(dict), 1, prob = dict$prob), "welsh"]
    #     print(word)
    #     ready <- readline(prompt="hit any key when you are ready for the translation: ")
    #     if (!is.na(ready)) {
    #       translation <- dict[dict$welsh==word, "english"]
    #       print(translation)
    #       ready <- NA
    #     }
    #     prob <- dict$prob[dict$welsh==word]
    #
    #     response <- readline(prompt="correct (1) or incorrect (-1)?: ")
    #
    #     if (response == -1) {
    #       dict$wrong[dict$welsh==word] <- dict$wrong[dict$welsh==word] + 1
    #     } else {
    #       dict$right[dict$welsh==word] <- dict$right[dict$welsh==word] + 1
    #     }
    #
    #     if (prob > 0.2) {
    #       dict$prob[dict$welsh==word] <- prob - as.numeric(response)/5
    #     } else {
    #       dict$prob[dict$welsh==word] <- 0
    #     }
    #   }
    #   print(dict)
    # }

  )
}


