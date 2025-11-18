library(blogdown)
library(shinyFiles)
library(bslib)
library(fs)
library(stringr)
library(shinycssloaders)
library(dplyr)

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
      textOutput(ns("curr_word")),

      actionButton(ns("button2"), "Get answer"),
      textOutput(ns("translation")),

      tableOutput(ns("test")),

      fluidRow(
        # state whether your guess was correct
        radioButtons(ns("result"), "choose one:",
                     choiceNames = list(
                       "reset",
                       "right",
                       "wrong"
                       ),
                     choiceValues = list(
                       0, 1, -1
                     )
        ),
        textOutput(ns("value"))

        # end of fluidRow:
      )
      # end of column (width 10):
    ),
    p("")

  )
}

playServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {

      dynamic_dict <- reactiveVal({
        data.frame(
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
        )}
      )

      curr_word <- reactiveVal(as.character())
      # translation <- reactiveVal(NULL)


      # hit button for next word
      observeEvent(input$button, {
        curr_word(dynamic_dict()[sample(1:nrow(dynamic_dict()),
                                        1,
                                        prob = dynamic_dict()$prob), "welsh"]
                  )

        output$curr_word <- renderText({curr_word()})

      })

      # hit button for correct answer
      observeEvent(input$button2, {
        curr_word <- curr_word()
        translation <- dynamic_dict()[dynamic_dict()$welsh==curr_word(), "english"]
        output$translation <- renderText(translation)
      })

      # update the dynamic_dict
      observeEvent(input$result, {

        if (input$result != 0){

          probability <- dynamic_dict() %>%
            filter(welsh==curr_word()) %>%
            pull(prob)


          if (input$result == -1) {

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

          } else if (input$result == 1) {

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


          }

            if (probability > 0.2) {
              dynamic_dict(
                mutate(
                  dynamic_dict(),
                  prob =
                    ifelse(
                      welsh == curr_word(),
                      prob - as.numeric(input$result)/5,
                      prob
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
          output$test <- renderTable(dynamic_dict())

        }
      })

      # # state whether your guess was correct
      # output$value <- renderText({
      #   paste("Proability: ",
      #         probability
      #   )
      # })

      }


    # function() {
    #   response <- 0
    #   while (response != "stop" & sum(dynamic_dict$prob) > 0) {
    #     word <- dynamic_dict[sample(1:nrow(dynamic_dict), 1, prob = dynamic_dict$prob), "welsh"]
    #     print(word)
    #     ready <- readline(prompt="hit any key when you are ready for the translation: ")
    #     if (!is.na(ready)) {
    #       translation <- dynamic_dict[dynamic_dict$welsh==word, "english"]
    #       print(translation)
    #       ready <- NA
    #     }
    #     prob <- dynamic_dict$prob[dynamic_dict$welsh==word]
    #
    #     response <- readline(prompt="correct (1) or incorrect (-1)?: ")
    #
    #     if (response == -1) {
    #       dynamic_dict$wrong[dynamic_dict$welsh==word] <- dynamic_dict$wrong[dynamic_dict$welsh==word] + 1
    #     } else {
    #       dynamic_dict$right[dynamic_dict$welsh==word] <- dynamic_dict$right[dynamic_dict$welsh==word] + 1
    #     }
    #
    #     if (prob > 0.2) {
    #       dynamic_dict$prob[dynamic_dict$welsh==word] <- prob - as.numeric(response)/5
    #     } else {
    #       dynamic_dict$prob[dynamic_dict$welsh==word] <- 0
    #     }
    #   }
    #   print(dynamic_dict)
    # }

  )
}


