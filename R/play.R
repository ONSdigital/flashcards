library(blogdown)
library(shinyFiles)
library(bslib)
library(fs)
library(stringr)
library(shinycssloaders)

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

      dynamic_dict <- reactive({
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

      # # update the dynamic_dict
      # observeEvent(input$result, {
      #
      #   prob <- dynamic_dict()$prob[dynamic_dict()$welsh==word()]
      #   if (input$result == -1) {
      #     dynamic_dict$wrong[dynamic_dict()$welsh==word()] <- 1 +
      #       dynamic_dict$wrong[dynamic_dict()$welsh==word()]
      #   } else {
      #     dynamic_dict$right[dynamic_dict()$welsh==word()] <- 1 +
      #       dynamic_dict$right[dynamic_dict()$welsh==word()]
      #   }
      #
      #   if (prob > 0.2) {
      #     dynamic_dict$prob[dynamic_dict()$welsh==word()] <- prob
      #     - as.numeric(input$result)/5
      #   } else {
      #     dynamic_dict$prob[dynamic_dict()$welsh==word()] <- 0
      #   }
      # })

      # state whether your guess was correct
      output$value <- renderText({
        paste("You chose: ", input$result)
      })


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


