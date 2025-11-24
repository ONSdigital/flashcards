library(blogdown)
library(shinyFiles)
library(bslib)
library(fs)
library(stringr)
library(shinycssloaders)
library(dplyr)

current_dir <- file.path("D:/", "coding_repos", "flashcards")
dict <- data.frame(
  welsh = NA, english = NA, gender = NA, right = NA, wrong = NA, weight = NA
)

get_reverse_frequency <- function(dat) {
  dat <- read.csv("D://welsh_dict.csv")


}

show_progress <- function(dat, max_y) {
  weight_counts <- dat %>%
    mutate(inverse_weight = 1 - weight) %>%
    group_by(inverse_weight) %>%
    count()

  freq <- weight_counts %>%
    mutate(proportion = ifelse(
      n > 0,
      n/sum(weight_counts$n),
      0
      ))

  ggplot(data = freq, aes(x = inverse_weight)) +
    geom_col(aes(y = proportion), fill = "darkgreen") +
    xlim(0, 1) + ylim(0,1) +
    ggtitle("words at 1 on the x axis are done") +
    theme_classic()

}

playUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      textInput(ns("dict_path"), NULL, "D://welsh_dict.csv"),
    ),
    card(
      actionButton(ns("path_btn"), "Set filepath to get dictionary"),
    ),
    card(
      textOutput(ns("row_count")),
      numericInput(
        ns("num_words"),
        "Number of least known words to practice:", ns("row_count"),
        min = 2, max = NA),
      actionButton(ns("set_words"), "set words"),
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
      actionButton(ns("update_btn"), "Save progress"),
      textOutput(ns("update_msg")),
      plotOutput(ns("progress_plot")),
      DTOutput(ns("table"))
    ),
    col_widths = c(6, 6, 12, 6, 6, 12)
  )
}

playServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {

      all_rows <- reactiveVal()
      dict <- reactiveVal({ distinct(dict) })
      curr_word <- reactiveVal(as.character())
      from_this <- reactiveVal()
      to_this <- reactiveVal()
      speed <- reactiveVal()
      row_count <- reactiveVal(as.character())

      observeEvent(input$path_btn, {
        file_path <- input$dict_path
        dict <- dict(read.csv(as.character(file_path)))
        row_count <- row_count(nrow(dict()))
        all_rows <- all_rows(nrow(read.csv(as.character(file_path))))
        output$row_count <- renderText(paste0("max: ", row_count()))
      })

      observeEvent(input$set_words, {
        dict <- dict(
          slice_max(dict(), weight, n = input$num_words)
        )
        if (nrow(dict()) > input$num_words) {
          dict <- dict(
            arrange(dict(), weight) %>%
              slice_head(n = input$num_words)
          )
        }
        output$table <- renderDT(dict(), options = list(lengthChange = FALSE))
        f_plot <- show_progress(dict())

        output$progress_plot <- renderPlot(f_plot)
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
          dict()[
            sample(1:nrow(dict()),
                   1,
                   prob = dict()$weight), from_this()
            ]
        )

        output$curr_word <- renderText({curr_word()})

      })

      # hit button for correct answer
      observeEvent(input$assess_btn, {
        curr_word <- curr_word()
        translation <- dict() %>%
          filter(!!sym(from_this()) == curr_word()) %>%
          select(!!sym(to_this())) %>%
          pull(!!sym(to_this()))
        output$translation <- renderText(translation)
        output$complete <- renderText("")
      })

      # update the dict with a correct answer
      observeEvent(input$right_button, {

        output$result <- renderText("")

        probability <- dict() %>%
          filter(!!sym(from_this())==curr_word()) %>%
          pull(weight)

        dict(
          dict() %>%
            mutate(right =
                     ifelse(
                       !!sym(from_this()) == curr_word(),
                       right + 1,
                       right
                     )
            )
        )

        if (probability > 0.3) {
          dict(
            mutate(
              dict(),
              weight =
                ifelse(
                  !!sym(from_this()) == curr_word(),
                  round(weight - 0.3, 1),
                  round(weight, 1)
                )
            )
          )


        } else {

          dict(
            dict() %>%
              mutate(weight =
                       ifelse(
                         !!sym(from_this()) == curr_word(),
                         0,
                         weight
                       )
              )
          )
        }
        if (sum(dict()$weight) < 0.3) {
          output$complete <- renderText(
            "Done! All words marked as high confidence. Probabilities reset based on count of guesses.")

          unique_right_counts <- unique(dict()$right)
          right_equal <- length(unique_right_counts) == 1
          unique_wrong_counts <- unique(dict()$wrong)
          wrong_equal = length(unique_wrong_counts) == 1

          dict(
            dict() %>%
              rowwise() %>%
              mutate(guesses = right + wrong,
                     weight = ifelse(all(wrong_equal, right_equal),
                                     0.5,
                                     round(1-(right/max(guesses)), 1)),
                     right = 0,
                     wrong = 0) %>%
              ungroup() %>%
              select(-guesses)
          )
        }
        summary <- dict() %>%
          select(-!!sym(to_this()))

        output$timer <- renderText(
          paste0("Timer: ", sum(dict()$weight))
        )
        output$result <- renderText("Well done!")
        output$table <- renderDT(summary, options = list(lengthChange = FALSE))
        f_plot <- show_progress(summary)
        output$progress_plot <- renderPlot(f_plot)
      })

      # update the dict with a wrong answer
      observeEvent( input$wrong_button, {

        output$result <- renderText("")

        probability <- dict() %>%
          filter(!!sym(from_this())==curr_word()) %>%
          pull(weight)

        dict(
          dict() %>%
            mutate(wrong =
                     ifelse(
                       !!sym(from_this()) == curr_word(),
                       wrong + 1,
                       wrong
                     )
            )
        )

        # constrain the weight to 1
        if (probability < 0.7) {
          dict(
            mutate(
              dict(),
              weight =
                ifelse(
                  !!sym(from_this()) == curr_word(),
                  round(weight + 0.3, 1),
                  weight
                )
            )
          )
        } else {
          dict(
            mutate(
              dict(),
              weight =
                ifelse(
                  !!sym(from_this()) == curr_word(),
                  1,
                  weight
                )
            )
          )
        }

        summary <- dict() %>%
          select(-!!sym(to_this()))

        output$timer <- renderText(
          paste0("Timer: ", sum(dict()$weight))
                 )
        output$result <- renderText("Better luck next time")
        output$table <- renderDT(summary, options = list(lengthChange = FALSE))

        f_plot <- show_progress(summary)
        output$progress_plot <- renderPlot(f_plot)


      })

      observeEvent(input$update_btn, {
        if (as.numeric(all_rows()) <= nrow(dict())) {
          write.csv(dict(), input$dict_path, row.names = FALSE)
          output$update_msg <- renderText(
            paste0("dictionary saved in '", input$dict_path, "'.")
          )
        } else {
          output$update_msg <- renderText(
            "dictionary not saved because only a subset were practiced."
            )

        }

      })
    }



  )
}


