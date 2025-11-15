welsh_dict <- data.frame(
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

ask <- function(input){
response <- 0
while (response != "stop" & sum(welsh_dict$prob) > 0) {
  word <- welsh_dict[sample(1:nrow(welsh_dict), 1, prob = welsh_dict$prob), "welsh"]
  print(word)
  ready <- readline(prompt="hit any key when you are ready for the translation: ")
  if (!is.na(ready)) {
    translation <- welsh_dict[welsh_dict$welsh==word, "english"]
    print(translation)
    ready <- NA
  }
  prob <- welsh_dict$prob[welsh_dict$welsh==word]

  response <- readline(prompt="correct (1) or incorrect (-1)?: ")

  if (response == -1) {
    welsh_dict$wrong[welsh_dict$welsh==word] <- welsh_dict$wrong[welsh_dict$welsh==word] + 1
  } else {
    welsh_dict$right[welsh_dict$welsh==word] <- welsh_dict$right[welsh_dict$welsh==word] + 1
  }

  if (prob > 0.2) {
    welsh_dict$prob[welsh_dict$welsh==word] <- prob - as.numeric(response)/5
  } else {
    welsh_dict$prob[welsh_dict$welsh==word] <- 0
  }
}
print(welsh_dict)
}
