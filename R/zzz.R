.onAttach <- function(...) {
  packageStartupMessage(
    emo::ji("beers"),
    crayon::white(" ezxfig "),
    crayon::cyan(utils::packageVersion("ezxfig"))
  )
}
