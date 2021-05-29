.onAttach <- function(...) {
  packageStartupMessage(
    emo::ji("beers"),
    crayon::white(" ezxfig "),
    crayon::cyan(packageVersion("ezxfig"))
  )
}
