#' @export
to_ton <- function(x) {
  x/1000
}

#' @export
br_format <- scales::label_comma(
  accuracy = 0.1,
  big.mark = ".",
  decimal.mark = ","
)

#' @export
perc_format <- function(perc) {
  ifelse(
    perc < 1,
    "< 1%",
    sprintf("%s%%", br_format(perc)))
}
