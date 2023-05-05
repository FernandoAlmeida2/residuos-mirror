to_ton <- function(x) {
  x/1000
}

br_format <- scales::label_comma(
  accuracy = 0.1,
  big.mark = ".",
  decimal.mark = ","
)
