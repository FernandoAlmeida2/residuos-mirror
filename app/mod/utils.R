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

#' @export
trata_string <- function (word) {
  gsub(" ","", gsub("á","a",
                    gsub("é","e",
                         gsub("í","i",
                              gsub("ó","o",
                                   gsub("ú","u",
                                        gsub("ã","a",
                                             gsub("õ","o",
                                                  gsub("ç","c",
                                                       gsub("â","a",
                                                            gsub("ô","o",
                                                                 gsub("-","", word))))))))))))
}

#' @export
remove_acentos_uppercase <- function (word) {
  gsub("Á","A",
         gsub("É","E",
              gsub("Í","I",
                   gsub("Ó","O",
                        gsub("Ú","U",
                             gsub("Ã","A",
                                  gsub("Õ","O",
                                       gsub("Ç","C",
                                            gsub("Â","A",
                                                 gsub("Ô","O",word))))))))))
}
  
#' @export
day_month_br_format <- function (date) {
  paste0(substr(date, 9, 10),"/",substr(date, 6, 7), "/",substr(date, 1, 4))
}

#' @export
month_en_text <- function (date) {
  
  switch (substr(date, 6, 7),
    "01" = "Janeiro",
    "02" = "Fevereiro",
    "03" = "Março",
    "04" = "Abril",
    "05" = "Maio",
    "06" = "Junho",
    "07" = "Julho",
    "08" = "Agosto",
    "09" = "Setembro",
    "10" = "Outubro",
    "11" = "Novembro",
    "12" = "Dezembro"
  )
}

#' @export
month_br_text <- function (date) {
  
  switch (substr(date, 4, 5),
          "01" = "Janeiro",
          "02" = "Fevereiro",
          "03" = "Março",
          "04" = "Abril",
          "05" = "Maio",
          "06" = "Junho",
          "07" = "Julho",
          "08" = "Agosto",
          "09" = "Setembro",
          "10" = "Outubro",
          "11" = "Novembro",
          "12" = "Dezembro"
  )
}

months_list <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro",
                 "Outubro", "Novembro", "Dezembro")

#' @export
months_array_slice <- function (max_month) {
  
  months_list[c(1:max_month)]
}

#' @export
month_br_to_number <- function (month) {
  
  switch (month,
          "Janeiro" = 1,
          "Fevereiro" = 2,
          "Março" = 3,
          "Abril" = 4,
          "Maio" = 5,
          "Junho" = 6,
          "Julho" = 7,
          "Agosto" = 8,
          "Setembro" = 9,
          "Outubro" = 10,
          "Novembro" = 11,
          "Dezembro" = 12
  )
}

#' @export
number_to_month_br <- function (numeric_month) {
  
  months_list[numeric_month]
}