box::use(
  utils[unzip],
  sf,
)

#' @export
readKMZ <- function(filePath) {
  zip <- sprintf("%s/%s", getwd(), filePath)
  print(zip)
  unzip_files <- unzip(zipfile = zip)
  kmlfile <- unzip_files[grep(pattern = "*.kml$", unzip_files)]

  ret <-  sf$st_transform(
    sf$read_sf(kmlfile),
    '+proj=longlat +datum=WGS84'
  )
  ##   %>%
  ## mutate_if(is.character, 
  ##           function(col) iconv(col, to="UTF-8"))
  ##print("teste")

  return(ret)

}
