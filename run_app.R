renv::refresh()

shiny::shinyAppDir(
  ".",
  options = list(
    port = 7171,
    launch.browser = TRUE
  )
)

