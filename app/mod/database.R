box::use(
  pool[dbPool, poolClose],
  RPostgres[Postgres],
  shiny[onStop]
)

#' @export
obsr <- dbPool(
  Postgres(),
  dbname = "obsr",
  host = "0.0.0.0",
  user = "postgres",
  password = "changeme"
)
onStop(function() {
  poolClose(obsr)
})
