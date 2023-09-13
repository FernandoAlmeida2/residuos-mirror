box::use(
  pool[dbPool, poolClose],
  RPostgres[Postgres],
  shiny[onStop]
)

#' @export
obsr <- dbPool(
  drv = Postgres(),
  dbname = "obsr",
  host = Sys.getenv("PGHOST"),
  user = Sys.getenv("PG_SHINY_USER"),
  password = Sys.getenv("PG_SHINY_PASSWD")
)

onStop(function() {
  poolClose(obsr)
})
