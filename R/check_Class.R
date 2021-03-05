#' This function tests if tables are considered data.tables in R package
#' @param chemnames
check_Class <- function(chemnames) {
  for (name in chemnames) {print(is.data.table(get(name)))}
}
