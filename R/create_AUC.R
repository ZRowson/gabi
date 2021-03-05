#' This function creates SUM response tables
#' @param chemnames this is a vector of chemical names as strings. Names are
#' spelled as they appear in
#' "DNT project sample sizes after SB corrections_use this one_BH edit.xlsx"
#' @import data.table
create_SUM <- function (chemnames) {
                for (name in chemnames) {
                sheet <- get(name) %>%
                          dplyr::filter(dplyr::across(c("t_02":"t_98100"), ~ !is.na(.x)))
                sheet[, SUM_L := rowSums(.SD), .SDcols = 16:35]
                sheet[, SUM_D := rowSums(.SD), .SDcols = 36:55]
                assign(paste0("SUM.", name),
                       sheet[, -c(6:55)],
                       envir = globalenv())
                }
              }
