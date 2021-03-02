#' This function creates AUC response tables
#' @param chemnames this is a vector of chemical names as strings. names are
#' spelled as they appear in
#' "DNT project sample sizes after SB corrections_use this one_BH edit.xlsx"
create_AUC <- function (chemnames) {
                for (name in chemnames) {
                sheet <- get(name) %>%
                          filter(across(c("t_02":"t_98100"), ~ !is.na(.x)))
                sheet[, AUC_L := rowSums(.SD), .SDcols = 16:35]
                sheet[, AUC_D := rowSums(.SD), .SDcols = 36:55]
                assign(paste0("AUC.", name), sheet[, -c(6:55)])
                }
              }
