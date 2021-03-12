#' This function creates endpoints and appends values to behavior data tables
#' @param chemnames this is a vector of chemical names as strings. Names are
#' spelled as they appear in
#' "DNT project sample sizes after SB corrections_use this one_BH edit.xlsx"
#' @import data.table
create_Endp <- function (chemnames) {
                for (name in chemnames) {
                  # remove observations where there are gaps in it's movement data
                  sheet <- get(name) #%>%
                            #dplyr::filter(dplyr::across(c("t_02":"t_98100"), ~ !is.na(.x)))

                  #--------------------
                  # Aggregate Movement
                  #--------------------
                  # Create aggregate movement endpoints, SUM, and AUC in dark, light, and complete time intervals
                  # Create matrix of endpoint names and their corresponding intervals
                  names <- c("SUM_L", "SUM_D", "SUM_T", "AUC_L", "AUC_D", "AUC_T")
                  intervals <- rep(list(names(`sheet`)[16:35],
                                    names(`sheet`)[36:55],
                                    names(`sheet`)[16:55]), 2)
                  endpoints <- as.matrix(cbind(names, intervals))
                  # Create SUM endpoints
                  apply(endpoints, 1, function(x) {
                    `sheet`[, x$`names` := rowSums(.SD), .SDcols = x$`intervals`]
                  })
                  # Create AUC endpoints
                  sheet[, SUM_1 := rowSums(.SD), .SDcols = sapply(seq(from=17, to=34, by= 1),
                                                                 function (x) paste0("t_", x))] %>%
                    .[, SUM_2 := 0.5*rowSums(.SD), .SDcols = c("t_16", "t_35")] %>%
                    .[, AUC_L := rowSums(.SD), .SDcols = c("SUM_1", "SUM_2")] %>%
                    .[, `:=` (SUM_1=NULL, SUM_2=NULL)] %>%
                    .[, SUM_1 := rowSums(.SD), .SDcols = sapply(seq(from=21, to=39, by= 1),
                                                                function (x) paste0("t_", x))] %>%
                    .[, SUM_2 := 0.5*rowSums(.SD), .SDcols = c("t_20", "t_40")] %>%
                    .[, AUC_D := rowSums(.SD), .SDcols = c("SUM_1", "SUM_2")] %>%
                    .[, `:=` (SUM_1=NULL, SUM_2=NULL)]

                  assign(name,
                         sheet,
                         envir = globalenv())
                }
              }
