#' This function creates endpoints and appends values to behavior data tables
#' @param chemnames this is a vector of chemical names as strings. Napmes are
#'   spelled as they appear in "DNT project sample sizes after SB
#'   corrections_use this one_BH edit.xlsx"
#' @import data.table
create_Endp <- function (chemnames) {
                for (name in chemnames) {
                  # remove observations where there are gaps in movement data
                  sheet <- get(name) #%>%
                            #dplyr::filter(dplyr::across(c("t_02":"t_98100"), ~ !is.na(.x)))

                  #--------------------
                  # Aggregate Movement
                  #--------------------
                  # Create aggregate movement endpoints, SUM and AUC, over dark, light, and total time intervals
                  # Create matrix of endpoint titles and their corresponding interval
                  title <- c("SUM_L", "SUM_D", "SUM_T", "AUC_L", "AUC_D", "AUC_T")
                  interval <- rep(list(names(`sheet`)[16:35], names(`sheet`)[36:55], names(`sheet`)[16:55]),
                                  2)
                  endpoints <- as.matrix(cbind(title, interval))
                  # Create SUM endpoints
                  invisible(apply(endpoints, 1, function(endp) {
                                                  `sheet`[, endp$`title`:=rowSums(.SD), .SDcols=endp$`interval`]}))
                  # Create AUC endpoints
                  invisible(apply(endpoints[4:6,], 1,
                                  function(endp) {
                                    `sheet`[, endp$`title`:=
                                                (rowSums(.SD[, endp$`interval`[3:length(endp$`interval`)-1], with=FALSE])+
                                                (0.5*rowSums(.SD[, endp$`interval`[c(1,length(endp$`interval`))], with=FALSE]))),
                                    .SDcols=endp$interval]}))
                  assign(name,
                         sheet,
                         envir = globalenv())
                }
              }
