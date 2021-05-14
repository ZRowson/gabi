#' Create endpoints from pmr0 formatted table
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Last edit: 04/06/2021
#'
#' Returns complete USA EPA ORD CCTE BCTD RADB Padilla lab DNT60 data
#' set in pmr0 format. pmr0 format is easily transformed
#' into mc0 with DNT.60.Analysis::as_mc0() for tcpl analysis.
#'
#' @param data is a pmr0 formatted data.table
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id
#'     \item cpid - chemical name
#'     \item apid - assay plate id
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'     \item wllq - well quality indicates if observation is viable for analysis
#'     \item conc - concentration of chemical
#'     \item tj - measurements at time period j for j /in {1,2,3,...,n}
#'   }
#' @param no.A number of measurements made in acclimation period
#' @param no.L number of measurements made in light period
#' @param no.D number of measurements made in dark period
#'
#' @return A table with endpoint measurements
#'   \itemize{
#'     \item ...first columns same as pmr0 format minus tj data
#'     \item
#'   }
#'   @import data.table
calc_endp <- function (data, no.A = 10, no.L = 20, no.D = 20) { # Goal of no.A etc is to generalize this function so user can specify the experimental methods
                table <- data.table::copy(data)

                #--------------------
                # Aggregate Movement
                #--------------------
                # Create matrix of endpoint titles and their corresponding interval
                  title <- c("SUM_L", "SUM_D", "SUM_T", "AUC_L", "AUC_D", "AUC_T")
                  light <-
                    interval <- rep(list(names(table[,vt11:vt30]), names(table[,vt30:vt50]), names(table[,vt11:vt50])), 2)
                  endpoints <- as.matrix(cbind(title, interval))

                # return(endpoints)
                # Create SUM endpoints
                  apply(endpoints, 1,
                        function(endp) table[, endp$`title`:=rowSums(.SD), .SDcols=endp$`interval`]
                        ) %>%
                    invisible()

                # Create AUC endpoints
                 apply(endpoints[4:6,], 1, function(endp) {
                                              table[, endp$`title`:=
                                                (rowSums(.SD[, endp$`interval`[3:length(endp$`interval`)-1], with=FALSE])+
                                                (0.5*rowSums(.SD[, endp$`interval`[c(1,length(endp$`interval`))], with=FALSE]))),
                                                .SDcols=endp$interval]
                                            }
                       ) %>%
                   invisible()

                #------------------------------
                # Immediate change in activity
                #------------------------------

                # Can do this a couple ways, simply calculate the difference between the two values or you could see how much
                # the value changes relative to the amount fish were already moving

                table <- table[, -(vt1:vt50)]

                return(table)
}