#' Calculate endpoint from pmr0 formatted table
#'
#' Calculates endpoint from pmr0 time-series response
#' data.Endpoints are used as measure of response for
#' tcpl analysis. calc_endp is a wrapper function for
#' various other funcitons of calc_ family. Endpoint
#' values fill rval column in mc0 table format after
#' conversion with DNT.60.Analysis::as_mc02.
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Details on how endpoints are calculated can be found
#' by searching for appropriate calc_ funciton (?calc_).
#'
#' Last edit 06/07/21.
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
#' @param no.A - number of measurements made in acclimation period
#' @param no.L - number of measurements made in light period
#' @param no.D - number of measurements made in dark period
#'
#' @param rval - string representing endpoint of interest. Can equal...
#'   \itemize{
#'     \item AUC_L - area under curve in light
#'     \item AUC_D - area under curve in dark
#'     \item avgS_L - average speed in light
#'     \item avgS_D - average speed in dark
#'     \item avgA_L - average acceleration in light
#'     \item avgA_D - average acceleration in dark
#'     \item strtl - startle acceleration during transition from light to dark
#'   }
#'
#' @return A vector with endpoint measurements
#'
#' @import data.table
calc_endp <- function (data, no.A = 10, no.L = 20, no.D = 20, rval = NULL) { # Goal of no.A etc is to generalize this function so user can specify the experimental methods
                if (!(rval %in% c("AUC_L", "AUC_D", "avgS_L", "avgS_D",
                                "avgA_L", "avgA_D", "strtlA", "strtlF"))) {
                  stop("argument rval either needs to be specified or is not in set \n
                        c(AUC_L, AUC_D, avgS_L, avgS_D, avgA_L, avgA_D, strtlA, strtlF)")
                }
                table <- data.table::copy(data)
                # Determine desired endpoint based on rval argument
                  if (rval %in% c("AUC_L", "AUC_D")) {
                      endp <- DNT.60.Analysis::calc_AUC(table)[[rval]]
                  } else if (rval %in% c("avgS_L", "avgS_D")) {
                      endp <- DNT.60.Analysis::calc_avgS(table)[[rval]]
                  } else if (rval %in% c("avgA_L", "avgA_D")) {
                      endp <- DNT.60.Analysis::calc_avgA(table)[[rval]]
                  } else if (rval %in% c("strtlA", "strtlF")) {
                      endp <- DNT.60.Analysis::calc_strtl(table)[[rval]]
                  }

                return(endp)
             }

                # #--------------------
                # # Aggregate Movement
                # #--------------------
                # # Create matrix of endpoint titles and their corresponding interval
                #   title <- c("SUM_L", "SUM_D", "SUM_T", "AUC_L", "AUC_D", "AUC_T")
                #   light <-
                #     interval <- rep(list(names(table[,vt11:vt30]), names(table[,vt30:vt50]), names(table[,vt11:vt50])), 2)
                #   endpoints <- as.matrix(cbind(title, interval))

                # # return(endpoints)
                # # Create SUM endpoints
                #   apply(endpoints, 1,
                #         function(endp) table[, endp$`title`:=rowSums(.SD), .SDcols=endp$`interval`]
                #         ) %>%
                #     invisible()
                #
                # # Create AUC endpoints
                #  apply(endpoints[4:6,], 1, function(endp) {
                #                               table[, endp$`title`:=
                #                                 (rowSums(.SD[, endp$`interval`[3:length(endp$`interval`)-1], with=FALSE])+
                #                                 (0.5*rowSums(.SD[, endp$`interval`[c(1,length(endp$`interval`))], with=FALSE]))),
                #                                 .SDcols=endp$interval]
                #                             }
                #        ) %>%
                #    invisible()

                #------------------------------
                # Immediate change in activity
                #------------------------------

                # Can do this a couple ways, simply calculate the difference between the two values or you could see how much
                # the value changes relative to the amount fish were already moving

#                 table <- table[, -(vt1:vt50)]
#
#                 return(table)
# }
