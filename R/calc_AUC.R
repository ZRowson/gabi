#' Calculate AUC in light and dark for individual fish
#'
#' Calculates area under curve per light level (dark or light) for
#' individual fish. Uses trapezoidal rule to approximate integral of
#' speed funciton per fish. User provides a pmr0 table and inputs
#' number of time periods for each experimental stage:
#' acclimation, light, and dark.
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Last edit 06/07/21.
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
#' @param length.A number of measurements made in acclimation period
#' @param length.L number of measurements made in light period
#' @param length.D number of measurements made in dark period
#'
#' @return A list with AUC in each light level for each fish
#'   \itemize{
#'     \item AUC_L - AUC in light
#'     \item AUC_D - AUC in dark
#'   }
#'
#'   @import data.table
calc_AUC <- function(data, length.A = 10, length.L = 20, length.D = 20) {
              table <- data.table::copy(data)
              tprds <- grep("vt", names(table), value = TRUE)
              sets <- list(AUC_L = tprds[(length.A+1):(length.A+length.L)],
                           AUC_D = tprds[(length.A+length.L+1):(length.A+length.L+length.D)]
                          )
              # Create AUC endpoints
                AUC <- lapply(sets, function(set) {
                                        table[, rowSums(.SD[, set[2:(length(set)-1)], with=FALSE]) +
                                                (0.5*rowSums(.SD[, ..set[c(1,length(set))], with=FALSE]))
                                             ]
                                     }
                             )

              return(AUC)
            }
