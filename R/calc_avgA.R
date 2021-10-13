#' Calculate average acceleration per light level for individual fish
#'
#' Calculates average acceleration per light level (dark or light) for
#' individual fish. User provides a pmr0 table and inputs
#' number of time periods for each experimental stage:
#' acclimation, light, and dark.
#'
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Excludes acceleration from last "acclimation" time-period
#' to first "light" time-period and acceleration during light-dark transition
#' from calulcation of averages.
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
#' @param length.A number of measurements made in acclimation period
#' @param length.L number of measurements made in light period
#' @param length.D number of measurements made in dark period
#'
#' @return A list with average acceleration in each light level for each fish
#'   \itemize{
#'     \item avgA_L - average speed in light
#'     \item avgA_D - average speed in dark
#'   }
#'
#'   @import data.table
calc_avgA <- function(data, length.A = 10, length.L = 20, length.D = 20) {
                table <- data.table::copy(data)
                # Find time periods and sets corresponding to light and dark
                ## Exclude first light time period and first dark time period
                  tprds <- grep("vt", names(table), value = TRUE)
                  sets <- list(avgA_L = tprds[(length.A+2):(length.A+length.L)],
                               avgA_D = tprds[(length.A+length.L+2):(length.A+length.L+length.D)]
                              )
                # Calculate acceleration between time periods
                  for (i in 2:50) {
                    n <- tprds[i]
                    `n-1` <- tprds[i-1]
                    table[[n]] <- table[[n]] - table[[`n-1`]]
                  }
                # Calculate average acceleration in light and dark per fish
                  avgA <- lapply(sets, function(set) table[, rowMeans(.SD), .SDcols = set])

                return(avgA)
             }
