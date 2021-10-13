#' Calculate average speed per light level for individual fish
#'
#' Calculates average speed per light level (dark or light) for
#' individual fish. User provides a pmr0 table and inputs
#' number of time periods for each experimental stage:
#' acclimation, light, and dark. A pmr0 table
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
#' @return A list with average speed in each light level for each fish
#'   \itemize{
#'     \item avgS_L - average speed in light
#'     \item avgS_D - average speed in dark
#'   }
#'
#'   @import data.table
calc_avgS <- function(data, length.A = 10, length.L = 20, length.D = 20) {
                table <- data.table::copy(data)
                # Find time-periods and sets corresponding to light and dark
                  tprds <- grep("vt", names(table), value = TRUE)
                  sets <- list(avgS_L = tprds[(length.A+1):(length.A+length.L)],
                               avgS_D = tprds[(length.A+length.L+1):(length.A+length.L+length.D)]
                              )
                # Calculate average speed in light and dark per fish
                  avgS <- lapply(sets, function(set) table[, rowMeans(.SD), .SDcols = set])

                return(avgS)
             }
