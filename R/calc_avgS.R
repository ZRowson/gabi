#' Calculate average speed per light level for individual fish
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Calculates average speed per light level (dark or light) for
#' individual fish. User provides a lmr0 table and inputs
#' number of time periods for each experimental stage:
#' acclimation, light, and dark. A lmr0 table
#'
#' @details Last edit 08/18/2022.
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
#' @return A list with average speed in each light level for each fish
#'   \itemize{
#'     \item avgS_L - average speed in light
#'     \item avgS_D - average speed in dark
#'   }
#' @export
calc_avgS <- function(data, no.A = 10, no.L = 20, no.D = 20) {

                  rownames(data) <- NULL
                # find time-periods and sets corresponding to light and dark
                  t <- grep("vt", names(data), value = TRUE)
                  sets <- list(avgS_L = t[(no.A+1):(no.A+no.L)],
                               avgS_D = t[(no.A+no.L+1):(no.A+no.L+no.D)],
                               avgS_T = t[(no.A+1):(no.A+no.L+no.D)]
                              )
                # calculate average speed in light and dark per fish
                  avgS <- lapply(sets, function(set) rowMeans(data[set]))

                return(avgS)
             }
