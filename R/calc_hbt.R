#' Calculate measures of habituation per light level for individual fish
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Calculates measures of habituation per light period (light or dark).
#' User provides a lmr0 table and inputs
#' number of time periods in each experimental stage:
#' acclimation, light, and dark.
#'
#' @details hbt_L = max(speed in light) / min(speed in light), hbt_D = max(speed in dark) / min(speed in dark).
#' Created 09/28/2021. Last edit 08/18/2022.
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
#' @return A list with habituation ednpoints for each fish
#'   \itemize{
#'     \item hbt_L - speed ratio in light. max(speed) / min(speed)
#'     \item hbt_D - speed ratio in dark. max(speed) / min(speed)
#'   }
#' @export
calc_hbt <- function(data, no.A = 10, no.L = 20, no.D = 20) {

  rownames(data) <- NULL
  # find time-periods and sets corresponding to light and dark
  t <- grep("vt", names(data), value = TRUE)
  sets <- list(hbt_L = t[(no.A+1):(no.A+no.L)],
               hbt_D = t[(no.A+no.L+1):(no.A+no.L+no.D)])
  # calculate average speed in light and dark per fish
  hbt <- lapply(sets, function(set) {
    temp <- data[set]
    max <- apply(temp, 1, max)
    min <- apply(temp, 1, min)
    max / (min+1)
  })

  return(hbt)
}
