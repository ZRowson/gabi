#' Calculate transition endpoints
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Calculates transition endpoints from user provided behavioral data.
#' Transition endpoints describe fish behavior at transitions between
#' different light levels: startle (light to dark).
#'
#' @details Last edit 08/18/2022.
#'
#' @param data is a lmr0 formatted data.table
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
#'
#' @param no.A number of measurements made in acclimation period
#' @param no.L number of measurements made in light period
#' @param no.D number of measurements made in dark period
#'
#' @return A list with vectors of transitory endpoint values for each fish
#'   \itemize{
#'     \item strtlA - "startle acceleration" acceleration at startle
#'     \item strtlAavg - "startle acceleration from average" acceleration at startle when speed in dark is compared to average speed in light
#'     \item strtlF - "startle factor" fold change in speed at startle
#'   }
#' @export
calc_trans <- function(data, no.A = 10, no.L = 20, no.D = 20) {

                rownames(data) <- NULL
                avgS_L <- gabi::calc_avgS(data,no.A,no.L,no.D)[["avgS_L"]]

                # gather speed values t boundary periods
                  t <- grep("vt", names(data), value = TRUE)
                  last.A <- t[no.A]
                  first.L <- t[no.A + 1]
                  last.L <- t[no.A + no.L]
                  first.D <- t[no.A + no.L + 1]

                # calculate startle endpoints
                  strtlA <- data[[first.D]] - data[[last.L]]
                  strtlAavg <- data[[first.D]] - avgS_L
                  strtlF <- data[[first.D]] / (data[[last.L]] + 1)

                trans <- list(strtlA = strtlA,
                              strtlAavg = strtlAavg,
                              strtlF = strtlF)

                return(trans)
}
