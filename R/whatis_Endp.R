#' Expand endpoint acronyms for clarity
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Expands endpoint acronym when given acronym in string form.
#'
#' @details
#' Created 11/02/2021. Last edit 12/13/21.
#'
#' @param endp string of endpoint name in acronym form
#'
#' @return endpoint name in expanded form
#' @export
whatis_Endp <- function(endp) {

                  # declare expanded endpoint names

                  ## describes behavior in Light
                  AUC_L <- "Area Under Curve in Light"
                    AUC_L.sub <- "AUC's calculated with trapezoidal rule "
                  avgS_L <- "Average Speed in Light"
                  avgA_L <- "Average Acceleration in Light"
                  avgJ_L <- "Average Jerk in Light"
                  hbt_L <- "Habituation in Light"
                    hbt_L.sub <- "hbt_L calculated as ratio of maximum to minumum speed in Light "

                  ## describes Transition
                  strtlA <- "Startle Acceleration"
                    strtlA.sub <- "difference between speed during first 2 minutes of Dark and last 2 minutes of Light "
                  strtlF <- "Startle Factor"
                    strtlF.sub <- "ratio of speed during first 2 minutes of Dark and last 2 minutes of Light "
                  strtlAavg <- "Startle Acceleration versus Average"
                    strtlAavg.sub <- "difference between speed during first 2 minutes of Dark and average speed in Light "

                  ## describes behavior in Light
                  AUC_D <- "Area Under Curve in Dark"
                    AUC_D.sub <- "AUC's calculated with trapezoidal rule "
                  avgS_D <- "Average Speed in Dark"
                  avgA_D <- "Average Acceleration in Dark"
                  avgJ_D <- "Average Jerk in Dark"
                  hbt_D <- "Habituation in Dark"
                    hbt_D.sub <- "hbt_D calculated as ratio of maximum to minumum speed in Dark "

                  ## miscellaneous endpoints
                  AUC_T <- "Area Under Curve over Total experimental period"
                    AUC_T.sub <- "AUC's calculated with trapezoidal rule "
                  avgS_T <- "Average Speed during Total experimental period"
                  AUC_r <- "Area Under Curve Ratio"
                    AUC_r.sub <- "AUC_r calculated as ratio of AUC_D to AUC_L "

                  # identify name expansion of interest using endp parameter and return

                  ## identify descriptors
                  desc <- grep(endp, ls(), value = TRUE)
                  expand <- lapply(desc, get, envir = sys.frame(sys.parent(0)))

                  return(expand)
}
