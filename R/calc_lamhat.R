#' Calculate lambda.hat for Box-Cox transformation
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Last edit: 05/14/2021
#'
#' @description
#' Calculates parameters for Box-Cox power transformation from data. Uses
#' MLE estimation based on residuals of linear model (rval + shift ~ egid).
#'
#' @param data is a mc0 dataset with experimental group id's
#' formatted as below.
#'   \itemize{
#'     \item egid - experimental group id's
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFlmrA/L/D-20-40-40)
#'       Zebrafish locomotor response, Acclimation/Light/Dark- _l_ minutes- _m_ minutes- _n_ minutes
#'     \item cpid - chemical name
#'     \item apid - assay plate id DNT###
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'      t = test, v = vehicle control
#'     \item wllq - well quality indicates if observation is viable for analysis
#'       1 = yes, 0 = no
#'     \item conc - concentration of chemical
#'     \item rval - endpoint of interest
#'   }
#' @return \itemize {
#'           \item lam.hat - MLE for lambda of Box-Cox transform.
#'           \item shift - shift to avoid rvals less than or equal to zero.
#'         }
#'
#' @import data.table
#' @export
calc_lamhat <- function (data) {
                  # Isolate vehicle control animals
                    table <- data.table::copy(data)
                  # Create linear model and estimate optimal lambda
                    # Find shift parameter
                    if (any(table[,rval] < 0, na.rm = TRUE)) {
                      shift <- floor( min(table[, rval], na.rm = TRUE) ) * (-1)
                    }  else if (any(table[,rval] == 0, na.rm = TRUE)) {
                      shift <- 1
                    } else shift <- 0
                    # Find maximum likelihood Box-Cox power parameter for vc groups
                    tester <- table[wllt == "v"]
                    lklhd <- MASS::boxcox(rval+shift ~ egid, data = tester,
                                         lambda = seq(-3, 3, by = 0.25),
                                         plotit = FALSE
                            )
                    lam.hat <- lklhd$x[which.max(lklhd$y)]

                  return(list(lam.hat = lam.hat,
                              shift = shift
                              )
                         )
                }
