#' Apply Box-Cox transformation to rval
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Applies Box-Cox power transformation to mc0 rval with goal
#' of normalizing data. Requires estimating an optimal
#' lambda parameter, lambda.hat. Lambda.hat is chosen via
#' MLE using control data. Shift parameter is
#' included if data is less than or equal to 0. Shift
#' parameter equals floor(minimum observed rval).
#'
#' @details Last edit: 08/18/2022
#'
#' @param data is a mc0 dataset formatted as below.
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFlmrA/L/D-20-40-40)
#'       Zebrafish locomotor response, Acclimation/Light/Dark- _l_ minutes- _m_ minutes- _n_ minutes
#'     \item cpid - chemical name
#'     \item apid - assay plate id DNT###
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'      t = test, v = vehicle control, c/o = positive control
#'     \item wllq - well quality indicates if observation is viable for analysis
#'       1 = yes, 0 = no
#'     \item conc - concentration of chemical
#'     \item rval - response value, endpoint of interest
#'   }
#' @return mc0 formatted table with Box-Cox transformed rvals
#'
#' @import data.table
#' @export
apply_bxcx <- function(data) {
                table <- data.table::copy(data)

                # lambda.hat will be estimated by normalizing residuals of
                #   a linear model.
                # Linear model has form rval ~ egid where egid is the experimental
                #   group id of a vehicle control sample
                  # Find experimental groups
                  tester <- gabi::data_egids(table)
                  # Estimate lambda
                  list2env(gabi::calc_lamhat(tester),
                           envir = environment()
                           )

                # Transform data
                  # Apply Box-Cox
                  if (is.null(lam.hat) | is.na(lam.hat)) {
                    stop("Error in gabi::calc_lamhat()")
                  } else if (lam.hat == 0) {
                      table[, rval := log10(rval + shift)]
                  } else if (lam.hat != 0) {
                      table[, rval := ((((rval+shift)^lam.hat)-1)/lam.hat)]
                  } else print("Error")

                return(list(data = table,
                            lam.hat = lam.hat,
                            shift = shift
                            )
                       )
              }
