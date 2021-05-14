#' Apply Box-Cox transformation to rval
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Last edit: 05/14/2021
#'
#' Applies Box-Cox power transformation to mc0 rval with goal
#' of normalizing data. Requires estimating an optimal
#' lambda paramater, lambda.hat. Lambda.hat is chosen via
#' MLE estimation using control data.
#'
#' @param data is a mc0 dataset formatted as below.
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFpmrA/L/D-20-40-40)
#'       Zebrafish photomotor resonse, Accilmation/Light/Dark-20 minutes-40 minutes-40 minutes
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
#' @return mc0 formatted table with Box-Cox tranformed rvals
#'
#'   @import data.table
apply_bxcx <- function(data) {
                table <- data.table::copy(data)

                # lambda.hat will be estimated by normalizing residuals of
                #   a linear model.
                # Linear model has form rval ~ egid where egid is the experimental
                #   group id of a vehicle control sample
                  # Find experimental groups
                  egroups <- DNT.60.Analysis::data_egids(table)
                  # Use apids to map animals to a group number
                  mapper <- egroups[, .(apid = unlist(apids)), by = egid]
                  tester <- table[mapper, on = "apid"]
                  # Estimate lambda
                  lam.hat <- DNT.60.Analysis::calc_lamhat(tester)

                # Transform data
                  if (is.null(lam.hat) | is.na(lam.hat)) {
                    stop("Error in DNT.60.Analysis::calc_lamhat()")
                  } else if (lam.hat == 0) {
                      table[, rval := log10(rval)]
                  } else if (lam.hat != 0) {
                      table[, rval := (((rval^lam.hat)-1)/lam.hat)]
                  } else print("Error")

                return(table)
              }