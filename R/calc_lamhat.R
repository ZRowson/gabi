#' Calculate lambda.hat for Box-Cox transformation
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Last edit: 05/14/2021
#'
#' Calcualtes lambda.hat for data by finding optimal lambda
#' fro vehicle control. I need to elaborate on this further.
#'
#' @param data is a mc0 dataset with experimental group id's
#' formatted as below.
#'   \itemize{
#'     \item egid - experimental group id's
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
#' @return MLE for lambda of Box-Cox transform.
#'
#' @import data.table
calc_lamhat <- function (data) {
                  # Isolate vehicle control animals
                    table <- data[wllt == "v"]
                  # Create linear model and estimate optimal lambda
                    lklhd <- MASS::boxcox(rval ~ egid, data = table,
                                         lambda = seq(-3, 3, by = 0.25),
                                         plotit = FALSE
                            )
                    lam.hat <- lklhd$x[which.max(lklhd$y)]

                  return(lam.hat)
                }
