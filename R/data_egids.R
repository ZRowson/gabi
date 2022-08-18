#' Identify experimental groups
#'
#' @author Zachary Rowson \email Rowson.Zachary at epa.gov
#'
#' @details Last edit: 08/18/2022
#'
#' @description
#' Takes a mc0 data.table and uses plate numbers to return
#' experimental groups. Experimental groups are returned
#' as a list with chemical names in each group and plate
#' id's in each group.
#' Note: Experimental groups are defined after bad quality
#' plates and concentration groups are removed in
#'
#' @param table is a mc0 dataset formatted as below.
#'   \itemize{
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
#' @return A data.table of chemical names in each group and corresponding plates.
#'   \itemize{
#'     \item
#'   }
#'
#' @import data.table
#'
#' @export
data_egids <- function(data) {
                  table <- data.table::copy(data)
                  # Find unique plate ids for each test chemical
                    chm.plts <- table[wllt == "t", .(apids = .( sort( unique(c(apid)) ) )),
                                      by = cpid]
                  # Find unique plate groups. Some chemicals were tested together
                    grps <- unique(chm.plts[, apids])
                  # Concatenate names of chemicals tested together into vectors
                    cpids <- lapply(grps, function(grp) {
                                  pos <- chm.plts[, apids] %in% list(grp)
                                  chm.plts[pos, cpid]
                                }
                              )

                  # Create and return data with column of egids
                    exp_groups <- data.table(egid = paste0("E", seq(1:length(grps))),
                                             cpids, apids = grps)
                    mapper <- exp_groups[, .(apid = unlist(apids)), by = egid]
                    t_w_egids <- table[mapper, on = "apid"]
                    return(t_w_egids)
                }
