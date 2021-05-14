#' Format pmr0 data set as mc0 for tcpl analysis
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Last edit: 04/06/2021
#'
#' Formats a pmr0 formatted dataset into mc0 format for
#' tcpl analysis. Requires the creation of endpoints via
#' DNT.60.Analysis::create_endp().
#'
#' @param table is a pmr0 dataset formatted as below
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
#'     \item vtj - measurements at time period j for j /in {1,2,...,n}
#'       Here n = 50
#'   }
#' @param rval is the name of desired endpoint as a string
#' @return A mc0 format data.table
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
#'     \item rval - endpoints resp values of each fish
#'   }
#'
#'   @import data.table
as_mc02 <- function(table, rval = NULL) {
            # Create endpoints and isolate user-specified endpoint
              endpnts <- DNT.60.Analysis::create_endp(table)
              endpnts[, rval := get(rval)]

            # Change assay component id
              acid <- unique(endpnts[, acid])
              newacid <- paste(rval, acid, sep = "_")

            # Remove test concentration groups that do not meet quality thresholds
            # Threshold: sample proportion of good quality test subjects = 0.75
              remove <- endpnts[wllt == "t", .(p.hat = (sum(wllq==1) / .N)), by = .(cpid,conc)][p.hat < .75, .(cpid, conc)]
              mc0 <- endpnts[!remove, on = .(cpid=cpid, conc=conc)]

            mc0 <- mc0[, .(srcf, acid = newacid, cpid, apid, rowi, coli, wllt, wllq, conc, rval)]
            return(mc0)
}
