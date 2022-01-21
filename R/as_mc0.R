#' Format pmr0 data set as mc0 for tcpl analysis
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Formats a pmr0 formatted dataset into mc0 format for
#' tcpl analysis. Requires the creation of endpoints via
#' gabi::create_endp().
#'
#' @details Last Edit: 12/03/2021
#'
#' @param table is a pmr0 dataset formatted as below
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFpmrALD-20-40-40)
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
#' @param no.A number of measurements made in acclimation period
#' @param no.L number of measurements made in light period
#' @param no.D number of measurements made in dark period
#'
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
#' @import data.table
#' @export

as_mc0 <- function(table, rval = NULL, no.A = 10, no.L = 20, no.D = 20) {

              # copy table to prevent writing to original copy
                data <- data.table::copy(table)

              # calculate user-specified endpoint
                endp <- gabi::calc_endp(data, rval=rval, no.A=no.A, no.L=no.L, no.D=no.D)
                data[, rval := endp]

              # change assay component id
                acid <- unique(data[,acid])
                newacid <- paste(rval, acid, sep = "_")
                data[, acid := newacid]

              # select mc0 columns
                mc0 <- data[, .(srcf,acid,cpid,apid,rowi,coli,wllt,wllq,conc,rval)]

              return(mc0)
}
