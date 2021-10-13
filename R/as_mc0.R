#' Format pmr0 data set as mc0 for tcpl analysis
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Formats a pmr0 formatted dataset into mc0 format for
#' tcpl analysis. Requires the creation of endpoints via
#' gabi::create_endp().
#'
#' @details Last Edit: 09/28/2021
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

as_mc0 <- function(data, rval = NULL) {
              # Calculate user-specified endpoint
                endp <- gabi::calc_endp(data, rval = rval)
                data$rval <- endp

              # Change assay component id
                acid <- unique(data$acid)
                newacid <- paste(rval, acid, sep = "_")
                data$acid <- newacid

              mc0 <- data[c('srcf', 'acid', 'cpid', 'apid', 'rowi', 'coli', 'wllt', 'wllq', 'conc', 'rval')]
              return(mc0)
}
