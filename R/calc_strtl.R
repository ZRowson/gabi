#' Calculate startle acceleration for individual fish
#'
#' Calculates startle for individual fish. User
#' provides a pmr0 table.
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Last edit 06/07/21.
#'
#' @param data is a pmr0 formatted data.table
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
#' @return A vector with startle statistics for each fish
#'   \itemize{
#'     \item strtlA - acceleration at startle
#'     \item strtlF - startle factor. fold change at startle
#'   }
#'
#' @import data.table
calc_strtl <- function(data, length.A = 10, length.L = 20, length.D = 20) {
                table <- data.table::copy(data)
                # Calculate acceleration and fold change at transition from light to dark
                  tprds <- grep("vt", names(table), value = TRUE)
                  last.L <- tprds[length.A + length.L]
                  first.D <- tprds[length.A + length.L + 1]
                  strtlA <- table[[first.D]] - table[[last.L]]
                  strtlF <- (table[[first.D]]) / (table[[last.L]] + 1)

                strtl <- list(strtlA = strtlA,
                              strtlF = strtlF
                              )

                return(strtl)
}
