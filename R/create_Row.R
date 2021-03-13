#' This function produces a list in format of tcplfit2 "row"
#' Currently limited to execution on one chemical.
#' @param table is a data.table of DNT-60 Behavioral data
#' for one chemical
#' @param endp is a string naming the endpoint to be evaluated
#' @import data.table
create_Row <- function(table, endp) {
                # Determine background variation
                temp <- table[FinalConc == 0]
                bmad <- stats::mad(temp[[endp]])
                onesd <- stats::sd(temp[[endp]])
                # 3 is an arbitrary value... How do we determine a better one?
                cutoff <- 3*bmad

                # Consolidate test data
                temp <- table[FinalConc != 0]
                conc <- temp$`FinalConc`
                resp <- temp[[endp]]

                # Generate row vector
                row <- list(conc = conc,
                            resp = resp,
                            bmed = 0,
                            cutoff = cutoff,
                            onesd = onesd)
                return(row)
              }
