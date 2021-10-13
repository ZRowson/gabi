#' Format chemical data into tcpl row object for tcpl analysis
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Last edit: 05/18/2021
#'
#' Formats a chemical data from a mc0 dataset into a row
#' object for tcpl analysis. Row objects are lists of
#' data and descriptive statistics necessary for tcpl
#' analysis.
#'
#' @param table is a mc0 dataset formatted as below
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
#'     \item rval - endpoints resp values of each fish
#'   }
#' @return A tcpl row object
#'   \itemize{
#'       \item conc - vector of concentrations at which each fish was tested
#'       \item resp - response of each fish ordered to align with conc vector
#'       \item bresp - baseline response: vector of baseline fish response values
#'       \item bmed - median of baseline response
#'       \item cutoff - numerical value of cutoff representing baseline noise.
#'         Calculated as 2*MAD(bresp)
#'       \item onesd - standard deviaiton of bresp
#'       \item name - name of tested chemical
#'      \item assay - name of assay and endpoint
#'   }
#'
#'   @import data.table
as_row_draft <- function(data, chemical) {
                  table <- DNT.60.Analysis::data_egids(data)

                  # Consolidate necessary data
                    assay <- table[cpid == chemical, acid] %>% unique()
                    group <- table[cpid == chemical & !is.na(rval), egid] %>% unique()
                    rval <- table[cpid == chemical & !is.na(rval), rval]
                    conc <- table[cpid == chemical & !is.na(rval), conc]
                    brval <- table[wllt == "v" & egid == group & !is.na(rval), rval]

                  # Transform data
                    bmed <- stats::median(brval)
                    resp <- (rval - bmed) / bmed
                    bresp <- (brval - bmed) / bmed

                  # Create summary statistics
                    bmed <- stats::median(bresp)
                    onesd <- stats::sd(bresp)
                    cutoff <- 2*stats::sd(bresp)

                  # Generate and assign row vector
                    row <- list(conc = conc,
                                resp = resp,
                                bresp = bresp,
                                bmed = bmed,
                                onesd = onesd,
                                cutoff = cutoff,
                                name = chemical,
                                assay = assay)
                  return(row)
                }
