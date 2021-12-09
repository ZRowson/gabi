#' Format chemical data into tcpl row object for tcpl analysis
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Created 06/01/2021
#'   Last edit: 12/09/2021
#'
#' Formats a chemical data from a mc0 dataset into a row
#' object for tcpl analysis. Row objects are lists of
#' data and descriptive statistics necessary for tcpl
#' analysis.
#'
#' @param data is a mc0 dataset formatted as below
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFpmrALD-20-40-40)
#'       Zebrafish photomotor response, Acclimation/Light/Dark-20 minutes-40 minutes-40 minutes
#'     \item cpid - chemical name
#'     \item apid - assay plate id DNT###
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'      t = test, v = vehicle control
#'     \item wllq - well quality indicates if observation is viable for analysis
#'       1 = yes, 0 = noS
#'     \item conc - concentration of chemical
#'     \item rval - endpoints resp values of each fish
#'   }
#' @param chemical is a string representing chemical of interest
#' @param lam.hat is power used to Box-Cox transform data
#' @param shift is value used to shift data values from zero for transformation
#'
#' @return A tcpl row object
#'   \itemize{
#'       \item conc - vector of concentrations at which each fish was tested
#'       \item resp - response of each fish ordered to align with conc vector
#'       \item bresp - baseline response: vector of baseline fish response values
#'       \item bmed - median of baseline response
#'       \item cutoff - numerical value representing noise about baseline
#'       \item onesd - standard deviation of bresp
#'       \item name - name of tested chemical
#'       \item assay - name of assay and endpoint
#'   }
#'
#'   @import data.table
#' @export
as_row <- function(data, chemical, lam.hat = 1, shift = 0) {
            table <- gabi::data_egids(data)

            # Consolidate necessary data
              assay <- table[cpid == chemical, acid] %>% unique()
              group <- table[cpid == chemical & !is.na(rval), egid] %>% unique()
              concrval <- table[(cpid==chemical|wllt =="v") & egid == group & !is.na(rval), .(conc, rval)]
              # new.conc <- min(concrval[conc != 0, conc])/1000
              # concrval[conc == 0, conc := new.conc]
              brval <- table[wllt == "v" & egid == group & !is.na(rval), rval]

            # Transform data
              bmed <- mean(brval)
              resp <- concrval[conc != 0, rval] - bmed
              bresp <- brval - bmed

            # Calculate cutoff
              bsd <- stats::sd(bresp)
              bvar <- bsd^2
              nc <- length(bresp)
              nt <- min(table(concrval$conc))
              onesd <- sqrt((bvar/nc) + (bvar/nt))
              cutoff <- 3*onesd

            # Generate and assign row vector
              row <- list(conc = concrval[conc!=0, conc],
                          resp = resp,
                          bresp = bresp,
                          bmed = bmed,
                          onesd = onesd,
                          cutoff = cutoff,
                          name = chemical,
                          assay = assay,
                          lam.hat = lam.hat,
                          shift = shift)
            return(row)
          }
