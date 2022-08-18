#' Format chemical data into tcpl row object for tcpl analysis
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Created 06/01/2021 Last edit: 08/18/2022
#'
#' @description
#' Formats a chemical data from a mc0 dataset into a row
#' object for tcpl analysis. Row objects are lists of
#' data and descriptive statistics necessary for tcpl
#' analysis.
#'
#' @param data is a mc0 dataset formatted as below
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFlmrALD-20-40-40)
#'       Zebrafish locomotor response, Acclimation/Light/Dark- _l_ minutes- _m_ minutes- _n_ minutes
#'     \item cpid - chemical name
#'     \item apid - assay plate id DNT###
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'      t = test, v = vehicle control, c/o = positive control
#'     \item wllq - well quality indicates if observation is viable for analysis
#'       1 = yes, 0 = no
#'     \item conc - concentration of chemical
#'     \item rval - endpoints resp values of each fish
#'   }
#'
#' @param chemical is a string representing chemical of interest.
#' @param endp is a string representing the endpoint of interest.
#' @param lam.hat is power used to Box-Cox transform data.
#' @param shift is value used to shift data values from zero for transformation.
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
as_row <- function(data, chemical, endp, lam.hat = 1, shift = 0) {

  # Extract data of interest
  data.w.egid <- gabi::data_egids(data)
  group <- unique( data.w.egid[cpid == chemical & !is.na(rval), egid] )
  data.extract <- data.w.egid[(cpid==chemical|wllt =="v") & egid == group & !is.na(rval)]

  # Consolidate necessary descriptive data
  assay <- data.extract[, unique(acid)]

  # Shift data so mean rval for control = 0
  bmed <- data.extract[wllt == "v", mean(rval)]
  data.extract[, rval := rval - bmed]

  # Gather data for row objects
  resp <- data.extract[wllt == "t", rval]
  conc <- data.extract[wllt == "t", conc]
  bresp <- data.extract[wllt == "v", rval]


  # Calculate cutoff as 3 * (standard error of difference)
  var.0 <- var(bresp)
  n.0 <- length(bresp)
  n.i <- data.extract[wllt == "t", .N, by = .(conc)][, mean(N)]
  onesd <- SE.diff <- sqrt( (var.0/n.0) + (var.0/n.i) )
  cutoff <- 3*onesd

  # Generate and assign row vector
  row <- list(conc = conc,
              resp = resp,
              bresp = bresp,
              bmed = bmed,
              onesd = onesd,
              cutoff = cutoff,
              name = chemical,
              assay = assay,
              endp = endp,
              lam.hat = lam.hat,
              shift = shift)
          }
