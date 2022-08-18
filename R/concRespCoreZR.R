#' Concentration Response Core - ZR Edits
#'
#' @details Last Edit: 02/08/2022
#'
#' @description
#' EDIT by Zachary Rowson (Rowson.Zachary#epa.gov).
#' Edit Tracking:
#' 1) force.fit = TRUE
#'   a) default changed from FALSE to avoid error in gabi::tcplfit2_coreZR (line #?)
#' 2) in documentation, changed recommended structure of row parameter
#' 3) inclusion of bresp in row and parameters
#' 4) removed centering of data about bmed as it is done in as_row (is this necessary?)
#' 5) call to gabi::tcplfit2_coreZR and tcplhit2_coreZR rather than tcplfit2 functions
#' 6) call to gabi::tcplggplotter and declaration of plot object
#' 7) add parameter verbose.plot
#'   a) logical indicator that decides if plot should have caption listing summarizing statistics
#'
#' Core of concentration response curve fitting for pvalue based cutoff. This
#' function calls tcplfit2_core to get curve fits, and then tcplhit2_core to
#' perform the hitcalling.
#'
#' @param row A named list that must include:
#'   \itemize{
#'     \item conc - list of concentrations (not in log units)
#'     \item resp - list of corresponding responses
#'     \item bresp - baseline response: vector of baseline fish response values #EDIT
#'     \item bmed - median of noise estimate.
#'     \item cutoff - noise cutoff
#'     \item onesd - 1 standard deviation of the noise (for bmd calculation)
#'     \item name - name of tested chemical #EDIT
#'      \item assay - name of assay and endpoint #EDIT
#'   }
#'   Other elements (usually identifiers, like casrn) of row will be attached to
#'   the final output.
#' @param fitmodels Vector of model names to use.
#' @param conthits conthits = T uses continuous hitcalls, otherwise they're
#'   discrete.
#' @param aicc aicc = TRUE uses corrected AIC to choose winning method; otherwise
#'   regular AIC.
#' @param force.fit If TRUE force the fitting to proceed even if there are no points
#'   outside of the bounds (default TRUE)
#' @param bidirectional If TRUE allow fitting to happen in both directions (default TRUE)
#' @param verbose  If TRUE, write extra output from tcplfit2_core (default FALSE)
#' @param verbose.plot If TRUE, ggplots will have a caption listing summarizing statistics (default TRUE)
#' @param do.plot If TRUE, create a plot in the tcplfit2_core function (default FALSE)
#' @param return.details If TRUE, return the hitcalling details and the summary, if FALSE (default), just return the summary
#' @param bmr_scale - bmr scaling factor (for bmd calculation) default = 1.349
#' @return A list of two elements. The first (summary) is the output from tcplhit2_core. The second, params is the
#' output from tcplfit2_core
#' a dataframe of one row containing
#' @param bmd_low_bnd Multiplier for bmd lower bound.  A value of .1 would require the bmd to be no lower
#'   than 1/10th of the lowest concentration tested.
#' @param bmd_up_bnd Multiplier for the bmd upper bound.  A value of 10 would require the bmd to be no lower
#'   than 10 times the highest concentration tested.
#'
#'
#' @export
#'
#' @examples
#' conc <- list(.03, .1, .3, 1, 3, 10, 30, 100)
#' resp <- list(0, .2, .1, .4, .7, .9, .6, 1.2)
#' row <- list(conc = conc,
#'             resp = resp,
#'             bmed = 0,
#'             cutoff = 1,
#'             onesd = .5,
#'             name = "some chemical")
#' concRespCoreZR(row, conthits = TRUE)
#' concRespCoreZR(row, aicc = TRUE)
concRespCoreZR <- function(row,
                         fitmodels = c(
                           "cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3",
                           "exp4", "exp5"
                         ),
                         conthits = TRUE,
                         aicc = FALSE,
                         force.fit = TRUE, #EDITED
                         bidirectional = TRUE,
                         verbose = FALSE,
                         verbose.plot = TRUE,
                         do.plot = FALSE,
                         return.details = FALSE,
                         bmr_scale = 1.349,
                         bmd_low_bnd = NULL,
                         bmd_up_bnd = NULL) {
  # variable binding to pass cmd checks
  bmed <- cutoff <- onesd <- plot <- NULL
  # row needs to include cutoff and bmed
  # unpack row into the local environment, for ease: sample_id, dtxsid, casrn, name, time, pathway, size, conc, resp
  list2env(row, envir = environment())
  resp <- unlist(resp)
  bresp <- unlist(bresp) # EDIT
  conc <- unlist(conc)

  # prepare input EDIT: removed centralization about bmed, this is done is as_row
  conc <- conc[!is.na(resp)]
  resp <- resp[!is.na(resp)]
  bresp <- bresp[!is.na(bresp)] # EDIT
  identifiers <- row[!names(row) %in% c("conc", "resp", "bmed", "onesd", "cutoff.int", "bresp")] #EDIT

  # EDIT: calculate response medians in concRespCore to avoid running it in tcplfit2_coreZR and tcplggplotter
  logc <- log10(conc)
  rmds <- tapply(resp, logc, mean)

  # run the fits
  params <- gabi::tcplfit2_coreZR(conc, resp, rmds, cutoff,
                                  force.fit = conthits, bidirectional = bidirectional,
                                  fitmodels = fitmodels, verbose = verbose,
                                  do.plot = do.plot
                                  ) # EDIT

  # calculate the hitcall
  summary <- tcplfit2::tcplhit2_core(params, conc, resp, cutoff, onesd, bmr_scale, bmed, conthits, aicc, identifiers, bmd_low_bnd, bmd_up_bnd)

  # EDIT: create plotting summary if can.plot == TRUE
  if (params[["can.plot"]]) {
    plot <- gabi::tcplggplotter(endp, resp, bresp, conc, logc, rmds, bmed, lam.hat, shift, params, summary, verbose.plot)
  }

  if (return.details) {
    return(list(summary = summary, all.models = params, plot = plot))
  } else {
    return(list(summary = summary, plot = plot))
  }
}
