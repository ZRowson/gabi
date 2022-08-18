#' Concentration-response curve fitting EDIT by Zachary Rowson
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details Last Edit: 08/18/2022
#' EDIT Tracking
#' Edits
#' 1) add name, and assay to parameters
#' 2) add can.plot to return
#'
#' @description
#' Edit of original tcplfit2 function tcplfit2::tcplfit2_core. Editted to
#' remove default plotting method provided by tcplfit2. Replaces plotting method
#' with declaration of a logical to be passed to gabi::concRespCoreZR
#' wrapper function to determine if gabi::tcplggplotter should be run.
#'
#'
#' @param conc Vector of concentrations (NOT in log units).
#' @param resp Vector of responses.
#' @param cutoff Desired cutoff. If no absolute responses > cutoff and
#'   force.fit = F, will only fit constant model.
#' @param name String of chemical name. #EDIT
#' @param assay String of assay name. #EDIT
#' @param force.fit If force.fit = T, will fit all models regardless of cutoff.
#' @param bidirectional If bidirectional = F, will only give positive fits.
#' @param verbose If verbose = T, will print optimization details and aics.
#' @param do.plot If do.plot = T, will generate a plot comparing model curves.
#' @param fitmodels Vector of model names to try fitting. Missing models still
#'   return a skeleton output filled with NAs.
#' @param ... Other fitting parameters (deprecated).
#'
#'
#' @return can.plot - logical indicating if plotting is possible
#' @return List of N(models) elements, one for each of the models run (up to 10),
#' followed by a last element "modelnames", which is a  vector of model names so
#' other functions can easily cycle through the output. For a full list, see the
#' documentation for the individual fitting method functions. For each model there
#' is a sublist with elements including:
#'   \itemize{
#'     \item success - was the model successfully fit
#'     \item aic - the AIC value
#'     \item cov - success of the the covariance matrix calculation
#'     \item rme - root mean error of the data around the curve
#'     \item modl - vector of model values at the civen concentrations
#'     \item tp - the top of the curve fit
#'     \item ga - the AC50 or Hill paramters
#'     \item er - the error term
#'     \item ... other paramters specific to the model (see the documentation for the specific models)
#'     \item tp_sd, ga_sd, p_sd, etc., the values of the standard deviations of the parameters for the models
#'     \item er_sd - standard deviation of the error term
#'     \item pars - the names of the parameters
#'     \item sds - the names of the standard deviations of the parameters
#'   }
#'
#' @examples
#' conc <- c(.03, .1, .3, 1, 3, 10, 30, 100)
#' resp <- c(0, .1, 0, .2, .6, .9, 1.1, 1)
#' output <- tcplfit2_core(conc, resp, .8,
#'   fitmodels = c("cnst", "hill"), verbose = TRUE,
#'   do.plot = TRUE
#' )
#' @import data.table
#' @export
tcplfit2_coreZR <- function(conc, resp, rmds, cutoff, force.fit = FALSE, bidirectional = TRUE, verbose = FALSE, do.plot = FALSE,
                          fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5"),
                          ...) {
  fitmodels <- unique(c("cnst", fitmodels)) # cnst models must be present for conthits but not chosen

  # first decide which of possible models will be fit
  modelnames <- c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5")
  # check for edge case where all responses are equal
  if(max(resp)==min(resp) && resp[1]==0){ # check if all response values are zero
    warning(paste("all response values are 0: add epsilon (1e-6) to all response elements.",
                  paste("\tResponse Range:",paste(range(resp),collapse = ",")),
                  sep = "\n")) # return a warning
    resp <- resp+1e-6 # adding epsilon to resp vector
  }
  # decide whether to run each model, then use generic functions to run model by name
  for (model in modelnames) {
    # only fit when four or more concentrations, the model is in fitmodels, and
    # ( either one response is above cutoff OR force.fit == T OR it's the constant model.)
    to.fit <- (length(rmds) >= 4 && model %in% fitmodels && (length(which(abs(rmds) >= cutoff)) > 0 || force.fit ||
               model == "cnst"))
    fname <- paste0("fit", model) # requires each model function have name "fit____" where ____ is the model name
    # use do.call to call fit function; cnst has different inputs than others.
    assign(model, do.call(fname, list(
      conc = conc, resp = resp, bidirectional = bidirectional, verbose = verbose,
      nofit = !to.fit
    )))
    if (to.fit) {
      if (model %in% c("poly1", "poly2", "pow", "exp2", "exp3")) {
        # methods that grow without bound: top defined as model value at max conc
        assign(model, append(get(model), list(top = get(model)$modl[which.max(abs(get(model)$modl))]))) # top is taken to be highest model value
        assign(model, append(get(model), list(ac50 = acy(.5 * get(model)$top, get(model), type = model))))
      } else if (model %in% c("hill", "exp4", "exp5")) {
        # methods with a theoretical top/ac50
        assign(model, append(get(model), list(top = get(model)$tp)))
        assign(model, append(get(model), list(ac50 = get(model)$ga)))
      } else if (model == "gnls") {
        # gnls methods; use calculated top/ac50, etc.
        assign(model, append(get(model), list(top = acy(0, get(model), type = model, returntop = T))))
        # check if the theoretical top was calculated
          if(is.na(get(model)$top)){
            # if the theoretical top is NA return NA for ac50 and ac50_loss
            if(verbose){
              warning("'top' for 'gnls' is not able to be calculated returning NA.  AC50 for gain and loss directions are returned as NA.")
            }
            assign(model,append(get(model), list(ac50 = NA_real_,ac50_loss = NA_real_)))
          } else {
            assign(model, append(get(model), list(ac50 = acy(.5 * get(model)$top, get(model), type = model))))
            assign(model, append(get(model), list(ac50_loss = acy(.5 * get(model)$top, get(model), type = model, getloss = T))))
          }
        }
      }
    assign(model, append(get(model), list(func = gabi::tcplfit2_funcfitZR(model, fit=get(model))))) # EDIT attach fitted function with parameters to model for future curve plotting
  }
  # optionally print out AICs
  if (verbose) {
    print("aic values:")
    aics <- sapply(modelnames, function(x) {
      get(x)[["aic"]]
    })
    names(aics) <- modelnames
    print(aics)
    cat("Winner: ", modelnames[which.min(aics)])
  }

  # Produce logical object can.plot for tcplggplotter()
  shortnames <- modelnames[modelnames != "cnst"]
  successes <- sapply(shortnames, function(x) {
    get(x)[["success"]]
  })
  # EDIT below: declare can.plot object and append to out object
  if (do.plot && sum(successes, na.rm = T) == length(shortnames)) {
    can.plot <- TRUE
  } else {can.plot <- FALSE}

  # put all the model outputs into one list and return
  out <- c(
    mget(modelnames),
    list(can.plot = can.plot, modelnames = modelnames, ...)
  )

  return(out)
}
