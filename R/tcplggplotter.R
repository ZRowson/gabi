#' Plot tcplfit2 output with ggplot2
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov)}
#'
#' Plots concentration response trend of multiple-concentration experiment
#' on a chemical. tcplfit2 functions are used to produce interpolating
#' fits on chemical response.
#'   \itemize {
#'   \item cnst - constant model
#'   \item hill - hill
#'   \item gnls - gain-loss
#'   \item poly1 - polynomial of 1st degree
#'   \item poly2 - polynomial of 2nd degree
#'   \item pwr - power function
#'   \item exp2 - exponential of 2nd degree
#'   \item exp3 - exponential of 3rd degree
#'   \item exp4 - exponential of 4th degree
#'   \item exp5 - exponential of 4th degree
#'   }
#'
#' @details
#' Last edit: 06/03/2021
#' Roxygen created this manual page on `r Sys.Date()` using R version
#' `r getRversion()`.
#' Used in conjunction with concRespCoreZR() and tcplfit2_coreZR().
#' Not meant to be used as a stand alone function. Graphics are meant to replicate
#' appearance of default graphics produced in tcplfit2::tcplfit2_core.
#'
#' @param resp Vector of test-group response extracted from tcpl row object
#' @param bresp Vector of baseline response extracted from tcpl row object
#' @param logc Log base 10 of vector of concentrations from tcpl row object
#' @param rmds Response medians of each test-concentration group
#' @param bmed Response median of baseline
#' @param params List of model-fit paramaeters returned from tcplfit2::tcplfit2_core.
#' List of N(models) elements, one for each of the models run (up to 10),
#' followed by a last element "modelnames", which is a  vector of model names so
#' other functions can easily cycle through the output. For a full list, see the
#' documentation for the individual fitting method functions. For each model there
#' is a sublist with elements including:
#'   \itemize{
#'     \item success - was the model successfully fit
#'     \item aic - the AIC value
#'     \item cov - success of the the covariance matrix calculation
#'     \item rme - root mean error of the data around the curve
#'     \item modl - vector of model values at the given concentrations
#'     \item tp - the top of the curve fit
#'     \item ga - the AC50 or Hill paramters
#'     \item er - the error term
#'     \item ... other paramters specific to the model (see the documentation for the specific models)
#'     \item tp_sd, ga_sd, p_sd, etc., the values of the standard deviations of the paramters for the models
#'     \item er_sd - standard deviation of the error term
#'     \item pars - the names of the parameters
#'     \item sds - the names of the standard deviations of the paramters
#'   }
#' @param summary list of tcplfit2 analysis output returned from tcplfit2::tcplhit2_core
#'
#' @return A ggplot2 object of tcplfit2 analysis displaying...
#'   \itemize {
#'     \item model-fits distinguished by color
#'     \item cutoff range respresented by gray box
#'     \item response by concentration
#'     \item median response by concentration group and \eqn{90 \leq} CI's
#'     \item hitcall produced by tcplfit2
#'     \item best fit by AIC
#'     }
#'
#' @import data.table
tcplggplotter <- function(resp, bresp, logc, rmds, bmed, params, summary) {
                    list2env(params, env = environment())

                    shortnames <- modelnames[modelnames != "cnst"]
                    # Consolidate x & y coordinates for resp and models
                      coordinates <- data.table::data.table(logc = logc[order(logc)], resp = resp[order(logc)],
                                                            sapply(shortnames, function(fit) {get(fit)[["modl"]][order(logc)]})
                                                            )
                      # "melt" to long format for easy grouping
                      coordinates <- data.table::melt(coordinates, id.vars = c("logc"), measure.vars = c("resp", as.character(shortnames)))
                      # Consolidate control response in df format for use with ggpolot2
                      bresp <- data.table::data.table(logc = min(logc)-1, value = bresp)
                    # Consolidate descriptive and inferential statistics
                      df.rmds <- data.frame(logc = c(as.numeric(row.names(rmds)), min(logc)-1), rmds = c(as.numeric(rmds), bmed)) # maybe possible to avoid declaring another df, try using ggplot2::stat_summary()
                      raw <- list(coordinates[variable=="resp"], bresp)
                      CIs <- lapply(raw, function (table){
                                            CIs <- table[, DNT.60.Analysis::calc_qCI(value, conf.level=0.90), by = logc]
                                            CIs[, .(lower=interval[2], upper=interval[1]), by = logc]
                                          }
                                    )
                      CIs <- data.table::rbindlist(CIs)

                    list2env(summary, env = environment())
                    # Create plot
                      plot <- ggplot2::ggplot() +
                                ggplot2::geom_point(data = coordinates, mapping = ggplot2::aes(x=logc, y=value, colour=variable)) + # Points for all models and response
                                ggplot2::geom_point(data = bresp, mapping = ggplot2::aes(x=logc, y=value), shape = 1) + # Response for control
                                ggplot2::geom_point(data = df.rmds, ggplot2::aes(x=logc, y=rmds), shape = 7, size = 4) + # Response medians
                                ggplot2::geom_errorbar(data = CIs, ggplot2::aes(x=logc, ymin=lower, ymax=upper), width = 0.095) + # CI's for response medians
                                ggplot2::geom_line(data = coordinates[variable!="resp"], ggplot2::aes(x=logc, y=value, colour=variable)) + # Plot model fits
                                ggplot2::scale_color_manual(values = c("black", RColorBrewer::brewer.pal(n=9,name="Set1"))) +
                                ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -cutoff, ymax= cutoff, alpha = .2) + # Add cutoff range
                                ggplot2::labs(title = paste("ConcResp of", name, "for", assay),
                                              # subtitle = paste("Cutoff =", ctoffStat, ", Best Fit =", fit_method),
                                              colour = "Models",
                                              y = "resp",
                                              caption = paste("Cutoff =", "2*MAD",
                                                              ", Best Fit =", fit_method,
                                                              ", hitcall =", summary$hitcall,
                                                              ", RMSE =", summary$rmse,
                                                              ", caikwt =", summary$caikwt)
                                                              )
                    return(plot)
                  }
