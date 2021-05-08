#' Plot tcplfit2 output with ggplot2
#'
#' Zachary Rowson (Rowson.Zachary#epa.gov)
#' Used in conjunction with concRespCoreZR() and tcplfit2_coreZR().
#' Not meant to be used as a stand alone function.
#'
#' @param row A named list that must include:
#'   \itemize{
#'     \item conc - list of concentrations (not in log units)
#'     \item resp - list of corresponding responses
#'     \item bmed - median of noise estimate.
#'     \item cutoff - noise cutoff
#'     \item onesd - 1 standard deviation of the noise (for bmd calculation)
#'   }
#'   Other elements (usually identifiers, like casrn) of row will be attached to
#'   the final output.
#' @param fitmodels Vector of model names to use.
#' @param conthits conthits = T uses continuous hitcalls, otherwise they're
#'   discrete.
#' @param aicc aicc = T uses corrected AIC to choose winning method; otherwise
#'   regular AIC.
#' @param force.fit If TRUE force the fitting to proceed even if there are no points
#'   outside of the bounds (default FALSE)
#' @param bidirectional If TRUE allow fitting to happen in both directions (default TRUE)
#' @param verbose  If TRUE, write extra output from tcplfit2_core (default FALSE)
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
#' @import data.table
tcplggplotter <- function(resp, bresp, logc, rmds, bmed, params, summary) {
  list2env(params, env = environment())

  shortnames <- modelnames[modelnames != "cnst"]
  # Consolidate x & y coordinates for resp and models
  coordinates <- data.table::data.table(logc = logc[order(logc)], resp = resp[order(logc)],
                                        sapply(shortnames, function(fit) {get(fit)[["modl"]][order(logc)]}))
  # "melt" to long format for easy grouping
  coordinates <- data.table::melt(coordinates, id.vars = c("logc"), measure.vars = c("resp", as.character(shortnames)))
  df.rmds <- data.frame(logc = c(as.numeric(row.names(rmds)), -Inf), rmds = c(as.numeric(rmds), bmed)) # maybe possible to avoid declaring another df, try using ggplot2::stat_summary()
  # Consolidate control response in df format for use with ggpolot2
  bresp <- data.frame(logc = -Inf, bresp = bresp)

  list2env(summary, env = environment())
  # Create plot
  print(ggplot2::ggplot() +
          ggplot2::geom_point(data = coordinates, mapping = ggplot2::aes(x=logc, y=value, colour=variable)) + # Points for all models and response
          ggplot2::geom_point(data = bresp, mapping = ggplot2::aes(x=logc, y=bresp), shape = 15) + # Response for control
          ggplot2::geom_point(data = df.rmds, ggplot2::aes(x=logc, y=rmds), shape = 7, size = 4) + # Response medians
          ggplot2::geom_line(data = coordinates[variable!="resp"], ggplot2::aes(x=logc, y=value, colour=variable)) + # Plot model fits
          ggplot2::scale_color_manual(values = c("black", RColorBrewer::brewer.pal(n=9,name="Set1"))) +
          ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -cutoff, ymax= cutoff, alpha = .2) + # Add cutoff range
          ggplot2::labs(title = paste("ConcResp of", name, "for", assay),
                        # subtitle = paste("Cutoff =", ctoffStat, ", Best Fit =", fit_method),
                        colour = "Models",
                        y = "resp",
                        caption = paste("Cutoff =", ctoffStat, ", Best Fit =", fit_method, ", hitcall =", summary$hitcall, ", RMSE =", summary$rmse, ", caikwt =", summary$caikwt))
  )
}
