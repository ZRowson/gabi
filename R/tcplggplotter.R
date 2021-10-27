#' Plot tcplfit2 output with ggplot2
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
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
#' @param unit.conc - unit of chemical concentration. Defaults to $\mu$M
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
tcplggplotter <- function(resp, bresp, conc, logc, rmds, bmed, params, summary, unit.conc = paste0("\U03BC","M")) {

                    # plot concentration response curves and other statistics related to model of chemical activity

                    # consolidate x & y coordinates for resp and bresp

                    ## create data.table for responses and a sudo concentration as x coordinate for bresp
                    conc.sudo <- min(conc) / 10
                    logc <- log10(c(conc[order(conc)], rep(conc.sudo, length(bresp))))
                    resp.xy <- data.table(wllt = c(rep("t", length(resp)), rep("v", length(bresp))),
                                          logc = logc,
                                          resp = c(resp[order(conc)], bresp))

                    # create x and y coordinates for plotting curve fits

                    ## import curve fits
                    list2env(params, env = environment())

                    ## use curve functions to find f(x)
                    x <- seq(from = min(conc)/10, to = max(conc), by = 0.01)
                    shortnames <- modelnames[modelnames != "cnst"] # this may throw an error if some functions were not fit
                    curve.xy <- data.table(logc = log10(x),
                                           sapply(shortnames, function(fit) {
                                                               func <- get(fit)[["func"]]
                                                               func(x)}))

                    ## "melt" curve coordinates to long format for plotting and grouping
                    curve.xy.lng <- data.table::melt(curve.xy, id.vars = "logc", measure.vars = as.character(shortnames))

                    # gather descriptive and inferential statistics for plotting

                    ## construct Dunnett's CI's

                    ## run test and create a data.frame to hold confidence intervals
                    D.test <- DescTools::DunnettTest(x = resp.xy[,resp], g = logc) # Dunnett's many-to-one test
                    CIs <- as.data.frame(D.test[[1]][, c("lwr.ci","upr.ci")])
                    logc.CI <- unique(log10(conc[order(conc)]))
                    row.names(CIs) <- logc.CI

                    ## gather other inferential and descriptive statistics from summary of tcplfit2 run
                    list2env(summary, env = environment()) # might not be necessary to call this into the environment

                    ## round summary statistics for captions
                    stats <- c("hitcall", "rmse", "caikwt", "top", "ac50", "bmr", "bmdl", "bmd", "bmdu")
                    cap <- round(summary[stats], 3)

                    ## create plot
                    plot <- ggplot() +
                      geom_jitter(resp.xy, mapping = aes(x=logc, y=resp, shape=wllt), width = 0.05) +
                        stat_summary(resp.xy, mapping = aes(x=logc, y=resp, size=4, shape="mean"),fun = "mean", geom = "point") +
                        scale_shape_manual(values = c(8, 16, 1), labels = c("Mean","Test","Vehicle Control")) +
                        geom_errorbar(CIs, mapping = aes(x=as.numeric(row.names(CIs)), ymin=lwr.ci, ymax=upr.ci), width = 0.095) +
                      geom_line(curve.xy.lng, mapping = aes(x=logc, y=value, colour=variable)) +
                        scale_color_manual(values = viridis::turbo(9)) +
                      geom_hline(aes(yintercept = c(-bmr, bmr), linetype = "BMR")) +
                        scale_linetype_manual(values = c("dotted")) +
                      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -cutoff, ymax= cutoff, fill = "Cutoff Range"), alpha = .2) +
                      guides(size = "none",
                             color = guide_legend(order=4),
                             linetype = guide_legend(order=2),
                             fill = guide_legend(order=3),
                             shape = guide_legend(order=1)) +
                      labs(title = paste("Concentration Response of", name, "for", assay),
                           colour = "Models",
                           linetype = "",
                           fill = "",
                           shape = "",
                           x = bquote("log"[10]~.(unit.conc)),
                           y = expression(paste("Response - ", bar("Vehicle Control Response"))),
                           caption = paste0("hitcall=", cap["hitcall"],
                                            ", Best Fit=", fit_method,
                                            ", RMSE=", cap["rmse"],
                                            ", caikwt=", cap["caikwt"],
                                            ", cutoff=", round(cutoff,3),
                                            ", top=", cap["top"],
                                            ", AC50=", cap["ac50"],
                                            ", BMR=", cap["bmr"],
                                            ", BMD.set=", paste0("{",cap["bmdl"],", ",cap["bmd"],", ",cap["bmdu"],"}")
                           )
                      )

                      # plot <- ggplot() +
                      #           geom_point(resp.xy, mapping = aes(x=logc, y=resp, shape=wllt)) +
                      #             stat_summary(resp.xy, mapping = aes(x=logc, y=resp, shape="mean"), fun = "mean", geom = "point") +
                      #             scale_shape_manual(values = c(7, 16, 1)) +
                      #             geom_errorbar(CIs, mapping = aes(x=as.numeric(row.names(CIs)), ymin=lwr.ci, ymax=upr.ci), width = 0.095) +
                      #           geom_line(curve.xy.lng, mapping = aes(x=logc, y=value, colour=variable)) +
                      #             scale_color_manual(values = viridis::turbo(9)) +
                      #           geom_hline(aes(yintercept = c(-bmr, bmr), linetype = "BMR")) +
                      #             scale_linetype_manual(values = c("dotted")) +
                      #           geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -cutoff, ymax= cutoff, fill = "Cutoff Range"), colour = "darkgrey", alpha = .2) + # Add cutoff range
                      #           labs(title = paste("Concentration Response of", name, "for", assay),
                      #                shape = "",
                      #                colour = "Models",
                      #                linetype = "",
                      #                fill = "",
                      #                x = bquote("log"[10]~.(unit.conc)),
                      #                y = expression(paste("Response - ", bar("Vehicle Control Response"))),
                      #                caption = paste0("hitcall=", cap["hitcall"],
                      #                                 ", Best Fit=", fit_method,
                      #                                 ", RMSE=", cap["rmse"],
                      #                                 ", caikwt=", cap["caikwt"],
                      #                                 ", cutoff=", round(cutoff,3),
                      #                                 ", top=", cap["top"],
                      #                                 ", AC50=", cap["ac50"],
                      #                                 ", BMR=", cap["bmr"],
                      #                                 ", BMD.set=", paste0("{",cap["bmdl"],", ",cap["bmd"],", ",cap["bmdu"],"}")
                      #                          )
                      #           )
                    return(plot)
}

# c("black", "cyan", "dark magenta", "red", "darkgoldenrod1",
#   "hotpink", "chartreuse", "darkred", "blue1")) +
