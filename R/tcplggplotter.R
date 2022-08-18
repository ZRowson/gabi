#' Plot tcplfit2 output with ggplot2
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
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
#' Last edit: 12/13/2021
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
#' @param lam.hat Numeric power used to Box-Cox transform data
#' @param shift Numeric value used to shift data values from zero for transformation
#' @param params List of model-fit parameters returned from tcplfit2::tcplfit2_core.
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
#' @param verbose.plot If TRUE, ggplots will have a caption listing summarizing statistics (default TRUE)
#' @param unit.conc unit of chemical concentration (default $\mu$M)
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
#' @import ggplot2
#' @export
tcplggplotter <- function(endp, resp, bresp, conc, logc, rmds, bmed, lam.hat, shift, params, summary, verbose.plot, unit.conc = paste0("\U03BC","M")) {

                    # plot concentration response curves and other statistics related to model of chemical activity

                    # consolidate x & y coordinates for resp and bresp

                    ## create data.table for responses and a pseudo concentration as x coordinate for bresp
                    logc <- log10(conc[order(conc)])
                    logc.sudo <- floor(min(logc)) - 1
                    conc.sudo <- 10^(logc.sudo)
                    concr <- c(conc[order(conc)], rep(conc.sudo, length(bresp)))
                    logc <- c(logc, rep(logc.sudo, length(bresp)))
                    resp.xy <- data.table(wllt = c(rep("t", length(resp)), rep("v", length(bresp))),
                                          conc = concr,
                                          resp = c(resp[order(conc)], bresp))

                    # create x and y coordinates for plotting curve fits

                    ## import curve fits
                    list2env(params, env = environment())

                    ## use winning fit's function to find f(x)
                    fit_method <- summary$fit_method
                    fit.func <- get(fit_method)[["func"]]
                    x <- seq(from = conc.sudo, to = max(conc), by = 0.01)
                    fit.xy <- data.table(fit = fit_method, conc = x, resp.hat = fit.func(x))

                    # gather descriptive and inferential statistics for plotting

                    ## construct Dunnett's CI's

                    ## run test and create a data.frame to hold confidence intervals
                    D.test <- DescTools::DunnettTest(x = resp.xy[,resp], g = concr) # Dunnett's many-to-one test
                    CIs <- as.data.frame(D.test[[1]][, c("lwr.ci","upr.ci")])
                    conc.CI <- unique(conc[order(conc)])
                    row.names(CIs) <- conc.CI

                    ## gather other inferential and descriptive statistics from summary of tcplfit2 run
                    need <- names(summary)[which(names(summary)!="conc")]
                    list2env(summary[need], env = environment()) # might not be necessary to call this into the environment

                    ## choose sign for bmr vy looking at direction of response
                    if(sign(top) == 1) {
                      bmr.plot <- bmr
                    } else {bmr.plot <- (-1*bmr)}

                    ## round summary statistics for captions
                    stats <- c("hitcall", "rmse", "caikwt", "top", "ac50", "bmr", "bmdl", "bmd", "bmdu")
                    cap <- round(as.numeric(summary[stats]), 3)
                    names(cap) <- stats

                    ## create caption
                    if (verbose.plot) {
                      caption <- paste0("hitcall=", cap["hitcall"],
                                        ", Winning Fit=", fit_method,
                                        ", cutoff=", round(cutoff,3),
                                        ", top=", cap["top"],
                                        ", AC50=", cap["ac50"],
                                        ", BMR=", cap["bmr"],
                                        ", BMC.set=", paste0("{",cap["bmdl"],", ",cap["bmd"],", ",cap["bmdu"],"}, "),
                                        paste0("Box-Cox Parameters: ","\U03BB","=",lam.hat," Shift=",shift)
                                  )
                    } else caption <- NULL

                    ## create endpoint descriptions
                    titles <- gabi::whatis_Endp(endp)
                    title <- titles[[1]]
                    if (length(titles) > 1) {
                      subtitle <- titles[[2]]
                    } else subtitle <- ""

                    ## create x-axis breaks and labels
                    x.max <- floor(log10(max(concr)))
                    x.breaks <- 10^(seq(from=min(logc), x.max))
                    x.labels <- c("Control", format(x.breaks[-1], scientific=TRUE))

                    # deal with errant bmd range calculations
                    if (!is.na(bmd)) {
                      if (is.na(bmdu)) bmdu <- Inf
                      if (is.na(bmdl)) bmdl <- 0
                    }

                    ## create plot
                    plot <- ggplot() +
                              geom_jitter(resp.xy, mapping = aes(x=conc, y=resp, shape=wllt), width = 0.05) +
                                stat_summary(resp.xy, mapping = aes(x=conc, y=resp, size=4, shape="mean"),fun = "mean", geom = "point") +
                                scale_shape_manual(values = c(8, 16, 1), labels = c("Mean","Test","Vehicle Control")) +
                              geom_errorbar(CIs, mapping = aes(x=as.numeric(row.names(CIs)), ymin=lwr.ci, ymax=upr.ci), width = 0.095) +
                              geom_line(fit.xy, mapping = aes(x=conc, y=resp.hat, linetype="Winning Fit")) +
                              geom_hline(aes(yintercept = c(bmr.plot), linetype = "BMR")) +
                              geom_hline(aes(yintercept = 0, linetype = "BMC"), alpha = 0) + # "hacky" way of avoiding "cross" bug in ggplot legend
                              geom_vline(aes(xintercept = bmd), linetype = "dashed") +
                                scale_linetype_manual(values = c("dashed","dotted","solid")) +
                              geom_rect(aes(xmin = bmdl, xmax = bmdu, ymin = -Inf, ymax = Inf, fill = "CI for BMC"), alpha = .2) +
                              geom_rect(aes(xmin = 0, xmax = Inf, ymin = -cutoff, ymax= cutoff, fill = "Cutoff Range"), alpha = .2) +
                                scale_fill_manual(values = c("#00BFC4","red")) +
                              guides(size = "none",
                                     color = guide_legend(order=4),
                                     linetype = guide_legend(order=2),
                                     fill = guide_legend(order=3),
                                     shape = guide_legend(order=1)) +
                              labs(title = paste0(name, " for ", title, " (", endp, ")"),
                                   subtitle = paste(subtitle),
                                   colour = "",
                                   linetype = "",
                                   fill = "",
                                   shape = "",
                                   x = unit.conc, # bquote("log"[10]~.(unit.conc)),
                                   y = expression(paste("Response - ", bar("Vehicle Control Response"))),
                                   caption = caption) +
                              scale_x_continuous(trans = "log10", breaks = x.breaks, labels = x.labels) +
                              theme_bw() +
                              theme(text=element_text(size=14))
                    return(plot)
}

# c("black", "cyan", "dark magenta", "red", "darkgoldenrod1",
#   "hotpink", "chartreuse", "darkred", "blue1")) +
