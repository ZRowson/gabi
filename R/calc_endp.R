#' Calculate endpoint values from lmr0 formatted table
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Calculates endpoint values for tcpl analysis from pmr0
#' formatted table. Function is a wrapper function for
#' endpoint calculating functions in calc_ family. Endpoint
#' values fill rval column in mc0 table after
#' conversion with gabi::as_mc0.
#'
#' @details Details on what endpoints represent can be found
#' by searching for appropriate calc_ function (?gabi::calc_...).
#' Last edit 08/18/2022.
#'
#' @param data is a lmr0 formatted data.table
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id
#'     \item cpid - chemical name
#'     \item apid - assay plate id
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'     \item wllq - well quality indicates if observation is viable for analysis
#'     \item conc - concentration of chemical
#'     \item tj - measurements at time period j for j /in {1,2,3,...,n}
#'   }
#' @param no.A - number of measurements made in acclimation period
#' @param no.L - number of measurements made in light period
#' @param no.D - number of measurements made in dark period
#'
#' @param rval - string representing endpoint of interest. Can equal...
#'   \itemize{
#'     \item AUC_L - area under curve in light
#'     \item AUC_D - area under curve in dark
#'     \item AUC_T - area under curve over whole time-series
#'     \item AUC_r - ratio of AUC_D/AUC_L
#'     \item avgS_L - average speed in light
#'     \item avgS_D - average speed in dark
#'     \item avgS_T - average speed over whole time series
#'     \item avgA_L - average acceleration in light
#'     \item avgA_D - average acceleration in dark
#'     \item avgJ_L - average jerk in light
#'     \item avgJ_D - average jerk in dark
#'     \item strtlA - acceleration during startle, light-dark, transition
#'     \item strtlAavg - difference between apeed in last period of dark and avgS_L
#'     \item strtlF - startle factor, ratio of speed in first period of dark over speed in last period of light
#'     \item frzA - acceleration during freeze, dark-light, transition
#'     \item frzF - freeze factor, ratio of speed in first period of light over speed in last period of acclimation
#'   }
#'
#' @param no.A number of measurements made in acclimation period
#' @param no.L number of measurements made in light period
#' @param no.D number of measurements made in dark period
#'
#' @return A vector with user specified endpoint values
#' @export
calc_endp <- function (data, no.A=10, no.L=20, no.D = 20, rval = NULL) {


                if (is.null(rval) || !(rval %in% c("AUC_L", "AUC_D", "AUC_T", "AUC_r", "avgS_L", "avgS_D",
                                                   "avgS_T", "avgA_L", "avgA_D", "avgJ_L", "avgJ_D","strtlA",
                                                   "strtlAavg", "strtlF", "frzA","frzF", "hbt_L", "hbt_D"))) {
                  stop("rval improperly specified")
                }

                # deal with gosh darn dependency thing
                  data.df <- as.data.frame(data)

                # make call to appropriate calc_ function based on rval argument
                  if (rval %in% c("AUC_L", "AUC_D", "AUC_T", "AUC_r")) {
                      endp <- gabi::calc_AUC(data.df, no.A, no.L, no.D)[[rval]]
                  } else if (rval %in% c("avgS_L", "avgS_D", "avgS_T")) {
                      endp <- gabi::calc_avgS(data.df, no.A, no.L, no.D)[[rval]]
                  } else if (rval %in% c("avgA_L", "avgA_D", "avgJ_L", "avgJ_D")) {
                      endp <- gabi::calc_avgAJ(data.df, no.A, no.L, no.D)[[rval]]
                  } else if (rval %in% c("frzA","frzF","strtlA", "strtlAavg", "strtlF")) {
                      endp <- gabi::calc_trans(data.df, no.A, no.L, no.D)[[rval]]
                  } else if (rval %in% c("hbt_L", "hbt_D")) {
                    endp <- gabi::calc_hbt(data.df, no.A, no.L, no.D)[[rval]]
                  }


                return(endp)
             }
