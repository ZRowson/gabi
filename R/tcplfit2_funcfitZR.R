#' Produce curve fit function for tcpl fits
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Returns a function with fitted parameters from tcplfit2 model fitting.
#' Goal here is to provide a function for visualization with
#' gabi::tcplggplotter
#'
#' @details
#' Created: 09/09/2021
#' Last edit: 09/09/2021
#' Roxygen created this manual page on `r Sys.Date()` using R version
#' `r getRversion()`.
#' All models are equal to 0 at 0 concentration (zero background).
#'
#' @param model name of model to be transformed into function form.
#' @param fit params of model provided by fitting function, fit___.
#'    Entries in params depend upon the model fit.
#'
#' @return func - function form of model params provided
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
tcplfit2_funcfitZR <- function(model, fit) {
                        list2env(fit, envir = environment())
                        if (model == "hill") {
                          func <- function(x) {
                            tp/((1 + (ga/x)^p))}
                        } else if (model == "gnls") {
                          func <- function(x) {
                            tp/((1 + (ga/x)^p)*(1 + (x/la)^q))}
                        } else if (model == "poly1") {
                          func <- function(x) {
                            a*x}
                        } else if (model == "poly2") {
                          func <- function(x) {
                            a*(x/b + x^2/b^2)}
                        } else if (model == "pow") {
                          func <- function(x) {
                            a*x^p}
                        } else if (model == "exp2") {
                          func <- function(x) {
                            a*(exp(x/b) - 1)}
                        } else if (model == "exp3") {
                           func <- function(x) {
                             a*(exp((x/b)^p) - 1)}
                        } else if (model == "exp4") {
                          func <- function(x) {
                            tp*(1 - 2^(-x/ga))}
                        } else if (model == "exp5") {
                          func <- function(x) {
                            tp*(1 - 2^(-(x/ga)^p))}
                        } else func <- NULL

                        return(func)
                      }
