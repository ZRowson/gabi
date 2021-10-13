#' Calculate confidence intervals for a quantile
#'
#' Calculates confidence intervals at specified confidence-level for a
#' quantile of users choosing using empirical order-statistics.
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @details
#' Last edit: 06/04/2021
#' Roxygen created this manual page on ``r Sys.Date()`` using R version
#' ``r getRversion()``.
#' Code written by Arthur Charpentier taken from his post on
#' freakonometrics \url{https://freakonometrics.hypotheses.org/4199}
#' Was checked by Zachary Rowson for correctness.
#'
#' @param x is a vector of sample values
#' @param quantile is a numeric representing quantile for which a CI is
#'   calculated. Defaults to median or 50% quantile.
#' @param conf.level is a numeric indicator of desired confidence-level
#'   of constructed interval.
#'
#' @return list
#'   \itemize{
#'     \item interval - vector of upper and lower bounds of confidence interval
#'     \item confidence - confidence level associated with calculated interval
#'     \item order.stats - location of interval bounds in ordered sample
#'    }
#'
calc_qCI <- function(x, quantile = 50, conf.level = 0.95) {
              # Probability that i-th order-statistic falls below quantile
              # can be modelled as a binomial RV with probability of
              # success pstar and n trials
                # Probability of success is equal to percent of data below quantile
                pstar <- quantile / 100
                # Number of trials equals size of sample
                n <- length(x)

              alpha <- 1 - conf.level

              # lower bound for a symmetric CI is ~alpha/2-quantile of Bin distribution
                a <- stats::qbinom(alpha/2, n , pstar)
              # Find P(X < a) where X ~ Bin(n, pstar)
                alpha1 <- stats::pbinom(a-1, n, pstar)
              # Median is between a and b so we want value of b where b-1 values are below
              # median, add 1
                b <- stats::qbinom(1-alpha/2, n, pstar) + 1
              # Find P(median > b)
                alpha2 <- 1 - stats::pbinom(b-1, n, pstar)

              c.lower <- sort(x)[a]
              c.upper <- sort(x)[b]
              conf.level <- 1 - alpha1 - alpha2

              return(list(interval = c(c.upper,c.lower),
                          confidence = conf.level,
                          order.stats = c(a, b)
                          )
                     )
            }
