#' This function produces a list in format of tcplfit2 "row" for named chemicals
#'   from a data.table formatted as mc0.
#' @param table is a mc0 level tcpl dataset.
#' @param chemicals is a vector of strings indicating test chemical data to be processed.
#' @param ctoff is a decimal in (0,1) or a string indicating the statistic to be used as a cutoff value.
#' @param ratio is a logical parameter indicating return of resp in log(test/mean(baseline)) form.
#' @param dist is a logical parameter indicating return of resp in resp-mean(baseline) format.
#' @param global is a logical parameter with TRUE indicating assignment in gloabl environment.
#' @import data.table
as_row <- function(table, chemicals, ctoff, ratio = FALSE, dist = FALSE, global = TRUE) {
                # Check that User input will not cause error
                  if (length(ctoff) > 1) {
                      stop("ctoff must have length 1.")
                  } else if (ctoff!="MAD" & !(ctoff %in% c(0.01,0.05,0.95,0.99))) {
                      stop("Set cutoff = 0.05 , 0.95 , 0.01 , 0.99 , or \"MAD\".")
                  }
                  if (ratio==FALSE && dist==FALSE){
                      stop("Set dist = TRUE or ratio = TRUE.")
                  } else if (ratio==TRUE && dist==TRUE) {
                      stop("Only one method can be chosen.")
                  } else if(ratio==TRUE && grepl("log",unique(`table`[,acid]))) {
                      stop("log transformed data cannot be analyzed via ratio method. /nSet ratio = FALSE and dist = TRUE.")
                  }
                chmcls <- chemicals
                acid <- unique(`table`[,acid])
                for (chm in chmcls) {
                  # Consolidate baseline and test data
                    plts <- unique(`table`[cpid==chm, apid])
                    brval <- `table`[wllt=="v" & apid%in%plts, rval]
                    trval <- `table`[cpid==chm & wllt!="0", rval]
                    conc <- `table`[cpid == chm & wllt != "0", conc]

                  bavg <- NULL
                  # Response produced depends on User chosen parameters
                    if (ratio == TRUE) {
                      resp <- log10((trval / bavg)+1)
                      bresp <- log10((brval / bavg)+1)
                      method <- "r"
                    } else if (dist == TRUE) {
                      resp <- log10(trval + 1)
                      bresp <- log10(brval + 1)
                      bavg <- stats::median(bresp)
                      resp <- resp - bavg
                      bresp <- bresp - bavg
                      method <- "d"
                    }
                    bmed <- stats::median(bresp)
                    if (ctoff == 0.05) {
                      cutoff <- abs(stats::quantile(bresp, probs=0.05))
                    } else if (ctoff == 0.95) {
                      cutoff <- abs(stats::quantile(bresp, probs=0.95))
                    } else if (ctoff == "MAD") {
                      cutoff <- 2*stats::mad(bresp)
                    } else if (ctoff == 0.01) {
                      cutoff <- abs(stats::quantile(bresp, probs=0.01))
                    } else if (ctoff == 0.99) {
                      cutoff <- abs(stats::quantile(bresp, probs=0.99))
                    }

                  onesd <- stats::sd(bresp)

                  # Generate and assign row vector
                    row <- list(conc = conc,
                                resp = resp,
                                bresp = bresp,
                                bmed = bmed,
                                cutoff = cutoff,
                                onesd = onesd,
                                name = chm,
                                assay = acid,
                                ctoffStat = ctoff)
                    if(global==TRUE) {
                      assign(paste("row", acid, method, ctoff, chm, sep="_"), row, envir = globalenv())
                    } else {
                      assign(paste("row", acid, method, ctoff, chm, sep="_"), row, envir = parent.frame())
                    }
                }
              }

