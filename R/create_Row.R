#' This function produces a list in format of tcplfit2 "row" for all chemicals tested in an assay.
#' @param table is a mc0 level tcpl dataset
#' @param ratio is a logical parameter indicating return of resp in log(test/mean(baseline)) form
#' @param dist is a logical parameter indicating return of resp in resp-mean(baseline) format
#' @param global is a logical parameter with TRUE indicating assignment in gloabl environment.
#' @import data.table
create_Row <- function(table, ratio = FALSE, dist = FALSE, global = TRUE) {
                # Check that User input will not cause error
                if (ratio==FALSE && dist==FALSE){
                    stop("Set dist = TRUE or ratio = TRUE.")
                } else if (ratio==TRUE && dist==TRUE) {
                    stop("Only one method can be chosen.")
                } else if(ratio==TRUE && grepl("log",unique(`table`[,acid]))) {
                    stop("log transformed data cannot be analyzed via ratio method. /nSet ratio = FALSE and dist = TRUE.")
                }
                chmcls <- unique(`table`[wllt=="t", cpid])
                acid <- unique(`table`[,acid])
                for (chm in chmcls) {
                  # Determine background variation
                  plts <- unique(`table`[cpid==chm, apid])
                  brval <- `table`[wllt=="v" & apid%in%plts, rval]
                  bmad <- stats::mad(brval)
                  bmed <- 0
                  cutoff <- 3*bmad # 3 is an arbitrary value... How do we determine a better one?
                  bavg <- median(brval)
                  onesd <- stats::sd(brval)

                  # Consolidate test data
                  conc <- `table`[cpid == chm & wllt != "0", conc]
                  # Response produced depends on User chosen parameters
                  if (ratio == TRUE) {
                    resp <- log10(`table`[cpid==chm & wllt!="0", rval] / bavg)
                    method <- "r"
                  } else if (dist == TRUE) {
                    resp <- `table`[cpid==chm & wllt!="0", rval] - bavg
                    method <- "d"
                  }

                  # Generate and assign row vector
                  row <- list(conc = conc,
                              resp = resp,
                              bmed = bmed,
                              cutoff = cutoff,
                              onesd = onesd)
                  if(global==TRUE) {
                    assign(paste("row", acid, method, chm, sep="_"), row, envir = globalenv())
                  } else {
                    assign(paste("row", acid, method, chm, sep="_"), row, envir = parent.frame())
                  }
                }
              }

