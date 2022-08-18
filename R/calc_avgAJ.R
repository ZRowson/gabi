#' Calculate average acceleration and jerk
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Calculates average acceleration and jerk per light level (dark or
#' light) for individual fish. User provides a lmr0 table and inputs
#' number of time periods for each experimental stage: acclimation,
#' light, and dark.
#'
#' @details Excludes acceleration and jerk values during transitions
#' (freeze and startle) from calculation of averages.
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
#' @param no.A number of measurements made in acclimation period
#' @param no.L number of measurements made in light period
#' @param no.D number of measurements made in dark period
#'
#' @return A list with average jerk in each light level for each fish
#'   \itemize{
#'     \item avgA_L - average acceleration in light
#'     \item avgA_D - average acceleration in dark
#'     \item avgJ_L - average jerk in light
#'     \item avgJ_D - average jerk in dark
#'   }
#' @export
calc_avgAJ <- function(data, no.A = 10, no.L = 20, no.D = 20) {

              # iteratively calculate acceleration and jerk values

                rownames(data) <- NULL
                t <- grep("vt", names(data), value = TRUE)

              # calculate acceleration data
                for (i in 2:50) {
                  n <- t[i]; `n-1` <- t[i-1]
                  data[, ncol(data)+1] <- data[[`n`]] - data[[`n-1`]]
                  colnames(data)[ncol(data)] <- paste0("za", `i`)
                }

              # calculate jerk data
                t_a <- grep("za", names(data), value = TRUE)
                for (i in 3:50) {
                  n <- t_a[i-1]
                  `n-1` <- t_a[i-2]
                  data[[paste0("zj", i)]] <- data[[n]] - data[[`n-1`]]
                }
                t_j <- grep("zj", names(data), value = TRUE)
              # calculate average acceleration in light and dark per fish
                sets <- list(avgA_L = t_a[(no.A+1):(no.A+no.L-1)],
                             avgA_D = t_a[(no.A+no.L+1):(no.A+no.L+no.D-1)],
                             avgJ_L = t_j[(no.A+1):(no.A+no.L-2)],
                             avgJ_D = t_j[(no.A+no.L+1):(no.A+no.L+no.D-2)]
                        )
                avgAJ <- lapply(sets, function(set) rowMeans(data[set]))

              return(avgAJ)
}
