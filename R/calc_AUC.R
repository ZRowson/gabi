#' Calculate AUC in light and dark for individual fish
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Calculates area under curve for a chosen light level (light or dark).
#' Uses trapezoidal rule to approximate integral of speed function.
#' User provides a pmr0 table and specifies number of time periods per
#' experimental stage: acclimation, light, and dark.
#'
#' @details Last edit 08/18/2022.
#'
#' @param data is a pmr0 formatted data.table
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
#' @return A list with AUC in each light level for each fish
#'   \itemize{
#'     \item AUC_L - AUC in light
#'     \item AUC_D - AUC in dark
#'     \item AUC_r - AUC_D / AUC_L
#'  }
#' @export
calc_AUC <- function(data, no.A = 10, no.L = 20, no.D = 20) {

                trapezoidal <- function(x, data) {
                  xy <- data[x]
                  fl <- x[c(1, length(x))] # first and last values in x
                  rowSums(xy[!(x%in%fl)]) + (rowSums(xy[fl])/2)}

                rownames(data) <- NULL
              # separate time periods based on associated experimental period
                t <- grep("vt", names(data), value = TRUE)
                sets <- list(AUC_L = t[(no.A+1):(no.A+no.L)],
                             AUC_D = t[(no.A+no.L+1):(no.A+no.L+no.D)],
                             AUC_T = t[(no.A+1):(no.A+no.L+no.D)]
                            )
              # calculate AUC endpoints
                temp <- data[t]
                AUC <- lapply(sets, trapezoidal, data = data)
                AUC[["AUC_r"]] <- AUC[["AUC_D"]] / (AUC[["AUC_L"]] + 1)

              return(AUC)
            }
