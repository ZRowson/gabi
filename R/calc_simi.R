#' Replicate "Similarity" endpoints from Hsieh et al. 2019
#'
#' Reference: Hsieh JH, Ryan K, Sedykh A, Lin JA, Shapiro AJ, Parham F, Behl M. Application of benchmark concentration (BMC) analysis on zebrafish data: A new perspective for quantifying toxicity in alternative animal models. Toxicol Sci. 2019 Jan 1;167(1):92-104. doi: 10.1093/toxsci/kfy258.
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Treats each zebrafish movement track as a vector and calculates pairwise correlation coefficients of tracking vector to
#' each control fish's tracking vector. These correlation coefficients are averaged for each individual fish and used as an
#' endpoint for BMC analysis.
#'
#' @details Pearson's or Spearman's correlation coefficient, or Cosine similarity is calculated. Negative values are
#' given a value of 0. Control response is calculated by comparing controls to one another. Correlation of a control
#' fish to itself is omitted from calculation of average.
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
#'
#' @param endpoint is a character object indicating the measure of correlation used. Can be "pearson", "spearman", or
#' "cosine". Must be all lower case. Defaults to "pearson".
#'
#' @return A vector with endpoint values for all fish.
#'
#' @export
calc_simi <- function(data, no.A = 10, endpoint = "pearson") {

              # For each chemical
                ## Identify control / egid
                ## Compare test to control, control to control, and produce endpoint values

              copy <- data.table::copy(data)
              data_egids <- gabi::data_egids(copy)
              chemicals <- data_egids[wllt == "t", unique(cpid)]

              rvalList <- lapply(chemicals, function(chemical) {

                # Format data -------------------------------------------------------------


                # Isolate exposure data for chemical
                group <- data_egids[cpid == chemical, unique(egid)]
                data_1chm <- data_egids[cpid==chemical | (wllt=="v"&egid==group)][wllq == 1]

                # Create unique identifier for fish
                data_1chm[, fishID := paste(apid,rowi,coli,sep=".")]

                # Melt data to long format and then widen such that columns are movement values for each unique fish
                long <- data.table::melt(data_1chm, measure.vars = grep("vt",names(data_1chm),value=T), variable.name = "t")
                long[, t := as.numeric(gsub("vt","",t))]
                # Exlude acclimation and widen
                data_by_fish <- data.table::dcast(long[!t <= no.A], t ~ fishID)[, t := NULL]

                # Identify fishID's of vehicle control
                v_fishID <- long[wllt == "v", unique(fishID)]

                # identify fish that do not move
                no_mov_fishID <- long[!t <= no.A, .(no_mov = all(value==0)), by = fishID][no_mov == TRUE, fishID]


                # Calculate similarity endpoints --------------------------------------


                # Calculate correlation of columns (vectors of movement data for each unique fish) and set negative correlation to 0
                # Set correlation coefficients of non-moving fish to 0
                if (endpoint == "cosine") {
                  x <- as.matrix(data_by_fish)
                  y <- t(x) %*% x
                  z <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
                  cor.mat <- 1 - z
                } else {
                  cor.mat <- cor(data_by_fish, method = endpoint, use = "pairwise.complete.obs")
                }
                cor.mat[which((cor.mat<0) == TRUE)] <- 0
                cor.mat[no_mov_fishID,] <- cor.mat[,no_mov_fishID] <- 0

                # Isolate correlation of vehicle control to treated fish and correlation of vehicle to vehicle
                cor_VT <- cor.mat[row.names(cor.mat)%in%v_fishID, !(colnames(cor.mat)%in%v_fishID)]
                cor_VV <- cor.mat[row.names(cor.mat)%in%v_fishID, colnames(cor.mat)%in%v_fishID]

                # Calculate mean correlation by column. Exclude matrix diagonal (=1) for vehicle to vehicle correlations.
                rval_t <- as.data.table(apply(cor_VT, 2, mean), keep.rownames = TRUE)
                diag(cor_VV) <- NA
                rval_v <- as.data.table(apply(cor_VV, 2, mean, na.rm = TRUE), keep.rownames = TRUE)

                # Row bind test and vehicle control rvals
                rval <- rbind(rval_t,rval_v)[, `:=` (fishID = V1, rval = V2)][, `:=` (V1 = NULL, V2 = NULL)]

                # Return endpoint values linked with fishID
                rval

              })

              # Create one data.table of rvals by fishID and merge with original data to maintain order of fish
              rvals <- data.table::rbindlist(rvalList)
              copy[, fishID := paste(apid,rowi,coli,sep=".")]
              copy[rvals, on = .(fishID)]

  return(simi)
}
