## ---------------------------
##
## Script Name: Hsieh Similarity Endpoints Draft
##
## Purpose of Script: Recreate similarity endpoints used in Hsieh et al. 2019 Application of
##                    Benchmark Concentration (BMC) Analysis on Zebrafish Data.
##
## Author: Zachary Rowson
##
## Date Created: 2022-02-24
##
## Email: Rowson.Zachary@epa.gov
##
## Working Directory: "/ccte/home2/zrowson/R/gabi"
##
## ---------------------------
##
## Notes: This is the rough draft of a function that will be exported from gabi.
##
##
## ---------------------------


library(data.table)


# Function will take exposure data for one chemical and an endpoint name; "Pearson", "Spearman", or "Cosine", as arguments.
load("data/DNT60pmr0.rda")
chemical <- "Fluoxetine"
pmr0 <- gabi::data_egids(as.data.table(DNT60pmr0))
group <- pmr0[cpid==chemical, unique(egid)]
data <- pmr0[cpid==chemical | (wllt=="v"&egid==group)][wllq == 1]

endpoint <- "pearson"


# Format data -------------------------------------------------------------


# Create fishID column to uniquely identify fish
data_copy <- data.table::copy(data)
data_copy[, fishID := paste(apid,coli,rowi,sep=".")]

# Melt data to long format and then widen such that columns are movement values for each unique fish
data_long <- data.table::melt(data_copy, measure.vars = grep("vt",names(data),value=T), variable.name = "t")
data_long[, t := as.numeric(gsub("vt","",t))]
data_by_fish <- data.table::dcast(data_long[!t%in%1:10], t ~ fishID)[, t := NULL]

# Identify fishID's of vehicle control
v_fishID <- data_long[wllt == "v", unique(fishID)]


# Calculate similarity endpoints --------------------------------------


# Calculate correlation of columns (vectors of movement data for each unique fish) and set negative correlation to 0
if (endpoint == "cosine") {
  x <- as.matrix(data_by_fish)
  y <- t(x) %*% x
  z <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  cor.mat <- 1 - z
} else {
  cor.mat <- cor(data_by_fish, method = endpoint, use = "pairwise.complete.obs")
  cor.mat[cor.mat < 0] <- 0
}

# Isolate correlation of vehicle control to treated fish and correlation of vehicle to vehicle
cor_VT <- cor.mat[row.names(cor.mat)%in%v_fishID, !(colnames(cor.mat)%in%v_fishID)]
cor_VV <- cor.mat[row.names(cor.mat)%in%v_fishID, colnames(cor.mat)%in%v_fishID]

# Calculate mean correlation by column,. Exclude matrix diagonal (==1) for vehicle correlations.
endpoint_t <- as.data.table(apply(cor_VT, 2, mean), keep.rownames = TRUE)
endpoint_v <- as.data.table(apply(cor_VV, 2, function(col) {
                                              relevant <- col[col!=1]
                                              mean(relevant)}),
                            keep.rownames = TRUE)

# Row bind rvals and fishIDs
endpoint <- rbind(endpoint_t, endpoint_v)[, `:=` (fishID = V1, rval = V2)][, `:=` (V1 = NULL, V2 = NULL)]

# Merge with original data
return <- data_copy[, !grep("vt",names(data),value=T), with=FALSE][endpoint, on = .(fishID)][, fishID := NULL]
