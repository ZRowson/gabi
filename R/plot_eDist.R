#' Plot Endpoint Distributions by Concentration Group as Scatter Plots
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Plots endpoint data for a specific chemical and corresponding control
#' as scatter-plots grouped by chemical concentration. Takes as arguments
#' data in mc0 format and a chemical name.
#'
#' @details
#' Created: 10/12/2021
#' Last edit: 11/15/2021
#'
#' Scatter-plots are provided rather than box-plots or violin-plots to provide
#' as much transparency as possible as to how data appear. This action is in
#' concordance with Martin C. Michel et. al. 2020,
#' *New Author Guidelines for Displaying Data and Reporting Data Analysis and Statistical Methods in Experimental Biology*.
#'
#' @param mc0 - is a mc0 dataset formatted as below
#'   \itemize{
#'     \item srcf - name of file holding raw data
#'     \item acid - assay component id (Here ZFlmrALD-20-40-40)
#'       Zebrafish locomotor resonse, Acclimation/Light/Dark- _l_ minutes- _m_ minutes- _n_ minutes
#'     \item cpid - chemical name
#'     \item apid - assay plate id DNT###
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'      t = test, v = vehicle control
#'     \item wllq - well quality indicates if observation is viable for analysis
#'      1 = yes, 0 = no
#'     \item conc - concentration of chemical
#'     \item rval - endpoint values for fish
#'   }
#'
#' @param chemical - string corresponding to name of chemical of interest
#' @param unit.conc - unit of chemical concentration. Defaults to $\mu$M
#' @param plot.log - boolean indicator of whether or not concentrations should be plotted on a
#' logarithmic, base 10, scale. Defaults to TRUE
#' @param lam.hat - power parameter used in Box-Cox transformation of the data
#' @param shift - shift parameter used in Box-Cox transformation
#'
#' @return A ggplot2 object displaying scatterplots of endpoint values grouped by concentration
#'
#' @import ggplot2
#' @import data.table
#' @export
plot_eDist <- function(mc0, chemical, unit.conc = paste0("\U03BC","M"), plot.log = TRUE, lam.hat = "NA", shift = "NA") {

                # isolate data of interest for plotting

                ## find egid related to chemical to gather test fish and vehicle control
                data <- data_egids(mc0)
                group <- unique(data[cpid == chemical, egid])
                to.fit <- data[cpid==chemical | (wllt=="v" & egid==group)]

                ## save concentration as a factor for plotting
                to.fit[, conc := as.factor(conc)]

                # create plot labels and set color scheme

                ## isolate endpoint name
                acid <- unique(mc0[,acid])
                endp <- gsub("_ZFlmrALD-20-40-40", "", acid)

                ## save labels as objects
                title <- paste0(chemical, " Exposure: ", "Boxplots of ", endp, " Values")
                label.resp <- "Response Value"
                label.conc <- paste0("Concentration (", unit.conc, ")")

                ## set color scheme
                n <- length(unique(to.fit[,conc]))
                colors <- viridis::viridis(n)

                ## plot jitter-plots by concentration of test chemical
                ggplot(data = to.fit, aes(x=conc, y=rval)) +
                  geom_jitter(aes(group=conc), width = 0.1) +
                  geom_boxplot(aes(fill=conc), outlier.alpha = 0, alpha = 0.25) +
                    scale_fill_manual(values = colors) +
                  guides(fill = "none") +
                  labs(title = title,
                       x = label.conc,
                       y = label.resp,
                       caption = paste0("Box-Cox Parameters: ","\U03BB","=",lam.hat," Shift=",shift)) +
                  theme_bw() +
                  theme(axis.text = element_text(size=10))
}
