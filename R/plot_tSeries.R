#' Plot Time-Series Data for a Test Chemical and Corresponding Vehicle Control
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Plots total movement per 2-minute bin time-series data of a test chemical and
#' it's corresponding vehicle control using either a Subject-Specific (SS)
#' perspective or a Population Averaged (PA) perspective. User provides pmr0
#' formatted table, a character object specifying chemical to be plotted, and a
#' character object "SS" or "PA" specifying graphical method.
#'
#' @details
#' Created: 10/06/2021
#' Last edit: 10/26/2021
#'
#' Subject Specific (SS) perspective fits curves to each individual's speed data
#' and plots. Population Averaged (PA) perspective fits to the mean speed at each
#' time period for a concentration group.
#'
#' @param pmr0 - is a pmr0 dataset formatted as below
#'   \itemize{
#'     \item srcf - name of file holding raw data
#'     \item acid - assay component id (Here ZFpmrALD-20-40-40)
#'       Zebrafish photomotor resonse, Acclimation/Light/Dark-20 minutes-40 minutes-40 minutes
#'     \item cpid - chemical name
#'     \item apid - assay plate id DNT###
#'     \item rowi - row on plate
#'     \item coli - column on plate
#'     \item wllt - well type according to tcpl mc0 format
#'      t = test, v = vehicle control
#'     \item wllq - well quality indicates if observation is viable for analysis
#'       1 = yes, 0 = no
#'     \item conc - concentration of chemical
#'     \item vtj - measurements at time period j for j /in {1,2,...,n}
#'       Here n = 50
#'   }
#'
#' @param chemical - string corresponding to name of chemical of interest
#' @param prsp - specifies SS or PA perspective, can take value "SS" or "PA"
#' @param no.A - number of time periods in acclimation period
#' @param unit.t - length of time contained in each time period. Defaults to "2 Minutes"
#' @param unit.mov - unit of distance traveled by subjects. Defaults to cm
#' @param unit.conc - unit of chemical concentration. Defaults to $\mu$M
#'
#' @return A ggplot2 object displaying total movement time-series data from one of two perspectives
#'   SS
#'   \itemize {
#'     \item time series by individual
#'     \item individuals are colored according to their plate #, row #, and col # s.t. name = plate #|row #|col #
#'     \item time series faceted by chemical concentration grouping
#'   }
#'   PA
#'   \itemize {
#'     \item time series of concentration-group means for each time period
#'     \item curves for each concentration are plotted together and are colored to distinguish them
#'   }
#'
#' @import ggplot2
#' @import data.table
plot_tSeries <- function(pmr0, chemical, prsp = "PA", no.A = 10, unit.t = "2 Minutes", unit.mov = "cm", unit.conc = paste0("\U03BC","M")) {

                  # extract data relating to user specified chemical

                  ## convert to data.table format and find egids
                  pmr0 <- as.data.table(pmr0)
                  data <- gabi::data_egids(pmr0)

                  ## extract chemical specific data excluding acclimation period
                  group <- unique(data[cpid == chemical, egid])
                  t.rmv <- paste0("vt",seq(from=1,to=no.A,by=1))
                  to.fit <- data[cpid==chemical | (wllt=="v" & egid==group), -t.rmv, with=FALSE]

                  # plot either as SS or PA

                  ## SS plotting
                  if (prsp == "SS") {

                    # format data for plotting

                    ## elongate data
                    to.fit_long <- data.table::melt(to.fit, id.vars = c(names(to.fit)[1:9],"egid"),
                                                    variable.name = "t", value.name = "val")
                    to.fit_long[, t := sub("vt", "", t)]

                    # plot time-series data

                    ## find maximum for x-axis (time) breaks
                    n <- as.integer(max(to.fit_long[,t]))

                    ## add units to plot labels
                    label.t <- paste0("Time (", unit.t, ")")
                    label.mov <- paste0("Total Movement (", unit.mov, ")")
                    val.conc <- unique(to.fit[,conc])
                    label.conc <- paste0(val.conc, unit.conc)
                    names(label.conc) <- val.conc

                    ## set color scheme
                    x <- to.fit_long[,apid]; y <- to.fit_long[,rowi]; z <- to.fit_long[,coli]
                    n <- length(interaction(x,y,z))
                    colors <- viridis::viridis(n)

                    ## plot
                    plot <- ggplot(data = to.fit_long, aes(color = interaction(apid,rowi,coli))) +
                              geom_point(aes(x = t, y = val)) +
                              geom_line(aes(x = t, y = val, group = interaction(apid,rowi,coli))) +
                              facet_wrap(~ conc, labeller = labeller(conc=label.conc)) +
                              theme(legend.position = "none") +
                              labs(title=paste0("SS Time-Series for ",chemical),
                                   subtitle="Acclimation Period Excluded",
                                   x=label.t,
                                   y=label.mov) +
                              scale_x_discrete(breaks = as.character(seq(from=no.A,to=n,by=5))) +
                              scale_color_manual(values = colors)
                  } else if (prsp == "PA") {

                    # format data for plotting

                    ## calculate mean movement at each time period by concentration group
                    t <- grep("vt", names(to.fit), value = TRUE)
                    means <- to.fit[, lapply(.SD, function(col) mean(col,na.rm=T)),
                                    .SDcols = t, by = conc]
                    SEs <- to.fit[, lapply(.SD, function(col) stats::sd(col,na.rm=T)/sqrt(length(col))),
                                  .SDcols = t, by = conc]

                    ## elongate means and SEs data, and join
                    means_long <- data.table::melt(means, id.vars = "conc", variable.name = "t", value.name = "mean")
                    means_long[, t := sub("vt","",t)]
                    SEs_long <- data.table::melt(SEs, id.vars = "conc", variable.name = "t", value.name = "SE")
                    SEs_long[, t := sub("vt","",t)]
                    stats <- means_long[SEs_long, on = c("conc","t")][, conc := as.factor(conc)]

                    # create standard error of mean estimates by time period and plot as ribbons or error bars

                    # plot time-series data

                    ## create title, x- and y-axis labels, and legend label
                    title <- paste0("PA Time-Series for ",chemical)
                    label.t <- paste0("Time (",unit.t,")")
                    label.mean <- paste0("Mean Speed (",unit.mov,")")
                    label.legend <- paste0("Concentration (", unit.conc, ")")

                    ## create x-axis breaks
                    n <- as.integer(max(means_long[,t]))
                    breaks.t <- as.character(seq(from=no.A,to=n,by=5))

                    ## get better colors for plotting
                    n <- length(unique(to.fit[,conc]))
                    colors <- viridis::viridis(n)

                    ## plot
                    plot <- ggplot() +
                              geom_point(data = stats, aes(x=t, y=mean, color=as.factor(conc))) +
                              geom_line(data = stats, aes(x=t, y=mean, color=conc, group=conc)) +
                                scale_color_manual(values = colors) +
                              geom_ribbon(data = stats, aes(x=t, ymax=mean+SE, ymin=mean-SE,
                                                            group=conc, fill=conc),
                                          alpha = 0.25) +
                                scale_fill_manual(values = colors) +
                              labs(title = title, subtitle = "Acclimation Period Excluded",
                                   x = label.t, y = label.mean, color = label.legend) +
                              guides(fill = "none") +
                              scale_x_discrete(breaks = breaks.t)
                  }

                  return(plot)
                }

# ## find Q2 and Q3 for data by concentration at each time period
# l <- length(unique(to.fit[, conc]))
# Q2Q3 <- to.fit[, lapply(.SD, function(col) summary(col)[c(2,5)]),
#                .SDcols = grep("vt", names(to.fit), value = T),
#                by = conc][, Q := rep(c("Q2", "Q3"),l)]
#
# ## format Q2Q3 data by elongating and then widening
# Q2Q3_long <- data.table::melt(Q2Q3, id.vars = c("conc","Q"), measure.vars = grep("vt",names(to.fit),value=T), variable.name = "t")
# Q2Q3_wide <- data.table::dcast(Q2Q3_long, conc + t ~ Q, value.var = "value")
# Q2Q3_wide[, t := gsub("vt", "", t)]

## t-distribution confidence intervals fitted to log10 data and then transformed back for fitting
# t <- grep("vt", names(to.fit), value = T)
# logCIs <- to.fit[, lapply(.SD, function(x) log10(x+1)), .SDcols = t, by = conc][, lapply(.SD, function(x) t.test(x,conf.int=0.95)$conf.int), .SDcols = t, by = conc]
# CIs <- logCIs[, lapply(.SD, function(x) (10^x)-1), by = conc]

## add variable indicating if value is upper of lower bound of interval. Melt and dcast data for fitting
# CIs[, pos := rep_len(c("min","max"),nrow(CIs))]
# temp <- data.table::melt(CIs, id.vars = c("conc","pos"), variable.name = "t", value.name = "y")
# temp[, t := gsub("vt","",t)]
# CIs.long <- dcast(temp, conc + t ~ pos, value.var = "y")
