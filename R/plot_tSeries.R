#' Plot Time-Series Data for a Test Chemical and Corresponding Vehicle Control
#'
#' @author Zachary Rowson \email{Rowson.Zachary@@epa.gov}
#'
#' @description
#' Plots total movement per 2-minute bin time-series data of a test chemical and
#' it's corresponding vehicle control using either a Subject-Specific (SS)
#' perspective or a Sample Averaged (SA) perspective. User provides pmr0
#' formatted table, a character object specifying chemical to be plotted, and a
#' string object "SS" or "SA" specifying graphical method.
#'
#' @details
#' Created: 10/06/2021
#' Last edit: 08/18/2022
#'
#' Subject Specific (SS) perspective fits curves to each individual's speed data
#' and plots. Population Averaged (SA) perspective fits to the mean speed at each
#' time period for a concentration group.
#'
#' @param pmr0 - is a pmr0 dataset formatted as below
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
#'       1 = yes, 0 = no
#'     \item conc - concentration of chemical
#'     \item vtj - measurements at time period j for j /in {1,2,...,n}
#'       Here n = 50
#'   }
#'
#' @param chemical - string corresponding to name of chemical of interest
#' @param prsp - specifies SS or SA perspective, can take value "SS" or "SA"
#' @param no.A - number of time periods in acclimation period
#' @param unit.t - length of time contained in each time period. Defaults to "2 Minutes"
#' @param unit.mov - unit of distance traveled by subjects. Defaults to cm / 2 min
#' @param unit.conc - unit of chemical concentration. Defaults to $\mu$M
#'
#' @return A ggplot2 object displaying total movement time-series data from one of two perspectives
#'   SS
#'   \itemize {
#'     \item time series by individual
#'     \item time series of concentration-group means for each time period
#'     \item time series faceted by chemical concentration grouping
#'   }
#'   SA
#'   \itemize {
#'     \item time series of concentration-group means for each time period
#'     \item curves for each concentration are plotted together and are colored
#'     \item colored ribbons representing 50% confidence intervals about time period means
#'   }
#'
#' @import ggplot2
#' @import data.table
#' @export
plot_tSeries <- function(pmr0, chemical, prsp = "SA", no.A = 10, unit.t = "2 min", unit.mov = "cm", unit.conc = paste0("\U03BC","M")) {


  # convert to data.table format and find egids
  pmr0 <- as.data.table(pmr0)
  data <- gabi::data_egids(pmr0)

  #### extract data of interest ####

  ## identify group of interest
  group <- unique(data[cpid == chemical, egid])

  ## Identify movement columns of interest
  t.cols <- grep("vt", names(data), value = TRUE)
  cols <- t.cols[(no.A+1):length(t.cols)]
  A.cols <- t.cols[!(t.cols%in%cols)]

  ## extract data to be plotted, exclude acclimation
  to.fit <- data[cpid==chemical | (wllt=="v" & egid==group), -A.cols, with=FALSE]

  #### end ###

  #### plot ####

  # create appropriate axes titles for plots
  label.y <- "Speed"

  # plot either as SS or SA

  ## SS plotting
  if (prsp == "SS") {

    # format data for plotting

    ## elongate data
    to.fit_long <- data.table::melt(to.fit, id.vars = c(names(to.fit)[1:9],"egid"),
                                    variable.name = "t", value.name = "val")
    to.fit_long[, t := sub("vt", "", t)]
    to.fit_long[, t := as.numeric(t)]

    # plot time-series data

    ## find maximum for x-axis (time) breaks
    n <- as.integer(max(to.fit_long[,t]))

    ## add units to plot labels
    label.t <- paste0("Time (", unit.t, ")")
    label.mov <- paste0(label.y, " (", unit.mov, "/", unit.t, ")")
    val.conc <- unique(to.fit[,conc])
    label.conc <- paste0(val.conc, unit.conc)
    names(label.conc) <- val.conc

    ## set color scheme
    m <- length(unique(to.fit[,conc]))
    colors <- viridis::viridis(m)

    # set x-axis breaks and labels
    l <- max(to.fit_long[,t])
    breaks <- as.character(seq(from=no.A,to=l,by=5))
    labels <- as.character(seq(from=no.A,to=l,by=5))

    ## plot
    plot <- ggplot(data = to.fit_long) +
      geom_point(aes(x = t, y = val), alpha = 0.4, color = "grey48") +
      geom_line(aes(x = t, y = val, group = interaction(apid,rowi,coli)), alpha = 0.4, color = "grey48") +
      stat_summary(aes(x = t, y = val, color = as.factor(conc), group = as.character(conc)), geom = "line", fun = "mean", size = 1.25) +
      scale_color_manual(values = colors) +
      facet_wrap(~ conc, labeller = labeller(conc=label.conc)) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text = element_text(size=10),
            plot.margin = unit(c(5.5,8.5,5.5,5.5),"points")) +
      labs(title=paste0("SS Time-Series for ",chemical),
           subtitle="Acclimation Period Excluded",
           x=label.t,
           y=label.mov) +
      scale_x_discrete(breaks = breaks, labels = labels)
  } else if (prsp == "SA") {

    # format data for plotting

    ## calculate mean and 50% CIs for each vector column by concentration group, excluding concentration
    exclude.A <- t.cols[!(t.cols%in%A.cols)]
    means <- to.fit[, lapply(.SD, function(col) mean(col,na.rm=T)),
                    .SDcols = exclude.A,
                    by = conc]

    ## calculate CI's for transformed values then transform back
    shift <- 1
    logCIs <- to.fit[, lapply(.SD, function(x) log10(x+shift)), .SDcols=exclude.A, by=conc][
                      , lapply(.SD, function(x) t.test(x,conf.level=0.50)$conf.int), .SDcols=exclude.A, by=conc]
    CIs <- logCIs[, lapply(.SD, function(x) (10^x)-shift), by=conc][
      ,lapply(.SD, function(col) abs(diff(col))/2), .SDcols=exclude.A, by=conc]

    ## elongate means and CIs data, and join
    means_long <- data.table::melt(means, id.vars = "conc", variable.name = "t", value.name = "mean")
    means_long[, t := sub("vt","",t)]
    CIs_long <- data.table::melt(CIs, id.vars = "conc", variable.name = "t", value.name = "CI")
    CIs_long[, t := sub("vt","",t)]
    stats <- means_long[CIs_long, on = c("conc","t")][, conc := as.factor(conc)]
    stats[, t := as.numeric(t)]

    # create standard error of mean estimates by time period and plot as ribbons or error bars

    # plot time-series data

    ## create title, x- and y-axis titles, and legend title
    title <- paste0("PA Time-Series for ",chemical)
    title.t <- paste0("Time (",unit.t,")")
    title.mean <- paste0("Mean ", label.y, " (",unit.mov,"/",unit.t,")")
    title.legend <- paste0("Concentration (", unit.conc, ")")

    ## get better colors for plotting
    n <- length(unique(to.fit[,conc]))
    colors <- viridis::viridis(n)

    ## create x-axis breaks and labels
    m <- as.integer(max(means_long[,t]))
    breaks <- as.character(seq(from=no.A,to=m,by=5))
    labels <- as.character(seq(from=no.A,to=m,by=5))

    ## plot
    plot <- ggplot() +
      geom_point(data = stats, aes(x=t, y=mean, color=as.factor(conc))) +
      geom_line(data = stats, aes(x=t, y=mean, color=conc, group=conc)) +
      scale_color_manual(values = colors) +
      geom_ribbon(data = stats,
                  aes(x=t, ymax=mean+CI, ymin=mean-CI, group=conc, fill=conc),
                  alpha = 0.25) +
      scale_fill_manual(values = colors) +
      labs(title = title, subtitle = "Acclimation Period Excluded: 50% Confidence Bands",
           x = title.t, y = title.mean, color = title.legend) +
      guides(fill = "none") +
      scale_x_discrete(breaks = breaks, labels = labels) +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
  }

  return(plot)
}
