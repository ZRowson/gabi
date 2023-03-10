% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concRespCoreZR.R
\name{concRespCoreZR}
\alias{concRespCoreZR}
\title{Concentration Response Core - ZR Edits}
\usage{
concRespCoreZR(
  row,
  fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3",
    "exp4", "exp5"),
  conthits = TRUE,
  aicc = FALSE,
  force.fit = TRUE,
  bidirectional = TRUE,
  verbose = FALSE,
  verbose.plot = TRUE,
  do.plot = FALSE,
  return.details = FALSE,
  bmr_scale = 1.349,
  bmd_low_bnd = NULL,
  bmd_up_bnd = NULL
)
}
\arguments{
\item{row}{A named list that must include:
\itemize{
  \item conc - list of concentrations (not in log units)
  \item resp - list of corresponding responses
  \item bresp - baseline response: vector of baseline fish response values #EDIT
  \item bmed - median of noise estimate.
  \item cutoff - noise cutoff
  \item onesd - 1 standard deviation of the noise (for bmd calculation)
  \item name - name of tested chemical #EDIT
   \item assay - name of assay and endpoint #EDIT
}
Other elements (usually identifiers, like casrn) of row will be attached to
the final output.}

\item{fitmodels}{Vector of model names to use.}

\item{conthits}{conthits = T uses continuous hitcalls, otherwise they're
discrete.}

\item{aicc}{aicc = TRUE uses corrected AIC to choose winning method; otherwise
regular AIC.}

\item{force.fit}{If TRUE force the fitting to proceed even if there are no points
outside of the bounds (default TRUE)}

\item{bidirectional}{If TRUE allow fitting to happen in both directions (default TRUE)}

\item{verbose}{If TRUE, write extra output from tcplfit2_core (default FALSE)}

\item{verbose.plot}{If TRUE, ggplots will have a caption listing summarizing statistics (default TRUE)}

\item{do.plot}{If TRUE, create a plot in the tcplfit2_core function (default FALSE)}

\item{return.details}{If TRUE, return the hitcalling details and the summary, if FALSE (default), just return the summary}

\item{bmr_scale}{- bmr scaling factor (for bmd calculation) default = 1.349}

\item{bmd_low_bnd}{Multiplier for bmd lower bound.  A value of .1 would require the bmd to be no lower
than 1/10th of the lowest concentration tested.}

\item{bmd_up_bnd}{Multiplier for the bmd upper bound.  A value of 10 would require the bmd to be no lower
than 10 times the highest concentration tested.}
}
\value{
A list of two elements. The first (summary) is the output from tcplhit2_core. The second, params is the
output from tcplfit2_core
a dataframe of one row containing
}
\description{
EDIT by Zachary Rowson (Rowson.Zachary#epa.gov).
Edit Tracking:
1) force.fit = TRUE
  a) default changed from FALSE to avoid error in gabi::tcplfit2_coreZR (line #?)
2) in documentation, changed recommended structure of row parameter
3) inclusion of bresp in row and parameters
4) removed centering of data about bmed as it is done in as_row (is this necessary?)
5) call to gabi::tcplfit2_coreZR and tcplhit2_coreZR rather than tcplfit2 functions
6) call to gabi::tcplggplotter and declaration of plot object
7) add parameter verbose.plot
  a) logical indicator that decides if plot should have caption listing summarizing statistics

Core of concentration response curve fitting for pvalue based cutoff. This
function calls tcplfit2_core to get curve fits, and then tcplhit2_core to
perform the hitcalling.
}
\details{
Last Edit: 02/08/2022
}
\examples{
conc <- list(.03, .1, .3, 1, 3, 10, 30, 100)
resp <- list(0, .2, .1, .4, .7, .9, .6, 1.2)
row <- list(conc = conc,
            resp = resp,
            bmed = 0,
            cutoff = 1,
            onesd = .5,
            name = "some chemical")
concRespCoreZR(row, conthits = TRUE)
concRespCoreZR(row, aicc = TRUE)
}
