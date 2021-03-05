#' This function uploads chemical and sample data from a directory and performs
#' formatting. Data will be seperated into 3 tables dnt.chems, dnt.spls.test,
#' and dnt.spls.ctrl.
#' @param path this is the path to
#' "DNT project sample sizes after SB corrections_use this one_BH edit.xlsx"
#' @import data.table
upload_Chem <- function(path) {

  #--------------#
  # Data upload
  #--------------#

  dnt.chems <- as.data.table(xlsx::read.xlsx(path,
                                             sheetName = "Chemical List"))
  dnt.spls <- as.data.table(xlsx::read.xlsx(path,
                                            sheetName = "Sample sizes",
                                            startRow = 2))

  #------------------------------------#
  # Data formatting and cleaning of
  # chemical and sample data
  #------------------------------------#

  # format dnt.chems
  # assign boolean value to tested/not-tested column
  dnt.chems$Bold.indicates.that.chemical.has.not.been.tested = 1
  dnt.chems$Bold.indicates.that.chemical.has.not.been.tested[which(dnt.chems$Chemical. %in% dnt.chems$NA.)] = 0
  #remove unnecessary columns
  dnt.chems <- dnt.chems %>%
                dplyr::select(-contains("NA"))
  #rename variables
  dnt.chems <- dplyr::rename(dnt.chems,
                          Chemical = Chemical.,
                          Tested = Bold.indicates.that.chemical.has.not.been.tested)
  # return dnt.chems
  assign("dnt.chems",
         dnt.chems,
         envir = globalenv())

  # format dnt.spls
  dnt.spls <- dnt.spls[, -15]
  # format column names to work with dplyr
  new_names <- c("Plate", "Chemical", "Conc", "X.Final", "P.Normal",
                 "X.Tested", "Plate", "Chemical", "X.Final", "P.Normal",
                 "X.Tested", "SB", "Other", "TrkPblms")
  names(dnt.spls) <- new_names
  rm(new_names)
  # divide into two df's one for control and one for test
  # will be related by "Plate #" key
  dnt.spls.test <- dnt.spls %>%
                    dplyr::select(1:6)
  dnt.spls.ctrl <- dnt.spls %>%
                    dplyr::select(7:length(dnt.spls))
  # Remove blank entries
  dnt.spls.test <- dnt.spls.test %>%
                    dplyr::filter(!is.na(Chemical))
  dnt.spls.ctrl <- dnt.spls.ctrl %>%
                    dplyr::filter(!is.na(Plate))
  # return tables
  assign("dnt.spls.test",
         dnt.spls.test,
         envir = globalenv())
  assign("dnt.spls.ctrl",
         dnt.spls.ctrl,
         envir = globalenv())
}
