#' This function uploads chemical and sample data from a directory and performs
#' formatting. Data will be seperated into 3 tables dnt.chems, dnt.spls.test,
#' and dnt.spls.ctrl.
#' @param path this is the path to
#' "DNT project sample sizes after SB corrections_use this one_BH edit.xlsx"
upload.Chem <- function(path) {

  #--------------#
  # Data upload
  #--------------#

  dnt.chems <- read.xlsx(path,
                             sheetName = "Chemical List") %>%
                    as.data.table()
  dnt.spls <- read.xlsx(path,
                        sheetName = "Sample sizes",
                        startRow = 2) %>%
                as.data.table()

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
    select(-contains("NA"))
  #rename variables
  dnt.chems <- rename(dnt.chems,
                          Chemical = Chemical.,
                          Tested = Bold.indicates.that.chemical.has.not.been.tested)

  # format dnt.spls
  # remove unnecessary columns
  dnt.spls <- dnt.spls %>%
    select(-contains("NA"))
  # format column names to work with dplyr
  names(dnt.spls) <- c("Plate", "Chemical", "Conc", "P.Normal",
                            "X.Tested", "Plate", "Chemical", "P.Normal",
                            "X.Tested", "SB", "Other", "TrkPblms")
  # divide into two dt's one for control and one for test
  # will be related by "Plate #" key
  dnt.spls.test <- dnt.spls[, 1:5]
  dnt.spls.ctrl <- dnt.spls[, 6:length(dnt.spls)]
  rm(dnt.spls)
  # Remove blank entries
  dnt.spls.test <- dnt.spls.test %>%
    filter(!is.na(Chemical))
  dnt.spls.ctrl <- dnt.spls.ctrl %>%
    filter(!is.na(Plate))
}
