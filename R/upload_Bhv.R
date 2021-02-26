#' This function uploads and formats behavioral data. Behavioral data are stored
#' in one .xlsx file names "All Chemicals one per sheet.xlsx". This .xlsx contains
#' unique worksheets with each sheet containing behavioral data of one test
#' chemical and it's corresponding control. These worksheets are imported as
#' data.tables and are named by the test chemical they describe.
#' @param path this is the path to "All Chemicals one per sheet.xlsx"

upload_Bhv <- function(path){

  #------------------------#
  # Upload Behavioral Data
  #------------------------#

  # behavioral data is grouped by tested chemical
  # each chemical has a unique sheet in xlsx file
  sheetnames <- excel_sheets(path)
  for (name in sheetnames){
    assign(name, read_excel(path,
                            sheet = name) %>%
             as.data.table())
  }
  rm(name)
  # some sheets had column names in the first row. Re-upload those sheets
  for (name in sheetnames){
    sheet <- get(name)
    if (names(sheet)[10] == "...10") {
      assign(name, read_excel(path,
                              sheet = name,
                              skip = 1) %>%
               as.data.table())
    }
  }
  rm(name, sheet)

  #------------------------#
  # Format Behavioral Data
  #------------------------#

  # primary issue is inconsistencies in variable order and names
  # to address this create a data.table where every column holds names(~a behavioral data.table~)
  var.table <- data.table()
  for (name in sheetnames) {
    sheet <- get(name)
    var.table <- cbind(var.table, names(sheet))
  }
  # rename var.table columns to work with dplyr
  names(var.table) <- sheetnames

  # Order Variables
  # find columns where "SB" does not appear in first column
  sb_first <- grep("SB", var.table[1,], ignore.case = TRUE)
  var.table.change <- var.table %>%
    select(!sb_first)
  # variable names for SB/morphology status were titled either "Status" or
  # contained "...SB..."
  # variables names "Status" were not in the desired variable order
  # create loop that relocates Status variable
  for (name in names(var.table.change)){
    sheet <- get(name)
    assign(name, sheet %>%
             relocate(Status))
  }
  rm(sheet, name)

  # Rename Variables
  # create a vector of variable names
  var_replace <- c("SB", "Plate", "Well", "Chemical", "FinalConc")
  first <- seq(from = 0, to = 98, by = 2)
  second <- seq(from = 2, to = 100, by = 2)
  var_replace <- c(var_replace, paste(first, second, sep = "."))
  # loop through replacing variable names
  for (name in sheetnames) {
    sheet <- get(name)
    names(sheet) <- var_replace
    assign(name, sheet)
  }
}
