#' This function uploads and formats behavioral data. Behavioral data are stored
#' in one .xlsx file names "All Chemicals one per sheet.xlsx". This .xlsx contains
#' unique worksheets with each sheet containing behavioral data of one test
#' chemical and it's corresponding control. These worksheets are imported as
#' data.tables and are named by the test chemical they describe.
#' @param path this is the path to "All Chemicals one per sheet.xlsx"

upload_Bhv <- function(path){

  #-----------------------------------#
  # Data upload from local file
  #-----------------------------------#

  # behavioral data is grouped by tested chemical
  # each chemical has a unique sheet in xlsx file
  sheetnames <<- readxl::excel_sheets(path)
  for (name in sheetnames){
    sheet <- readxl::read_excel(path,
                                sheet = name) %>%
              as.data.table()
    assign(name,
           sheet)
  }
  rm(name)
  # some sheets had column names in the first row. Re-upload those sheets
  for (name in sheetnames){
    sheet <- get(name)
    if (names(sheet)[10] == "...10") {
      sheet <- readxl::read_excel(path,
                          sheet = name,
                          skip = 1) %>%
                as.data.table()
      assign(name,
             sheet)
    }
  }
  rm(name, sheet)

  #------------------------#
  # Format Behavioral Data
  #------------------------#

  # primary issues are inconsistencies in variable order and names
  # to address this it would help to first visualize the variables used for all tables
  ## create a data.table where every column holds names(~a behavioral data.table~)
  var.table <- data.table()
  for (name in sheetnames) {
    sheet <- get(name)
    var.table <- cbind(var.table, names(sheet))
  }
  # rename var.table columns to work with dplyr
  names(var.table) <- sheetnames
  # count occurrences of variable names
  ## I did not know how to accomplish this with var.table in data.table format
  ### decided to change var.table to a vector
  var_list <- as.list(var.table)
  var_list <- unlist(var.table, use.names = FALSE)
  var_count <- table(var_list)

  # Changing Variable Order
  # now identify SB/status variables that need to be moved
  ### it can be seen in var_count that variables used to indicate status/SB are
  ### named with "...SB..." or "Status"
  # now identify which tables do not have SB/status variable in first column
  ### upon visual analysis of var.table, status does not ever appear in row 1
  # find columns where "SB" does not appear in first column
  sb_first <- grep("SB", var.table[1,], ignore.case = TRUE)
  var.table.change <- var.table %>%
                        dplyr::select(!sb_first)
  # create loop that relocates Status variable
  for (name in names(var.table.change)){
    sheet <- get(name)
    sheet <- sheet %>%
              dplyr::relocate(Status)
    assign(name,
           sheet)
  }
  rm(sheet, name)

  # Rename Variables
  # create a vector of variable names
  var_replace <- c("SB", "Plate", "Well", "Chemical", "FinalConc")
  first <- seq(from = 0, to = 98, by = 2)
  second <- seq(from = 2, to = 100, by = 2)
  var_replace <- c(var_replace, paste0("t_", first, second))
  # loop through replacing variable names
  for (name in sheetnames) {
    sheet <- get(name)
    names(sheet) <- var_replace
    assign(name,
           as.data.table(sheet))
  }
  # Edit Variable Types
  # for certain chemicals, some behavioral data time-periods were not uploaded as
  # numeric. Change this for future AUC calculations
  # To change
  ## Chloramben[, 6], Diethylene Glycol[, 55], Triethyl Tin[1, ]
  change_0002 <- c("Chloramben", "Triethyl Tin")
  change_98100 <- c("Diethylene Glycol (#75)")
  # for (name in change_0002) {
  #   sheet <- get(name)
  #   sheet$t_02 <- as.numeric(sheet$t_02)
  #   assign(name,
  #          sheet)
  # }
  # for (name in change_98100) {
  #   sheet <- get(name)
  #   sheet$t_98100 <- as.numeric(sheet$t_98100)
  #   assign(name,
  #          sheet)
  # }
  for (name in change_0002) {
    sheet <- get(name)
    sheet <- sheet[, t_02 := as.numeric(t_02)]
    assign(name,
           sheet)
  }
  for (name in change_98100) {
    sheet <- get(name)
    sheet <- sheet[, t_98100 := as.numeric(t_98100)]
    assign(name,
           sheet)
  }
  # some data sheets still contain mean, count, SEM data
  # identify these tables and remove these rows
  for (name in sheetnames) {
    sheet <- get(name)
    if (length(grep("mean|count|sem", sheet$FinalConc, ignore.case = TRUE)) > 0) {
      assign(name, sheet[-grep("mean|count|sem", sheet$FinalConc, ignore.case = TRUE), ])
    }
    # Remove bad concentrations
    if (name %in% unique(dnt.spls.test[P.Normal<75, Chemical])) {
      concs <- unique(dnt.spls.test[P.Normal<75][Chemical==name, Conc])
      assign(name, sheet[!(FinalConc %in% concs)])
    }
  }

  for (name in sheetnames) {
    sheet <- get(name)
    assign(name,
           sheet,
           envir = globalenv())
  }
}

