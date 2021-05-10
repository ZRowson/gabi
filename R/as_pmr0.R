#' Format data as pmr0
#'   Zachary Rowson
#'   Rowson.Zachary at epa.gov
#'   Last edit: 04/06/2021
#'
#' Formats a US EPA ORD CCTE BCTD RADB Padilla lab DNT60 data
#' set into pmr0 format. pmr0 format is easily transformed
#' into mc0 with DNT.60.Analysis::as_mc0() for tcpl analysis.
#'
#' @param table is a single sheet from "All Chemicals one per sheet.xlsx"
#' @return A pmr0 format data.table
#'   \itemize{
#'     \item srcf - name of file that is being formatted
#'     \item acid - assay component id (Here ZFpmrA/L/D-20-40-40)
#'       Zebrafish photomotor resonse, Accilmation/Light/Dark-20 minutes-40 minutes-40 minutes
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
#'   @import data.table

as_pmr0 <- function(table) {
              # Some tables were uploaded with column names in first row
              # Check and edit if TRUE
                if (names(table)[10] == "...10") {
                  names(table) <- as.character(table[1,])
                  table <- table[-1,]
                }

              # Find column names that correspond with certain factors
                names <- names(table)
                kywrds <- c("chem", "plat", "\\..|well", "Status|SB", "conc")
                fctrs <- sapply(kywrds, function(kywrd) grep(kywrd, names, value=TRUE, ignore.case=TRUE))
                names(fctrs) <- c("cpid", "apid", "rowcol", "wllq", "conc")

              # Rename columns already in pmr0 format with pmr0 column names
                data.table::setnames(table, fctrs[c("cpid", "apid", "conc")], c("cpid", "apid", "conc"))
                # Rename time period columns
                tps <- names[6:55] # renaming by location is bad, develop a better way
                newtps <- paste0("vt", seq(1, 50, by=1))
                data.table::setnames(table, tps, newtps)

              # Add pmr0 formatted columns to table
                srcf <- "All Chemicals one per sheet.xlsx"
                acid <- "ZFpmrA/L/D-20-40-40"
                table[, `:=` (srcf = srcf, acid = acid)]
                # Create rowi and coli columns by separating rowcol
                rowcol <- table[[fctrs[["rowcol"]]]]
                row <- sapply(rowcol, function (pos) substring(pos, 1, 1)) %>%
                        match(LETTERS)
                col <- sapply(rowcol, function (pos) substring(pos, 2)) %>%
                        as.integer()
                table[["rowi"]] <- row
                table[["coli"]] <- col
                # Create wllt column
                table[, wllt := "t"][cpid %in% c("DMSO", "Water"), wllt := "v"] # this could throw errors if spelling errors are made, think about using grepl
                # Create wllq column
                qlty <- table[[fctrs[["wllq"]]]]
                gdqlty <- which(qlty == "Normal" | is.na(qlty))
                table[, wllq := 0][gdqlty, wllq := 1][is.na(t1), wllq := 0]

              # Some tables have statistics embedded in their rows. Remove those rows
                if ("mean|count|sem" %in% table[, conc]) {
                  remove <- grep("mean|count|sem", table[, conc], ignore.case = TRUE)
                  table <- table[-remove, ]
                }

              # Ensure concentration and behavior data is of numeric class
                table[, (c("conc", newtps)) := lapply(.SD, as.numeric), .SDcols = c("conc", newtps)]

              # Create pmr0 table
                pmrcol <- c("srcf", "acid", "cpid", "apid", "rowi",
                            "coli", "wllt", "wllq", "conc", newtps)
                pmr0 <- table[, ..pmrcol]

              return(pmr0)
            }
