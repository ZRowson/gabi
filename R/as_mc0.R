#' This function reformats behavioral data as mc0. Much of this function copies code from
#' Katie Paul-Friedmans "PADILLA_ZF_BEHAVIOR_s_to_tcpl(editted).R"
#' Future updates to DNT.60.Analysis will likey not require use
#' of this function.
#' @param chemnames vector of chemical names as strings
#' @param endp string name of endpoint to saved as rval
#' @import data.table
as_mc0 <- function(chemnames, endp) {
  for (name in chemnames) {
    # Create large table
    mc0 <- data.table()
    for (name in sheetnames) {
      sheet <- get(name)
      mc0 <- rbind(mc0, sheet)
    }
    # Format as mc0:
    ## 1 Create acid, wllt, wllq, rowi and coli, and srcf
    ## 2 Change data types
    ## 3 Rename Chemical as cpid, Plate as apid, FinalConc as conc, and endpoint as rval
    ## 4 Select necessary columns and rows
    mc0[, `:=` (acid = endp, wllt="t", wllq=0, rowi=substring(Well,1,1), coli=substring(Well,2), srcf="All Chemicals one per sheet.xlsx") #1
         ][Chemical == "DMSO"|Chemical == "Water", wllt := "v"
           ][is.na(get(endp)), wllt := 0
             ][is.na(SB)|SB == "Normal"|SB == "X", wllq := 1
               ][, `:=` (rowi = match(rowi, LETTERS), coli = as.integer(coli), FinalConc = as.numeric(FinalConc))] #2
    setnames(mc0, c("Chemical","Plate","FinalConc",endp), c("cpid","apid","conc","rval")) #3
    mc0 <- mc0[!is.na(conc), .(acid, cpid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)] #4
    return(mc0)
  }
}