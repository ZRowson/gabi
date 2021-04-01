#' This function uses tcplFit2 to create a table of hitcalls for chemicals tested in various assays.
#' Currently limited to execution on one chemical.
#' @param tables is a list of mc0 level tcpl datasets
#' @param chemicals is vector of chemical names as they appear in mc0
#' @import data.table
create_Hitcalls <- function(tables, chemicals) {
  hitcalls <- data.table(cpid=chemicals,
                         SUM_L_r=as.numeric(rep("NA",61)), SUM_L_d=as.numeric(rep("NA",61)),
                         SUM_D_r=as.numeric(rep("NA",61)), SUM_D_d=as.numeric(rep("NA",61)),
                         SUM_T_r=as.numeric(rep("NA",61)), SUM_T_d=as.numeric(rep("NA",61)),
                         logSUM_L_r=as.numeric(rep("NA",61)), logSUM_L_d=as.numeric(rep("NA",61)),
                         logSUM_D_r=as.numeric(rep("NA",61)), logSUM_D_d=as.numeric(rep("NA",61)),
                         logSUM_T_r=as.numeric(rep("NA",61)), logSUM_T_d=as.numeric(rep("NA",61)),
                         AUC_L_r=as.numeric(rep("NA",61)), AUC_L_d=as.numeric(rep("NA",61)),
                         AUC_D_r=as.numeric(rep("NA",61)), AUC_D_d=as.numeric(rep("NA",61)),
                         AUC_T_r=as.numeric(rep("NA",61)), AUC_T_d=as.numeric(rep("NA",61)),
                         logAUC_L_r=as.numeric(rep("NA",61)), logAUC_L_d=as.numeric(rep("NA",61)),
                         logAUC_D_r=as.numeric(rep("NA",61)), logAUC_D_d=as.numeric(rep("NA",61)),
                         logAUC_T_r=as.numeric(rep("NA",61)), logAUC_T_d=as.numeric(rep("NA",61))
                         )
  for (table in tables) {
    # DNT.60.Analysis::create_Row(`table`, ratio = TRUE, global = FALSE)
    DNT.60.Analysis::create_Row(`table`, dist = TRUE, global = FALSE)
    for (rw in ls()[grep("row_", ls())]) {
      name <- chemicals[which(sapply(chemicals, function(chem) grepl(chem, rw))==TRUE)]
      assay <- names(hitcalls)[which(sapply(names(hitcalls), function(assay) grepl(assay, rw))==TRUE)]
      hitcalls[cpid %in% name, assay := tcplfit2::concRespCore(get(rw))$hitcall]
    }
  }
}
