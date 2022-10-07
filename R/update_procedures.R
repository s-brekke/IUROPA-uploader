
update_procedures <- function(ID){
  # "procedure_type_id" "procedure_type"    "successful"        "unfounded"         "inadmissible"      "interlocutory"     "dismissed"         "date_updated"  
  
  dbRun(con, "DELETE FROM procedures WHERE `cjeu_decision_id` = ?", list(ID))
  # dbRun(con2, "DELETE FROM procedures WHERE `ecli` = ?", list(ID))

  bProcedures <- getProcedures(ID)
  bProcedures$date_updated <- NULL
  bProcedures <- bProcedures[which(!is.na(bProcedures$procedure_type)),]
  colnames(bProcedures)[1] <- "cjeu_decision_id"
  dbAppendTable(con, "procedures", bProcedures) 
  # dbAppendTable(con2, "procedures", bProcedures) 
  
  return(nrow(bProcedures))
}
