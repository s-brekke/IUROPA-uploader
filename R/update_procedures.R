
update_procedures <- function(ID){
  # "procedure_type_id" "procedure_type"    "successful"        "unfounded"         "inadmissible"      "interlocutory"     "dismissed"         "date_updated"  
  
  dbRun(con, "DELETE FROM procedures WHERE `ecli` = ?", list(ID))
  # dbRun(con2, "DELETE FROM procedures WHERE `ecli` = ?", list(ID))

  bProcedures <- getProcedures(ID)
  
  dbAppendTable(con, "procedures", bProcedures) 
  # dbAppendTable(con2, "procedures", bProcedures) 
  
  return(nrow(bProcedures))
  
  # dbRun(con, paste0("DELETE FROM procedures WHERE `ECLI_number` = ?"), list(ecli))
  # dbRun(con2, paste0("DELETE FROM procedures WHERE `ECLI_number` = ?"), list(ecli))
  # 
  # pID <- unique(bProcedures$procedure_ID, fProcedures$procedure_ID)
  # Procedures <- data.frame(procedure_ID = pID,
  #                          ecli = ecli,
  #                          decision_ID = metadata$decision_ID,
  #                          proceeding = metadata$proceeding,
  #                          decision_type = metadata$decision_type)
  # Procedures_source <- Procedures
  # for(n in 1:nrow(Procedures)){
  #   p <- Procedures$procedure_ID[n]
  #   f <- which(fProcedures$procedure_ID == p)
  #   b <- which(bProcedures$procedure_ID == p)
  #   if(p %in% bProcedures$procedure_ID){
  #     Procedures$procedure[n] <- bProcedures$procedure[b]
  #     if(p %in% fProcedures$procedure_ID){
  #       Procedures_source$procedure[n] <- "fjelstul; brekke"
  #     } else {
  #       Procedures_source$procedure[n] <- "brekke"
  #     }
  #   } else {
  #     Procedures_source$procedure[n] <- "fjelstul"
  #     stop("Get from server - or fix")
  #   }
  #   Procedures$successful[n] <- c(as.logical(fProcedures$successful[f]), bProcedures$successful[b])[1]
  #   Procedures_source$successful[n] <-
  #     paste(c("fjelstul", "brekke")[which(c(c(as.logical(fProcedures$successful[f]), "nope")[1], c(bProcedures$successful[b], "nope")[1]) == Procedures$successful[n])], collapse="; ")
  #   Procedures_source$successful[n] <-
  #     gsub("\\W*$", "", paste(Procedures_source$successful[n],
  #                             c("NOT fjelstul", "NOT brekke")[which(c(c(as.logical(fProcedures$successful[f]), "nope")[1], c(bProcedures$successful[b], "nope")[1]) == !Procedures$successful[n])],
  #                             sep="; ", collapse="; "))
  #   
  #   Procedures$unfounded[n] <- c(as.logical(fProcedures$unfounded[f]), bProcedures$unfounded[b])[1]
  #   Procedures_source$unfounded[n] <-
  #     paste(c("fjelstul", "brekke")[which(c(c(as.logical(fProcedures$unfounded[f]), "nope")[1], c(bProcedures$unfounded[b], "nope")[1]) == Procedures$unfounded[n])], collapse="; ")
  #   Procedures_source$unfounded[n] <-
  #     gsub("\\W*$", "", paste(Procedures_source$unfounded[n],
  #                             c("NOT fjelstul", "NOT brekke")[which(c(c(as.logical(fProcedures$unfounded[f]), "nope")[1], c(bProcedures$unfounded[b], "nope")[1]) == !Procedures$unfounded[n])],
  #                             sep="; ", collapse="; "))
  #   
  #   Procedures$inadmissible[n] <- c(as.logical(fProcedures$inadmissible[f]), bProcedures$inadmissible[b])[1]
  #   Procedures_source$inadmissible[n] <-
  #     paste(c("fjelstul", "brekke")[which(c(c(as.logical(fProcedures$inadmissible[f]), "nope")[1], c(bProcedures$inadmissible[b], "nope")[1]) == Procedures$inadmissible[n])], collapse="; ")
  #   Procedures_source$inadmissible[n] <-
  #     gsub("\\W*$", "", paste(Procedures_source$inadmissible[n],
  #                             c("NOT fjelstul", "NOT brekke")[which(c(c(as.logical(fProcedures$inadmissible[f]), "nope")[1], c(bProcedures$inadmissible[b], "nope")[1]) == !Procedures$inadmissible[n])],
  #                             sep="; ", collapse="; "))
  #   if(length(b) == 1){
  #     Procedures$interlocutory <- bProcedures$interlocutory[b]
  #     Procedures$dismissed <- bProcedures$dismissed[b]
  #     Procedures_source[,c("interlocutory", "dismissed")] <- "brekke"
  #   }
  #   
  #   metacols <- c("origin", "court_ID", "court", "proceeding_ID", "proceeding_year", "proceeding_number",
  #                 "proceeding_date", "CELEX_number", "decision_date") #, "procedure_type_ID", "procedure_type")
  #   Procedures[,metacols] <- metadata[,metacols]
  #   Procedures_source[,metacols] <- metadata_source[,metacols]
  #   
  #   Procedures$date_updated <- as.character(Sys.Date())
  #   Procedures_source$date_updated <- as.character(Sys.Date())
  # }
  # Procedures <- as.data.frame(t(apply(Procedures, 2, function(y) gsub("'", "`", y))))
  # if("ecli" %in% rownames(Procedures)){ # I do not understand why this is necessary - ECLI:EU:C:2019:870
  #   Procedures <- as.data.frame(t(Procedures))
  # }
  # if(nrow(Procedures) > 0){
  #   
  #   colnames(Procedures)[2] <- "ECLI_number"
  #   colnames(Procedures)[6] <- "procedure_type" 
  #   
  #   Procedures$origin <- NULL
  #   Procedures_source$origin <- NULL  
  #   Procedures$inadmissible <- as.numeric(as.logical(Procedures$inadmissible))
  #   Procedures$successful <- as.numeric(as.logical(Procedures$successful))
  #   Procedures$unfounded <- as.numeric(as.logical(Procedures$unfounded))
  #   Procedures$interlocutory <- as.numeric(as.logical(Procedures$interlocutory))
  #   Procedures$dismissed  <- as.numeric(as.logical(Procedures$dismissed))
  #   statement <- gsub("'\\?'", "?", paste0("INSERT INTO procedures (`",
  #                                          paste(colnames(Procedures), collapse="`, `"), 
  #                                          "`) VALUES (",
  #                                          paste(unlist(lapply(1:nrow(Procedures), function(y)
  #                                            gsub("''|'NA'", "NULL", paste0("'", paste(Procedures[y,], collapse="', '"), "'"))
  #                                          )),
  #                                          collapse="), (")
  #                                          ,")"))
  #   statement_source <- gsub("'\\?'", "?", paste0("INSERT INTO procedures (`",
  #                                                 paste(colnames(Procedures), collapse="`, `"), 
  #                                                 "`) VALUES (",
  #                                                 paste(unlist(lapply(1:nrow(Procedures), function(y)
  #                                                   gsub("''|'NA'", "NULL", paste0("'", paste(Procedures_source[y,], collapse="', '"), "'"))
  #                                                 )),
  #                                                 collapse="), (")
  #                                                 ,")"))
  #   dbRun(con, statement)
  #   dbRun(con2, statement_source)
  # }
}
