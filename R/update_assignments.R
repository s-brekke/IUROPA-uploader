update_assignments <- function(ecli){
  good_date <- dbGetQuery(con, "SELECT `decision_date` FROM decisions WHERE ecli = ?", list(ecli))$decision_date
  
  jj <- dbGetQuery(loc, "SELECT `ecli`, `judgeID`, `position`, `rapporteur`, `acting` FROM JudgesJudgments WHERE `ecli` = ?", list(ecli))
  if(grepl("^\\d*$", jj$judgeID[1])){
    jj$judgeID <- getJudgeID(jj$judgeID)
  }
  
  
  fj <- f_assignments[which(f_assignments$ecli == ecli),]
  
  # Judge IDs have been updated since the last time fjelstul data was
  if(nrow(fj) > 0){
  fj$judge_ID <- getJudgeID(fj$judge, date=fj$document_date[1])
  } 
  
  # Check that document dates match server to verify that it's the right document
  brekkedate <- dbGetQuery(loc, "SELECT `date_document` FROM Decisions WHERE ecli = ?", list(ecli))$date_document
  
  if(length(brekkedate) == 0 & nrow(fj) == 0){
    return(0)
  }
  if(paste(fj$document_date[1]) != paste(good_date)){
    fj <- fj[0,]
  }
  if(paste(brekkedate[1]) != paste(good_date)){
    jj <- jj[0,]
  }
  
  dbRun(con, paste0("DELETE FROM assignments WHERE `ecli` = ?"), list(ecli))
  if(nrow(jj) > 0){
    
    judges <- dbGetQuery(loc, paste0("SELECT `surname`, `judge_id` FROM judges WHERE `judge_id` IN ", paste0("(", paste0("'", jj$judgeID, "'", collapse=", "), ")")))
    
    df <- data.frame(ecli=ecli,
                     judge_id = jj$judgeID,
                     president = as.numeric(grepl("president", jj$position, ignore.case = TRUE)),
                     rapporteur = jj$rapporteur,
                     acting = jj$acting,
                     date_updated = Sys.Date())
    df$last_name <- judges$surname[match(df$judge_id, judges$ID)]
    df$last_name <- iconv(df$last_name, to='ASCII//TRANSLIT')
    

    
    if(dbIsValid(con)){
      dbAppendTable(con, "assignments", df)
    } else {
      stop("Invalid connection")
    }
    chamber_size <- as.numeric(dbGetQuery(con, "SELECT COUNT(judge_id) FROM assignments WHERE ecli = ?;", list(ecli))[1,1])
    chamber_size <- ifelse(chamber_size %% 2 == 0, chamber_size+1, chamber_size)
    dbRun(con, "UPDATE decisions SET `chamber_size` = ? WHERE `ecli` = ?", list(chamber_size, ecli))
  return(nrow(df))
  }
  return(0)
}