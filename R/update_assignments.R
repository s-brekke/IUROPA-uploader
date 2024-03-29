update_assignments <- function(ecli){
  good_date <- dbGetQuery(con, "SELECT `decision_date` FROM decisions WHERE cjeu_decision_id = ?", list(ecli))$decision_date
  
  jj <- dbGetQuery(loc, "SELECT `ecli`, `judgeID`, `position`, `rapporteur`, `acting` FROM JudgesJudgments WHERE `ecli` = ?", list(ecli))
  
  
  if(TRUE %in% grepl("J:1209|J:121|NA", paste(jj$judgeID))){ # Some disturbance appears to have happened in the spanish judges
    jj <- dbGetQuery(loc, "SELECT `ecli`, `judgeID`, `position`, `rapporteur`, `acting`, `name_text`, `date` FROM JudgesJudgments WHERE `ecli` = ?", list(ecli))
    jj$judgeID <- getJudgeID(jj$name_text, date=jj$date[1], ecli = ecli)
    jj <- jj[which(!is.na(jj$judgeID)),]
  }
  
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
  
  
  dbRun(con, paste0("DELETE FROM assignments WHERE `cjeu_decision_id` = ?"), list(ecli))
  if(nrow(jj) > 0){
    jj <- jj[which(!duplicated(jj$judgeID)),]
    judges <- dbGetQuery(loc, paste0("SELECT `surname`, `judge_id` FROM judges WHERE `judge_id` IN ", paste0("(", paste0("'", jj$judgeID, "'", collapse=", "), ")")))
    
    df <- data.frame(cjeu_decision_id=ecli,
                     iuropa_judge_id = jj$judgeID,
                     president = as.numeric(grepl("president", jj$position, ignore.case = TRUE)),
                     rapporteur = jj$rapporteur,
                     acting = jj$acting)
    

    
    if(dbIsValid(con)){
      dbAppendTable(con, "assignments", df)
    } else {
      stop("Invalid connection")
    }
    # chamber_size <- as.numeric(dbGetQuery(con, "SELECT COUNT(judge_id) FROM assignments WHERE ecli = ?;", list(ecli))[1,1])
    # chamber_size <- ifelse(chamber_size %% 2 == 0, chamber_size+1, chamber_size)
    # dbRun(con, "UPDATE decisions SET `chamber_size` = ? WHERE `ecli` = ?", list(chamber_size, ecli))
  return(nrow(df))
  }
  return(0)
}