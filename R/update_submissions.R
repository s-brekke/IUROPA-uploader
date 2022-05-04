update_submissions <- function(ecli){
  
  # "actor_role_id" "actor_role"    "actor"         "actor_type_id" "actor_type"    "supporting_id" "supporting"    "date_updated" 
  
  sub <- dbGetQuery(loc, "SELECT * FROM Submissions WHERE `ecli` = ?", list(ecli))
  if(nrow(sub) == 0){
    return(0)
  }
  sub <- sub[which(sub$role %in% c("Intervening party", "Observation", "Oral observation")),]
  sub <- sub[which(!duplicated(sub[,c("role", "supporting", "name")])),] # I observed in ECLI:EU:C:1960:18 that the same interveners could be listed multiple times. 
  
  # Last attempt to remove bad observations
  rm <- paste("judgment|the costs|the guarantor|having regard|after hearing|principal admin|^the court$", 
              "council regulation|a declaration|^and by|thes?e? questions|the dispute|dismissed|hearing date",
              "of that directive", "paragraph \\d", "the concept", "the product", "the answer", " marketed", " obtained", "therefore", "the.*question",
              "the.*principle", "the condition", " considers", "composed of", "advocate general", "registrar", "court cannot", "Proceedings", 
              collapse="|")
  sub <- sub[which(!grepl(rm, tolower(sub$name))),]
  
  if(nrow(sub) == 0){
    return(0)
  }
  
  sub$actor[which(is.na(sub$actor))] <- sub$name
  
  
  
  sub$actor_role <- sub$role
  sub$actor_role_id <- match(sub$actor_role, c("Observation", "Oral observation", "Intervening party"))
  sub$actor_role_id[which(is.na(sub$actor_role))] <- 0
  sub$actor_type <- sub$type
  sub$actor_type_id <- match(sub$actor_type, c("EU institution",  "Employee of EU institution", "State", "Region", "State institution", "Employee of state institution","Company","Individual"))
  sub$actor_type_id[which(is.na(sub$actor_type))] <- 0
  sub$supporting_id <- match(sub$supporting, c(c("Applicant", "Defendant", "Intervening party", "Third party", "Other party")))
  sub$supporting_id[which(is.na(sub$supporting))] <- 0
  sub$date_updated <- as.character(Sys.time())
  
  Submissions <- sub[,c("ecli", "actor_role", "actor_role_id", "actor_type", "actor_type_id", "supporting", "supporting_id", "date_updated")]
  
  dbRun(con, paste0("DELETE FROM submissions WHERE `ecli` = ?"), list(ecli))
  # dbRun(con2, paste0("DELETE FROM submissions WHERE `ecli` = ?"), list(ecli))
  if(nrow(Submissions)>0){
  dbAppendTable(con, "submissions", Submissions)
  # dbAppendTable(con2, "submissions", Submissions)
  
  }
  return(nrow(Submissions))
}
