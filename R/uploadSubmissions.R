uploadSubmissions <- function(proceeding, con=con, loc=loc){
  sub <- dbGetQuery(loc, "SELECT * FROM Submissions WHERE `proceeding` = ?", list(proceeding))
  if(nrow(sub) == 0){
    return(0)
  }
  sub <- sub[which(sub$role %in% c("Intervening party", "Observation", "Oral observation")),]
  
  # Remove the following ####
  # I observed in ECLI:EU:C:1960:18 that the same interveners could be listed multiple times. Fix in local.
  sub <- sub[which(!duplicated(sub[,c("role", "supporting", "name")])),]
  # Remove bad observations. Do locally instead.
  rm <- paste("judgment|the costs|the guarantor|having regard|after hearing|principal admin|^the court$", 
              "council regulation|a declaration|^and by|thes?e? questions|the dispute|dismissed|hearing date",
              "of that directive", "paragraph \\d", "the concept", "the product", "the answer", " marketed", " obtained", "therefore", "the.*question",
              "the.*principle", "the condition", " considers", "composed of", "advocate general", "registrar", "court cannot", "Proceedings", "to the report",
              sep="|")
  
  sub <- sub[which(!grepl(rm, sub$name, ignore.case = TRUE)),]
  
  if(nrow(sub) == 0){
    return(0)
  }
  # End remove ####
  
  sub$actor[which(is.na(sub$actor))] <- sub$name[which(is.na(sub$actor))]
  sub <- sub[which(!duplicated(sub[,c("role", "supporting", "actor")])),]
  sub$actor_role <- sub$role
  sub$actor_role_id <- match(sub$actor_role, c("Observation", "Oral observation", "Intervening party"))
  sub$actor_role_id[which(is.na(sub$actor_role))] <- 0
  sub$actor_type <- sub$type
  sub$actor_type_id <- match(sub$actor_type, c("EU institution",  "Employee of EU institution", "State", "Region", "State institution", "Employee of state institution","Company","Individual"))
  sub$actor_type_id[which(is.na(sub$actor_type))] <- 0
  sub$supporting_id <- match(sub$supporting, c(c("Applicant", "Defendant", "Intervening party", "Third party", "Other party")))
  sub$supporting_id[which(is.na(sub$supporting))] <- 0
  sub$date_updated <- as.character(Sys.time())
  
  Submissions <- sub[,c("proceeding", "actor_role", "actor_role_id", "actor_type", "actor_type_id", "supporting", "supporting_id", "date_updated")]
  
  dbRun(con, paste0("DELETE FROM submissions WHERE `proceeding` = ?"), list(proceeding))
  # dbRun(con2, paste0("DELETE FROM submissions WHERE `ecli` = ?"), list(ecli))
  if(nrow(Submissions)>0){
    dbAppendTable(con, "submissions", Submissions)
  }
  return(nrow(Submissions))
}