uploadSubmissions <- function(proceeding, con=con, loc=loc){
  dbRun(con, paste0("DELETE FROM submissions WHERE `cjeu_proceeding_id` = ?"), list(proceeding))
  
  sub <- dbGetQuery(loc, "SELECT * FROM Submissions WHERE `proceeding` = ?", list(proceeding))
  if(nrow(sub) == 0){
    return(0)
  }
  sub <- sub[which(sub$role %in% c("Intervening party", "Observation", "Oral observation")),]
  
  dates <- cjeu("decisions", c("ecli", "date_document"), unique(sub$ecli))
  
  sub$date <- dates$date_document[match(sub$ecli, dates$ecli)]
  
  # Only most recent document: 
  sub <- sub[which(sub$date == max(dates$date_document, na.rm = TRUE)),]
  
  # # I used to gather from all documents; now only the most recent. Neither are ideal. 
  # # Remove the following #
  # # I observed in ECLI:EU:C:1960:18 that the same interveners could be listed multiple times. Fix in local.
  # sub <- sub[which(!duplicated(sub[,c("role", "supporting", "name")])),]
  # Remove bad observations. Do locally instead.
  rm <- paste("judgment|the costs|the guarantor|having regard|after hearing|principal admin|^the court$", 
              "council regulation|a declaration|^and by|thes?e? questions|the dispute|dismissed|hearing date",
              "of that directive", "paragraph \\d", "the concept", "the product", "the answer", " marketed", " obtained", "therefore", "the.*question",
              "the.*principle", "the condition", " considers", "composed of", "advocate general", "registrar", 
              "court cannot", "Proceedings", "to the report", "the \\w* Chamber of",
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
  sub$actor_type[which(sub$actor_type == "NA")] <- NA
  # Allow for recent improvements of getActorType
  if(TRUE %in% is.na(sub$actor_type)){
    sub$actor_type[which(is.na(sub$actor_type))] <- getActorType(sub$actor[which(is.na(sub$actor_type))])$type
  }
  
  sub$actor_type_id <- match(sub$actor_type, c("EU institution",  "Employee of EU institution", "State", "Region", "State institution", "Employee of state institution","Company","Individual"))
  sub$actor_type_id[which(is.na(sub$actor_type))] <- 0
  sub$supporting_id <- match(sub$supporting, c(c("Applicant", "Defendant", "Intervening party", "Third party", "Other party", "Natural person")))
  sub$supporting_id[which(is.na(sub$supporting))] <- 0
  # sub$date_updated <- as.character(Sys.time())
  
  sub$actor_type[which(sub$actor_type == "NGO")] <- "organization"
  
  sub$actor[which(paste(sub$actor) == "NA")] <-sub$name[which(paste(sub$actor) == "NA")]
  
  
  
  Submissions <- sub[,c("proceeding",  "actor_role", "actor", "actor_type", "supporting")]
  Submissions$actor_role <- tolower(Submissions$actor_role)
  colnames(Submissions)[3] <- "actor_name"
  Submissions$actor_type[grep("individual|employee", Submissions$actor_type, ignore.case = TRUE)] <- "natural person"
  colnames(Submissions)[1] <- "cjeu_proceeding_id"
  colnames(Submissions)[5] <- "party_supporting"
  
  
  # Supporting parties
  types <- c(   'applicant',
                'defendant',
                'intervening party',
                'third party',
                'other party', NA
  )
  
  Submissions$party_supporting <- tolower(Submissions$party_supporting)
  Submissions$party_supporting[which(Submissions$party_supporting == "other party")] <- "defendant"
  Submissions <- Submissions[which(nchar(Submissions$actor_name) <= 200),]
  
  last_removal <- c("article \\d|No \\d+/\\d")
  Submissions <- Submissions[which(!grepl(paste(last_removal, collapse="|"), Submissions$actor_name, ignore.case = TRUE)),]
  
  if(nrow(Submissions) == 0){
    return()
  }
  
  if(TRUE %in% !Submissions$party_supporting %in% types){
    parties <- cjeu("parties",proceeding)
    for(n in Submissions$party_supporting[which(!Submissions$party_supporting %in% types)]){
      if(TRUE %in% grepl(n, parties$name, fixed = TRUE)){
        Submissions$party_supporting[which(Submissions$party_supporting==n)] <-  parties$role[grep(n, parties$name)[1]]
      } else {
        Submissions$party_supporting[which(Submissions$party_supporting==n)] <- NA
      }
    }
  }
  
  # dbRun(con2, paste0("DELETE FROM submissions WHERE `ecli` = ?"), list(ecli))
  successful_run <- NA
  if(nrow(Submissions)>0){
    successful_run <- try(dbAppendTable(con, "submissions", Submissions), silent=TRUE)
  }
  if(class(successful_run) == "try-error"){
    Submissions$actor_name <- iconv(Submissions$actor_name, to="ASCII//TRANSLIT", sub="?")
    dbAppendTable(con, "submissions", Submissions)
  }
  return(nrow(Submissions))
}