# Stein Arne Brekke 2022

uploadCases <- function(case, con, loc, proceeding = NA){
  case <- gsub("Avis", "OPINION", case)
  case <- gsub("Ruling", "RULING", case)
  for(i in case){
    
    if(grepl("[[:upper:]]$", i) & !grepl(" SA$", i)){
      i <- gsub("\\D*$", "", i)
    }
    
    caseyear <- as.numeric(gsub("^.*/(\\d\\d).*$", "\\1", i))
    caseyear <- ifelse(caseyear > 50, caseyear + 1900, caseyear + 2000)
    
    
    joined_cases <- cjeu("cases", "joined_cases", i, con=loc)$joined
    joined <- as.numeric(grepl(",", joined_cases))
    
    if(joined == 1){
      if(is.na(proceeding)){
      case_nrs <- unlist(unfold_list(joined_cases))
      
      case_yrs <- as.numeric(gsub("^.*/(\\d\\d).*$", "\\1", case_nrs))
      case_long<- as.numeric(paste0(ifelse(case_yrs > 50, case_yrs + 1900, case_yrs + 2000),
            str_pad(gsub("^\\D*(\\d+)/.*$", "\\1", case_nrs), 5, "left", "0")))
      
      proceeding <- case_nrs[which(case_long == min(case_long, na.rm=TRUE))]
      if(grepl("[[:upper:]]$", proceeding) & !grepl(" SA$", proceeding)){
        proceeding <- gsub("\\D*$", "", proceeding)
      }
      }
      # This would be easier if it wasn't for some broken observation:
      # proceeding <- cjeu("cases", "procedure_number", i,con=loc)$procedure_number 
    } else { 
      if(!is.na(proceeding) & i != proceeding){
        return() # this case will have to be updated later - proceeding might be missing
      }
      proceeding <- i
    }
    
    iuropa_proceeding <- iuropa(proceeding)
    court <- getCourt(i, long=TRUE)
    casenr <- gsub("^\\D+(\\d+)\\D.*$", "\\1", i)
    register <- as.numeric(TRUE %in% cjeu("curia", "Removed_from_register", i))
    
   columns <- c("cjeu_case_id",
   "cjeu_proceeding_id",
   "iuropa_case_id",
   "court",
   "case_year",
   "case_number",
   "joined",
   "transfered",
   "removed",
   "date_updated"
    )
   
   if(i %in% cjeuniv::cases_template$cjeu_case_id){
     transfered <- cjeuniv::cases_template$transferred[which(cjeuniv::cases_template$cjeu_case_id == i)][1]
     register <- cjeuniv::cases_template$removed[which(cjeuniv::cases_template$cjeu_case_id == i)][1]
     # joined <- cjeuniv::cases_template$joined[which(cjeuniv::cases_template$cjeu_case_id == i)]
   } else {
     transfered <- 0
   }
   
    dbExecute(con,
              paste0("INSERT INTO cases(`", 
                     paste(columns, collapse="`, `"),
                     "`) VALUES (", 
                     paste(rep("?", length(columns)), collapse=", "),") ",
              "ON DUPLICATE KEY UPDATE ", 
              paste0("`", columns, "` = ?", collapse=", ")),
              list(i, 
                   proceeding, 
                   iuropa_proceeding, 
                   court, 
                   caseyear, 
                   casenr, 
                   joined, 
                   transfered, 
                   register,
                   Sys.time(),
                   
                   i,
                   proceeding,
                   iuropa_proceeding,
                   court, 
                   caseyear,
                   casenr, 
                   joined, 
                   transfered, 
                   register,
                   Sys.time()
              )
    )
    
  }
}
