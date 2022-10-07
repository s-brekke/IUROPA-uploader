# Stein Arne Brekke 2022

cjeu_sql <- function(id = NA, con, loc){
  
  ecli_fails <- NULL
  
  for(i in id){
    
    message(i)
    
    case <- NA
    if(grepl("\\d/\\d", i)){
      case <- cjeu("cases", "procedure_number", i)$procedure_number
      if(!grepl("\\d/\\d\\d", case)){
        case <- i
      }
      ecli <- getCaseIDs(i)
    }
    
    if(grepl("ECLI", i)){
      ecli <- i
      case <- getCaseIDs(i)
    }
    
    if(grepl("[[:upper:]]$", case) & !grepl(" SA$", case)){
      case <- gsub("\\D*$", "", case)
    }
    
    # Case level tables
    proceeding <- uploadProceeding(case, con, loc)
    for(c in unique(na.omit(c(case, proceeding, unlist(unfold_list(cjeu("cases", "joined_cases", case))))))){
      # message(c)
      if(grepl("[[:upper:]]$", c) & !grepl(" SA$", c)){
        c <- gsub("\\D*$", "", c)
      }
      if(!grepl("c\\(", c)){
        uploadCases(c, con, loc, proceeding=proceeding)
      }
    }
    uploadParties(proceeding, con, loc)
    uploadSubmissions(proceeding, con, loc)
    
    # Decision level tables
    for(e in na.omit(ecli)){
      message(e)
      
      x <- uploadDecisions(e, con, loc, proceeding)
      if(x != "failure"){
      update_procedures(e)
      update_assignments(e)
      update_citations(e)
      } else {
        ecli_fails <- c(ecli_fails, e)
      }
    }
  }
}