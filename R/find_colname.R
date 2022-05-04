# find_colname: Match column names across data sets  
find_colname <- function(variable, data){
  
  # Names with defined associated IDs 
  if(variable %in% c("president", "advocate_general", "rapporteur")){
    if(paste0(variable, "_ID") %in% colnames(data)){
      variable <- paste0(variable, "_ID")
    } else {
      if(variable == "rapporteur" & "judge_rapporteur" %in% colnames(data)){
        return("judge_rapporteur")
      }
    }
  }
  
  if(variable == "judge_ID" & "ID" %in% colnames(data) | variable == "judge_ID" & "judgeID" %in% colnames(data)){
    variable <- "judgeID"
  }
  
  if(variable %in% colnames(data)){
    return(variable)
  }
  
  if(variable == "subject_matter_subcategory"){
    variable <- "subject_matter"
  }
  
  if(variable == "proceeding" & "procedure_ID" %in% colnames(data)){
    variable <- "procedure_ID"
  }
  
  
  if(variable == "procedure"){
    if("Procedural_Analysis_Information__Procedure_and_result" %in% colnames(data)){
      variable <- "Procedural_Analysis_Information__Procedure_and_result"
    }
  }
  
  if(variable == "type"){
    if("document_type" %in% colnames(data)){
      return("document_type")
    }
  }
  
  # Date of document:
  if(variable == "decision_date"){
    if("delivery_date" %in% colnames(data)){ # Curia
      return("delivery_date")
    }
    if("date_document" %in% colnames(data)){ # Decisions
      return("date_document")
    }
    if("date" %in% colnames(data)){ # text
      return("date")
    }
  }
  
  # Other dates
  if(grepl("date_|_date", variable)){
    out_name <- colnames(data)[which(grepl("date", tolower(colnames(data))) & 
                                       grepl(gsub("_?date_?", "", variable), tolower(colnames(data))))][1]
    if(!is.na(out_name)){
      return(out_name)
    }
  }
  if(grepl("[[:upper:]]{2}_.*_e.*_date", variable)){
    shortened <- tolower(gsub(".*([[:upper:]]{2,}).*_(e.*)_(date)", "\\3_\\2_\\1", variable))
    if(shortened %in% colnames(data))
    return(shortened)
  }
  
  if(variable == "joined_cases" & "joined_case_IDs" %in% colnames(data)){
    return("joined_case_IDs")
  }
  
  if(variable == "origin" & "referring_member_state" %in% colnames(data)){
    return("referring_member_state")
  }
  
  if(variable == "appeal" & "appeal_of" %in% colnames(data)){
    return("appeal_of")
  }
  
  variable <- gsub("^background_", "", variable)
  if(grepl("civil_servant", variable) & "civil_service" %in% colnames(data)){
    return("civil_service")
  }
  if(grepl("academia", variable) & "academic" %in% colnames(data)){
    return("academic")
  }
  
  if(variable=="ecli" & "ECLI_number" %in% colnames(data)){
    return("ECLI_number")
  }
  if(variable=="language_authentic" & "authentic_language" %in% colnames(data)){
    return("authentic_language")
  }
  
  if(variable %in% colnames(data)){
    return(variable)
  }
  
  if(variable=="procedure_ID" | variable=="proceeding" & "case" %in% colnames(data)){
    return("case")
  }
  if(variable=="case_IDs"  & "joined_case_IDs" %in% colnames(data)){
    return("joined_case_IDs")
  }
  if(variable=="case_IDs"  & "cases" %in% colnames(data)){
    return("cases")
  }
  if(variable=="case_IDs_suffix"  & "cases_full" %in% colnames(data)){
    return("cases_full")
  }
  if(tolower(variable) %in% tolower(colnames(data))){
    return(colnames(data)[which(tolower(colnames(data)) == tolower(variable))])
  }
  
  if(gsub("\\W", "", variable) %in% colnames(data)){
    return(gsub("\\W", "", variable))
  }
  
  if(variable == "decision_ID"){
    return("CJEU_ID")
  }
  
  return(NULL)
}
