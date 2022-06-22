# synonyms(): Standardize values accross data sets
synonyms <- function(values, variable="NA"){
  
  unfold_list(values)
  
  values <- unlist(values)
  
  if(variable %in% c("advocate_general_id", "judge_id", "president_id", "rapporteur_id")){
    values_split <- str_split(values, "; ")
    for(n in 1:length(values_split)){
      values[n] <- paste(getJudgeID(unlist(values_split[n])), collapse="; ")
    }
    return(unlist(values))
  }
  
  if(variable == "type"){
    values <- gsub("Abstract", "", values)
  }
  
  if(TRUE %in% grepl("actions ", values, ignore.case = TRUE) & TRUE %in% grepl("action ", values, ignore.case = TRUE)){
    values <- gsub("actions", "Action", values, ignore.case = TRUE)
  }
  values <- gsub("fulfill", "fulfil", values, ignore.case = TRUE)
  
  dict <- data.frame(name="NA",
                     variations="NA")[0,]
  # List of lists separated by "; ", where the first value will become the standardized one.
  synonym_list <-
    c("Staff Regulations; Staff Regulations of officials and Conditions of Employment of other servants; Staff regulations and employment conditions - EC",
      "Staff cases; action brought by an official; Actions brought by officials; Action brought by officials",
      "Czech Republic; Czechia",
      "State aid; State aids",
      "Action for failure to fulfil obligations; Action for a declaration of failure to fulfil obligations",
      "Appeal; Appeals",
      "Reference for a preliminary ruling; Preliminary reference",
      "Order; Order of President; Order of the Court",
      "Decision; Decision to review",
      "Opinion; Opinion of the Court",
      "AG Opinion; Opinion of the Advocate General; view; View of the Advocate General; AG opinion",
      "Appeal against penalty; Appeal brought against a sanction; appeal against a penalty",
      "Court of Justice; ECJ; CJ",
      "General Court; GC",
      "Civil Service Tribunal; FT; CST",
      "Legal aid; Application for legal aid")
  
  s_list <- str_split(synonym_list, "; ")
  s_names <- unlist(lapply(s_list, function(y) y[1]))
  
  # Subject matter variables: Sort out categories and subcategories
  subject_matters <- NA
  if(variable %in% c("subject_matter", "subject_matter_subcategory")){
    subject_matters <- table(unlist(Decisions$subject_matter))
    subject_matters <- names(subject_matters[which(subject_matters >= 50)])
    if(variable=="subject_matter_subcategory"){
      sm <- unique(unlist(Decisions$subject_matter_subcategory))
      subject_matters <- sm[which(!sm %in% subject_matters)]
    }
  }
  
  values_out <- values
  for(v in 1:length(values)){
    
    value_list_in <- values[v]
    if(variable == "procedure"){
      value_list_in <- gsub("\\s*\\(.*\\)", "", value_list_in)
      value_list_in <- gsub("\\W*urgent procedure\\W*", "", value_list_in)
    }
    if(TRUE %in% grepl("%&%|\\s:\\s[[:lower:]]|&&", value_list_in)){
      value_list_in <- gsub("&&", "%&%", value_list_in)
      value_list_in <- gsub("\\W*$", "", gsub(":[^[:upper:]]*%&%", "%&%", paste0(value_list_in, "%&%")))
      value_list_in <- gsub("\\W*%&%\\W*", "; ", value_list_in)
    }
    
    value_list_in <- str_split(value_list_in, "; ", simplify = TRUE)
    value_list_out <- NULL
    
    for(n in 1:length(value_list_in)){
      value_list_out[n] <- value_list_in[n]
      value_list_out[n] <- c(s_names[which(unlist(lapply(s_list, function(y) value_list_in[n] %in% y)))], value_list_out[n])[1]
    }
    
    if(!all(is.na(subject_matters))){
      value_list_out <- value_list_out[which(value_list_out %in% subject_matters)]
    }
    
    values[v] <- paste(sort(value_list_out), collapse = "; ")
  }
  
  # values <- values[which(values != "")
  
  # Fix lowercase
  name <- unique(unlist(str_split(values, "; ")))
  if(TRUE %in% duplicated(tolower(name))){
    for(dup in tolower(name)[which(duplicated(tolower(name)))]){
      values <- gsub(dup, name[which(tolower(name) == dup & grepl("[[:upper:]]", name))][1], values)
    }
  }
  
  if(variable == "type" & TRUE %in% !grepl("^[[:upper:]]", as.character(values))){
    values[which(!grepl("^[[:upper:]]", as.character(values)))] <- gsub("^([[:lower:]])", "\\U\\1",  values[which(!grepl("^[[:upper:]]", as.character(values)))], perl = TRUE)
    values[grep("Order of", values)] <- "Order"
    return(values)
  }
  
  values <- gsub("^; |; $|\\s*$|^\\s*", "", values)
  
  return(values)
}
