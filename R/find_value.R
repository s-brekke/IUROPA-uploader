# find_value: Find the appropriate value for a given variable by cross checking multiple sources.
# output: value
find_value <- function(ID, variable, frame="decisions", silent=TRUE, loc=NA){
  
  
  if(variable == "iuropa_decision_id"){
    return(NA)
  }
  
  if(variable %in% c("iuropa_case_id")){
    return(rep(iuropa(ID, con=loc), 2))
  }
  
  if(grepl("\\d/\\d{2}", ID)){
    if(variable == "cjeu_case_id"){
      return(rep(ID,2))
    }
    
    if(variable == "case_year"){
      year <- as.numeric(gsub("^.*\\d/(\\d{2}).*$", "\\1", ID))
      return(rep(ifelse(year > 50, year + 1900, year+2000), 2))
    }
    if(variable == "case_number"){
      return(rep(as.numeric(gsub("^.*?(\\d+)/\\d{2}.*$", "\\1", ID)),2))
    }
    
    if(variable=="removed"){
      return(rep(as.numeric(TRUE %in% cjeu("curia", "Removed_from_register", ID)$Removed_from_register), 2))
    }
    if(variable=="transfered"){
      return(c(0, 0))
    }
    
    if(variable %in% c("proceeding", "joined_cases", "iuropa_proceeding_id", "proceeding_number", "proceeding_year")){
      joined <- unlist(unfold_list(dbGetQuery(loc, "SELECT `joined_cases` FROM Cases WHERE `case` = ?", list(ID))$joined_cases))
      
      if(na(joined)){
        joined <- ID
      }
      if(!TRUE %in% grepl("\\d/\\d{2}", joined)){
        joined <- ID
      }
      proceeding <- joined[1]
      if(variable == "proceeding"){
        return(rep(proceeding, 2))
      }
      if(variable == "iuropa_proceeding_id"){
        return(rep(iuropa(proceeding),2))
      } 
      if(variable == "joined_cases"){
        return(rep(paste(joined, collapse=", "),2))
      }
      if(variable == "proceeding_number"){
        return(rep(as.numeric(gsub("^\\D*(\\d+)/\\d{2}.*$", "\\1", proceeding)),2))
      }
      if(variable == "proceeding_year"){
        year <- as.numeric(gsub("^.*/(\\d{2}).*$", "\\1", proceeding))
        year <- ifelse(year > 50, year+1900, year+2000)
        return(rep(year, 2))
      }
    }
  }
  
  # Post processing - find value first, then do some extra work at the end of script
  post_process <- "none"
  
  if(variable %in% c("court_id", "iuropa_proceeding_id", "decision_type_id")){
    post_process <- variable
    variable <- gsub("_id|iuropa_", "", variable)
  }
  
  if(variable == "president" & frame == "assignments"){
    post_process <- ID
    variable <- paste0(variable, "_id")
  }
  
  # Force decision level 
  if(frame == "assignments" & 
     variable %in% c("court", "proceeding", "proceeding_date", "decision_id", "ecli",
                     "CELEX_number", "decision_date", "decision_type", "president")){
    frame <- "decisions"
    ID <- gsub(":\\d*$", "", ID)
  }
  
  # Rename variables ####
  # Make sure lists match and are of equal length.
  new_names <- c("proceeding_date", # 1
                 "decision_id", #     2
                 "decision_date", #   3
                 "subject_matter_category", #    4
                 "ECLI_number", #     5
                 "decision_type", #   6
                 "hearing_date", #    7
                 "chamber_size", #    8
                 "staff_case", #      9
                 "case_date", #       10
                 "case_title", #      11
                 "date_AG_opinion"
  )
  old_names <- c("date_lodged", #     1
                 "CJEU_ID", #         2
                 "date_document", #   3
                 "subject_matter", #  4
                 "ecli", #            5
                 "type", #            6
                 "date_hearing", #    7
                 "n_judges", #        8
                 "staff_cases", #     9
                 "date_lodged", #     10
                 "title", #           11
                 "date_AG"
  )
  
  # assignment name 
  if(frame == "assignments" & variable == "judge"){
    variable <- "name"
  }
  
  if(variable %in% new_names){
    variable <- old_names[match(variable, new_names)]
  }
  
  if(variable=="date_updated"){
    return(c(as.character(Sys.time()), as.character(Sys.time())))
  }
  
  out_value <- NA
  out_source <- NA
  
  ecli <- NA
  multiple <- FALSE
  
  if(frame %in% c("decisions","cases")){
    ecli <- ID
    
    
    
    if(variable == "procedure_ID" | variable == "proceeding"){
      if(grepl("ECLI", ID)){
        Decisions <- dbGetQuery(loc, "SELECT * FROM Decisions WHERE ecli = ?", list(ID))
        out_value <- Decisions$procedure_ID[which(Decisions$ecli == ecli)][1]
      } else {
        Procedures <- dbGetQuery(loc, "SELECT * FROM Cases WHERE `case` = ?", list(ID))
        out_value <- Procedures$procedure_number[which(Procedures$case == ID)]
      }
      
      # proceeding number: digits following / in case number
      if(post_process == "proceeding_number"){
        out_value <- as.numeric(gsub("^\\D*(\\d+)/\\d{2}.*$", "\\1", out_value))
        out_source <- out_value 
      }
      if(post_process == "iuropa_proceeding_id"){ 
        # Patch - should be added to data set properly later on 
        # and getCaseIDs, which could easily be used here when updated 
        
        out_value <- iuropa(out_value, con=loc)
        out_source <- out_value
      }
      return(c(out_value, out_source))
    }
  }
  
  if(frame == "judges"){
    Judges <- dbGetQuery(loc, "SELECT * FROM judges WHERE `judge_id` = ?", list(ID))
    
    Judges$CJ_judge <- as.numeric(!is.na(Judges$date_entry_cj))
    Judges$GC_judge <- as.numeric(!is.na(Judges$date_entry_gc))
    Judges$CST_judge <- as.numeric(!is.na(Judges$date_entry_cst))
    Judges$CJ_AG <- as.numeric(!is.na(Judges$date_entry_ag))
    Judges$member_state <- Judges$nominating_ms
    Judges$member_state_ID <- gsub("..$", "", Judges$judgeID)
    Judges$birth_date <- Judges$date_birth
    Judges$birth_date <- Judges$date_birth
    Judges$gender <- ifelse(Judges$female, "Female", "Male")
    Judges$female <- as.numeric(Judges$female)
    Judges$gender_ID <- ifelse(Judges$female, "1", "2")
    Judges$civil_servant <- Judges$civil_service
    Judges$academia <- Judges$academic
    colnames(Judges)[which(colnames(Judges) %in% c("judge", "lawyer", "politics", "civil_servant", "academia"))] <-
      paste0("background_", colnames(Judges)[which(colnames(Judges) %in% c("judge", "lawyer", "politics", "civil_servant", "academia"))])
    Judges$background_lawyer <- as.numeric(Judges$background_lawyer)
    colnames(Judges) <- gsub("unis_", "university_", colnames(Judges))
    
    f_judges <- as.data.frame(cjeujudges::judges)
    f_name <- find_colname(variable, f_judges)
    b_name <- find_colname(variable, Judges)
    
    values <- data.frame(
      brekke = flatify(Judges[which(Judges$judgeID == ID), b_name]),
      fjelstul = flatify(f_judges[which(f_judges$iuropa_judge_id == ID), f_name])
    )
    
    if(paste(b_name, "source", sep = "_") %in% colnames(JudgesBackground)){
      source <- JudgesBackground[which(JudgesBackground$judgeID == ID), paste(b_name, "source", sep = "_")]
      if(!is.na(source)){
        colnames(values)[1] <- source
      }
    }
  }
  # 
  # if(frame == "assignments"){
  #   
  #   ecli <- gsub(":\\d*$", "", ID)
  #   if(variable == "name"){
  #     Judges <- dbGetQuery(loc, "SELECT * FROM judges WHERE `ID` = ?", list(gsub("^.*:", "", ID)))
  #     return(rep(iconv(Judges$name, to="ASCII//TRANSLIT", sub="?"), 2))
  #   }
  #   
  #   JudgesJudgments <- dbGetQuery(loc, "SELECT * FROM JudgesJudgments WHERE `ID` = ?", list(ID))
  #   
  #   f_name <- find_colname(variable, f_assignments)
  #   b_name <- find_colname(variable, JudgesJudgments)
  #   
  #   
  #   
  #   values <- data.frame(
  #     brekke = flatify(unique(JudgesJudgments[, b_name])), #remove duplicates from judgesjudgments!
  #     fjelstul = flatify(f_assignments[which(f_assignments$eclijudge == ID), f_name])
  #   )
  #   
  #   # Observations not to trust in Brekke
  #   if(is.na(values["fjelstul"]) & !is.na(values["brekke"]) & 
  #      variable %in% c("type", "rapporteur")){
  #     if(gsub("\\..*$", "", ID) %in% f_assignments$ecli){
  #       if(!is.na(text$type[which(text$ecli == gsub("\\..*$", "", ID))])){
  #         if(text$type[which(text$ecli == gsub("\\..*$", "", ID))] != Decisions$type[which(Decisions$ecli == gsub("\\..*$", "", ID))]){
  #           return("delete this observation")
  #         }
  #       }
  #       if(!is.na(text$date[which(text$ecli == gsub("\\..*$", "", ID))])){
  #         if(!grepl(paste0(":", format(text$date[which(text$ecli == gsub("\\..*$", "", ID))], "%Y"), ":"), ID)){
  #           return("delete this observation")
  #         }
  #       }
  #       if(nchar(as.character(JudgesJudgments$name_text[which(JudgesJudgments$eclijudge == ID)])) > 30 | 
  #          nchar(as.character(JudgesJudgments$name_text[which(JudgesJudgments$eclijudge == ID)])) < 5){
  #         return("delete this observation")
  #       }
  #     }
  #   }
  #   # Observations not to trust in Fjelstul
  #   if(is.na(values["brekke"]) & 
  #      !is.na(values["fjelstul"])){
  #     if(variable %in% c("type", "rapporteur")){
  #       if(f_assignments[which(f_assignments$eclijudge == ID), "ECLI_number"][1] %in% fjelstul$ECLI_number[which(duplicated(fjelstul$ECLI_number))]){
  #         return("delete this observation")
  #       }
  #     }
  #   }
  # }
  # 
  if(frame == "cases"){
    # docs <- as.data.frame(Cases$documents[which(Cases$case == ID)])
    
    if(!grepl("\\d/\\d{2}", ID)){
      case <- getCaseIDs(ID, "case", maxone=TRUE, con=loc)
    } else {
      case <- ID
    }
    # ecli <- getCaseIDs(ID, "ecli", maxone=TRUE, con=loc)
    
    Cases <- dbGetQuery(loc, "SELECT * FROM Cases WHERE `case` = ?", list(case))
    Decisions <- dbGetQuery(loc, "SELECT * FROM Cases WHERE `case` = ?", list(case))
    
    # d_name <- find_colname(variable, docs)
    f_name <- find_colname(variable, f_procedures)
    # p_name <- find_colname(variable, Procedures)
    c_name <- find_colname(variable, Cases)
    
    values <- data.frame(
      # curia = flatify(docs[,d_name]),
      # brekke = flatify(Cases[which(Cases$case == ID), p_name]),
      cases = flatify(Cases[which(Cases$case == ID), c_name]),
      fjelstul = flatify(f_procedures[which(f_procedures$case_ID == ID), f_name])
    )
    
    if(variable %in% c("appeal")){
      values <- (values != "" & values != "NA")
    }
    
    # Decision level info
    if(variable %in% c("procedure", "origin", "date_hearing", "direct_action", "preliminary_ruling",
                       "advocate_general", "advocate_general_id")){
      values2 <- values[which(!is.na(values))]
      ecli <- getCaseIDs(ID, maxone = TRUE, con=loc)
      multiple = TRUE
    }
    
    # From curia document search (Decisions$documents)
    if(variable %in% c("date_order", "date_judgment", "date_AG",
                       "AG_opinion", "judgment", "order", "subject_matter", "subject_matter_subcategory")){
      doctype <- gsub("date_|_opinion", "", variable)
      doctype <- gsub("^(.)", "\\U\\1", doctype, perl=TRUE)
      docs <- as.data.frame(Cases$documents[which(Cases$case == ID)])
      docs <- docs[which(!grepl("OJ", docs$type)),]
      if(!all(is.na(docs))){
        docs$type <- gsub("Opinion", "AG Opinion", as.character(docs$type))
      } else {
        docs <- as.data.frame(NA)
      }
      if(grepl("date", variable)){
        values <- data.frame(
          curia = flatify(docs[which(docs$case == ID & grepl(doctype, docs$type)),"date"]),
          brekke = flatify(Decisions[which(Decisions$case == ID & grepl(doctype, Decisions$type)),"date_document"])
        )
      } else {
        if(grepl("subject_matter", variable)){
          values <- data.frame(
            curia = flatify(docs[which(docs$case == ID),"subjectmatter"]),
            brekke = flatify(Decisions[which(Decisions$case == ID),variable])
          )
        } else {
          values <- data.frame(
            curia = flatify(docs[which(docs$case == ID & grepl(doctype, docs$type)),"ecli"]),
            brekke = flatify(Decisions[which(Decisions$case == ID & grepl(doctype, Decisions$type)),"ecli"])
          )
        }
      }
    }
  }
  
  if(grepl("ECLI", paste(ecli)) & frame != "assignments"){
    
    text <- dbGetQuery(loc, "SELECT * FROM text WHERE `ecli` = ?", ecli)
    curia <- dbGetQuery(loc, "SELECT * FROM curia WHERE `ecli` = ?", ecli)
    eurlex <- dbGetQuery(loc, "SELECT * FROM eurlex WHERE `ecli` = ?", ecli)
    Decisions <- dbGetQuery(loc, "SELECT * FROM decisions WHERE `ecli` = ?", ecli)
    
    c_name <- find_colname(variable, curia)
    t_name <- find_colname(variable, text)
    e_name <- find_colname(variable, eurlex)
    d_name <- find_colname(variable, Decisions)
    f_name <- find_colname(variable, fjelstul)
    # Find values from all applicable data sets: 
    values <- data.frame(
      curia = flatify(curia[which(curia$ecli == ecli), c_name]),
      text = flatify(text[which(text$ecli == ecli), t_name]),
      eurlex = flatify(eurlex[which(eurlex$ecli == ecli),e_name]),
      fjelstul = flatify(fjelstul[which(fjelstul$ecli == ecli),f_name]),
      brekke = flatify(Decisions[which(Decisions$ecli == ecli),d_name])
    )
  }
  
  
  
  if(multiple){
    values <- cbind(values[which(!names(values) %in% names(values2))], values2)
  }
  if(grepl("procedure", variable)){
    values[,"fjelstul"] <- gsub("\\W*\\(.*", "", as.character(values[,"fjelstul"]))
  }
  na_variations <- c("Information not available", "not coded", "not\\W*applicable", "not available")
  values <- lapply(values, function(y) gsub(paste(na_variations, collapse="|"), "NA", y))
  values[which(gsub("\\W|NA|NULL", "", values) == "")] <- NA
  
  values <- as.data.frame(values)
  
  if(variable %in% c("rapporteur", "advocate_general", "president")){
    values[grep("^\\d*$", values)] <- getJudgeID(values[grep("^\\d*$", values)])
  }
  
  # colnames(values) <- c("curia", "text", "eurlex", "decisions")[which(!c(is.null(c_name), is.null(t_name), is.null(e_name), is.null(d_name)))]
  var_type <- "text"
  if(grepl("_date", variable)){
    var_type <- "date"
    # Convert all date formats into YYYY-MM-DD using my beloved getDate() function:
    values <- data.frame(t(apply(values, 2, function(y) as.character(getDate(y)))))
  }
  
  values <- apply(values, 2, as.character)
  
  if(variable == "celex" & is.na(values["fjelstul"]) & !is.na(values["brekke"]) & is.na(values["eurlex"])){
    if(!is.na(values["curia"]) & paste(values["curia"]) != paste(values["brekke"])){
      values[1:length(values)] <- NA
    }
  }
  
  if(variable %in% c("origin", "joined_cases")){
    values <- gsub(", ", "; ", values)
  }
  
  values <- gsub("\\W*Provisional data\\W*", "", values)
  NA_versions <- c("Information not available", "NA", "", "not coded", "not applicable", "not available")
  
  values[which(values %in% NA_versions)] <- NA
  
  values <- values[which(!is.na(values))]
  
  if(grepl("advocate_general", variable) & "fjelstul" %in% names(values)){
    date <- NA
    if(!is.na(ecli)){
      date <- paste(gsub("^.*?:(\\d{4}):.*$", "\\1", ecli), "01", "01",sep="-")
    }
    values["fjelstul"] <- paste(getJudgeIDs(unlist(str_split(values["fjelstul"], ", ")), AG = TRUE, date=date), collapse = "; ")
  }
  if(grepl("rapporteur", variable) & "fjelstul" %in% names(values) & frame != "assignments"){
    date <- NA
    if(!is.na(ecli)){
      date <- paste(gsub("^.*?:(\\d{4}):.*$", "\\1", ecli), "01", "01",sep="-")
    }
    values["fjelstul"] <- paste(getJudgeIDs(unlist(str_split(values["fjelstul"], ", ")), AG = FALSE, date=date), collapse = "; ")
  }
  
  
  
  if(length(values) == 0){
    msg(paste0(ecli, " - ", variable, ": No value found, returning NA"))
    return(NA)
  }
  
  # when a table, assume first column
  values[grep("list\\(", values)] <- lapply(unfold_list(values[grep("list\\(", values)]), function(y) y[1])
  
  values <- unfold_list(values)
  values <- lapply(values, function(y) paste(y, collapse = "; "))
  
  
  for(v_fix in unique(gsub("\\d+$", "", names(values[grep("\\d$", names(values))])))){
    values[v_fix] <- paste(values[grep(v_fix, names(values))], collapse="; ")
  }
  values <- values[which(!grepl("\\d$", names(values)))]
  
  # Try to fix synonyms
  values <- synonyms(values, variable = variable)
  
  for(v_fix in unique(gsub("\\d+$", "", names(values[grep("\\d$", names(values))])))){
    values[v_fix] <- paste(values[grep(v_fix, names(values))], collapse="; ")
  }
  values <- values[which(!grepl("\\d$", names(values)))]
  
  if(length(values) > 0){
    table <- table(values)
    # table <- table(na.omit(unlist(str_split(values, "; "))))
    
    # Simplest possible outcome: Only one observed value
    if(length(table) == 1){
      out_source <- names(values)
      out_value <- as.character(names(table), var_type)
    }
    
    if(length(which(names(table) != "")) == 1 & is.na(out_value)){
      out_value <- names(table[which(names(table) != "")])
      out_source <- c(names(values)[which(values == out_value)],
                      paste("NOT", names(values))[which(values != out_value)])
    }
    
    if(length(unique(tolower(values))) == 1 & is.na(out_value)){
      out_value <- values[grep("[[:upper:]]", values)][1]
      out_source <- c(names(values)[which(tolower(values) == tolower(out_value))],
                      paste("NOT", names(values))[which(tolower(values) != tolower(out_value))])
    }
    
    
    if(is.na(out_value) & !is.na(values["curia"]) & !grepl(paste(values["brekke"]), paste(values["curia"]), fixed = TRUE) & frame=="cases" & grepl("date", variable)){
      out_value <- values["curia"]
    }
    
    # List values: Allow options to be missing from one source, but no more.
    # This could probably be done better, but I can't imagine how right now - make it relative? 
    if(length(unique(names(table))) > 1 &
       is.na(out_value) &
       TRUE %in% grepl(";", names(table))  & 
       is.na(out_value) &
       !variable %in% c("type", "decision_type", "date_lodged") # These shall never be lists
       ){
      t <- table(unlist(str_split(values, "; ")))
      
      out_value <- paste(names(which(t >= max(t)-2)), collapse="; ")
      
      # Identify source, including non-complete sources
      sources <- lapply(unlist(str_split(out_value, "; ")), function(y) grepl(y, values, fixed = TRUE))
      sources <- as.data.frame(sources)
      rownames(sources) <- names(values)
      sources <- apply(sources, 1, sum)/ncol(sources)
      out_source <-c(names(sources[which(sources == 1)]),
                     paste("PARTLY", names(sources))[which(sources > 0 & sources < 1)],
                     paste("NOT", names(sources))[which(sources == 0)])
      
    }
    
    # If there are issues regarding capitalization only:
    # Return value from Decisions or most common one
    if(length(table(tolower(values))) == 1 & is.na(out_value)){
      msg(paste("Value identified on basis of: ", paste(names(values), collapse=", ")))
      out_source <- names(values)
      out_value <- as.character(sort(table(values), decreasing = T)[1])
    }
    
    
  } else {
    msg(paste0("No value for ", variable, " found in case ", ecli))
  }
  
  # If sources disagree. Work Fjelstul hand coding into this 
  if(length(unique(values)) == 2 & is.na(out_value)){
    
    # More than one value in only one source
    if(1 %in% table & length(unique(table)) > 1){
      if(grepl(";", names(table)[which(table==1)])){
        if(names(table)[which(table>1)] %in% unlist(str_split(names(table)[which(table==1)], "; "))){
          out_value <- names(table)[which(table>1)]
        }
      }
    }
    
    # Nothing better than text for AG - see for example ECLI:EU:C:1964:57
    # Rapporteur also seems more reliable from text (ECLI:EU:C:1967:28)
    # Control check with brekke data - this is controlled for different language versions when in doubt.
    if(is.na(out_value) & variable %in% c("advocate_general", "rapporteur") &
       !is.na(values["text"]) & 
       (is.na(values["brekke"]) | paste(values["brekke"]) == paste(values["text"]))){
      out_value <- values["text"]
    }
    
    
    # Trust the text if it is mirrored in either metadata catalogue
    if(!is.na(values["text"]) & is.na(out_value)){
      if(paste(values["curia"]) == values["text"] | paste(values["eurlex"]) == values["text"]){
        out_value <- values["text"]
      }
    }
    
    if(!is.na(values["text"]) & !is.na(values["curia"]) & is.na(out_value)){
      # Same value in curia and eurlex, but not the text? If so, trust these sources over text
      if(paste(values["curia"]) == paste(values["eurlex"]) &
         paste(values["eurlex"]) != paste(values["text"])){
        msg(paste0(ecli, ": Trusting curia and eurlex over text"))
        out_value <- values["curia"]
        
      }
    }
    # Three of us cannot be wrong
    if(table[which(table != 1)][1] > 2 & length(which(table != 1)) == 1){
      out_value <- names(table)[which(table != 1)]
    }
  }
  if(is.na(out_value)){
    if(variable=="type" &
       paste(values["curia"]) != paste(values["brekke"]) &
       paste(values["fjelstul"]) == paste(values["brekke"]) &
       is.na(values["eurlex"])
    ){
      out_value <- values["brekke"] # this used to go for curia, but that gave bad results 
    }
    
    
    # brekke data (decisions) if built to be aware of the reliability of the document text, and is therefore
    # a good source of last resort for date of documents.
    if(is.na(out_value) & variable == "date_document" & !is.na(values["brekke"])){
      out_value <- values["brekke"]
    }
    if(is.na(out_value) &
       paste(values["brekke"]) == paste(values["eurlex"]) &
       c(table[which(names(table) == paste(values["curia"]))], 1)[1] == 1 &
       !is.na(values["curia"])){
      out_value <- values["curia"]
    }
    
    if(length(which(table >1)) == 1){
      out_value <- names(table[which(table >1)])
    }
    
    
    if(is.na(out_value) & variable == "date_lodged"){
      if(grepl("DEP", Decisions$cases_full[which(Decisions$ecli == ecli)]) & !is.na(values["curia"])){
        out_value <- values["curia"]
      }
    }
    
    # If text and eurlex are the only sources, eurlex is probably more likely to be confused.
    # added this when the text was found to be correct about AG in ECLI:EU:C:1964:34.
    if(is.na(out_value) & length(values) == 2 &
       "text" %in% names(values) & "eurlex" %in% names(values)){
      out_value <- values["text"]
    }
    
    
    
    if(is.na(out_value) & grepl("date", variable)){
      if(length(unique(table)) == 1){
        if(!is.na(values["fjelstul"])){ # Fjelstul has the most hand coding 
          out_value <- values["fjelstul"]
        } else {
          if(!is.na(values["brekke"])) # Brekke has some sophisticated algorithms 
            out_value <- values["brekke"]
        }
      }
    }
  }
  
  if(is.na(out_value) & !is.na(values["fjelstul"])){
    out_value <- values["fjelstul"]
  }
  
  if(is.na(out_value) & !is.na(values["brekke"])){
    out_value <- values["brekke"]
  }
  
  
  if(is.na(out_value) & length(values) > 1){
    out_value <- values[1]
  }
  
  
  
  
  if(is.na(out_value)){
    stop("find_value: Take a look at this one: ", ecli, " - ", variable)
  }
  
  if(variable %in% c("case", "procedure_id", "judge_id", "ecli", "assignment_id", "AID", "celex", "proceeding")){
    out_source <- out_value
  }
  
  if(is.na(out_source[1]) & !is.na(out_value)){
    out_source <- c(names(values)[which(values == out_value)],
                    paste("NOT", names(values)[which(values != out_value)]))
  }
  
  # # "Brekke" is not hand coded, and therefore never a valid source on its own.
  # # My algorithms are supposed to identify the information I found to be the most
  # # reliable, not to generate new info. It does so for certain dates.
  # if(length(out_source) == 1 & paste(out_source[1]) == "brekke"){
  #   out_value <- NA
  #   out_source <- NA
  # }
  
  if(variable %in% c("president", "advocate_general", "rapporteur") & frame != "assignments"){
    out_value <- 
      paste(unlist(lapply(unlist(str_split(out_value, "; ")), 
                          function(y) dbGetQuery(loc, "SELECT `name` FROM `judges` WHERE judge_id = ?", list(y))$name)), collapse="; ")
    
    
  }
  
  # Post processing ####
  # Initiate post processing. This is a bit of a lazy fix to make the script compatible with later changes to the
  # SQL server
  
  # court ID: 1 to 3
  if(post_process == "court_id"){
    court_names <- c("Court of Justice", "General Court", "Civil Service Tribunal")
    out_value <- which(court_names == out_value)
    variable <- post_process
    out_source <- out_value
  }
  
  # proceeding_year: Date of lodging in year format
  if(post_process == "proceeding_year"){
    out_value <- format(as.Date(out_value), "%Y")
    out_source <- out_value
  }
  
  
  # Assignments: TRUE/FALSE for roles
  if(variable == "president_id" & grepl("ECLI", post_process)){
    judge_ID <- gsub("^.*:(\\d+)$", "\\1", post_process)
    out_value <- ifelse(paste(out_value) == judge_ID, 1, 0)
    variable == "president"
  }
  
  
  # decision_type_ID (1 to 6)
  if(post_process == "decision_type_id"){
    decision_types <- c("Judgment", "Order", "Opinion", "Decision", "Ruling", "AG Opinion")
    
    out_value <- which(decision_types == out_value)
    if(length(out_value)==0){
      out_value <- 0
    }
    out_source <- out_value
  }
  
  
  # clean procedure types: NA instead of zero (not applicable)
  if(variable %in% c("infringement", "action_for_annulment", "action_for_damages", 
                     "staff_cases", "action_for_failure_to_act", "appeal_against_penalty") &
     paste(out_value[1]) == 0){
    out_value <- NA
  }
  
  # Turn logical values (T/F) into numeric (1/0)
  if(variable %in% c("direct_action", "preliminary_ruling", "urgent_procedure") | 
     frame == "assignments" & variable %in% c("rapporteur") | 
     frame == "cases" & variable %in% c("appeal") ){
    if(!grepl("\\d", paste(out_value))){
      out_value <- as.numeric(as.logical(out_value))
    }
  }
  
  if(post_process != "none"){
    variable <- post_process
  }
  
  if(grepl("c\\(|list\\(", out_value)){
    out_value <- paste(unlist(unfold_list(gsub(";", ",", out_value))), collapse="; ")
  }
  
  if(out_value %in% c("c", "list"))
  
  msg(paste0(ID, ": ", variable, " identified as ", out_value, " on basis of: ", paste(out_source, collapse=", ")))
  # Fix broken symbols (Š and Č are known trouble makers)
  if(length(out_value) == 1){
    if(!is.na(out_value)){
      if(iconv(out_value, to="latin1", sub="?") != out_value){
        out_value <- iconv(out_value, to="ASCII//TRANSLIT", sub="?")
      }
    }
  }
  
  return(c(as.character(out_value), paste(out_source, collapse="; ")))
}
