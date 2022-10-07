uploadDecisions <- function(ecli, con, loc, proceeding=NA){
  
  columns <- c("cjeu_decision_id",
               "cjeu_proceeding_id",
               "iuropa_decision_id",
               "court" ,
               "celex",
               "decision_date",
               "decision_type", # 'judgment', 'order', 'opinion', 'decision', 'ruling', 'AG opinion'
               "origin",
               "authentic_language",
               "direct_action",
               "preliminary_ruling",
               "hearing",
               "hearing_date",
               "chamber",
               "president_id",
               "rapporteur_id", 
               "advocate_general_id",
               "subject_matter_category",
               "subject_matter_subcategory",
               "referring_court",
               "referring_court_id",
               "date_updated")
  
  if(is.na(proceeding)){
    proceeding <- gsub("c\\(\"", "", getCaseIDs(ecli)) # fix properly!
  }
  iuropa_decision_id <- iuropa(ecli)
  court <- getCourt(ecli, long=TRUE)
  celex <- getCaseIDs(ecli, "celex")
  decision_date <- cjeu("decisions", "date_document", ecli)$date_document
  type <- gsub("ag ", "AG ", tolower(cjeu("decisions", "type", ecli)$type))
  origin <- cjeu("decisions", "origin", ecli)$origin
  authentic_language <- paste(unlist(unfold_list(cjeu("decisions", "language_authentic", ecli)$language_authentic)), collapse="; ")
  direct_action <- cjeu("decisions", "direct_action", ecli)$direct_action
  preliminary_ruling <- cjeu("decisions", "preliminary_ruling", ecli)$preliminary_ruling
  hearing <- as.numeric(is.na(cjeu("decisions", "date_hearing", ecli)$date_hearing))
  hearing_date <- getDate(unfold_list(cjeu("decisions", "date_hearing", ecli)$date_hearing)[1])
  chamber <- cjeu("decisions", "chamber", ecli)$chamber
  president_id <- cjeu("decisions", "president_ID", ecli)$president_ID
  rapporteur_id <- cjeu("decisions", "rapporteur_ID", ecli)$rapporteur_ID
  advocate_general_id <- cjeu("decisions", "advocate_general_ID", ecli)$advocate_general_ID
  subject_matter_category <- paste(unlist(unfold_list(cjeu("decisions", "subject_matter", ecli)$subject_matter)), collapse="; ")
  subject_matter_subcategory <- paste(unlist(unfold_list(cjeu("decisions", "subject_matter_subcategory", ecli)$subject_matter_subcategory)), collapse="; ")
  referring_court <- iconv(cjeu("decisions", "referring_court", ecli)$referring_court, to="ASCII//TRANSLIT", sub="?")
  referring_court_ID <- paste(unlist(unfold_list(cjeu("decisions", "referring_court_ID", ecli)$referring_court_ID)), collapse="; ")
  
  if(nchar(paste(referring_court)) > 150){
    referring_court <- NA
  }
  
  
  
  bad_types <- c("abstract", "application", "removal", "document.type.deli")
  if(type %in% bad_types){
    type <- tolower(cjeu("curia", "type", ecli)$type)
    type <- type[which(!type %in% bad_types)]
    if(na(type)){
      table <- as.data.frame(unfold_table(cjeu("cases", id=proceeding)$documents))
      table <- table[which(table$ecli == ecli),]
      if(!na(type)){
        table$type <- gsub("ag ", "AG ", tolower(table$type))
        type <- na.omit(table$type[which(table$type != "abstract")])[1]
      }
    }
  }
  
  if(na(type) | is.na(iuropa_decision_id) | court != getCourt(proceeding, long=TRUE)){
    return("failure")
  }
  if(grepl("\\(", type)){
    type <- gsub("\\W*\\(.*$", "", type)
  }
  
  if(getCourt(proceeding, long=TRUE) != court){return("Something is very wrong")}
  
  #    'Full Court', 'Grand Chamber', 
  if(!is.na(chamber)){
    if(grepl("^\\d+$", chamber)){
      chamber <- c('First Chamber', 'Second Chamber', 
                   'Third Chamber', 'Fourth Chamber', 'Fifth Chamber', 'Sixth Chamber', 'Seventh Chamber',
                   'Eighth Chamber', 'Ninth Chamber', 'Tenth Chamber')[as.numeric(chamber)]
    } else {
      if(chamber == "Full court"){
        chamber <- "Full Court"
      }
      if(chamber == "Grand"){
        chamber <- "Grand Chamber"
      }
      if(chamber == "Appeal"){
        chamber <- "Appeals Chamber"
      }
    }
    if(!paste(chamber) %in% c("Full Court",  "Grand Chamber", "Appeals Chamber", 'First Chamber', 'Second Chamber', 
                              'Third Chamber', 'Fourth Chamber', 'Fifth Chamber', 'Sixth Chamber', 'Seventh Chamber',
                              'Eighth Chamber', 'Ninth Chamber', 'Tenth Chamber')){
      chamber <- NA
    }
  }
  
  if(grepl("order", type)){
    type <- "order"
  }
  if(grepl("opinions? of the court", type)){
    type <- "opinion"
  }
  if(grepl("decision", type)){
    type <- "decision"
  }
  if(grepl("view", type)){
    type <- "AG opinion"
  }
  
  # 'Appeals Chamber'
  dbExecute(con,
            paste0("INSERT INTO decisions(`", 
                   paste(columns, collapse="`, `"),
                   "`) VALUES (", 
                   paste(rep("?", length(columns)), collapse=", "),") ",
                   "ON DUPLICATE KEY UPDATE ", 
                   paste0("`", columns, "` = ?", collapse=", ")),
            list(ecli,
                 proceeding,
                 iuropa_decision_id,
                 court,
                 celex,
                 decision_date,
                 type,
                 origin,
                 authentic_language,
                 direct_action,
                 preliminary_ruling,
                 hearing,
                 hearing_date,
                 chamber,
                 president_id,
                 rapporteur_id,
                 advocate_general_id,
                 subject_matter_category,
                 subject_matter_subcategory,
                 referring_court,
                 referring_court_ID,
                 Sys.time(),
                 
                 ecli,
                 proceeding,
                 iuropa_decision_id,
                 court,
                 celex,
                 decision_date,
                 type,
                 origin,
                 authentic_language,
                 direct_action,
                 preliminary_ruling,
                 hearing,
                 hearing_date,
                 chamber,
                 president_id,
                 rapporteur_id,
                 advocate_general_id,
                 subject_matter_category,
                 subject_matter_subcategory,
                 referring_court,
                 referring_court_ID,
                 Sys.time()
            )
  )
  return("success")
}



