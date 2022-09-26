
update_citations <- function(ecli){
  # Data:
  refs <- dbGetQuery(loc, "SELECT * FROM 'References' WHERE `ecli` = ?", list(ecli))
  
  
  if(nrow(refs) == 0){
    return(0)
  }
  
  refs$section <- unfold_list(refs$section)
  refs$section_t <- unfold_list(refs$section_t)
  refs$section_c <- unfold_list(refs$section_c)
  refs$paragraph <- unfold_list(refs$paragraph)
  refs$paragraph_t <- unfold_list(refs$paragraph_t)
  refs$paragraph_c <- unfold_list(refs$paragraph_c)
  refs$article <- unfold_list(refs$article)
  
  # Deal with bug in cited ECLI
  refs$ecli_ref <- gsub("^.*(ECLI:.*?),?\\s.*$", "\\1", as.character(unlist(refs$ecli_ref)))
  
  refs <- refs[which(!(is.na(refs$ecli_ref) & is.na(refs$celex_ref) & is.na(refs$document))),]
  refs$ecli_ref[which(grepl("^6", refs$celex_ref) & is.na(refs$ecli_ref))] <- 
    getCaseIDs(refs$celex_ref[which(grepl("^6", refs$celex_ref) & is.na(refs$ecli_ref))], maxone = TRUE, con=loc)
  refs$celex_ref[which(is.na(refs$celex_ref) & !is.na(refs$ecli_ref))] <- 
    getCaseIDs(refs$celex_ref[which(is.na(refs$celex_ref) & !is.na(refs$ecli_ref))], "celex", maxone = TRUE, con=loc)
  refs$keep <- !duplicated(refs$celex_ref)
  refs$keep[which(is.na(refs$celex_ref))] <- !duplicated(refs$ecli[which(is.na(refs$celex_ref))])
  refs$keep[which(is.na(refs$celex_ref) & is.na(refs$ecli))] <- !duplicated(refs$document[which(is.na(refs$celex_ref) & is.na(refs$ecli))])
  
  Decisions <- dbGetQuery(loc, paste0("SELECT `case`, `ecli`, `title`, `court` FROM Decisions WHERE `ecli` IN (", paste0("'", unique(refs$ecli_ref), "'", collapse=", "), ")"))
  
  
  # Case level information: ####
  procedure_ID <- Decisions$case[which(Decisions$ecli == ecli)]
  type <- Decisions$type[which(Decisions$ecli == ecli)]
  
  # Citation level information: ####
  # celex_ref
  celex_ref <- refs$celex_ref[which(refs$keep)]
  celex_ref_source <- gsub("^\\W*|\\W*$", "", unlist(lapply(celex_ref, function(y)
    paste(ifelse(y %in% refs$celex_ref[which(unlist(lapply(refs$paragraph_t, length))!=0)], 
                 "Text", ""),
          ifelse(y %in% refs$celex_ref[which(unlist(lapply(refs$paragraph_c, length))!=0)], 
                 "Curia", ""), sep="; "))))
  
  # paragraph
  paragraphs <- unlist(lapply(celex_ref, function(y)
    paste(unique(unlist(refs$paragraph[which(refs$celex_ref == y)])), collapse="; ")))
  
  
  text_source <- unlist(lapply(celex_ref, function(y)
    length(which(unique(unlist(refs$paragraph[which(refs$celex_ref == y)])) %in% unique(unlist(refs$paragraph_t[which(refs$celex_ref == y)]))))/
      length(unique(unlist(refs$paragraph[which(refs$celex_ref == y)])))
  ))
  curia_source <- unlist(lapply(celex_ref, function(y)
    length(which(unique(unlist(refs$paragraph[which(refs$celex_ref == y)])) %in% unique(unlist(refs$paragraph_c[which(refs$celex_ref == y)]))))/
      length(unique(unlist(refs$paragraph[which(refs$celex_ref == y)])))
  ))
  
  paragraphs_source <- rep(NA, length(celex_ref))
  paragraphs_source[which(text_source == 1)] <- "Text"
  paragraphs_source[which(curia_source == 1)] <- paste(paragraphs_source[which(curia_source == 1)], "Curia", sep="; ")
  
  paragraphs_source[which(curia_source > 0 & curia_source < 1)] <-
    paste(paragraphs_source[which(curia_source > 0 & curia_source < 1)], "PARTLY Curia", sep="; ")
  paragraphs_source[which(text_source > 0 & text_source < 1)] <-
    paste(paragraphs_source[which(text_source > 0 & text_source < 1)], "PARTLY Text", sep="; ")
  paragraphs_source <- gsub("NA; ", "", paragraphs_source)
  
  
  
  # ecli_ref
  ecli_ref <- refs$ecli_ref[which(refs$keep)]
  ecli_ref_source <- gsub("^\\W*|\\W*$", "", unlist(lapply(ecli_ref, function(y)
    paste(ifelse(y %in% refs$ecli_ref[which(unlist(lapply(refs$paragraph_t, length))!=0)], 
                 "Text", ""),
          ifelse(y %in% refs$ecli_ref[which(unlist(lapply(refs$paragraph_c, length))!=0)], 
                 "Curia", ""), sep="; "))))
  
  # document_ref
  refs$document[which(!is.na(refs$ecli_ref))] <- Decisions$title[match(refs$ecli_ref[which(!is.na(refs$ecli_ref))], Decisions$ecli)]
  # document_ref <- iconv(refs$document[which(refs$keep)], to="ASCII//TRANSLIT")
  document_ref <- refs$document[which(refs$keep)]
  document_ref <- gsub("'|\"", "`", document_ref)
  document_ref_source <- celex_ref_source
  document_ref_source[which(document_ref_source == "")] <- "Curia"
  
  # type_ref
  type_ref <- refs$type[which(refs$keep)]
  type_ref[which(!is.na(ecli_ref))] <- getCaseIDs(ecli_ref[which(!is.na(ecli_ref))], "type", maxone = TRUE, con=loc)
  type_ref[which(!is.na(ecli_ref))] <- paste(Decisions$court[match(ecli_ref[which(!is.na(ecli_ref))], Decisions$ecli)], type_ref[which(!is.na(ecli_ref))])
  type_ref_source <- document_ref_source
  
  # paragraph_ref
  colnames(refs) <- gsub("section", "paragraph_ref", colnames(refs))
  paragraph_refs <- unlist(lapply(celex_ref, function(y)
    paste(unique(unlist(refs$paragraph_ref[which(refs$celex_ref == y)])), collapse="; ")))
  
  
  text_source <- unlist(lapply(celex_ref, function(y)
    length(which(unique(unlist(refs$paragraph_ref[which(refs$celex_ref == y)])) %in% unique(unlist(refs$paragraph_ref_t[which(refs$celex_ref == y)]))))/
      length(unique(unlist(refs$paragraph_ref[which(refs$celex_ref == y)])))
  ))
  curia_source <- unlist(lapply(celex_ref, function(y)
    length(which(unique(unlist(refs$paragraph_ref[which(refs$celex_ref == y)])) %in% unique(unlist(refs$paragraph_ref_c[which(refs$celex_ref == y)]))))/
      length(unique(unlist(refs$paragraph_ref[which(refs$celex_ref == y)])))
  ))
  
  paragraph_refs_source <- rep(NA, length(celex_ref))
  paragraph_refs_source[which(text_source == 1)] <- "Text"
  paragraph_refs_source[which(curia_source == 1)] <- paste(paragraph_refs_source[which(curia_source == 1)], "Curia", sep="; ")
  
  paragraph_refs_source[which(curia_source > 0 & curia_source < 1)] <-
    paste(paragraph_refs_source[which(curia_source > 0 & curia_source < 1)], "PARTLY Curia", sep="; ")
  paragraph_refs_source[which(text_source > 0 & text_source < 1)] <-
    paste(paragraph_refs_source[which(text_source > 0 & text_source < 1)], "PARTLY Text", sep="; ")
  paragraph_refs_source <- gsub("NA; ", "", paragraph_refs_source)
  
  # article_ref
  article_refs <- unlist(lapply(celex_ref, function(y)
    paste(unique(na.omit(unlist(refs$article[which(refs$celex_ref == y)]))), collapse="; ")))
  article_refs[which(article_refs == "")] <- NA
  
  article_refs_source <- rep(NA, length(celex_ref))
  article_refs_source[which(!is.na(article_refs))] <- "Curia"
  
  # Document types
  decision_types <- c("Judgment", "Order", "Opinion", "Decision", "Ruling", "AG Opinion")
  type_ref <- gsub("Order.*", "Order", type_ref)
  type_ref_id <- match(gsub("^[[:upper:]]*\\s*", "", type_ref), decision_types)
  
  # Update server ####
  # delete old observations
  
  ecli_ref <- gsub("С", "C", ecli_ref) #I hate encoding
  
  dbRun(con, paste0("DELETE FROM citations WHERE `ecli` = ?"), list(ecli))
  dbRun(con2, paste0("DELETE FROM citations WHERE `ecli` = ?"), list(ecli))
  
  add_refs <- data.frame(ecli=ecli, 
                     paragraphs=paragraphs,
                     cited_ecli=unlist(ecli_ref), 
                     cited_celex=celex_ref, 
                     cited_title=gsub("'", "`", iconv(document_ref, to='ASCII//TRANSLIT')),
                     cited_document_type = type_ref, 
                     cited_document_type_id = type_ref_id, 
                     cited_paragraphs = paragraph_refs, 
                     cited_articles=article_refs, 
                     date_updated = Sys.Date())
  
  
  add_refs <- as.data.frame(apply(add_refs, 2, function(y) ifelse(y %in% c("NA"), NA, y)))
  if(ncol(add_refs) == 1){
    add_refs <- as.data.frame(t(add_refs))
  }
  
  # add_sources <- data.frame(ecli=ecli, 
  #                           paragraphs=paragraphs_source,
  #                           cited_ecli=unlist(ecli_ref),
  #                           cited_celex=celex_ref, 
  #                           cited_title=gsub("'", "`", iconv(document_ref, to='ASCII//TRANSLIT')),
  #                           cited_document_type = type_ref, 
  #                           cited_document_type_id = type_ref_id, 
  #                           cited_paragraphs = paragraph_refs_source, 
  #                           cited_articles=article_refs_source, 
  #                           date_updated = Sys.Date())
  # 
  dbAppendTable(con, "citations", add_refs)
  # dbAppendTable(con2, "citations", add_sources)
  return(nrow(add_refs))
}
