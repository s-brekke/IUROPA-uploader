# Get matadata
metadata <- function(ID,
                     table = "submissions",
                     cols = NA ){

  if(grepl("ECLI", ID)){
    
    ID_column <- "ECLI_number"
    
    if(all(is.na(cols))){
      cols <- c("court_ID", 
                "court", 
                "proceeding_ID", 
                "proceeding_year", 
                "proceeding_number",
                "proceeding_date",
                "decision_ID",
                "CELEX_number", 
                "decision_date",
                "decision_type",
                "decision_type_ID")
    }
    
    columns <- paste("`", cols, "`", collapse=", ", sep="")
    
    metadata <- dbGet(con, 
                      statement = paste0("select ", columns, " from decisions where `ECLI_number` = ?"), 
                      more = list(ID))
    metadata_source <- dbGet(con2, 
                             statement = paste0("select ", columns, " from decisions where `ECLI_number` = ?"), list(ID))
  } else {
    
    ID_column <- "case"
    
    if(all(is.na(cols))){
      cols <- c("court_ID", 
                "court", 
                "case_ID", 
                "case",
                "case_year",
                "case_number",
                "case_date",
                "proceeding",
                "proceeding_number",
                "proceeding_year")
    }
    
    columns <- paste("`", cols, "`", collapse=", ", sep="")
    
    
    metadata <- dbGet(con, 
                      statement = paste0("select ", columns, " from cases where `case` = ?"), 
                      more = list(ID))
    metadata_source <- dbGet(con2, 
                             statement = paste0("select ", columns, " from cases where `case` = ?"), list(ID))
  }
  
  statement <- gsub("'\\?'", "?", paste0("UPDATE ", table," SET (`",
                                         paste(colnames(metadata), collapse="`, `"), 
                                         "`) VALUES (",
                                         paste(unlist(lapply(1:nrow(metadata), function(y)
                                           gsub("''|'NA'", "NULL", paste0("'", paste(metadata[y,], collapse="', '"), "'"))
                                         )),
                                         collapse="), (")
                                         ,")"))
  
  statement <- paste0("UPDATE ", table," SET ", paste0("`", colnames(metadata), "` = ?", collapse=", "), " WHERE `", ID_column, "` = '", ID, "'")
  
  dbRun(con, 
        command = statement, 
        list = as.character(lapply(metadata, as.character)))
  dbRun(con2, 
        command = statement, 
        list = as.character(lapply(metadata_source, as.character)))
  
}