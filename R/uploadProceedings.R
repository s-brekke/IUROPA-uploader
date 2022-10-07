uploadProceeding <- function(case, con, loc){
  
  for(i in case){
    
    columns <- c("cjeu_proceeding_id",
                 "iuropa_proceeding_id",
                 "court" ,
                 "proceeding_year",
                 "proceeding_number",
                 "proceeding_date",
                 "proceeding_name")
    
    joined_cases <- cjeu("cases", "joined_cases", i, con=loc)$joined
    joined <- as.numeric(grepl(",", joined_cases))
    
    if(joined == 1){
      case_nrs <- unlist(unfold_list(joined_cases))
      case_nrs <- case_nrs[!grepl("c\\(", case_nrs)]
      case_yrs <- as.numeric(gsub("^.*/(\\d\\d).*$", "\\1", case_nrs))
      case_long<- as.numeric(paste0(ifelse(case_yrs > 50, case_yrs + 1900, case_yrs + 2000),
                                    str_pad(gsub("^\\D*(\\d+)/.*$", "\\1", case_nrs), 5, "left", "0")))
      
      proceeding <- case_nrs[which(case_long == min(case_long, na.rm=TRUE))][1]
      if(grepl("[[:upper:]]$", proceeding) & !grepl(" SA$", proceeding)){
        proceeding <- gsub("\\D*$", "", proceeding)
      }
      
    } else { 
      proceeding <- i
    }
    
    i <- proceeding
    
    iuropa_proceeding <- iuropa(i)
    court <- getCourt(i, long=TRUE)
    casenr <- gsub("^\\D+(\\d+)\\D.*$", "\\1", i)
    caseyear <- as.numeric(gsub("^.*/(\\d\\d).*$", "\\1", i))
    caseyear <- ifelse(caseyear > 50, caseyear + 1900, caseyear + 2000)
    date <- cjeu("cases", "lodged_date", i, con=loc)$lodged_date
    name <- cjeu("cases", "title", i, con= loc)$title
    
    proceeding <- gsub("Avis", "OPINION", proceeding)
    proceeding <- gsub("Ruling", "RULING", proceeding)
    
    out <- NA
    try(out <- dbExecute(con,
                         paste0("INSERT INTO proceedings(`", 
                                paste(columns, collapse="`, `"),
                                "`) VALUES (", 
                                paste(rep("?", length(columns)), collapse=", "),") ",
                                "ON DUPLICATE KEY UPDATE ", 
                                paste0("`", columns, "` = ?", collapse=", ")),
                         list(proceeding, 
                              iuropa_proceeding, 
                              court, 
                              caseyear, 
                              casenr,
                              date,
                              name,
                              
                              proceeding, 
                              iuropa_proceeding, 
                              court, 
                              caseyear, 
                              casenr,
                              date,
                              name
                         )
    ), silent=TRUE)
    
    if(is.na(out)){
      name <- iconv(name, to="ASCII//TRANSLIT", sub="?")
      out <- dbExecute(con,
                       paste0("INSERT INTO proceedings(`", 
                              paste(columns, collapse="`, `"),
                              "`) VALUES (", 
                              paste(rep("?", length(columns)), collapse=", "),") ",
                              "ON DUPLICATE KEY UPDATE ", 
                              paste0("`", columns, "` = ?", collapse=", ")),
                       list(proceeding, 
                            iuropa_proceeding, 
                            court, 
                            caseyear, 
                            casenr,
                            date,
                            name,
                            
                            proceeding, 
                            iuropa_proceeding, 
                            court, 
                            caseyear, 
                            casenr,
                            date,
                            name
                       )
      )
    }
    
  }
  return(proceeding)
}