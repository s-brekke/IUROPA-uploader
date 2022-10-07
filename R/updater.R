# cjeuUpload: Identify cases that were updated locally since they were uploaded to the server.
cjeuUpload <- function(newest.first="random", 
                       silent = TRUE,
                       con =  brekkeR::gucon("cjeu_rolling"),
                       loc=cjeudb()){
  # library(CJEU)
  silent <<- silent
  # 
  # con <- dbConnect(MariaDB(), 
  #                  host = "iuropa.pol.gu.se", 
  #                  username = user,
  #                  password = password, 
  #                  dbname = "cjeu_rolling")
  # 
  judge_ids <- dbGet(con, "select `iuropa_judge_id`, `date_updated` from judges")
  judge_ids <- judge_ids[which(!is.na(judge_ids$date_updated)),]
  judge_ids_loc <- dbGet(loc, "select `judge_id`, `date_updated` from Judges")
  judge_ids_loc$update_server <- judge_ids$date_updated[match(judge_ids_loc$judge_id, judge_ids$iuropa_judge_id)]
  judge_ids_loc$update_server[which(is.na(judge_ids_loc$update_server))] <- "2000-01-01"
  judge_ids <- judge_ids_loc$judge_id[which(as.Date(judge_ids_loc$update_server) < as.Date(judge_ids_loc$date_updated))]
  failed_judges <- update_judges(judge_ids, con, loc)
  
  message("Please update the following judges:")
  message(paste(failed_judges, collapse="\n"))
  
  # Delete false observations
  ecli_server <- dbGetQuery(con, "SELECT `cjeu_decision_id` FROM decisions")$cjeu_decision_id
  ecli_local <- dbGetQuery(loc, "SELECT `ecli` FROM Decisions")$ecli
  ecli_remove <- ecli_server[which(!ecli_server %in% ecli_local)]
  
  case_server <- dbGetQuery(con, "SELECT `cjeu_case_id` FROM cases")$cjeu_case_id
  case_server <- case_server[which(!grepl("Avis", case_server))]
  case_server <- gsub("OPINION", "Avis", case_server)
  
  case_local <- dbGetQuery(loc, "SELECT `case` FROM Cases")$case
  case_remove <- case_server[which(!case_server %in% case_local)]
  
  
  if(length(ecli_remove) > 700){
    stop("There seems to be a lot of decisions on the server not in the local data base. Look into what in the world is going on.")
  }
  if(length(case_remove) > 700){
    stop("There seems to be a lot of cases on the server not in the local data base. Look into what in the world is going on.")
  }
  
  if(length(ecli_remove) > 1){
    for(t in tables[which(ids == "ecli")]){
      dbExecute(con, paste0("DELETE FROM ", t, " WHERE `cjeu_decision_id` IN ('", paste(ecli_remove, collapse="', '"), "');"))
    }
  }
  if(length(case_remove) > 1){
    for(t in tables[which(ids == "case")]){
      dbExecute(con, paste0("DELETE FROM ", t, " WHERE `cjeu_decision_id` IN ('", paste(case_remove, collapse="', '"), "');"))
    }
  }
  
  # dbDisconnect(con)
  for(rounds in 1:10000){
    cases <- dbGet(con, "select `cjeu_case_id`, `date_updated` from cases")
    
    # loc <- cjeudb()
    Procedures <- dbGet(loc, "SELECT `case`, `date_updated` FROM Cases")
    # dbDisconnect(loc)
    
    Procedures$date_updated <- as.Date(gsub("\\s.*", "", Procedures$date_updated))
    cases$date_updated_info <- as.Date(Procedures$date_updated[match(cases$cjeu_case_id, Procedures$case)])
    cases$date_updated_server <- as.Date(gsub("\\s.*", "", cases$date_updated))
    
    updated <- cases$case[which(cases$date_updated_info > cases$date_updated_server)]
    new <- Procedures$case[which(!Procedures$case %in% cases$cjeu_case_id)]
    
    
    cases <- c(new, updated)
    message("\033[32m", length(cases), " cases need to be updated", "\033[39m")
    if(paste(newest.first) == "TRUE"){
      cases <- rev(cases)
    }
    if(paste(newest.first) == "random"){
      cases <- sample(cases, length(cases))
    }
    
    break_now <- FALSE
    if(length(cases) > 100){
      cases <- cases[1:100]
    } else {
      break_now <- TRUE
    }
    
    cjeu_sql(cases, con, loc)
    
    
    # for(t in tables){
    #   var <- table_sort[which(tables == t)]
    #   dbExecute(con, paste0("ALTER TABLE ", t, " ORDER BY `", var, "`;"))
    #   dbExecute(con, paste0("SET @row_number = 0; "))
    #   dbExecute(con, paste0("UPDATE ", t, " SET `key_id` = (SELECT (@row_number:=@row_number + 1));"))
    # }
    # 
    if(break_now){
      break
    } else {
      message("\033[32m", Sys.time(), "\n", 100*rounds, " cases updated. Reloading!", "\033[39m")
    }
  }
  
  # 
  # # Delete false observations
  # ecli_server <- dbGetQuery(con, "SELECT `ecli` FROM decisions")$ecli
  # ecli_local <- dbGetQuery(loc, "SELECT `ecli` FROM Decisions")$ecli
  # ecli_remove <- ecli_server[which(!ecli_server %in% ecli_local)]
  # 
  # case_server <- dbGetQuery(con, "SELECT `case` FROM cases")$ecli
  # case_local <- dbGetQuery(loc, "SELECT `case` FROM Cases")$ecli
  # case_remove <- case_server[which(!case_server %in% case_local)]
  # 
  # # Chamber size zero should not occur
  # 
  # 
  # if(length(ecli_remove) > 700){
  #   stop("There seems to be a lot of decisions on the server not in the local data base. Look into what in the world is going on.")
  # }
  # if(length(case_remove) > 700){
  #   stop("There seems to be a lot of cases on the server not in the local data base. Look into what in the world is going on.")
  # }
  # 
  # if(length(ecli_remove) > 1){
  #   for(t in tables[which(ids == "ecli")]){
  #     dbExecute(con, paste0("DELETE FROM ", t, " WHERE `ecli` IN ('", paste(ecli_remove, collapse="', '"), "');"))
  #   }
  # }
  # if(length(case_remove) > 1){
  #   for(t in tables[which(ids == "case")]){
  #     dbExecute(con, paste0("DELETE FROM ", t, " WHERE `case` IN ('", paste(case_remove, collapse="', '"), "');"))
  #   }
  # }
  # 
  
  message("Please update the following judges:")
  message(paste(failed_judges, collapse="\n"))
  
}
