# cjeuUpload: Identify cases that were updated locally since they were uploaded to the server.
cjeuUpload <- function(newest.first="random", 
                       silent = TRUE){
  # library(CJEU)
  silent <<- silent
  # 
  # con <- dbConnect(MariaDB(), 
  #                  host = "iuropa.pol.gu.se", 
  #                  username = user,
  #                  password = password, 
  #                  dbname = "cjeu_rolling")
  # 
  cases <- dbGet(con, "select `case`, `date_updated` from cases")
  judge_ids <- dbGet(con, "select `judge_id`, `date_updated` from judges")
  judge_ids <- judge_ids[which(!is.na(judge_ids$date_updated)),]
  judge_ids_loc <- dbGet(loc, "select `judge_id`, `date_updated` from Judges")
  judge_ids_loc$update_server <- judge_ids$date_updated[match(judge_ids_loc$judge_id, judge_ids$judge_id)]
  judge_ids_loc$update_server[which(is.na(judge_ids_loc$update_server))] <- "2000-01-01"
  judge_ids <- judge_ids_loc$judge_id[which(as.Date(judge_ids_loc$update_server) < as.Date(judge_ids_loc$date_updated))]
  # dbDisconnect(con)
  
  # loc <- cjeudb()
  Procedures <- dbGet(loc, "SELECT `case`, `date_updated` FROM Cases")
  # dbDisconnect(loc)
  
  Procedures$date_updated <- as.Date(gsub("\\s.*", "", Procedures$date_updated))
  cases$date_updated_info <- as.Date(Procedures$date_updated[match(cases$case, Procedures$case)])
  cases$date_updated_server <- as.Date(gsub("\\s.*", "", cases$date_updated))
  cases$date_updated_server[which(is.na(cases$date_updated_server))] <- "2020-10-01"
  updated <- cases$case[which(cases$date_updated_info > cases$date_updated_server)]
  new <- Procedures$case[which(!Procedures$case %in% cases$case)]
  
  update_judges(judge_ids, con, loc)
  
  cases <- c(new, updated)
  if(paste(newest.first) == "TRUE"){
    cases <- rev(cases)
  }
  if(paste(newest.first) == "random"){
    cases <- sample(cases, length(cases))
  }
  cjeu_sql(cases)
  
  tables <- c("decisions", "cases", "assignments", "citations", "procedures", "parties", "submissions", "judges")
  ids <-  c("ecli", "case", "ecli", "ecli", "ecli", "case", "case", "judge_id")
  table_sort <-  c("decision_date", "case_date", "decision_date", "decision_date", "decision_date", "case_date", "proceeding_date", "judge_id")
  
  # Delete false observations
  ecli_server <- dbGetQuery(con, "SELECT `ecli` FROM decisions")$ecli
  ecli_local <- dbGetQuery(loc, "SELECT `ecli` FROM Decisions")$ecli
  ecli_remove <- ecli_server[which(!ecli_server %in% ecli_local)]
  
  case_server <- dbGetQuery(con, "SELECT `case` FROM cases")$ecli
  case_local <- dbGetQuery(loc, "SELECT `case` FROM Cases")$ecli
  case_remove <- case_server[which(!case_server %in% case_local)]
  
  
  if(length(ecli_remove) > 200){
    stop("There seems to be a lot of decisions on the server not in the local data base. Look into what in the world is going on.")
  }
  if(length(case_remove) > 200){
    stop("There seems to be a lot of cases on the server not in the local data base. Look into what in the world is going on.")
  }
  
  if(length(ecli_remove) > 1){
    for(t in tables[which(ids == "ecli")]){
      dbExecute(con, paste0("DELETE FROM ", t, " WHERE `ecli` IN ('", paste(ecli_remove, collapse="', '"), "');"))
    }
  }
  if(length(case_remove) > 1){
    for(t in tables[which(ids == "case")]){
      dbExecute(con, paste0("DELETE FROM ", t, " WHERE `case` IN ('", paste(case_remove, collapse="', '"), "');"))
    }
  }
  
  for(t in tables){
    var <- table_sort[which(tables == t)]
    dbExecute(con, paste0("ALTER TABLE ", t, " ORDER BY `", var, "`;"))
    dbExecute(con, paste0("SET @row_number = 0; "))
    dbExecute(con, paste0("UPDATE ", t, " SET `key_id` = (SELECT (@row_number:=@row_number + 1));"))
  }
}

cjeu_sql <- function(id = NA, 
                     from = NA, 
                     silent = FALSE){
  
  tables <- c("decisions", "cases", "assignments", "citations", "procedures", "parties", "submissions")
  ids <-  c("ecli", "case", "ecli", "ecli", "ecli", "case", "case")
  
  cases_updated_now <- NULL
  eclis_updated_now <- NULL
  
  
  
  if(all(is.na(id)) & !is.na(from)){
    Procedures <- dbGet(loc, "SELECT `case`, `date_updated` FROM Cases")
    
    id <- Procedures$case[which(Procedures$case == from):nrow(Procedures)]
  }
  
    
    cases_columns <- colnames(dbGet(con, paste0("SELECT * FROM cases WHERE `case` = 'missing'")))
  
    decisions_columns <- colnames(dbGet(con, paste0("SELECT * FROM decisions WHERE `ecli` = 'missing'")))
    
    proceedings_template <- as.data.frame(cjeuniv::proceedings_template)
    
  for(i in id){
    message("\n", i, " - ", which(id == i), "/", length(id))
    message(Sys.time())
    if(grepl("ECLI", i)){
      ecli <- i
      case <- getCaseIDs(i, "case", con=loc)[1]
      proceeding <- case
    }
    if(grepl("\\d/\\d{2}", i)){
      case <- i
      ecli <- unique(c(cjeu("decisions", c("ecli"), i)$ecli,
        unlist(str_split(proceedings_template$ecli_list[which(proceedings_template$cjeu_proceeding_id == i)], ", "))))
      if(length(ecli) > 0){
        proceeding <- case
      } else {
        proceeding <- unique(getCaseIDs(i, "case", con=loc))[1]
      }
    }
    
    ecli <- ecli[which(!ecli %in% eclis_updated_now)]
    case <- case[which(!case %in% cases_updated_now)]
    
    ecli <- ecli[grep("ECLI:", ecli)]
    case <- case[grep("\\d/\\d{2}", case)]
    if(length(case) > 0){
      
      # null <- lapply(tables[which(ids=="case")], function(t) # this didn't seem to make anything much faster
      #   update_obs(IDs=case, table = t, silent=silent, loc=loc, cases_columns = cases_columns))
      for(t in tables[which(ids=="case")]){
        message("\033[32m", t, ":\033[39m ", paste(case, collapse=", "))
        update_obs(IDs=case, table = t, silent=silent, loc=loc, cases_columns = cases_columns)
      }
    }
    if(length(ecli) > 0){
      
      # null <- lapply(tables[which(ids=="ecli")], function(t)
      #   update_obs(IDs=ecli, table = t, silent=silent, loc=loc, cases_columns = cases_columns))
      for(t in tables[which(ids=="ecli")]){
        message("\033[32m", t, ":\033[39m ", paste(ecli, collapse=", "))
        update_obs(IDs=ecli, table = t, silent=silent, loc=loc, case = case[1], decisions_columns = decisions_columns)
      }
    }
    eclis_updated_now <- c(eclis_updated_now, ecli)
    cases_updated_now <- c(eclis_updated_now, case)
    
    
  }
}


update_obs <- function(IDs, 
                       table=NA, 
                       variables=NA, 
                       silent=FALSE, 
                       case=NA, 
                       ecli=NA, 
                       cases_columns=NA,
                       decisions_columns=NA,
                       loc=NA){ 
  
  if(is.na(table)){
    if(is.na(table) & grepl("ECLI", IDs[1]) & !grepl("\\.", IDs[1]) & !grepl("[[:upper:]]$", IDs[1])){
      table <- "decisions"
    }
    if(is.na(table) & grepl("ECLI.*\\.", IDs[1])){
      table <- "assignments"
    }
    if(is.na(table) & grepl("\\d/\\d", IDs[1])){
      table <- "cases"
    }
    if(is.na(table) & IDs[1] %in% Judges$judgeID){
      table <- "judges"
    }
  }
  
  if(table %in% c("decisions", "citations")){
    ID_name <- "ecli"
  }
  
  # Better way to find ID_name: First column of data table! Remember exception for eclis in assignments 
  judgeIDs <- NA
  if(table == "assignments"){
    ID_name <- "ecli"
    IDs <- gsub("^(.*)\\..*$", "\\1", IDs)
  }
  if(table %in% c("cases", "parties")){
    ID_name <- "case"
  }
  if(table %in% c("procedures")){
    ID_name <- "ecli"
  }
  if(table == "judges"){
    ID_name <- "judge_id"
  }
  if(table %in% c("submissions")){
    ID_name <- "proceeding"
  }
  # We need to think about how this should work when matching tables and ID codes on different levels.
  # Currently only works with ECLI.
  n <- 0
  # ID <- NULL
  
  # data_columns <- unique(unlist(dbGet(con, paste0("SELECT `COLUMN_NAME` FROM `INFORMATION_SCHEMA`.`COLUMNS` WHERE `TABLE_NAME`='", table, "';"))))
  data_columns <- colnames(dbGet(con, paste0("SELECT * FROM ", table, " WHERE `", ID_name, "` = 'missing'")))
  if(all(is.na(cases_columns))){
    cases_columns <- colnames(dbGet(con, paste0("SELECT * FROM cases WHERE `case` = 'missing'")))
  }
  if(all(is.na(decisions_columns))){
    decisions_columns <- colnames(dbGet(con, paste0("SELECT * FROM decisions WHERE `ecli` = 'missing'")))
  }
  
  decisions_columns <- c(decisions_columns[which(!decisions_columns %in% cases_columns)], "advocate_general_id", "advocate_general")
  cases_columns <- cases_columns[which(cases_columns != "date_updated")]
  
  # Set variables to all except default ID if missing 
  if(all(is.na(variables))){
    variables <- as.character(data_columns[2:length(data_columns)])
  }
  if(!"date_updated" %in% variables){
    variables <- c(variables, "date_updated")
  }
  
  # # Add missing columns
  # for(v in variables){
  #   if(!v %in% unlist(data_columns)){
  #     var_type <- "text" # This needs to get smarter - from class(variable) to corresponding sql classes
  #     if(v != "date_updated"){
  #     stop("Wants to add column")
  #       }
  #     if(v == "date_updated"){
  #       var_type <- "DATETIME"
  #     }
  #     dbRun(con, paste0("ALTER TABLE ", table, " ADD COLUMN `", v, "` ", var_type))
  #     # dbRun(con2, paste0("ALTER TABLE ", table, " ADD COLUMN `", v, "` ", var_type))
  #   }
  # }
  
  # When updating assignments by ECLI, it is essential for the script to remove all observations before adding new ones. This is so that 
  # past wrong observation are removed.
  drop_obs <- FALSE

  loop <- 0
  for(i in as.character(IDs)){
    
    if(grepl("ECLI:", i)){
      ecli <- i
      if(is.na(case)){
        case <- getCaseIDs(i, "case", con=loc)
      }
    }
    if(grepl("\\d/\\d{2}", i)){
      case <- i
      ecli <- getCaseIDs(i, "ecli", con=loc, maxone = TRUE)
    }
    
    loop <- loop+1
    msg(paste0("\n\033[34m____________________\033[39m ", i, " - ", table, " \033[34m____________________\033[39m"))
    
    if(drop_obs){ # Delete pre-existing rows from SQL table when needed (in assingments when updating on case level)
      if(!duplicated_ecli[which(IDs == i)]){
        
        dbRun(con, paste0("DELETE FROM ", table, " WHERE `ecli` = ?"), list(eclis[which(IDs == gsub(":\\d+?$", "", i))]))
        dbRun(con2, paste0("DELETE FROM ", table, " WHERE `ecli` = ?"), list(eclis[which(IDs == gsub(":\\d+?$", "", i))]))

      }
    }
    vars <- variables
    variables_cases <- NA
    variables_decisions <- NA
    columns_case_and_decision <- c("advocate_general", "advocate_general_id", "subject_matter", "subject_matter_subcategory")
    
    if(table != "cases"){
      # get shared columns from these tables instead
      if(grepl("ECLI", i)){
        cases_columns <- cases_columns[which(!cases_columns %in% columns_case_and_decision)]
      }
      variables_cases <- variables[which(variables %in% cases_columns)]
      vars <- vars[which(!vars %in% cases_columns)]
      if(table != "decisions"){
        # get shared columns from these tables instead
        
        variables_decisions <- vars[which(vars %in% decisions_columns)]
        
        
        if(table == "assignments"){
          variables_decisions <- variables_decisions[which(!variables_decisions %in% c("president", "rapporteur"))]
        }
        
        vars <- vars[which(!vars %in% variables_decisions)]
      } 
    }
    
    if(table == "parties"){
      if(grepl("\\.[[:lower:]]$", i)){
        case <- gsub("\\.[[:lower:]]$", "", i)
      } else {
        case <- i
      }
      number_of_rows_affected <- uploadParties(case, con=con, loc=loc)
    }    
    if(table == "submissions"){
      if(grepl("\\.[[:lower:]]$", i)){
        case <- gsub("\\.[[:lower:]]$", "", i)
      } else {
        case <- i
      }
      number_of_rows_affected <- uploadSubmissions(case, con=con, loc=loc)
    }
    
    number_of_rows_affected <- 1 # Default to > 0 == T
    
    if(table == "citations"){
      number_of_rows_affected <- update_citations(i)
    }
    # if(table == "submissions"){
    #   number_of_rows_affected <- uploadSubmissions(i)
    # }
    if(table == "procedures"){
      number_of_rows_affected <- update_procedures(i)
    }
    if(table == "assignments"){
      number_of_rows_affected <- update_assignments(i)
    }

    
    if(!table %in% c("parties", "citations", "submissions", "procedures","assignments")){ # multiple rows for each observation
      
      ID_list <- dbGet(con, paste0("SELECT `", ID_name, "` FROM ", table, " WHERE `", ID_name, "` = ?"), list(i))
      # Also sources
      ID_list2 <- dbGet(con2, paste0("SELECT `", ID_name, "` FROM ", table, " WHERE `", ID_name, "` = ?"), list(i))
      
      if(nrow(ID_list) > 1 | nrow(ID_list2) > 1){
        message("\033[31mDeleting duplicates: \033[39m", i, " - ", table)
        
        dbRun(con, paste0("DELETE FROM ", table, " WHERE `", ID_name, "` = ?"), list(i))
        dbRun(con2, paste0("DELETE FROM ", table, " WHERE `", ID_name, "` = ?"), list(i))
        dbRun(con, paste0("INSERT INTO ", table, " (`", ID_name,"`) VALUES (?)"), list(i))
        dbRun(con2, paste0("INSERT INTO ", table, " (`", ID_name,"`) VALUES (?)"), list(i))
      }
      
      if(nrow(ID_list) == 0 | nrow(ID_list2) == 0){
        
        dbRun(con, paste0("DELETE FROM ", table, " WHERE `", ID_name, "` = ?"), list(i))
        dbRun(con2, paste0("DELETE FROM ", table, " WHERE `", ID_name, "` = ?"), list(i))
        dbRun(con, paste0("INSERT INTO ", table, " (`", ID_name,"`) VALUES (?)"), list(i))
        dbRun(con2, paste0("INSERT INTO ", table, " (`", ID_name,"`) VALUES (?)"), list(i))
        
      }
      
    }
    
    
    # if(table == "decisions"){
    #   update_decisions(i)
    # }
    
    vars <- vars[which(vars != ID_name)]
    

    
    if(!table %in% c("parties", "citations", "submissions", "procedures", "assignments")){
      
      value_source <- lapply(vars, function(v)
        find_value(ID = i, variable = v, frame=table, silent=silent, loc=loc)
      )
      value <- unlist(lapply(value_source, function(y) y[1]))
      source <- unlist(lapply(value_source, function(y) y[2]))
      variable <- vars
      
      if("iuropa_decision_id" %in% vars & grepl("ECLI", i)){
        iuropa_decision_id_info <- 
          dbGetQuery(con, 
                     "SELECT proceeding_date, proceeding, decision_type, decision_date, court FROM decisions WHERE `ecli` = ?",
                     list(i))
        iuropa_decision_id <- iuropa(i, 
               con=loc,
               date_lodged = iuropa_decision_id_info$proceeding_date,
               case = iuropa_decision_id_info$proceeding,
               date_document = iuropa_decision_id_info$decision_date,
               court = iuropa_decision_id_info$court,
               decision_type = iuropa_decision_id_info$decision_type
               )
        value[which(variable == "iuropa_decision_id")] <- iuropa_decision_id
        source[which(variable == "iuropa_decision_id")] <- iuropa_decision_id
      }
      
      
      # # I used a loop here before, changed with apply to make it a bit faster. 
      # # The following  code can be deleted once the lapply function has proven itself. 
      # variable <- NULL
      # value <- NULL
      # source <- NULL
      # 
      # for(v in vars){
      #   msg(paste0("\033[34m    ID='", i,"'; variable='", v, "'    \033[39m"))
      #   
      #   value_out <- find_value(ID = i, variable = v, frame=table, silent=silent, loc=loc)
      #   # 
      #   # if(all(is.na(value_out)) & v != "appeal_of"){stop("NA - controlled :)")}
      #   variable <- c(variable, v)
      #   value <- c(value, value_out[1])
      #   source <- c(source, value_out[2])
      # }
      # 
      
      # data.frame(variable=variable, value=value, source=source)
      
        update_sql(ID = i, variable = variable, value = value, table=table, ID_name = ID_name, source = source, new_row = drop_obs)
        # update_sql(i, variable, value = source, table=table, ID_name = ID_name, source = TRUE)
        # update_sql(i, variable, value = source, table=paste0(table, "_source"), ID_name = ID_name)
        
    }
    
    if(table=="cases"){ # List decisions
      update_cases(i, proceeding = value[which(variable == "proceeding")])
    }
    
    

    if(!na(variables_cases) & !na(case) & number_of_rows_affected > 0){
      variables_cases <- variables_cases[which(variables_cases != ID_name)]
      
      # for(v in variables_cases){
      #   dbRun(con, 
      #         paste0("UPDATE ", table, " SET ", v, " = (SELECT cases.`", v, "` FROM cases WHERE cases.`case` = ?) WHERE ", table,".`", ID_name, "` = ?;"),
      #         list(case, i)
      #   )
      # }
      # 
      # null <- lapply(variables_cases, function(v)
      #   dbRun(con2,
      #         paste0("UPDATE ", table, " SET ", v, " = (SELECT cases.`", v, "` FROM cases WHERE cases.`case` = ?) WHERE ", table,".`", ID_name, "` = ?;"),
      #         list(case, i)
      #   )
      # )
      
      dbExecute(con, 
                paste0("UPDATE ", table, " SET ", 
                       paste0("`", variables_cases,"` = (SELECT cases.`", variables_cases, "` FROM cases WHERE cases.`case` = '", case, "')", collapse=", "),
                       " WHERE ", table,".`", ID_name, "` = ?;"),
                list(i))
      dbExecute(con2, 
                paste0("UPDATE ", table, " SET ", 
                       paste0("`", variables_cases,"` = (SELECT cases.`", variables_cases, "` FROM cases WHERE cases.`case` = '", case, "')", collapse=", "),
                       " WHERE ", table,".`", ID_name, "` = ?;"),
                list(i))
      
      # values_cases <- dbGet(con, paste0("SELECT ", paste0("`", variables_cases, "`", collapse=", "), " FROM cases WHERE `case` = ?"), list(case))[1,]
      # sources_cases <- dbGet(con2, paste0("SELECT ", paste0("`", variables_cases, "`", collapse=", "), " FROM cases WHERE `case` = ?"), list(case))[1,]
      # update_sql(ID = i, variable = names(values_cases), value = values_cases, table=table, ID_name = ID_name, source = sources_cases, new_row = FALSE)
    }
    if(!na(variables_decisions) & !na(ecli) & number_of_rows_affected > 0){
      variables_decisions <- variables_decisions[which(variables_decisions != ID_name)]
      
      # null <- lapply(variables_decisions, function(v)
      # for(v in variables_decisions){
      dbExecute(con, 
            paste0("UPDATE ", table, " SET ", 
               paste0("`", variables_decisions,"` = (SELECT decisions.`", variables_decisions, "` FROM decisions WHERE decisions.`ecli` = '", ecli, "')", collapse=", "),
               " WHERE ", table,".`", ID_name, "` = ?;"),
            list(i))
      dbExecute(con2, 
                paste0("UPDATE ", table, " SET ", 
                       paste0("`", variables_decisions,"` = (SELECT decisions.`", variables_decisions, "` FROM decisions WHERE decisions.`ecli` = '", ecli, "')", collapse=", "),
                       " WHERE ", table,".`", ID_name, "` = ?;"),
                list(i))
        
        # dbRun(con, 
        #       paste0("UPDATE ", table, " SET `", v, "` = (SELECT decisions.`", v, "` FROM decisions WHERE decisions.`ecli` = ?) WHERE ", table,".`", ID_name, "` = ?;"),
        #       list(ecli, i)
        # )
      # }
      # )
      
      # null <- lapply(variables_decisions, function(v)
      #   dbRun(con2,
      #         paste0("UPDATE ", table, " SET `", v, "` = (SELECT decisions.`", v, "` FROM decisions WHERE decisions.`ecli` = ?) WHERE ", table,".`", ID_name, "` = ?;"),
      #         list(ecli, i)
      #   )
      # )
      # values_decisions <- dbGet(con, paste0("SELECT ", paste0("`", variables_decisions, "`", collapse=", "), " FROM decisions WHERE `ecli` = ?"), list(ecli))[1,]
      # sources_decisions <- dbGet(con2, paste0("SELECT ", paste0("`", variables_decisions, "`", collapse=", "), " FROM decisions WHERE `ecli` = ?"), list(ecli))[1,]
      # update_sql(ID = i, variable = names(values_decisions), value = values_decisions, table=table, ID_name = ID_name, source = sources_decisions)
    }
    n <- n+1
    # if(n %% 10 == 0){
    #   Sys.sleep(1)
    # }
  }
  
  
  # if(length(ID) != length(variable) | 
  #    length(variable) != length(value) |
  #    length(value) != length(source)){
  #   stop("Uneven length of lists!")
  # }
  
  
  # for(n in 1:length(ID)){
  #   # Normal data set:
  #   update_sql(ID[n], variable[n], value = value[n], table=table, ID_list=ID_list, data_columns=data_columns)
  #   # Sources:
  #   update_sql(ID[n], variable[n], value = source[n], table=paste0(table, "_source"), ID_list=ID_list, data_columns=data_columns)
  # }
}


