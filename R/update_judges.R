# Update judges
# frame <- dbGetQuery(con, "SELECT * FROM judges")

update_judges <- function(judge_id, con, loc){
  
  judges <- cjeu("Judges", c("judge_id", "firstname", "surname"))
    
  judges$label <- judges$surname
  x <- which(judges$surname %in% names(which(table(judges$surname) > 1)))
  judges$label[x] <- paste(gsub("(.).*$", "\\1.", judges$firstname[x]), judges$surname[x])
  
  if(TRUE %in% duplicated(judges$label)){
    message("\033[31mWarning in update_judges:\033[39m Surname labels are not unique.")
  }
  
  for(j in judge_id){
    message("Updating judge ", j)
    data <- cjeu("Judges", cases=j)
    phd <- grepl("phd|doctor|S\\W*J\\W*D", tolower(data$university_education))
    
    data$civil_service <- as.numeric(gsub("3", "2", gsub("2", "1", data$civil_service)))
    data$judge <- as.numeric(gsub("3", "2", gsub("2", "1", data$judge)))
    data$lawyer <- data$lawyer!=0
    academic <- gsub("1", "2", data$professor)
    academic[which(academic == 0 & phd)] <- 1
    
    current_status <- NULL
    if(!is.na(data$date_entry_ag)){
      if(data$date_exit_ag < Sys.Date()){
        current_status <- c(current_status, "Former AG")
      } else {
        current_status <- c(current_status, "Current AG")
      }
    }
    if(!is.na(data$date_entry_cj)){
      if(data$date_exit_cj < Sys.Date()){
        current_status <- c(current_status, "Former CJ judge")
      } else {
        current_status <- c(current_status, "Current CJ judge")
      }
    }
    if(!is.na(data$date_entry_gc)){
      if(data$date_exit_gc < Sys.Date()){
        current_status <- c(current_status, "Former GC judge")
      } else {
        current_status <- c(current_status, "Current GC judge")
      }
    }
    if(!is.na(data$date_entry_cst)){
      if(data$date_exit_cst < Sys.Date()){
        current_status <- c(current_status, "Former CST judge")
      } else {
        current_status <- c(current_status, "Current CST judge")
      }
    }
    
    non_consecutive <- 0
    entry <- na.omit(c(data$date_entry_ag, data$date_entry_cj, data$date_entry_cst, data$date_entry_gc))
    exit <- na.omit(c(data$date_exit_ag, data$date_exit_cj, data$date_exit_cst, data$date_exit_gc))
    for(n in 1:length(entry)){
      if(!TRUE %in% ((exit[n]-entry) < 365)){
        non_consecutive <- non_consecutive+1
      }
    }
    non_consecutive <-as.numeric(non_consecutive > 1)
    
    judge <- data.frame(judge_id = j,
               full_name = simplify_encoding(data$name),
               first_name = simplify_encoding(data$firstname),
               last_name = simplify_encoding(data$surname),
               last_name_latin = iconv(data$surname, to="ASCII//TRANSLIT"),
               last_name_label = simplify_encoding(judges$label[which(judges$judge_id == j)]),
               last_name_latin_label = iconv(judges$label[which(judges$judge_id == j)], to="ASCII//TRANSLIT"),
               member_state_id = ms_id[, data$nominating_ms],
               member_state = data$nominating_ms,
               member_state_code = ms_code[, data$nominating_ms],
               birth_year = format(data$date_birth, "%Y"),
               female = data$female,
               judge_court_of_justice = as.numeric(grepl("CJ_judge", data$positions)),
               judge_general_court = as.numeric(grepl("GC_judge", data$positions)),
               judge_civil_service_tribunal = as.numeric(grepl("CST_judge", data$positions)),
               advocate_general = as.numeric(grepl("CJ_AG", data$positions)),
               current_status = paste(sort(current_status), collapse="; "),
               current_member = as.numeric(TRUE %in% grepl("Current", current_status)),
               count_positions = length(current_status),
               nonconsecutive_positions = non_consecutive,
               start_date = min(data$date_entry_ag, data$date_entry_cj, data$date_entry_cst, data$date_entry_gc, na.rm=TRUE),
               end_date = max(data$date_exit_ag, data$date_exit_cj, data$date_exit_cst, data$date_exit_gc, na.rm=TRUE),
               start_date_court_of_justice = data$date_entry_cj,
               end_date_court_of_justice = data$date_exit_cj,
               start_date_general_court = data$date_entry_gc,
               end_date_general_court = data$date_exit_gc,
               start_date_civil_service_tribunal = data$date_entry_cst,
               end_date_service_tribunal = data$date_exit_cst,
               start_date_advocate_general = data$date_entry_ag,
               end_date_advocate_general = data$date_entry_cj,
               background_judge = data$judge,
               background_lawyer = data$lawyer,
               background_politics = data$politics,
               background_civil_servant = data$civil_service,
               background_academia = academic,
               university_education = simplify_encoding(paste(gsub(" \\|.*$", "", unlist(str_split(data$university_education, "\n"))), collapse="; ")),
               university_work = simplify_encoding(paste(gsub(" \\|.*$", "", unlist(str_split(data$university_work, "\n"))), collapse="; ")),
               date_updated = Sys.time()
               )
    
    # full_name <- judge$full_name
    # judge$full_name <- iconv(judge$full_name, to="ASCII//TRANSLIT", sub="?")
    # first_name <- judge$first_name
    # judge$first_name <- iconv(judge$first_name, to="ASCII//TRANSLIT", sub="?")
    # last_name <- judge$last_name
    # judge$last_name <- iconv(judge$last_name, to="ASCII//TRANSLIT", sub="?")
    # last_name_label <- judge$last_name_label
    # judge$last_name_label <- iconv(judge$last_name_label, to="ASCII//TRANSLIT", sub="?")

    dbRun(con, paste0("DELETE FROM judges WHERE `judge_id` = ?"), list(j))
    # If you run into problems here, update the "simplify_encoding" script below to weed out whatever character is causing trouble.
    dbAppendTable(con, "judges", judge)
  }
}


simplify_encoding <- function(text){ # Our MariaDB server is more picky on encoding than my SQLite server
  not_supported <- c("ř", "Ř", "ń", "Ł", "ł", "Ć", "ē", "ć")
  counterparts  <- c("r", "R", "n", "L", "l", "C", "e", "c")
  for(y in which(unlist(lapply(not_supported, function(y) grepl(y, text))))){
    text <- gsub(not_supported[y], counterparts[y], text)
  }
  return(text)
}
