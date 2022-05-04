
update_parties <- function(case, con=NA, loc=NA){
  type_s <- NA
  name_s <- NA
  Parties <- dbGetQuery(loc, "SELECT * FROM Parties WHERE `case` = ?", list(case))
  
  Parties$role <- as.character(Parties$role)
  Parties$role[which(Parties$role == "other party")] <- "defendant"
  
  dbRun(con, paste0("DELETE FROM parties WHERE `case` = ?"), list(case))
  dbRun(con2, paste0("DELETE FROM parties WHERE `case` = ?"), list(case))
  
  for(actor in c("applicant", "defendant")){
    name <- NA
    type <- NA
    ID <- paste0(case, ".", gsub("^(.).*$", "\\1", actor))

    if(case %in% Parties$case){
      name <- as.character(Parties$name[which(Parties$role == actor)][1])
      if(!is.na(Parties$actor[which(Parties$role == actor)][1])){
        name <- as.character(Parties$actor[which(Parties$role == actor)][1])
      }
      type <- as.character(Parties$type[which(Parties$role == actor)][1])
    }
    
    if(!is.na(name)){
      name_s <- "brekke"
    }
    if(!is.na(type)){
      type_s <- "brekke"
    }
    
    if(case %in% f_parties$joined_case_IDs){
      f_name <- as.character(f_parties$party[which(f_parties$joined_case_IDs == case & f_parties$side == actor)][1])
      f_info <- getActorType(f_name)
      f_type <- NA
      
      standardized_name <- f_info$actor
      if(!is.na(standardized_name)){
        f_name <- standardized_name
        f_type <- f_info$type[1]
      }
      
      if(!is.na(f_name)){
        if(is.na(name)){
          name <- f_name
          name_s <- "fjelstul"
        }
        if(f_name == name){
          name_s <- "fjelstul; brekke"
        }
        if(f_name != name & nchar(name) > 80 & f_name != "legal person"){
          name <- f_name
          name_s <- "fjelstul"
          type_s <- f_type
          if(!is.na(f_type)){
            type_s <- "fjelstul"
          }
        }
      }
      if(!is.na(f_type)){
        if(is.na(type)){
          type <- f_type
          type_s <- "fjelstul"
        } else {
          if(f_type == type){
            type_s <- "fjelstul; brekke"
          }
        }
      }
    }
    
    if(is.null(type_s)){
      type_s <- NA
    }
    if(is.null(name_s)){
      name_s <- NA
    }
    
    if(length(name) == 1){
      if(!is.na(name)){
        if(iconv(name, to="latin1", sub="?") != name){
          name <- iconv(name, to="ASCII//TRANSLIT", sub="?")
        }
      }
    }
    
    if(!is.na(name)){
    dbRun(con, paste0("INSERT INTO parties (`case`, `actor`, `actor_role`, `actor_type`, `date_updated`) VALUES (?, ?, ?, ?, ?)"),
          list(case, name, actor, type, as.character(Sys.time())))
    # dbRun(con2, paste0("INSERT INTO parties (`case`,  `actor`, `actor_role`, `actor_type`) VALUES (?, ?, ?, ?)"),
    #       list(case, name_s, actor, type_s))
    }
  }
}
