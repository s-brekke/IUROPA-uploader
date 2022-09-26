uploadParties <- function(proceeding, con=con, loc=cjeudb()){
  
  parties_all <- dbGetQuery(loc, "SELECT * FROM Parties WHERE `proceeding` = ?", list(proceeding))
  if(nrow(parties_all) == 0){
    return(NA)
  }
  
  parties_all$eclicounter <- paste0(gsub("^.*:(\\d{4}):.*$", "\\1", parties_all$ecli),
                                    str_pad(gsub("^.*:", "", parties_all$ecli), 5, pad=0, side="left"))
  parties_all$eclicounter[grep("NA", parties_all$eclicounter)] <- 1
  parties_all$eclicounter <- as.numeric(parties_all$eclicounter)
  
  if(nrow(parties_all) == 0){
    return(NA)
  }
  dbRun(con, paste0("DELETE FROM parties WHERE `proceeding` = ?"), list(proceeding))
  # dbRun(con2, paste0("DELETE FROM parties WHERE `proceeding` = ?"), list(proceeding))
  
  parties_all$actor[which(paste(parties_all$actor) == "NA")] <- parties_all$name[which(paste(parties_all$actor) == "NA")]
  
  parties <- NA
  
  parties_all$case[which(parties_all$case == "NA")] <- NA
  
  parties_all$case[grep("R-", parties_all$case)] <- NA
  
  if(all(is.na(parties_all$case))){
    parties_all$case <- parties_all$proceeding
  }
  
  if(length(na.omit(unique(parties_all$case))) == 1){
    parties_all$case <- unique(na.omit(parties_all$case))
  }
  
  for(actor in unique(parties_all$role[which(parties_all$role %in% c("applicant", "appellant", "defendant", "other party"))])){
    cases <- unique(unlist(unfold_list(parties_all$case)))
    cases <- unique(na.omit(cases[!grepl("\"", cases)]))
    for(c in cases){
      parties_c <- parties_all[which(parties_all$role == actor & grepl(c, parties_all$case)),]
      if(nrow(parties_c) > 0){
      if(length(unique(parties_c$ecli)) == nrow(parties_c)){
        t <- table(parties_c$type)
        if(length(t) > 0){
          parties_c$type <- names(t[which(t == max(t))][1])
        }
        t <- table(parties_c$actor)
        if(length(t) > 0){
          parties_c$actor <- names(t[which(t == max(t))][1])
        }
      }
      
      # here bad observations can be weeded out 
      parties_c <- parties_c[which(parties_c$eclicounter == max(parties_c$eclicounter)),]
      parties_c$actor_type <- parties_c$type
      parties_c$actor_role <- parties_c$role
      if(na(parties)){
        parties <- parties_c[,c("case", "proceeding","actor_role","actor", "actor_type")]
      } else {
        parties <- rbind(parties_c[,c("case", "proceeding","actor_role","actor", "actor_type")], parties)
      }
      parties <- parties[order(parties$actor_role),]
      }
    }
  }
  
  for(repeated_actor in names(which(table(paste(parties$actor, parties$actor_role, sep="¼")) > 1))){
    ra <- gsub("¼.*$", "", repeated_actor)
    rr <- gsub("^.*¼", "", repeated_actor)
    rows <- which(parties$actor == ra & parties$actor_role == rr)
    row <- rows[1]
    parties$actor[rows[2:length(rows)]] <- NA
    parties$case[row] <- paste(unique(unlist(unfold_list(parties$case[rows]))), collapse="; ")
    parties <- parties[which(!is.na(parties$actor)),]
  }
  parties$date_updated <- Sys.time()
  
  if(TRUE %in% grepl("\"", parties$case)){
    cases <- lapply(parties$case, unfold_list)
    cases <- lapply(cases, function(y) paste(unlist(y)[which(!grepl("\"", unlist(y)))], collapse="; "))
    parties$case <- unlist(cases)
  }
  cases <- unique(na.omit(cases[!grepl("\"", cases)]))
  
  parties$case <- iconv(parties$case, to="ASCII//TRANSLIT", sub="?")
  out <- NA
  try(out <- dbAppendTable(con, "parties", parties), silent = TRUE)
  if(is.na(out)){
    parties$actor <- iconv(parties$actor, to="ASCII//TRANSLIT", sub="?")
    out <- dbAppendTable(con, "parties", parties)
  }
  return(nrow(parties))

}
