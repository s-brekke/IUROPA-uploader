# Create procedures data set like the one found in Fjelstul, based on data from Brekke.
getProcedures <- function(ecli){
  
  eurlex <- dbGetQuery(loc, "SELECT * FROM eurlex WHERE `ecli` = ?", list(ecli))
  curia <- dbGetQuery(loc, "SELECT * FROM curia WHERE `ecli` = ?", list(ecli))
  
  
  obs <- dbGetQuery(loc, "select procedure from decisions where ecli = ?", list(ecli))
  obs$procedure <- unfold_list(obs$procedure)
  proced <- if(length(unique(unlist(obs$procedure))) == 0){NA} else {unique(unlist(obs$procedure))}
  df <- data.frame(ecli = ecli,
                   procedure_type=proced,
                   successful = NA,
                   unfounded = NA,
                   inadmissible = NA,
                   interlocutory = NA,
                   dismissed = NA,
                   date_updated=Sys.Date())
  
  for(n in 1:nrow(df)){
    p <- gsub(" ", "_", tolower(df$procedure_type[n]))
    out <- NA
    if(paste(p) == "reference_for_a_preliminary_ruling"){
      p <- "reference_for_preliminary_ruling"
    }
    p <- gsub("actions", "action", p)
    if(p %in% colnames(obs)){
      out <- unlist(obs[,p])
    } else {
      if(paste(p) != "NA"){
        
        proc <- as.data.frame(unfold_table(eurlex$procedure[which(eurlex$ecli==ecli)]))
        if(nrow(proc) > 0){
          out <- paste(unlist(proc[which(proc$procedure == df$procedure_type[n]),"outcome"]))
        } else {
          outs <- unlist(strsplit(curia$Procedural_Analysis_Information__Procedure_and_result[which(curia$ecli == ecli)], "%&%"))
          out <- gsub(paste0("^.*", df$procedure_type[n], "\\W*"), "",
                      gsub("for leave |a declaration of ", "", outs[grep(gsub("for leave |a declaration of ", "", df$procedure_type[n]),
                                                                         gsub("for leave |a declaration of ", "", outs), ignore.case = TRUE)]), ignore.case = TRUE)
        }
      }
      
    }
    if(all(paste(out)=="")){
      out <- NA
    }
    if(!all(is.na(out))){
      df$inadmissible[n] <- "inadmissible" %in% out
      df$unfounded[n] <- "unfounded" %in% out
      df$successful[n] <- "successful" %in% out
      df$interlocutory[n] <- TRUE %in% grepl("interlocutory", out)
      df$dismissed[n] <- TRUE %in% grepl("dismiss", out)
    }
  }
  
  return(df)
}
