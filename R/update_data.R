# Script to prepare data for CJEU updater 
# Run after updating CJEU database package, then rebuild updater 

updater_data <- function(path = "~/Dropbox/Dokumenter/CJEU updater/data"){
  
  library(RMariaDB)
  library(CJEU2)
  library(cjeuniv)
  library(stringr)
  
loc <- cjeudb()
  fjelstul <- as.data.frame(cjeuniv::decisions)
# 
#   f_procedures <- as.data.frame(cjeuniv::procedures)
#   
#   f_judges <- as.data.frame(cjeuniv::judge_data)
#   f_judges$judgeID <- getJudgeID(f_judges$name, con=loc)
#   f_judges$f_name <- f_judges$name
#   f_judges$name <- NULL
#   f_judges$judgeID[which(f_judges$judge_ID == "J:010")] <- "1502"
#   f_judges$judgeID[which(f_judges$judge_ID == "J:099")] <- "1516"
#   f_judges$judgeID[which(f_judges$judge_ID == "J:034")] <- "1403"
#   f_judges$judgeID[which(f_judges$judge_ID == "J:141")] <- "1409"
#   f_judges$nominating_ms <- f_judges$member_state
#   f_judges$female <- f_judges$gender=="female"
# 
#   f_parties <- as.data.frame(cjeuniv::parties)
#   f_parties$side[which(f_parties$side == "plaintiff")] <- "applicant"
# 
#   f_assignments <- as.data.frame(cjeuniv::judge_assignments)
#   f_assignments$judgeID <- f_judges$judgeID[match(f_assignments$judge_ID, f_judges$judge_ID)]
#   f_assignments$eclijudge <- paste(f_assignments$ecli, f_assignments$judgeID, sep=":")
#   f_assignments$judge_rapporteur <- f_assignments$judge_rapporteur == 1


  # f_citations <- as.data.frame(cjeuniv::citations)
  # f_citations$ecli <- Decisions$ecli[match(f_citations$CJEU_ID, Decisions$CJEU_ID)]
  # colnames in Fjelstul
  colnames(fjelstul) <- gsub("document_date", "date_document", colnames(fjelstul))
  colnames(fjelstul) <- gsub("case_date", "date_lodged", colnames(fjelstul))
  colnames(fjelstul) <- gsub("plaintiff", "applicant", colnames(fjelstul))

  # chambers_f <- c("First Chamber", "Second Chamber", "Third Chamber", "Fourth Chamber", "Fifth Chamber", "Sixth Chamber",
  #                 "Seventh Chamber", "Eigth Chamber", "Ninth Chamber", "Tenth Chamber", "Grand Chamber", "Appeals Chamber", "Plenary Assembly")
  # chambers_n <- c(1:10, "Grand", "Appeal", "Plenary Assembly")
  # fjelstul$chamber <- NA
  # for(n in 1:length(chambers_f)){
  #   fjelstul$chamber[grep(chambers_f[n], fjelstul$formation)] <- chambers_n[n]
  # }
  # fjelstul$n_judges <- unlist(lapply(fjelstul$judges, function(y) length(unlist(str_split(y, ",")))))
  save(fjelstul, file=file.path(path,"fjelstul.rda"))
  # save(f_assignments, file="f_assignments.rda")
  # save(f_citations, file="f_citations.rda")
  # save(f_judges, file="f_judges.rda")
  # save(f_parties, file="f_parties.rda")
  # save(f_procedures, file="f_procedures.rda")
}
