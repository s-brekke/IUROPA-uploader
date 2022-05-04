# user <- "brekke"
# password <-"rENC1HDmj6"
# 
# library(updateCJEU)
# library(CJEU2)
# 
# con <- dbConnect(MariaDB(),
#                  host = "iuropa.pol.gu.se",
#                  username = user, password = password,
#                  dbname = "cjeu_rolling")
# loc <- cjeudb()
# 
# uploadCJEU <- function(case=NA){
#   tables <- c("decisions", "cases", "assignments", "citations", "procedures", "parties", "submissions")
#   ids <-  c("ecli", "case", "ecli", "ecli", "ecli", "case", "ecli")
#   
#   for(t in tables){
#     data_columns <- unique(unlist(dbGet(con, paste0("SELECT `COLUMN_NAME` FROM `INFORMATION_SCHEMA`.`COLUMNS` WHERE `TABLE_NAME`='", t, "';"))))
#   }
# }