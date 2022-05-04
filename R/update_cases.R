update_cases <- function(case, proceeding=NA){
  
  if(is.na(proceeding)){
    proceeding <- case
  }
  cols <- c("judgment", "ag_opinion", "order")
  types <- c("Judgment", "AG Opinion", "Order")
  
  for(n in 1:3){
    obs <- dbGetQuery(loc, "SELECT `ecli`, `date_document` FROM Decisions WHERE `type` = ? AND `case` = ?", list(types[n], proceeding))
    ecli <- paste(obs$ecli, collapse=", ")
    date <- paste(obs$date_document, collapse=", ")
    
    update_sql(ID = case,
               variable = c(cols[n], paste0("date_", cols[n])),
               value = c(ecli, date), 
               table="cases",
               ID_name = "case",
               # source = c(ecli, date),
               new_row = FALSE)
  }
  
  # draw_backwards <- dbGet(con, "SELECT `subject_matter`, `subject_matter_subcategory`, `advocate_general`, `advocate_general_id` FROM cases WHERE `case` = ?", list(case))
  # 
  # colnames(draw_backwards)[which(is.na(draw_backwards))] # These could be gathered from the decision level at this stage
}