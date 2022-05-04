# Random smaller scripts ####

# message if not silent
msg <- function(message=""){
  if(!exists("silent")){
    silent <- TRUE
  }
  if(!silent){return(message(message))}
}

# dbGetQuery alternative - dbGet and dbRun. These will keep trying when they can't connect.
dbGet <- function(con, statement, more=NULL){
  if(!dbIsValid(con)){
    stop("Connection no longer valid")
  }
  result <- as.factor("Error! Try again")
  try(result <- dbGetQuery(con, statement, more), silent=TRUE)
  if(class(result) == "factor"){
    if(result[1] == "Error! Try again"){
      message("connection to SQL failed (dbGet). Trying again in 20 seconds.")
      Sys.sleep(20)
      try(result <- dbGetQuery(con, statement, more), silent=TRUE)
    }
  }
  if(class(result) == "factor"){
    if(result[1] == "Error! Try again"){
      message("connection to SQL failed. Trying again in 20 seconds.")
      Sys.sleep(20)
      try(result <- dbGetQuery(con, statement, more), silent=TRUE)
    }
  }
  if(class(result) == "factor"){
    if(result[1] == "Error! Try again"){
      message("connection to SQL failed. Trying again in 30 seconds.")
      Sys.sleep(20)
      try(result <- dbGetQuery(con, statement, more), silent=TRUE)
    }
  }
  
  if(class(result) == "factor"){
    if(result[1] == "Error! Try again"){
      stop("giving up on dbGet")
    }
  }
  return(result)
}

dbRun <- function(conn, command, list=NULL){
  if(!dbIsValid(conn)){
    stop("Connection no longer valid")
  }
  list[which(list=="NA")] <- NA
  run <- NA
  
  try(run <- dbExecute(conn, command, list), silent=TRUE)
  
  # Fix listed dates to only include the first one:
  if(class(run) != "integer"){
    list2 <- list[grep("^\\d{4}-\\d{2}-\\d{2};", list)] <- 
      gsub("^(\\d{4}-\\d{2}-\\d{2}).*$", "\\1", list[grep("\\d{4}-\\d{2}-\\d{2}.", list)])
    if(is.null(list)){list2 <- list}
    command2 <- gsub("('\\d{4}-\\d{2}-\\d{2});[^']*'", "\\1'", command)
    try(run <- dbExecute(conn, command2, list2), silent=TRUE)
  }
  
  if(class(run) != "integer"){
    message("\n", command, ":\n")
    message("connnection to SQL failed. Trying again in 20 seconnds.")
    
    Sys.sleep(20)
    try(run <- dbExecute(conn, command, list), silent=TRUE)
  }
  
  # Try to fix potential problems on encoding 
  if(class(run) != "integer"){
    if(iconv(command2, to="latin1", sub="?") != command2){
      command2 <- iconv(gsub("’", "`", command2), to="ASCII//TRANSLIT", sub="?")
    }
    
    for(n in which(iconv(unlist(list2), to="latin1", sub="?") != unlist(list2))){
      list2[n] <- iconv(unlist(list2)[n], to="ASCII//TRANSLIT", sub="?")
    }
    if(is.null(list)){list2 <- list}
    
    try(run <- dbExecute(conn, command2, list2), silent=TRUE)
  }
  if(class(run) != "integer"){
    message("connnection to SQL failed again. Trying again in 20 seconnds.")
    Sys.sleep(20)
    try(run <- dbExecute(conn, command, list), silent=TRUE)
  }
  if(class(run) != "integer"){
    message("connnection to SQL failed. Trying again in 30 seconnds.")
    Sys.sleep(30)
    try(run <- dbExecute(conn, command2, list2), silent=TRUE)
  }
  if(class(run) != "integer"){
    message("connnection to SQL failed. Trying again in 30 seconnds. Last attempt.")
    Sys.sleep(30)
    run <- dbExecute(conn, command2, list2) 
  }
  if(class(run) != "integer"){
    stop("Cannot connnect to server")
  }
}

# Flatten lists; separate by semicolon instead
flatify <- function(input){
  if(length(input) == 0){
    return(NA)
  }
  if(class(input) %in% c("numeric", "character")){
    return(c(paste(input, collapse="; "), NA)[1])
  }
  if(length(input) == 1 & lapply(input, class)[[1]] == "list"){
    input <- lapply(input[[1]], as.character)
  }
  if(class(input[[1]]) == "matrix"){
    input <- input[[1]][,1]
  }
  
  input <- lapply(input, as.character)
  
  
  input <- unlist(input)
  input <- gsub(" ; ", "; ", input)
  
  if(TRUE %in% grepl("c\\(\"", input)){
    x <- grep("c\\(\"", input)
    input[x] <- gsub("c\\(\"|\"\\)", "", input[x])
    input[x] <- gsub("\", \"", "; ", input[x])
  }
  
  
  input <- unlist(str_split(paste(gsub("^\\W*|\\W*$", "", input), collapse="; "), "; "))
  
  return(c(paste(sort(gsub(";", ":", c(unique(input)))), collapse="; "), NA)[1])
}


# classify_value: Convert value into correct appropriate class
# output: value in class var_type
classify_value <- function(value, var_type){
  return(as.character(value)) # I think I prefer characters for everything, really 
  # if(var_type=="text"){return(value)}
  # if(var_type=="numerc"){return(as.numeric(value))}
  # if(var_type=="date"){return(getDate(value))}
}


# update_sql: Communicate with SQL server
update_sql <- function(ID, variable, value, table, ID_name, source=NA, new_row = FALSE){
  
  if(table %in% c("decisions", "cases")){
    new_row <- FALSE
  }
  
  if(na(source)){
    source <- rep(NA, length(value))
  }
  
  if(new_row){
    dbRun(con,
          command = paste0("INSERT INTO ", table, " (`", ID_name, "`) VALUES (?)"),
          list = ID
    )
    dbRun(con2,
          command = paste0("INSERT INTO ", table, " (`", ID_name, "`) VALUES (?)"),
          list = ID
    )
  }
  
  
  
  # Update main data (con)
  dbRun(con, 
        command = paste0("UPDATE ", table," SET ", paste0("`", variable,"` = ?", collapse=", "), " WHERE `", ID_name, "` = ?"), 
        list = as.list(c(as.character(value), ID))
        )
  
  # Update source table (con2)
  dbRun(con2,
        command = paste0("UPDATE ", table," SET ", paste0("`", variable,"` = ?", collapse=", "), " WHERE `", ID_name, "` = ?"),
        list = as.list(c(as.character(source), ID))
  )
  
}




# all_cons <- dbListConnections(MariaDB())
# 
# print(all_cons)
# 
# for(con in all_cons){dbDisconnect(con)}
