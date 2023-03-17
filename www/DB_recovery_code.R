# Functions for use with MEADOWS DB RECOVERY APP version 2_1
# Amended to use DB with "surveys" not "surveys"
# Libraries
library("RMySQL")
library(tidyverse)

# Globals:
survey_choices <- data.frame(
  Namestring = character(),
  survey_id = integer(),
  stringsAsFactors = FALSE)

# Functions:
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

SurveyYears <- function(){
  # Returns a list of years as character: "2006", "2007", ...
  mydb = dbConnect(MySQL(), user='guest', password='guest', dbname='meadows', port = 3306, host='sxouse.ddns.net')
  q <- 'SELECT distinct convert (year(visit_date), CHAR) from visit_dates order by visit_date;'
  rs1 = dbSendQuery(mydb, q)
  years <- fetch(rs1, n=-1) %>% rename(visited = "convert (year(visit_date), CHAR)")
  dbDisconnectAll()
  return(list(c("All", years$visited)))
}

surveyList <- function(visitYear){
  # Returns a list of surveys to choose from according to visitYear -
  # "All" or a year as character: "2011"
  # Open connection to meadows DB
  mydb = dbConnect(MySQL(), user='guest', password='guest', dbname='meadows', port = 3306, host='sxouse.ddns.net')
  # Get the list of surveys. NOTE must return surveys_id not survey_id
  if (visitYear == "All") 
  {
    q <- paste("SELECT site_name, assembly_name, surveys_id FROM sites
               join meadows on sites_id = site_id
               join surveys on meadows_id = meadow_id;")
  }
  else
  {
    q <- paste("SELECT distinct site_name, assembly_name, surveys_id FROM sites
        join meadows on sites_id = site_id
        join surveys on meadows_id = meadow_id
        join quadrats on survey_id = surveys_id
        join visit_dates on vd_id = vds_id
        where year(visit_date) = ", visitYear,";")
  }
  rs1 = dbSendQuery(mydb, q)
  d <- fetch(rs1, n=-1)
  dbDisconnectAll()
  # Paste site_name and assembly_name into a single string to uniquely define
  # a site and survey. Interseting use of Tidyverse "unite".
  d <- d %>% unite("Namestring", site_name:assembly_name, sep = ":", remove = T)
  return(d)
}

surveyTable <- function(assId){
  # Returns a data frame formatted for spreadsheet like the original Excel files
  # GET DATA FROM DB
  # Open connection to meadows DB
  mydb = dbConnect(MySQL(), user='guest', password='guest', dbname='meadows', port = 3306, host='sxouse.ddns.net')
  # Get the records for this survey
  q <- paste("select species_name, cast(quadrats_id AS CHAR), domin from meadows
      join surveys on meadow_id = meadows_id
      join quadrats on survey_id = surveys_id
      join records on quadrat_id = quadrats_id
      join species on records.species_id = species.species_id 
      where surveys_id = ", assId, ";")  
  rs1 = dbSendQuery(mydb, q)
  data <- fetch(rs1, n=-1)
  dbDisconnectAll()
  # FINISHED GETTING DATA FROM DB
  # Data wrangling
  tbl <- data %>% 
    rename(quadrat = 2) %>% # Column 2 renamed from something complicated to "quadrat"
    pivot_wider(
      names_from = quadrat, 
      values_from = domin,
      values_fill = 0)
  return(tbl)
} # End of function surveyTable

surveyData <- function(assId){
  # retrieve supplementary data: file name, quadrat size grid ref and community
  # Open connection to meadows DB
  mydb = dbConnect(MySQL(), user='guest', password='guest', dbname='meadows', port = 3306, host='sxouse.ddns.net')
  q <- paste("SELECT source_file, quadrat_size, grid_ref, community FROM meadows.surveys
        where surveys_id = ", assId, ";") 
  rs1 = dbSendQuery(mydb, q)
  data <- fetch(rs1, n=-1)
  dbDisconnectAll()
  return(data)
}
## MAIN
# Required to initialise survey_choices etc
survey_choices <- surveyList("All")

  