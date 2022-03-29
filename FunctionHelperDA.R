#### FUNCTION HELPER #####
library("tidyverse")

# getAbbreviatedCode takes a full MAXQDA code as parameter, categories included,
# e.g., "Stereotypes (AS)\S14 Instrumentalisation of antisemitism", and returns the 
# corresponding abbreviated code, e.g., "S14".
getAbbreviatedCode <- function(code){
  codeMin <- gsub(".*\\\\(\\w*)\\\ .*", "\\1", c(code)) #get last part of path
  codeMin <- gsub(".*\\\\(.*)$", "\\1", c(codeMin)) # even when there is no space at the end
  codeMin <- gsub("^(\\w*)\\\ .*", "\\1", c(codeMin)) # keep only first word when there is no path
  return(codeMin)  
}


# getOutlet takes the document name as defined by the crawler as parameter, e.g., 
# "FB-MONDE_20200630_YouTube_supprime_la_chaine_de_Dieudonne_sofort", and returns
# the corresponding outlet, e.g. "FB-MONDE".
# Note that for Twitter threads the outlet will be "TWITT"
getOutlet <- function(documentName){
  outlet <- gsub("^(.*?)_.*", "\\1", c(documentName))
  return(outlet)
}


# getMaxqdaCSVExport takes the csv files obtained as described below and return a new dataframe
# Export the Maxqda project file (Reports > Export > "Projects components as Excel File"), 
# open this Excel file to the "Coded segments" sheet and "Save as CSV" 
# (you will get an alert that it will save only the active sheet).
getMaxqdaCSVExport <- function(){
  csvFilter <- matrix(c("CSV files (*.csv)", "*.csv"), nrow = 1)
  filePath <- choose.files(default = "", caption = "Select Maxqda csv export",
                           filters = csvFilter,
                           index = 1)
  df <- read.csv(filePath, header=TRUE)
  return(df)
  #give the option to preselect a folder?
}

# getCodersName take the name used in the Maxqda file and return the coders name 
# according to who submitted the intercoder test 1
getCodersName <- function(createdBy){
  return (case_when(
    createdBy == "pseudo" ~ "name",
    createdBy == "pseudo"~ "name",
    TRUE ~ createdBy))
}

### !!!! If the list of Names is modified, the list of pairs needs to be modified 
# accordingly, as well as the function get CodersName if needed
codersPairs <- c('name1.name2', 'name1.name3', 'name2.name3')
# codersPairs can be recreated using find&replace in Notepad: copy-paste codersNames and 
# replace " '" with "'name.", then copy-paste codersNames again, etc. 
codersPairs.red <- c('name1.name2.red', 'name1.name3.red', 'name2.name3.red')
# from codersPairs, replace "'," with ".red',"
codersNames <- c( 'name1', 'name2', 'name3')


getResultTable <- function(dataframe, CP, CP.red, reduced = FALSE){
  result <- dataframe %>% mutate(CodeMin = getAbbreviatedCode(Code))
  
  if (reduced){
    result <- result %>% mutate(CodeMin = substring(CodeMin, 1,2)) ## REDUCED IDEATION!
  }
  
  result <- result %>% 
    mutate(Created.by = getCodersName(Created.by)) %>%
    mutate(CodeMin = ifelse(CodeMin=="I0c/CS Counter Speech", "I0c", CodeMin)) %>% #could not introduce an if else in the function
    filter(substring(CodeMin, 1, 1) == "I")%>%
    select(Created.by, End, CodeMin, Segment) %>%
    pivot_wider(names_from = Created.by, values_from = CodeMin, values_fill = "noCode") %>%
    mutate(name1.name2 = ifelse(name1==name2,1,0),
           name1.name3 = ifelse(name1==name3,1,0),
           name2.name3 = ifelse(name2==name3,1,0),
                      #once you have codersPairs, you can use find& replace, using regular expression:
           # find: '(.*)\.(.*)',
           # replace: $1.$2 = ifelse\($1==$2,1,0\),
           name1.name2.red = ifelse(substring(name1,1,2)==substring(name2,1,2),1,0),
           name1.name3.red = ifelse(substring(name1,1,2)==substring(name3,1,2),1,0),
           name2.name3.red = ifelse(substring(name2,1,2)==substring(name3,1,2),1,0),
           # get the reduced pairs by replacing '(', '==', ',0,1' and '= '
    ) %>%
    mutate(commentAgreement = rowMeans(select(., all_of(CP))))%>%
    mutate(commentAgreement.red = rowMeans(select(., all_of(CP.red))))%>%
    arrange(End)
  return(result)
}


getSummary <- function(dfCalc, CN, CP, CP.red){
  result <- dfCalc %>%
    select(-End, -Segment, -all_of(CN), -all_of(CP.red))%>%
    pivot_longer(names_to = "CoderPair", values_to = "VALUES", cols=all_of(CP)) %>% 
    group_by(CoderPair) %>%
    summarise(Agreement=mean(VALUES)) %>%
    rowwise()%>%
    mutate(rowName = unlist(strsplit(CoderPair, ".", fixed=TRUE))[1],
           colName = unlist(strsplit(CoderPair, ".", fixed=TRUE))[2])%>%
    select(-CoderPair)%>%
    pivot_wider(names_from = colName, values_from = Agreement)
  
  return(result)
}
