#### FUNCTION HELPER #####
library("tidyverse")

source(censoredInfoPath)

# CensoredInfo contains the list codersNames and function getCodersName(createdBy)
# described below

# codersNames is the list of coders in the team
# codersNames <- c("coder1", "coder2")

# getCodersName takes the name used in the Maxqda file and returns 
# the corresponding coders names 
# according to who submitted the intercoder test 1
# getCodersName <- function(createdBy){
#   return (case_when(
#     createdBy == "name in MAXQDA for coder1" ~ "coder1",
#     createdBy == "coder2 pseudo"~ "coder2",
#     ...,
#     TRUE ~ createdBy))
# }

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


#prompt for team members with the possibility to keep the default list of team members
# promptForTeamMembers <- function(){
#   wholeTeam <- codersNames
#   if(readline("Do you want to use the default list of team members? y/n: ") == "y"){
#     return(wholeTeam)
#   } else{
#     listOfNames <- c()
#     question <-"Write the name of one coder and press Enter (if you're done, press Enter):"
#     while ((name = readline(question)) != "" ) {
#       listOfNames <- append(listOfNames, name)
#     }
#     return(listOfNames)
#   }
# }
# 
createPairsDf <- function(listOfNames){
  parsedNames <- c()
  df.pairs <- data.frame()
  for (name in listOfNames) {
    if(length(parsedNames)!=0){
      for (parsedName in parsedNames) {
        df.pairs <- rbind(df.pairs, c( paste0(name,".",parsedName),name, parsedName))
      }
    }
    parsedNames <- append(parsedNames, name)
  }
  
  colnames(df.pairs)<- c("Pair","C1","C2")
  return(df.pairs %>% mutate(Pair.red = paste0(Pair, ".red")))
}


pairs.df <- createPairsDf(codersNames)

getAgreement<- function(coder1, coder2){
  codeList1 = unlist(strsplit(coder1, ", "))
  codeList2 = unlist(strsplit(coder2, ", "))
  
  if (length(codeList1)>1 |length(codeList2)>1){
    #only works if we assume there are max double codes (not triple code...)
    score = 0
    for (code in codeList1) {
      if(code %in% codeList2){
        score = score+0.5
      }
    }
    return(score)
  }else if (coder1==coder2){
    return(1)
  }else{
    return(0)
  }
}

getStringToEval<- function(){
  
  pairs.df <- pairs.df %>%
        mutate(stringToEval = paste0(Pair," = mapply(getAgreement,",C1,",", C2,"),",
                                     Pair.red, " = ifelse(substring(", C1,",1,2)==substring(",C2,",1,2),1,0),"))

  returnString <- "result <- result %>% mutate("
  
  for (i in 1:nrow(pairs.df)) {
    returnString <- paste(returnString, pairs.df[i, "stringToEval"])
  }
  
  returnString <- paste(returnString,
  ") %>%
    mutate(commentAgreement = rowMeans(select(., all_of(pairs.df$Pair))))%>%
    mutate(commentAgreement.red = rowMeans(select(., all_of(pairs.df$Pair.red))))%>%
    arrange(End)")
  return(returnString)
}


getResultTable <- function(dataframe, reduced = FALSE){
  result <- dataframe %>% mutate(CodeMin = getAbbreviatedCode(Code))
  
  if (reduced){
    result <- result %>% mutate(CodeMin = substring(CodeMin, 1,2)) ## REDUCED IDEATION!
  }
  
  result <- result %>% 
    mutate(Created.by = mapply(getCodersName, Created.by)) %>%
    mutate(CodeMin = ifelse(CodeMin=="I0c/CS Counter Speech", "I0c", CodeMin)) %>% #could not introduce an if else in the function
    filter(substring(CodeMin, 1, 1) == "I")%>%
    select(Created.by, End, CodeMin, Segment) %>%
    pivot_wider(names_from = Created.by, values_from = CodeMin, 
                values_fill = "noCode",
                values_fn=function(values) paste(values, collapse = ", "))
  
  eval(parse(text=getStringToEval()))
  return(result)
}

getSummary <- function(dfCalc){
  result <- dfCalc %>%
    select(-End, -Segment, -all_of(codersNames))%>%
    pivot_longer(names_to = "Pair", values_to = "VALUES", cols=all_of(pairs.df$Pair)) %>% 
    group_by(Pair) %>%
    summarise(Agreement=mean(VALUES)) %>%
    rowwise()%>%
    merge(pairs.df)%>%
    select(-Pair, -Pair.red)%>%
    pivot_wider(names_from = C1, values_from = Agreement)%>%
    rename(Coders = C2)
  
  return(result)
}

getListColumnToEval <- function(type, nameList){
  stringToEval <- ""
  virgule <- ''
  for (name in nameList) {
    stringToEval <- paste0(stringToEval, virgule, name, "= ",type,"_column()")
    virgule <- ','
  }
  return(stringToEval)
}

getAgreementListColumnToEval <- function(){
  stringToEval <- paste0("list(", getListColumnToEval("agreement", codersNames[-1]), ")")
  return(stringToEval)
}

getCodeListColumnToEval <- function(){
  stringToEval <- paste0("list(
    commentAgreement = agreement_column(name = 'Mean Agreement*'),", getListColumnToEval("code", codersNames), ",
                         commentAgreement.red = agreement_column(name = 'AS vs. not AS'),
                         Segment =comment_column()
  )")
  return(stringToEval)
}