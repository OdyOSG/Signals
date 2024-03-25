# Script to pull stratifications from json files. 
# Built for Signals 3/24
library(jsonlite)
library(dplyr)
library(waldo)
# List files


# Loop
# Read file
# read vars
# Bind output
getwd()
filePath = "inst/cohorts/class"
fileList <- list.files(path = filePath)
# 7 is picked to ensure cohort is not a simple cohort
table = data.frame()
currentFile = fileList[2]

for (currentFilenum in 1:length(fileList)){
  metFlag = 0
  obesityflag = 0
  Genderflag = 0
  Ageflag = 0
  currentFile = fileList[currentFilenum]
  #print(currentFile)
  jsonData = fromJSON(paste0(filePath,'/',currentFile))
  # print(paste0(list(jsonData$InclusionRules$expression$Type[1:8]),'     ',currentFile))
   #if ((jsonData$InclusionRules$expression$Type[7]) == 'ALL'){ obesityflag = 1}
  # if(InclusionRules$expression$DemographicCriteriaList[[5]]=="Gender"
  if(length(jsonData$InclusionRules$expression$CriteriaList[[5]]$Criteria$DrugEra$CodesetId) == 2){metFlag = 1}
  if(length(jsonData$InclusionRules$expression$CriteriaList) == 7
     & length(jsonData$InclusionRules$expression$DemographicCriteriaList[[5]]) == 0 ) {obesityflag = 1}
    if (length(jsonData$InclusionRules$expression$DemographicCriteriaList[[5]]) != 0){
      if(length(jsonData$InclusionRules$expression$DemographicCriteriaList[[5]]$Gender) !=0) {
       # Genderflag = jsonData$InclusionRules$expression$DemographicCriteriaList[[5]]$Gender[[1]]$CONCEPT_NAME}
      Genderflag = 1}
      if(length(jsonData$InclusionRules$expression$DemographicCriteriaList[[5]]$Age) !=0) {Ageflag = 1}
  }
  print(paste0(currentFile,
               ' m ',metFlag,
               ' a ',Ageflag,
               ' g ',Genderflag,
               ' o ',obesityflag))
               
}
  
# The below can take 2 cohort numbers and compare to get the differences.   

# Check for diffs between 2 jsons
getwd()
filePath = "inst/cohorts/class"
fileList <- list.files(path = filePath)
cohort1 = 1010000
cohort2 = 1011000
cohort3 = 1010030
path = paste0(filePath,'/ID',cohort1,'.json')
json_file_path <- path

# Use fromJSON() to read the JSON file into an R object
json1 =  fromJSON(paste0(filePath,'/ID',cohort1,'.json'))
json2 =  fromJSON(paste0(filePath,'/ID',cohort2,'.json'))
json3 =  fromJSON(paste0(filePath,'/ID',cohort3,'.json'))
summary(json1)
differences <- waldo::compare(json1, json2)
differences

