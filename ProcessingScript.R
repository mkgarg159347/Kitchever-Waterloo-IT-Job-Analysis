
###############################################################################################
#Step1: This script generates 2 csv files, processed_JobPosting, processed_LMI 
#which converts pivot table to data table.
#These files are provided with in the data folder. But one can generate it again.
#The scrpit will just update the same files again unless the file names are changed.
###############################################################################################

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")
if(!require(readxl)){install.packages("readxl")}
library("readxl")
if(!require(readr)){install.packages("readr")}
library("readr")

if(!is.null(dev.list())) dev.off()      # Clear plots
rm(list=ls())                           # Clean workspace
cat("\014")                             # Clear console


#set working directory
setwd("E:/Conestoga2019/Prog8430DataAnalysis/final/Data")


###############################################################################################
#Importing files 

#Read excel file EMSI Data - Labour Market
LMI <- read_excel('EMSI Data - Labour Market.xlsx', sheet = 1)
JobPosting <- read_excel('EMSI Data - Labour Market.xlsx', sheet = 2)
###############################################################################################
#                                                                                             #
#                     Processing Pivot table to data table                                    #
#                               JobPosting table                                              #
###############################################################################################

processed_JobPosting<-data.frame('NOC'=character(0), 
                                  'Occupation'=character(0),
                                  'year'=integer(0),
                                  'month'=character(0),
                                  'Local_Entry'=integer(0), 
                                  'Local_All'=integer(0),
                                  'Provincial_Entry'=integer(0),
                                  'Provincial_All'=integer(0),
                                  'National_Entry'=integer(0),
                                  'National_All'=integer(0)
                                  )

###############################################################################################

#Populate the Local_All values to processed_JobPosting data frame
for(cname in names(JobPosting[4:19])){
  columnName <- strsplit(cname, "\ ")
  month <- as.character(columnName[[1]][1])
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(JobPosting)){
    noc = as.character(JobPosting[row, 'NOC'])
    occupation = as.character(JobPosting[row,'Occupation'])
    value = as.integer(JobPosting[row, cname])
    dummy<-data.frame('NOC'=noc, 
                      'Occupation'=occupation, 
                      'Year'=year, 
                      'Month'=month,
                      'Local_All'=value,
                      'Local_Entry'=NA, 
                      'Provincial_All'=NA,
                      'Provincial_Entry'=NA,
                      'National_All'=NA,
                      'National_Entry'=NA
                      )
    processed_JobPosting<-rbind(processed_JobPosting, dummy)
    rm(dummy)
  }
}
rm(columnName, row, cname, noc,occupation, month, year, value)

###############################################################################################

#Populate the values to Local Entry level jobs
for(cname in names(JobPosting[21:36])){
  columnName <- strsplit(cname, "\ ")
  month <- as.character(columnName[[1]][1])
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(JobPosting)){
    noc = as.character(JobPosting[row, 'NOC'])
    value = as.integer(JobPosting[row, cname])
    index<-which(processed_JobPosting$NOC == noc & processed_JobPosting$Year == year & processed_JobPosting$Month == month)
    processed_JobPosting[[index, 'Local_Entry']]<-value
  }
}
rm(index, columnName, cname, row, value, month, year, noc)

###############################################################################################

#Populate the values to Provincial All level jobs
for(cname in names(JobPosting[38:53])){
  columnName <- strsplit(cname, "\ ")
  month <- as.character(columnName[[1]][1])
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(JobPosting)){
    noc = as.character(JobPosting[row, 'NOC'])
    value = as.integer(JobPosting[row, cname])
    index<-which(processed_JobPosting$NOC == noc & processed_JobPosting$Year == year & processed_JobPosting$Month == month)
    processed_JobPosting[[index, 'Provincial_All']]<-value
  }
}
rm(index, columnName, cname, row, value, month, year)

###############################################################################################

#Populate the values to Provincial Entry Level jobs
for(cname in names(JobPosting[55:70])){
  columnName <- strsplit(cname, "\ ")
  month <- as.character(columnName[[1]][1])
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(JobPosting)){
    noc = as.character(JobPosting[row, 'NOC'])
    value = as.integer(JobPosting[row, cname])
    index<-which(processed_JobPosting$NOC == noc & processed_JobPosting$Year == year & processed_JobPosting$Month == month)
    processed_JobPosting[[index, 'Provincial_Entry']]<-value
  }
}
rm(index, columnName, cname, row, value, month, year)

###############################################################################################

#Populate the values to National All Level Jobs
for(cname in names(JobPosting[72:87])){
  columnName <- strsplit(cname, "\ ")
  month <- as.character(columnName[[1]][1])
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(JobPosting)){
    noc = as.character(JobPosting[row, 'NOC'])
    value = as.integer(JobPosting[row, cname])
    index<-which(processed_JobPosting$NOC == noc & processed_JobPosting$Year == year & processed_JobPosting$Month == month)
    processed_JobPosting[[index, 'National_All']]<-value
  }
}
rm(index, columnName, cname, row, value, month, year)

###############################################################################################

#Populate the values to National Entry Level jobs
for(cname in names(JobPosting[89:104])){
  columnName <- strsplit(cname, "\ ")
  month <- as.character(columnName[[1]][1])
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(JobPosting)){
    noc = as.character(JobPosting[row, 'NOC'])
    value = as.integer(JobPosting[row, cname])
    index<-which(processed_JobPosting$NOC == noc & processed_JobPosting$Year == year & processed_JobPosting$Month == month)
    processed_JobPosting[[index, 'National_Entry']]<-value
  }
}
rm(index, columnName, cname, row, value, month, year)

###############################################################################################

# Write processed_JobPosting to CSV 
processed_JobPosting <- processed_JobPosting[with(processed_JobPosting, order(NOC)), ]
head(processed_JobPosting)
write.csv(processed_JobPosting, file = "processed_JobPosting.csv", row.names=FALSE)


###############################################################################################
#                                                                                             #
#                     Processing Pivot table to data table                                    #
#                                   LMI table                                                 #
###############################################################################################
#create new data frame
processed_LMI<-data.frame('NOC'=character(0), 
                                 'Description'=character(0),
                                 'Year'=integer(0),
                                 'Local'=integer(0),
                                 'Provincial'=integer(0),
                                 'National'=integer(0)
                                  )
###############################################################################################

#Populate the Local values to processed_LMI data frame
for(cname in names(LMI[3:20])){
  columnName <- strsplit(cname, "\ ")
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(LMI)){
    noc = as.character(LMI[row, 'NOC'])
    occupation = as.character(LMI[row,'Description'])
    value = as.integer(LMI[row, cname])
    dummy<-data.frame('NOC'=noc, 
                      'Description'=occupation, 
                      'Year'=year, 
                      'Local'= value,
                      'Provincial'= NA,
                      'National'= NA
    )
    processed_LMI<-rbind(processed_LMI, dummy)
  }
}
rm(dummy, columnName, year, row, cname, noc, occupation, value)

###############################################################################################

#Populate the Values to Provincial
for(cname in names(LMI[21:38])){
  columnName <- strsplit(cname, "\ ")
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(LMI)){
    noc = as.character(LMI[row, 'NOC'])
    value = as.integer(LMI[row, cname])
    index<-which(processed_LMI$NOC == noc & processed_LMI$Year == year)
    processed_LMI[[index, 'Provincial']]<-value
  }
}
rm(columnName, cname, row, value, year, noc, index)

###############################################################################################

#Populate the Values to National
for(cname in names(LMI[39:56])){
  columnName <- strsplit(cname, "\ ")
  year <- as.integer(columnName[[1]][2])
  
  for(row in 1:nrow(LMI)){
    noc = as.character(LMI[row, 'NOC'])
    value = as.integer(LMI[row, cname])
    index<-which(processed_LMI$NOC == noc & processed_LMI$Year == year)
    processed_LMI[[index, 'National']]<-value
  }
}
rm(columnName, cname, row, value, year, noc, index)

###############################################################################################
# Write processed_LMI to CSV 
processed_LMI <- processed_LMI[with(processed_LMI, order(NOC)), ]
head(processed_LMI)
write.csv(processed_LMI, file = "processed_LMI.csv", row.names=FALSE)

