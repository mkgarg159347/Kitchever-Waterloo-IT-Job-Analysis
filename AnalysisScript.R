###############################################################################################
#Step2: Now we will perform the descriptive and exploratory analysis on the data
###############################################################################################

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")
if(!require(readxl)){install.packages("readxl")}
library("readxl")
if(!require(readr)){install.packages("readr")}
library("readr")
if(!require(tidyverse)){install.packages("tidyverse")}
library("tidyverse")
if(!require(modelr)){install.packages("modelr")}
library(modelr)
if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
library("RColorBrewer")
if(!require(sqldf)){install.packages("sqldf")}
library("sqldf") 
if(!require(gclus)){install.packages("gclus")}
library("gclus")

if(!is.null(dev.list())) dev.off()      # Clear plots
rm(list=ls())                           # Clean workspace
cat("\014")                             # Clear console

#set working directory
setwd("E:/Conestoga2019/Prog8430DataAnalysis/final/Data")

###############################################################################################
#Read files

JobPosting <- read_csv('processed_JobPosting.csv')
list(JobPosting)

LMI <- read_csv('processed_LMI.csv')
list(LMI)

ProgToNoc <- read_excel('EMSI Data - Labour Market.xlsx', sheet = 3)
list(ProgToNoc)

SurveyOutcome <- read_csv('Employment Outcomes 2018.csv')
head(SurveyOutcome)
str(SurveyOutcome)

#changing names for survey data frame
names(SurveyOutcome) <- c( "ProgramCode", "ProgramName", "MTCU", "TotalGrads", "FurtherEducation", "SeekingJob", "NotSeekingJob", 
                           "StatusUnknown", "AvailableforJob", "WorkingFT_ProgramRelated", "WorkingPT_ProgramRelated", 
                           "WorkingFT_ProgramUnrelated", "WorkingPT_ProgramUnrelated", "AverageSalaries_FT_R", "SalariesReported_FT_R" )

#convert salary to numeric
SurveyOutcome$AverageSalaries_FT_R <- as.numeric(gsub('[$,]', '', SurveyOutcome$AverageSalaries_FT_R))
print(head(SurveyOutcome$AverageSalaries_FT_R ))
###############################################################################################
#Read files

JobPosting <- read_csv('processed_JobPosting.csv')
list(JobPosting)

LMI <- read_csv('processed_LMI.csv')
list(LMI)

ProgToNoc <- read_excel('EMSI Data - Labour Market.xlsx', sheet = 3)
list(ProgToNoc)

SurveyOutcome <- read_csv('Employment Outcomes 2018.csv')
head(SurveyOutcome)
str(SurveyOutcome)

#changing names for survey data frame
names(SurveyOutcome) <- c( "ProgramCode", "ProgramName", "MTCU", "TotalGrads", "FurtherEducation", "SeekingJob", "NotSeekingJob", 
                           "StatusUnknown", "AvailableforJob", "WorkingFT_ProgramRelated", "WorkingPT_ProgramRelated", 
                           "WorkingFT_ProgramUnrelated", "WorkingPT_ProgramUnrelated", "AverageSalaries_FT_R", "SalariesReported_FT_R" )

#convert salary to numeric
SurveyOutcome$AverageSalaries_FT_R <- as.numeric(gsub('[$,]', '', SurveyOutcome$AverageSalaries_FT_R))
print(head(SurveyOutcome$AverageSalaries_FT_R ))

###############################################################################################
#Trend in IT sector job market since 2001

listOfITNoc <- c('0213', '2147', '5241', '2171', '2172', '2173', '2174', '2175', '2281', '2282', '2283')

IT_NOC_LMI <- LMI[LMI$NOC %in% listOfITNoc,]
list(IT_NOC_LMI)

print("NOC categrories, which are the focus in following analysis")
unique(IT_NOC_LMI$Description)

summary(IT_NOC_LMI)


#plot the line graph to analyse the job market in local, provincial and national levels
ggplot(IT_NOC_LMI, aes(Year,Local, group = Description, color=Description)) +
  xlab("Year") +
  ylab("Number of Jobs") +
  ggtitle('Trend in local job market since 2001- IT jobs only') + 
  scale_colour_brewer(palette="Paired") +
  geom_line(size = 1.5)

ggplot(IT_NOC_LMI, aes(Year,Provincial, group = Description, color=Description)) +
  xlab("Year") +
  ylab("Number of Jobs") +
  ggtitle('Trend in Provincial job market since 2001- IT jobs only') + 
  scale_colour_brewer(palette="Paired") +
  geom_line(size = 1.5)

ggplot(IT_NOC_LMI, aes(Year,National, group = Description, color=Description))  + 
  xlab("Year") +
  ylab("Number of Jobs") +
  ggtitle('Trend in National job market since 2001- IT jobs only') + 
  scale_colour_brewer(palette="Paired") +
  geom_line(size = 1.5)

###############################################################################################
#Programs offered by Conestoga College related to the NOC in IT sector
#According to the survey, give the number of students graduating from these programs

IT_Programs <- ProgToNoc[ProgToNoc$NOC %in% listOfITNoc,]
IT_Programs
names(IT_Programs) <- c( "ProgramCode", "ProgramTitle", "NOC", "NOCTitle")

print("Total number of Programs in Conestoga College offers in IT sector: ")
total_programs = count(IT_Programs)
print(total_programs)

#programs with coop
indexs_Coop <- grep("C", IT_Programs$ProgramCode)
IT_Programs_coop <- IT_Programs[indexs_Coop,]
print("Number of Programs with coop: ")
coop_programs <- count(IT_Programs_coop)
print(coop_programs)

print("Programs with optional Coop: ")
unique_programs <- total_programs - coop_programs
print(unique_programs)

#Analyse the survey for IT programs
IT_survey <- subset(SurveyOutcome, SurveyOutcome$ProgramCode %in% IT_Programs$ProgramCode)
str(IT_survey)
summary(IT_survey)

ggplot(IT_survey, aes(ProgramCode,TotalGrads, fill=TotalGrads)) + 
  geom_bar(stat="identity") + 
  xlab("IT Program Code") +
  ylab("Number of Graduates") +
  ggtitle("Graduates from IT Program in Conestoga College, Year 2018")

ITgraduates <- sum(IT_survey$TotalGrads)
print("Number of students graduation in IT sector in year 2018: ")
print(ITgraduates)

ITgraduates_max <- subset(IT_survey, IT_survey$TotalGrads>=30 )
print("Programs with more than or equal to 30 students: ")
print(ITgraduates_max$ProgramName)

###############################################################################################
#Jobs secured by Conestoga graduates in year 2018, related to their program

FT_relatedJob <- sum(IT_survey$WorkingFT_ProgramRelated, na.rm=TRUE )
PT_relatedJob <- sum(IT_survey$WorkingPT_ProgramRelated, na.rm=TRUE )
TotalJobs_RP <- FT_relatedJob + PT_relatedJob

print("Total jobs secured by Conestoga graduates related to their program(full time + part time) in year 2018")
print(TotalJobs_RP)

JobGraduateRatio <- (TotalJobs_RP/ITgraduates) *100
print("Ratio of job secured in related program to number of graduates")
print(JobGraduateRatio)

# Get the stacked barplot
color <- c('grey','green','blue')
barplot(t(IT_survey[c('TotalGrads','WorkingFT_ProgramRelated', 'WorkingPT_ProgramRelated')]), 
  border="white", space=0.05,
  cex.names=0.75,
  xlab="Program Code", 
  ylab="Number of graduates", 
  main = "Graduates from Conestoga working in full time and part time jobs in related programs",
  names.arg=IT_survey$ProgramCode,
  col = color,
  legend.text=c('TotalGrads','WorkingFT_ProgramRelated','WorkingPT_ProgramRelated'),
  args.legend=list(x = 'topleft',text.col = color,col = color,border= color,bty='n'))


###############################################################################################
# IT Program in Conestoga having high job oppertunity

IT_survey$WorkingPT_ProgramRelated[is.na(IT_survey$WorkingPT_ProgramRelated)] <- 0
IT_survey$WorkingRatio <- ((IT_survey$WorkingFT_ProgramRelated + IT_survey$WorkingPT_ProgramRelated) /IT_survey$TotalGrads)

IT_survey$WorkingRatio[is.na(IT_survey$WorkingRatio)] <- 0
programs <- IT_survey[IT_survey$WorkingRatio >=0.40,]       #classifying the programs with 40% or more job

print("Programs with max job oppertunities by survey 2018")
programs[order(programs$WorkingRatio, decreasing = TRUE), c('ProgramCode', 'ProgramName', 'WorkingRatio')] 

ggplot(programs, aes(x= ProgramCode, fill=WorkingRatio )) + 
  geom_bar(aes(y = WorkingRatio), stat="identity") +
  geom_text(aes( label = scales::percent(WorkingRatio),y= WorkingRatio), stat= "identity", vjust = -.5) +
  labs(y = "Percent", fill= 'day') +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("IT Programs with max job oppertunities by survey 2018 in Conestoga")


###############################################################################################
#Analyzing IT Job's on monthly basis for the year 2018
all_jobs_2018 <- subset(JobPosting, Year == "2018")
head(all_jobs_2018)

all_it_jobs_2018 <- all_jobs_2018[all_jobs_2018$NOC %in% listOfITNoc,]
all_it_jobs_2018 <- all_it_jobs_2018[, -c(1,2, 3)]
head(all_it_jobs_2018)

#Check if any NA values
sum(is.na(all_it_jobs_2018))

#Summarize the data
summary(all_it_jobs_2018)

all_it_jobs_aggregate_2018 <- aggregate(. ~Month, data=all_it_jobs_2018, sum, na.rm=TRUE)
head(all_it_jobs_aggregate_2018)

all_it_jobs_aggregate_2018$Month <- factor(all_it_jobs_aggregate_2018$Month, month.abb, ordered=TRUE)
all_it_jobs_aggregate_2018 <- all_it_jobs_aggregate_2018[order(all_it_jobs_aggregate_2018$Month), ]

#Plot Local Job Data
plot(all_it_jobs_aggregate_2018$Local_All, type = "b", lwd=2, xaxt = "n", ylim = c(0,2500),
     col = "Black", xlab="month", ylab = "No. Of Posts", main = "Local Job Posting")
axis(1, at = 1:length(all_it_jobs_aggregate_2018$Month), labels = all_it_jobs_aggregate_2018$Month)
lines(all_it_jobs_aggregate_2018$Local_All, col="red", type = "b", lwd = 2)
lines(all_it_jobs_aggregate_2018$Local_Entry, col="blue", type = "b", lwd = 2)
legend(1, 2500, legend=c("All Level Jobs", "Entery Level Jobs"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#Plot Provincial Data
plot(all_it_jobs_aggregate_2018$Provincial_All, type = "b", lwd=2, xaxt = "n", 
     ylim = c(0,40000), col = "Black", xlab="month", ylab = "Job Posting",
     main = "Provincial job postings")
axis(1, at = 1:length(all_it_jobs_aggregate_2018$Month), labels = all_it_jobs_aggregate_2018$Month)
lines(all_it_jobs_aggregate_2018$Provincial_All, col="red", type = "b", lwd = 2)
lines(all_it_jobs_aggregate_2018$Provincial_Entry, col="blue", type = "b", lwd = 2)
legend(1, 40000, legend=c("All Level Jobs", "Entery Level Jobs"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


plot(all_it_jobs_aggregate_2018$National_All, type = "b", lwd=2, xaxt = "n",  ylim = c(0,90000), 
     col = "Black", xlab="month", ylab = "Job Posting", 
     main = "National job postings")
axis(1, at = 1:length(all_it_jobs_aggregate_2018$Month), labels = all_it_jobs_aggregate_2018$Month)
lines(all_it_jobs_aggregate_2018$National_All, col="red", type = "b", lwd = 2)
lines(all_it_jobs_aggregate_2018$National_Entry, col="blue", type = "b", lwd = 2)
legend(1, 90000, legend=c("All Level Jobs", "Entery Level Jobs"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

summary(df)

#Conclusion
# While Analyzing Local, Provincial and Natiaonal IT Jobs data, we find that there is a similar in all level i.e. Number of jobs increase at all level which peak in the month of number but decrease in Dec
# which is fair as most of the poeple do enjoy winte holidays.


###############################################################################################
#Analyze Graduates along with the Seeking a job and already working
#Only considering full time working graduates

IT_survey$SeekingJob[is.na(IT_survey$SeekingJob)] <- 0
IT_survey$WorkingFT_ProgramRelated[is.na(IT_survey$WorkingFT_ProgramRelated)] <- 0
IT_survey$WorkingPT_ProgramRelated[is.na(IT_survey$WorkingPT_ProgramRelated)] <- 0
IT_survey$WorkingFT_ProgramUnrelated[is.na(IT_survey$WorkingFT_ProgramUnrelated)] <- 0
IT_survey$WorkingPT_ProgramUnrelated[is.na(IT_survey$WorkingPT_ProgramUnrelated)] <- 0
IT_survey$Total_WorkingGrads <- IT_survey$WorkingFT_ProgramRelated + IT_survey$WorkingFT_ProgramUnrelated

plot(IT_survey$TotalGrads, type = "b", lwd=2, xaxt = "n", col = "Green", xlab="month", ylab = "Number of graduates",
     main = "Jobs Obtained By Conestoga Graduates")
axis(1, at = 1:length(IT_survey$ProgramCode), labels = IT_survey$ProgramCode)
lines(IT_survey$SeekingJob, col="red", type = "b", lwd = 2)
lines(IT_survey$Total_WorkingGrads, col="blue", type = "b", lwd = 2)
legend(1, 50, legend=c("Total Graduates", "Still Seeking A Job", "Job Obtained"),
       col=c("green", "red", "blue"), lty=1:2, cex=0.8)
#Conclusion
#From the graph we could precieve that graduates who studied 1415G and 1416G program are more likely to get jobs
#Also to note down that for program 1186, there are more number of people who are still seeking job which would draw attention towards the program
#structure. It could be that program outline is old now or it is not in demand in the current job market. 
#Also it is observed that program 1218 which has the highest number of graduates has very low jobs obtained by them. it could be that there are more students than demand
# It is worth to watch the program outline and what type of jobs from this program can a graduate capture

t.test(IT_survey$TotalGrads, IT_survey$Total_WorkingGrads)
# We used T-test for total Graduates and Total working Grads to analyse if Graduate have high possibility of getting a job 
# But from the t-test, we found that Graduating does not gurantee in job

###############################################################################################
# Are there enough IT jobs for IT graduates from Conestoga
totalGrad <- sum(IT_survey$TotalGrads)
totalLocalJobAllLevel <- sum(all_it_jobs_aggregate_2018$Local_All)
totalLocalJobEnterLevel <- sum(all_it_jobs_aggregate_2018$Local_Entry)
details <- c(totalGrad,totalLocalJobEnterLevel, totalLocalJobAllLevel)
print(paste0("total number of graduates in 2018", totalGrad))
print(paste0("total number of IT jobs for entry level in 2018", totalLocalJobEnterLevel))
print(paste0("total number of IT jobs for all level in 2018", totalLocalJobAllLevel))
barplot(details, col=c("darkblue","darkorange", "darkred" ), main = "No. of Graduates Vs Jobs In 2018", ylab = "Total", names.arg = c("Graduates", "Local Entry Level", "Local All Level Jobs"))

#From the analysis, we found that there are many jobs in IT sector

###############################################################################################
# Jobs on all level
totalLocalJobAllLevel <- sum(all_it_jobs_aggregate_2018$Local_All)
totalNationalJobAllLevel <- sum(all_it_jobs_aggregate_2018$National_All)
totalProvincialJobAllLevel <- sum(all_it_jobs_aggregate_2018$Provincial_All)

details <- c(totalLocalJobAllLevel, totalProvincialJobAllLevel, totalNationalJobAllLevel)
print(paste0("total number of IT jobs in Local level in 2018: ", totalLocalJobAllLevel))
print(paste0("total number of IT jobs in provincial level in 2018: ", totalProvincialJobAllLevel))
print(paste0("total number of IT jobs in National level in 2018: ", totalNationalJobAllLevel))
barplot(details, col=c("darkblue","darkorange", "darkred" ), main = "Total Number of Local At All Level 2018", ylab = "Total", names.arg = c("Local", "Provincial", "National"))

# we observed that Ontario has almost 50% job of IT jobs
# which is a good sign for Conestoga Students
# In the waterloo region, it only shows almost 7% of the IT jobs. However, as the our knowledge, there should be high percentage of the IT jobs. 
# Which leads to the further investigation into others important regions.
