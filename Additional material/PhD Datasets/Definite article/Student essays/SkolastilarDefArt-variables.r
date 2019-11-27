
skolastilarDefArt <- read.csv(file = "SuffArt.csv", sep = "\t", quote = "\"")[ ,c('NewStudID', 'YEAR', 'GRADE', 'SCORE', 'VAR', 'VARTYPE', 'VALIDITY', 'FILE')]


# Select cases: Only select "valid" cases
skolastilarDefArtVal1 <- subset(skolastilarDefArt, VALIDITY == '1')

# Overwrite
skolastilarDefArt <- skolastilarDefArtVal1

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)

table(skolastilarDefArt$VARTYPE)
skolastilarDefArt$VARTYPE = as.character(skolastilarDefArt$VARTYPE)
skolastilarDefArt$VARTYPE[skolastilarDefArt$VARTYPE=="Adjectival"] <- "DET-ADJ"
#skolastilarDefArt$VARTYPE[skolastilarDefArt$VARTYPE=="adjectival"] <- "DET-ADJ"
skolastilarDefArt$VARTYPE[skolastilarDefArt$VARTYPE=="Adnominal"] <- "DET-ADJ-NOM"
table(skolastilarDefArt$VARTYPE)

skolastilarDefArt$YEAR <- as.numeric(as.character(skolastilarDefArt$YEAR))

# Teacher periods
skolastilarDefArt$TEACHERS <- cut(skolastilarDefArt$YEAR, breaks=c(1850,1880,1895,1910), labels = FALSE)
str(skolastilarDefArt$TEACHERS)

# Periods
skolastilarDefArt$PERIOD <- cut(skolastilarDefArt$YEAR, breaks=c(1850,1864,1874,1884,1894,1900,1910), labels = FALSE)

skolastilarDefArt$GRADE4 = as.character(skolastilarDefArt$GRADE)
skolastilarDefArt$GRADE6 = as.character(skolastilarDefArt$GRADE)
skolastilarDefArt$SCOREcont = as.character(skolastilarDefArt$SCORE)


# Grades 
skolastilarDefArt$GRADE = as.character(skolastilarDefArt$GRADE)
skolastilarDefArt$GRADE[skolastilarDefArt$GRADE=="1"] <- "Grades 1-3"
skolastilarDefArt$GRADE[skolastilarDefArt$GRADE=="2"] <- "Grades 1-3"
skolastilarDefArt$GRADE[skolastilarDefArt$GRADE=="3"] <- "Grades 1-3"
skolastilarDefArt$GRADE[skolastilarDefArt$GRADE=="4"] <- "Grades 4-6"
skolastilarDefArt$GRADE[skolastilarDefArt$GRADE=="5"] <- "Grades 4-6"
skolastilarDefArt$GRADE[skolastilarDefArt$GRADE=="6"] <- "Grades 4-6"

# Grades 
skolastilarDefArt$GRADE4 = as.character(skolastilarDefArt$GRADE4)
skolastilarDefArt$GRADE4[skolastilarDefArt$GRADE4=="1"] <- "1"
skolastilarDefArt$GRADE4[skolastilarDefArt$GRADE4=="2"] <- "2"
skolastilarDefArt$GRADE4[skolastilarDefArt$GRADE4=="3"] <- "3"
skolastilarDefArt$GRADE4[skolastilarDefArt$GRADE4=="4"] <- "4"
skolastilarDefArt$GRADE4[skolastilarDefArt$GRADE4=="5"] <- "4"
skolastilarDefArt$GRADE4[skolastilarDefArt$GRADE4=="6"] <- "4"

# Grades 
skolastilarDefArt$GRADE6 = as.character(skolastilarDefArt$GRADE6)
skolastilarDefArt$GRADE6[skolastilarDefArt$GRADE6=="1"] <- "1"
skolastilarDefArt$GRADE6[skolastilarDefArt$GRADE6=="2"] <- "2"
skolastilarDefArt$GRADE6[skolastilarDefArt$GRADE6=="3"] <- "3"
skolastilarDefArt$GRADE6[skolastilarDefArt$GRADE6=="4"] <- "4"
skolastilarDefArt$GRADE6[skolastilarDefArt$GRADE6=="5"] <- "5"
skolastilarDefArt$GRADE6[skolastilarDefArt$GRADE6=="6"] <- "6"

skolastilarDefArt$PHRASE.TYPE  =  skolastilarDefArt$VARTYPE 


skolastilarDefArt$SCORE = as.character(skolastilarDefArt$SCORE)
skolastilarDefArt$SCORE[skolastilarDefArt$SCORE=="Excellent"] <- "HIGH"
skolastilarDefArt$SCORE[skolastilarDefArt$SCORE=="First"] <- "HIGH"
skolastilarDefArt$SCORE[skolastilarDefArt$SCORE=="Second"] <- "LOW"
skolastilarDefArt$SCORE[skolastilarDefArt$SCORE=="Third"] <- "LOW"
skolastilarDefArt$SCORE[skolastilarDefArt$SCORE=="None"] <- "NONE" # XDropout
skolastilarDefArt$SCORE[skolastilarDefArt$SCORE=="DDS"] <- "NONE" # XDropout

skolastilarDefArt$SCOREcont = as.character(skolastilarDefArt$SCOREcont)
skolastilarDefArt$SCOREcont[skolastilarDefArt$SCOREcont=="Excellent"] <- "0"
skolastilarDefArt$SCOREcont[skolastilarDefArt$SCOREcont=="First"] <- "1"
skolastilarDefArt$SCOREcont[skolastilarDefArt$SCOREcont=="Second"] <- "2"
skolastilarDefArt$SCOREcont[skolastilarDefArt$SCOREcont=="Third"] <- "3"
skolastilarDefArt$SCOREcont[skolastilarDefArt$SCOREcont=="None"] <- "4" # XDropout
skolastilarDefArt$SCOREcont[skolastilarDefArt$SCOREcont=="DDS"] <- "4" # XDropout



