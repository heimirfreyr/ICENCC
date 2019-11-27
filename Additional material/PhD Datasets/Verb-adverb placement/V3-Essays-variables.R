
skolastilarV3 <- read.csv(file = "/V3-AllCorpora.csv", stringsAsFactors=FALSE, sep = "\t", quote = "\"", na.strings=c("","NA"))[ ,c('ENV', 'SCORE', 'GRADE', 'NEG', 'YEAR', 'INDIVIDUAL', 'newIDstud',  'HEYCOCK' , 'CPTYPE', 'CORPUSGEN', 'FILE' )]

skolastilarV3 <- subset(skolastilarV3, CORPUSGEN == 'StudentEssays')
#skolastilarV3 <- subset(skolastilarV3, MEDSENTADV == '1')


skolastilarV3$ENV[skolastilarV3$ENV==2] <- "Vfin-Adv"
skolastilarV3$ENV[skolastilarV3$ENV==3] <- "Adv-Vfin"
#skolastilarV3$SCORE[skolastilarV3$SCORE=="gott"] <- "I"

skolastilarV3$SCORE = as.character(skolastilarV3$SCORE)
skolastilarV3$SCORE[skolastilarV3$SCORE==0] <- "HIGH"
skolastilarV3$SCORE[skolastilarV3$SCORE==1] <- "HIGH"
skolastilarV3$SCORE[skolastilarV3$SCORE==2] <- "LOW"
skolastilarV3$SCORE[skolastilarV3$SCORE==3] <- "LOW"
skolastilarV3$SCORE[skolastilarV3$SCORE=="None"] <- "NONE" # XDropout
skolastilarV3$SCORE[skolastilarV3$SCORE=="DDS"] <- "NONE" # XDropout

skolastilarV3$GRAD.SCORE = skolastilarV3$SCORE

skolastilarV3$YEAR = as.numeric(as.character(skolastilarV3$YEAR))
skolastilarV3$DECADES <- cut(skolastilarV3$YEAR, breaks=c(1850,1860,1870,1880,1890,1900,1910), labels = FALSE)
skolastilarV3$DECADE = skolastilarV3$DECADES
# Teacher periods
#skolastilarV3$TEACHERS <- cut(skolastilarV3$YEAR, breaks=c(1850,1881,1896,1910), labels = FALSE)
skolastilarV3$TEACHERS <- cut(skolastilarV3$YEAR, breaks=c(1850,1880,1895,1910), labels = FALSE)

# Recode grades into X groups
skolastilarV3$GRADE = as.character(skolastilarV3$GRADE)
skolastilarV3$GRADE[skolastilarV3$GRADE=="1"] <- "Group 1"
skolastilarV3$GRADE[skolastilarV3$GRADE=="2"] <- "Group 1"
skolastilarV3$GRADE[skolastilarV3$GRADE=="3"] <- "Group 1"
skolastilarV3$GRADE[skolastilarV3$GRADE=="4"] <- "Group 2"
skolastilarV3$GRADE[skolastilarV3$GRADE=="5"] <- "Group 2"
skolastilarV3$GRADE[skolastilarV3$GRADE=="6"] <- "Group 2"



skolastilarV3$CPTYPE = as.character(skolastilarV3$CPTYPE)
skolastilarV3$CLAUSE.TYPE = as.character(skolastilarV3$CPTYPE)

skolastilarV3$CLAUSE.TYPE[skolastilarV3$CLAUSE.TYPE=="ADV"] <- "MIXED"
skolastilarV3$CLAUSE.TYPE[skolastilarV3$CLAUSE.TYPE=="INT"] <- "NON-V2"
skolastilarV3$CLAUSE.TYPE[skolastilarV3$CLAUSE.TYPE=="REL"] <- "NON-V2"
skolastilarV3$CLAUSE.TYPE[skolastilarV3$CLAUSE.TYPE=="THT"] <- "V2"

skolastilarV3$HEYCOCK = as.character(skolastilarV3$HEYCOCK)
skolastilarV3$HEYCOCK2 = skolastilarV3$HEYCOCK

skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Adv"] <- "Mixed"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Cause"] <- "V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Cond"] <- "Non-V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="ConsDeg"] <- "V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Decl"] <- "V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="IndQu"] <- "Non-V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Purpose"] <- "Non-V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Rel"] <- "Non-V2"
skolastilarV3$HEYCOCK[skolastilarV3$HEYCOCK=="Result"] <- "V2"

skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Adv"] <- "Non-V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Cause"] <- "V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Cond"] <- "Non-V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="ConsDeg"] <- "V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Decl"] <- "V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="IndQu"] <- "Non-V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Purpose"] <- "Non-V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Rel"] <- "Non-V2"
skolastilarV3$HEYCOCK2[skolastilarV3$HEYCOCK2=="Result"] <- "V2"

#
#table(skolastilarV3$ENV,skolastilarV3$rankStFTypicalV3profile,skolastilarV3$CLAUSE.TYPE,skolastilarV3$SCORE)


skolastilarV3$newIDstud = as.factor(skolastilarV3$newIDstud)
skolastilarV3$GRADE = as.factor(skolastilarV3$GRADE)
skolastilarV3$GRAD.SCORE = as.factor(skolastilarV3$GRAD.SCORE)
#skolastilarV3$YEAR = as.numeric(skolastilarV3$YEAR)
skolastilarV3$ENV = factor(skolastilarV3$ENV, level=c("Vfin-Adv", "Adv-Vfin"))
# contrast coding of PERIOD / TEACHERS
library(MASS)
skolastilarV3$TEACHERS.contrasts <- skolastilarV3$TEACHERS
skolastilarV3$TEACHERS.contrasts <- as.factor(skolastilarV3$TEACHERS.contrasts)
#str(skolastilarV3$TEACHERS.contrasts)
contrasts(skolastilarV3$TEACHERS.contrasts) <- contr.sdif(3) # the number of levels 

skolastilarV3$TEACHERS = as.factor(skolastilarV3$TEACHERS)
skolastilarV3$CLAUSE.TYPE = factor(skolastilarV3$CLAUSE.TYPE, level = c("V2", "NON-V2", "MIXED"))
skolastilarV3$HEYCOCK = as.factor(skolastilarV3$HEYCOCK)
skolastilarV3$HEYCOCK <- factor(skolastilarV3$HEYCOCK, level=c("V2", "Mixed", "Non-V2")) 
skolastilarV3$HEYCOCK2 <- factor(skolastilarV3$HEYCOCK, level=c("V2", "Non-V2")) 
skolastilarV3$NEG <- factor(skolastilarV3$NEG, level=c("1", "0"))
skolastilarV3$INDIVIDUAL = skolastilarV3$newIDstud


