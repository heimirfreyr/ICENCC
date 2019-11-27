
V3all <- read.csv(file = "V3-AllCorpora.csv", stringsAsFactors=FALSE, sep = "\t", quote = "\"", na.strings=c("","NA"))[ ,c('CORPUSSPEC', 'CORPUSGEN', 'ENV', 'YEAR', 'YEARFIX', 'YEARTRI', 'RANKNR', 'DEFINITENESS', 'PRONOMINALITY', 'ORIGINREGION', 'PLACEREGION', 'HEYCOCK', 'CLASS', 'VFINTYPE', 'MODAL', 'ADVERB', 'CPTYPE', 'NEG', 'SEKS', 'INDIVIDUAL')]


library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)


V3all$YEAR = as.numeric(as.character(V3all$YEAR))
V3all$YEARRAW = as.numeric(as.character(V3all$YEAR))

V3all$DECADE = V3all$YEAR
V3all$YEAR <- cut(V3all$YEAR, breaks=c(1780,1799,1824,1849,1874,1899,1925), labels = FALSE)
V3all$DECADE <- cut(V3all$DECADE, breaks=c(1780,1789,1799,1809,1819,1829,1839,1849,1859,1869,1879,1889,1899,1909,1919,1925), labels = FALSE)


V3all$DEFINITENESS[V3all$DEFINITENESS=="DEFINITE"] <- "DEF"
V3all$DEFINITENESS[V3all$DEFINITENESS=="INDEFINITE"] <- "INDEF"

V3all$CLASS[V3all$CLASS=="0"] <- "OTHER"
V3all$CLASS[V3all$CLASS=="2"] <- "OTHER"
V3all$CLASS[V3all$CLASS=="A"] <- "ABE"
V3all$CLASS[V3all$CLASS=="B"] <- "ABE"
V3all$CLASS[V3all$CLASS=="C"] <- "CD"
V3all$CLASS[V3all$CLASS=="D"] <- "CD"
V3all$CLASS[V3all$CLASS=="E"] <- "ABE"
V3all$CLASS[V3all$CLASS=="E/Ass."] <- "ABE"
V3all$CLASS[V3all$CLASS=="O"] <- "OTHER"
V3all$CLASS[V3all$CLASS=="OTHER"] <- "OTHER"

V3all$HEYCOCK.CLASS = V3all$HEYCOCK
# Recode HEYCOCK and CPTYPE into three-way vars
#V3all$HEYCOCK[V3all$HEYCOCK=="Decl" & V3all$CLASS=="ABE"] <- "V2"
#V3all$HEYCOCK[V3all$HEYCOCK=="Decl" & V3all$CLASS=="CD"] <- "NON-V2"
#V3all$HEYCOCK[V3all$HEYCOCK=="Decl" & V3all$CLASS=="OTHER"] <- "MIXED"
V3all$HEYCOCK[V3all$HEYCOCK=="Decl"] <- "V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Result"] <- "V2"
V3all$HEYCOCK[V3all$HEYCOCK=="ConsDeg"] <- "V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Cause"] <- "V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Adv" & V3all$CPSUBTYPE=="CMP"] <- "NON-V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Adv"] <- "MIXED"
V3all$HEYCOCK[V3all$HEYCOCK=="Purpose"] <- "MIXED" #"Typical Non-V2" #"MIXED"
V3all$HEYCOCK[V3all$HEYCOCK=="IndQu"] <- "NON-V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Rel"] <- "NON-V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Cond"] <- "NON-V2"
V3all$HEYCOCK[V3all$HEYCOCK=="Cmp"] <- "NON-V2"

V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Decl" & V3all$CLASS=="ABE"] <- "V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Decl" & V3all$CLASS=="CD"] <- "NON-V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Decl" & V3all$CLASS=="OTHER"] <- "MIXED"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Decl"] <- "V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Result"] <- "V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="ConsDeg"] <- "V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Cause"] <- "V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Adv" & V3all$CPSUBTYPE=="CMP"] <- "NON-V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Adv"] <- "MIXED"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Purpose"] <- "MIXED" #"Typical Non-V2" #"MIXED"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="IndQu"] <- "NON-V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Rel"] <- "NON-V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Cond"] <- "NON-V2"
V3all$HEYCOCK.CLASS[V3all$HEYCOCK.CLASS=="Cmp"] <- "NON-V2"

V3all$SEX = V3all$SEKS 
V3all$SEX[V3all$SEX=="karl"] <- "MALE"
V3all$SEX[V3all$SEX=="kona"] <- "FEMALE"

#V3all$SEX = V3all$SEX 
V3all$ORIGINREGION.2 = V3all$ORIGINREGION

table(V3all$ORIGINREGION)
V3all$ORIGINREGION[V3all$ORIGINREGION=="N"] <- "North"
V3all$ORIGINREGION[V3all$ORIGINREGION=="NE"] <- "North"
V3all$ORIGINREGION[V3all$ORIGINREGION=="NW"] <- "North"
V3all$ORIGINREGION[V3all$ORIGINREGION=="E"] <- "East"
V3all$ORIGINREGION[V3all$ORIGINREGION=="S"] <- "South"
V3all$ORIGINREGION[V3all$ORIGINREGION=="SW"] <- "Southwest"
V3all$ORIGINREGION[V3all$ORIGINREGION=="W"] <- "West"
V3all$ORIGINREGION[V3all$ORIGINREGION=="Wf."] <- "Westfjords"
V3all$ORIGINREGION[V3all$ORIGINREGION=="Wf"] <- "Westfjords"
V3all$ORIGINREGION[V3all$ORIGINREGION=="Westfjords"] <- "Westfjords"
V3all$ORIGINREGION[V3all$ORIGINREGION=="UNKNOWN"] <- "Unknown"

V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="NE"] <- "Rest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="NW"] <- "Rest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="E"] <- "Rest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="S"] <- "Rest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="SW"] <- "Southwest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="W"] <- "Rest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="Wf."] <- "Rest"
V3all$ORIGINREGION.2[V3all$ORIGINREGION.2=="UNKNOWN"] <- "Unknown"


V3all$PLACEREG2 = V3all$PLACEREGION

V3all$PLACEREGION[V3all$PLACEREGION=="Abroad-Other"] <- "ABROAD OTHER"
V3all$PLACEREGION[V3all$PLACEREGION=="Copenhagen"] <- "COPENHAGEN"
V3all$PLACEREGION[V3all$PLACEREGION=="E"] <- "EAST"
V3all$PLACEREGION[V3all$PLACEREGION=="NE"] <- "NORTH"
V3all$PLACEREGION[V3all$PLACEREGION=="North-America"] <- "ABROAD OTHER"
V3all$PLACEREGION[V3all$PLACEREGION=="NW"] <- "NORTH"
V3all$PLACEREGION[V3all$PLACEREGION=="S"] <- "SOUTH"
V3all$PLACEREGION[V3all$PLACEREGION=="SW"] <- "SOUTHWEST"
V3all$PLACEREGION[V3all$PLACEREGION=="W"] <- "WEST"
V3all$PLACEREGION[V3all$PLACEREGION=="Wf."] <- "WESTFJORDS"
V3all$PLACEREGION[V3all$PLACEREGION=="UNKNOWN"] <- NA

V3all$PLACEREG2[V3all$PLACEREG2=="Abroad-Other"] <- "ABROAD OTHER"
V3all$PLACEREG2[V3all$PLACEREG2=="Copenhagen"] <- "COPENHAGEN"
V3all$PLACEREG2[V3all$PLACEREG2=="E"] <- "EAST"
V3all$PLACEREG2[V3all$PLACEREG2=="NE"] <- "NORTH"
V3all$PLACEREG2[V3all$PLACEREG2=="North-America"] <- "ABROAD OTHER"
V3all$PLACEREG2[V3all$PLACEREG2=="NW"] <- "NORTH"
V3all$PLACEREG2[V3all$PLACEREG2=="S"] <- "SOUTH"
V3all$PLACEREG2[V3all$PLACEREG2=="SW"] <- "SOUTHWEST"
V3all$PLACEREG2[V3all$PLACEREG2=="W"] <- "WEST"
V3all$PLACEREG2[V3all$PLACEREG2=="Wf."] <- "WESTFJORDS"
V3all$PLACEREG2[V3all$PLACEREG2=="UNKNOWN"] <- "UNKNOWN"


V3all.letters = subset(V3all, CORPUSGEN=="PrivateLettersABC")


library(lme4)


V3all.letters.socsyn = V3all.letters

V3all.letters.socsyn$NEG[V3all.letters.socsyn$NEG==0] <- "SADV"
V3all.letters.socsyn$NEG[V3all.letters.socsyn$NEG==1] <- "NEG"

V3all.letters.socsyn$VFINTYPE[V3all.letters.socsyn$VFINTYPE=="aux"] <- "AUX"
V3all.letters.socsyn$VFINTYPE[V3all.letters.socsyn$VFINTYPE=="lex"] <- "LEX"

V3all.letters.socsyn$MUNUSKULU = V3all.letters.socsyn$MODAL
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="mega"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="munu"] <- "MUNUSKULU"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="skulu"] <- "MUNUSKULU"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="vilja"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="geta"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="eiga"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="fá"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="kunna"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="hljóta"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="verða"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="þurfa"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="ætla"] <- "MOD"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="aux"] <- "AUX"
V3all.letters.socsyn$MUNUSKULU[V3all.letters.socsyn$MUNUSKULU=="lex"] <- "LEX"

V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="mega"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="munu"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="skulu"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="vilja"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="geta"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="eiga"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="fá"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="kunna"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="hljóta"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="verða"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="þurfa"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="ætla"] <- "MODAL"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="aux"] <- "AUX"
V3all.letters.socsyn$MODAL[V3all.letters.socsyn$MODAL=="lex"] <- "LEX"


V3all.letters.socsyn$RANKNR3 =V3all.letters.socsyn$RANKNR
V3all.letters.socsyn$RANKNR2 =V3all.letters.socsyn$RANKNR
V3all.letters.socsyn$RANKNR0 =V3all.letters.socsyn$RANKNR

V3all.letters.socsyn$RANKNR[V3all.letters.socsyn$RANKNR=="HIGH"] <- "OFFICIALS/LETTERED"
V3all.letters.socsyn$RANKNR[V3all.letters.socsyn$RANKNR=="PEASANTS"] <- "PEASANTS/LABOURERS"
V3all.letters.socsyn$RANKNR[V3all.letters.socsyn$RANKNR=="LOW"] <- "PEASANTS/LABOURERS"
V3all.letters.socsyn$RANKNR[V3all.letters.socsyn$RANKNR=="MIDDLE"] <- "OTHER PROFESSIONS"
#V3all.letters.socsyn$RANKNR[V3all.letters.socsyn$RANKNR=="UNKNOWN"] <- NA


V3all.letters.socsyn$RANKNR0[V3all.letters.socsyn$RANKNR0=="HIGH"] <- "OFFICIALS/LETTERED"
V3all.letters.socsyn$RANKNR0[V3all.letters.socsyn$RANKNR0=="PEASANTS"] <- "PEASANTS/LABOURERS"
V3all.letters.socsyn$RANKNR0[V3all.letters.socsyn$RANKNR0=="LOW"] <- "PEASANTS/LABOURERS"
V3all.letters.socsyn$RANKNR0[V3all.letters.socsyn$RANKNR0=="MIDDLE"] <- "OTHER PROFESSIONS"
V3all.letters.socsyn$RANKNR0[V3all.letters.socsyn$RANKNR0=="UNKNOWN"] <- NA

V3all.letters.socsyn$RANKNR2[V3all.letters.socsyn$RANKNR2=="HIGH"] <- "OFFICIALS/LETTERED"
V3all.letters.socsyn$RANKNR2[V3all.letters.socsyn$RANKNR2=="PEASANTS"] <- "PEASANTS/LABOURERS"
V3all.letters.socsyn$RANKNR2[V3all.letters.socsyn$RANKNR2=="LOW"] <- "PEASANTS/LABOURERS"
V3all.letters.socsyn$RANKNR2[V3all.letters.socsyn$RANKNR2=="MIDDLE"] <- "OTHER PROFESSIONS"
V3all.letters.socsyn$RANKNR2[V3all.letters.socsyn$RANKNR2=="UNKNOWN"] <- NA

V3all.letters.socsyn$RANKNR3[V3all.letters.socsyn$RANKNR3=="HIGH"] <- "OFFICIALS/LETTERED"
V3all.letters.socsyn$RANKNR3[V3all.letters.socsyn$RANKNR3=="PEASANTS/LABOURERS"] <- "PEASANTS"
V3all.letters.socsyn$RANKNR3[V3all.letters.socsyn$RANKNR3=="LOW"] <- "LABOURERS"
V3all.letters.socsyn$RANKNR3[V3all.letters.socsyn$RANKNR3=="MIDDLE"] <- "OTHER PROFESSIONS"
V3all.letters.socsyn$RANKNR3[V3all.letters.socsyn$RANKNR3=="UNKNOWN"] <- NA



V3all.letters.socsyn$YEARTRI = as.numeric(V3all.letters.socsyn$YEARTRI)
V3all.letters.socsyn$ENV = as.factor(V3all.letters.socsyn$ENV)
V3all.letters.socsyn$YEARFIX = as.factor(V3all.letters.socsyn$YEARFIX)
V3all.letters.socsyn$RANKNR = as.factor(V3all.letters.socsyn$RANKNR)
V3all.letters.socsyn$RANKNR2 = as.factor(V3all.letters.socsyn$RANKNR2)
V3all.letters.socsyn$RANKNR3 = as.factor(V3all.letters.socsyn$RANKNR3)
V3all.letters.socsyn$DEFINITENESS = as.factor(V3all.letters.socsyn$DEFINITENESS)
V3all.letters.socsyn$PRONOMINALITY = as.factor(V3all.letters.socsyn$PRONOMINALITY)
V3all.letters.socsyn$ORIGINREGION = as.factor(V3all.letters.socsyn$ORIGINREGION)
V3all.letters.socsyn$PLACEREGION = as.factor(V3all.letters.socsyn$PLACEREGION)
V3all.letters.socsyn$PLACEREG2 = as.factor(V3all.letters.socsyn$PLACEREG2)
V3all.letters.socsyn$HEYCOCK = as.factor(V3all.letters.socsyn$HEYCOCK)
V3all.letters.socsyn$HEYCOCK.CLASS = as.factor(V3all.letters.socsyn$HEYCOCK.CLASS)
V3all.letters.socsyn$VFINTYPE = as.factor(V3all.letters.socsyn$VFINTYPE)
V3all.letters.socsyn$ADVEFB = as.factor(V3all.letters.socsyn$ADVERB)
V3all.letters.socsyn$MODAL = as.factor(V3all.letters.socsyn$MODAL)

V3all.letters.socsyn$RANKNR <- factor(V3all.letters.socsyn$RANKNR, level=c("OFFICIALS/LETTERED", "OTHER PROFESSIONS", "PEASANTS/LABOURERS", "UNKNOWN"))


V3all.letters.socsyn$VFINTYPE <- factor(V3all.letters.socsyn$VFINTYPE, level=c("AUX", "LEX"))
V3all.letters.socsyn$MODAL <- factor(V3all.letters.socsyn$MODAL, level=c("AUX", "MODAL", "LEX"))
V3all.letters.socsyn$ADVERB <- factor(V3all.letters.socsyn$ADVERB, level=c("NEG", "ALWAYS", "NEVER", "SELDOM", "SOMETIMES"))
V3all.letters.socsyn$MUNUSKULU <- factor(V3all.letters.socsyn$MUNUSKULU, level=c("AUX", "MUNUSKULU", "MOD", "LEX"))

#V3all.letters.socsyn$GENERATION = as.factor(V3all.letters.socsyn$GENERATION)
V3all.letters.socsyn$CPTYPE = as.factor(V3all.letters.socsyn$CPTYPE)
V3all.letters.socsyn$NEG = as.factor(V3all.letters.socsyn$NEG)
V3all.letters.socsyn$SEX = as.factor(V3all.letters.socsyn$SEX)
V3all.letters.socsyn$INDIVIDUAL = as.factor(V3all.letters.socsyn$INDIVIDUAL)
V3all.letters.socsyn$YEAR = as.factor(V3all.letters.socsyn$YEAR)

V3all.letters.socsyn$PERIOD = V3all.letters.socsyn$YEAR
V3all.letters.socsyn$PERIOD = as.numeric(V3all.letters.socsyn$PERIOD)

library(MASS)
V3all.letters.socsyn$PERIOD.contrasts <- V3all.letters.socsyn$PERIOD
V3all.letters.socsyn$PERIOD.contrasts <- as.factor(V3all.letters.socsyn$PERIOD.contrasts)
#str(V3all.letters.socsyn$PERIOD.contrasts)
contrasts(V3all.letters.socsyn$PERIOD.contrasts) <- contr.sdif(6) # the number of levels 

V3all.letters.socsyn$DECADE.contrasts <- V3all.letters.socsyn$DECADE
V3all.letters.socsyn$DECADE.contrasts <- as.factor(V3all.letters.socsyn$DECADE.contrasts)
#str(V3all.letters.socsyn$DECADE.contrasts)
contrasts(V3all.letters.socsyn$DECADE.contrasts) <- contr.sdif(14) # the number of levels 

V3all.letters.socsyn$DECADE = as.numeric(V3all.letters.socsyn$DECADE)

V3all.letters.socsyn$PLACEREGION <- factor(V3all.letters.socsyn$PLACEREGION, level=c("SOUTHWEST", "NORTH", "EAST", "SOUTH", "WEST", "WESTFJORDS", "DK", "ABROAD OTHER"))
V3all.letters.socsyn$PLACEREG2 <- factor(V3all.letters.socsyn$PLACEREG2, level=c("SOUTHWEST", "NORTH", "EAST", "SOUTH", "WEST", "WESTFJORDS", "COPENHAGEN", "ABROAD OTHER", "UNKNOWN"))


table(V3all.letters.socsyn$PLACEREG2)
V3all.letters.socsyn$ORIGINREGION.2 <- factor(V3all.letters.socsyn$ORIGINREGION.2, level=c("Southwest", "Rest"))
V3all.letters.socsyn$ORIGINREGION <- factor(V3all.letters.socsyn$ORIGINREGION, level=c("Southwest", "North", "West", "Westfjords", "East", "South", "Unknown"))
V3all.letters.socsyn$NEG <- factor(V3all.letters.socsyn$NEG, level=c("SADV", "NEG"))
V3all.letters.socsyn$YEARFIX <- factor(V3all.letters.socsyn$YEARFIX, level=c("Before 1850", "After 1850"))
V3all.letters.socsyn$HEYCOCK <- factor(V3all.letters.socsyn$HEYCOCK, level=c("V2", "NON-V2", "MIXED"))
V3all.letters.socsyn$SEX <- factor(V3all.letters.socsyn$SEX, level=c("MALE", "FEMALE"))

V3all.letters.socsyn$YEARRAW <- as.numeric(as.character(V3all.letters.socsyn$YEARRAW))


