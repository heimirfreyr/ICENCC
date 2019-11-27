DefArt <- read.csv(file = "DefArt-AllCorp.csv", stringsAsFactors=FALSE, sep = "\t", quote = "\"", na.strings=c("","NA","#N/A"))[ ,c('VARIANT', 'INDIVIDUAL', 'GENDER', 'REL', 'YEAR', 'YEAR.FIX', 'BIRTHYEAR', 'STATUS', 'RANK.NR', 'ORIG.REG', 'PLACE.REG', 'ORIGIN.REGION', 'PLACE.REGION', 'CORPUS', 'PhraseType', 'NewCat', 'AdjDeg', 'YEAR.FIX', 'YEAR.TRI', 'CommonCat')]

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
DefArt$PERIOD = DefArt$YEAR
DefArt$PERIOD.test = DefArt$YEAR

DefArt$YEAR = as.numeric(as.character(DefArt$YEAR))
DefArt$PERIOD = as.numeric(as.character(DefArt$PERIOD))
DefArt$PERIOD.test = as.numeric(as.character(DefArt$PERIOD.test))

DefArt$DECADE = DefArt$PERIOD
DefArt$PERIOD <- cut(DefArt$PERIOD, breaks=c(1780,1824,1849,1874,1899,1925), labels = FALSE)
DefArt$PERIOD.test <- cut(DefArt$PERIOD.test, breaks=c(1800,1819,1839,1849,1859,1869,1879,1889,1899,1909,1919,1925), labels = FALSE)
DefArt$DECADE <- cut(DefArt$DECADE, breaks=c(1780,1789,1799,1809,1819,1829,1839,1849,1859,1869,1879,1889,1899,1909,1919,1925), labels = FALSE)
table(DefArt$VARIANT)
DefArt$VARIANT[DefArt$VARIANT=="INN"] <- "HINN"

DefArt$PhraseType[DefArt$PhraseType=="Adjectival"] <- "DET-ADJ"
DefArt$PhraseType[DefArt$PhraseType=="adjectival"] <- "DET-ADJ"
DefArt$PhraseType[DefArt$PhraseType=="nominal"] <- "DET-ADJ-NOM"

DefArt$RANK = DefArt$RANK.NR 

DefArt$RANK.NR[DefArt$RANK.NR=="LOW"] <- "Peasants/labourers"
DefArt$RANK.NR[DefArt$RANK.NR=="PEASANTS"] <- "Peasants/labourers"
DefArt$RANK.NR[DefArt$RANK.NR=="HIGH"] <- "Officials/lettered"
DefArt$RANK.NR[DefArt$RANK.NR=="MIDDLE"] <- "Other"
DefArt$RANK.NR[DefArt$RANK.NR=="UNKNOWN"] <- NA

DefArt$RANK[DefArt$RANK=="LOW"] <- "PEASANTS/LABOURERS"
DefArt$RANK[DefArt$RANK=="PEASANTS"] <- "PEASANTS/LABOURERS"
DefArt$RANK[DefArt$RANK=="HIGH"] <- "OFFICIALS/LETTERED"
DefArt$RANK[DefArt$RANK=="MIDDLE"] <- "OTHER PROFESSIONS"
DefArt$RANK[DefArt$RANK=="UNKNOWN"] <- NA # 

DefArt$REL[DefArt$REL=="1"] <- "RELATIVE"
DefArt$REL[DefArt$REL=="?1"] <- "RELATIVE"
DefArt$REL[DefArt$REL=="OVERFIVE"] <- "INDEPENDENT" # 
DefArt$REL[DefArt$REL=="0"] <- "INDEPENDENT" # 
DefArt$REL[DefArt$REL=="?0"] <- "INDEPENDENT" # 


DefArt$NewCat[DefArt$NewCat=="DATE"] <- "DATE"
DefArt$NewCat[DefArt$NewCat=="descriptive"] <- "DESC/OTHER"
DefArt$NewCat[DefArt$NewCat=="other"] <- "DESC/OTHER"
DefArt$NewCat[DefArt$NewCat=="nationality/origin"] <- "NAT/ORIGIN"
DefArt$NewCat[DefArt$NewCat=="ordinal"] <- "DESC/OTHER"
DefArt$NewCat[DefArt$NewCat=="relative/dimensional"] <- "DESC/OTHER"
DefArt$NewCat[DefArt$NewCat=="evaluative"] <- "EVAL"

DefArt$AdjDeg[DefArt$AdjDeg=="comparative"] <- "CMP"
DefArt$AdjDeg[DefArt$AdjDeg=="positive"] <- "POS"
DefArt$AdjDeg[DefArt$AdjDeg=="superlative"] <- "SUP"

DefArt$GENDER[DefArt$GENDER=="karl"] <- "MALE"
DefArt$GENDER[DefArt$GENDER=="kona"] <- "FEMALE"

DefArt$CommonCat[DefArt$CommonCat=="HELSTI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="MESTI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="MIKLI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="BESTI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="FAGRI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="GÓÐI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="RÉTTI"] <- NA
DefArt$CommonCat[DefArt$CommonCat=="FRÆGI"] <- "FRÆGI"
DefArt$CommonCat[DefArt$CommonCat=="FYRRNEFNDI"] <- "FYRRNEFNDI"
DefArt$CommonCat[DefArt$CommonCat=="NÚVERANDI"] <- NA

DefArt$CORPUS.GEN = DefArt$CORPUS

DefArt$CORPUS.GEN[DefArt$CORPUS.GEN=="Private Letters 19LCLV"] <- "Letters"
DefArt$CORPUS.GEN[DefArt$CORPUS.GEN=="Private Letters OCR 1800-1850"] <- "Letters"
DefArt$CORPUS.GEN[DefArt$CORPUS.GEN=="Private Letters C"] <- "Letters"


DefArt$GENERATION[DefArt$BIRTHYEAR<=1799] <- "Born before 1800"
DefArt$GENERATION[DefArt$BIRTHYEAR>=1800 & DefArt$BIRTHYEAR<=1824] <- "Born 1800-1824"
DefArt$GENERATION[DefArt$BIRTHYEAR>=1825 & DefArt$BIRTHYEAR<=1849] <- "Born 1825-1849"
DefArt$GENERATION[DefArt$BIRTHYEAR>=1850 & DefArt$BIRTHYEAR<=1874] <- "Born 1850-1874"
DefArt$GENERATION[DefArt$BIRTHYEAR>=1875] <- "Born 1875 or later"


DefArt$GENERATION = as.factor(DefArt$GENERATION)
DefArt$GENERATION = factor(DefArt$GENERATION, levels=c("Born before 1800", "Born 1800-1824", "Born 1825-1849", "Born 1850-1874", "Born 1875 or later"))


DefArt$ORIG.REG.ALT = DefArt$ORIG.REG 
DefArt$ORIG.REG.BI = DefArt$ORIG.REG
DefArt$ORIG.REG.BI.NS = DefArt$ORIG.REG
DefArt$ORIG.REG.BI.N = DefArt$ORIG.REG
DefArt$ORIG.REG.BI.DK =  DefArt$ORIG.REG

DefArt$ORIG.REG[DefArt$ORIG.REG=="N"] <- "North"
DefArt$ORIG.REG[DefArt$ORIG.REG=="NE"] <- "North"
DefArt$ORIG.REG[DefArt$ORIG.REG=="NW"] <- "North"
DefArt$ORIG.REG[DefArt$ORIG.REG=="E"] <- "East"
DefArt$ORIG.REG[DefArt$ORIG.REG=="S"] <- "South"
DefArt$ORIG.REG[DefArt$ORIG.REG=="SW"] <- "Southwest"
DefArt$ORIG.REG[DefArt$ORIG.REG=="W"] <- "West"
DefArt$ORIG.REG[DefArt$ORIG.REG=="Wf."] <- "Westfjords"
DefArt$ORIG.REG[DefArt$ORIG.REG=="UNKNOWN"] <- NA
DefArt$ORIG.REG[DefArt$ORIG.REG=="DK"] <- "Copenhagen"

DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="N"] <- "North"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="NE"] <- "North"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="NW"] <- "North"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="E"] <- "East"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="S"] <- "1South"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="SW"] <- "1South"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="W"] <- "West"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="Wf."] <- "West"
DefArt$ORIG.REG.ALT[DefArt$ORIG.REG.ALT=="UNKNOWN"] <- "Unknown"

DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="DK"] <- "COPENHAGEN"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="N"] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="NE"] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="NW"] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="E"] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="S"] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="SW"] <- "1Suðvesturland"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="W"] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="Wf."] <- "COUNTRYSIDE"
DefArt$ORIG.REG.BI[DefArt$ORIG.REG.BI=="UNKNOWN"] <- NA
DefArt$ORIGIN = DefArt$ORIG.REG.BI

DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="N"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="NE"] <- "1REYKJAVIKAKUREYRI"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="NW"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="E"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="S"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="SW"] <- "1REYKJAVIKAKUREYRI"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="W"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="Wf."] <- "Landsbyggð"
DefArt$ORIG.REG.BI.NS[DefArt$ORIG.REG.BI.NS=="UNKNOWN"] <- "Unknown"

DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="N"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="NE"] <- "AkureyriNE"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="NW"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="E"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="S"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="SW"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="W"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="Wf."] <- "Landsbyggð"
DefArt$ORIG.REG.BI.N[DefArt$ORIG.REG.BI.N=="UNKNOWN"] <- "Unknown"

DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="N"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="NE"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="NW"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="E"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="S"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="SW"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="W"] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="Wf."] <- "Landsbyggð"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="UNKNOWN"] <- "Unknown"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="Copenhagen"] <- "Copenhagen"
DefArt$ORIG.REG.BI.DK[DefArt$ORIG.REG.BI.DK=="DK"] <- "Copenhagen"

DefArt$PLACE.REG2 = DefArt$PLACE.REG
DefArt$PLACE.REG3 = DefArt$PLACE.REG
DefArt$PLACE.REG4 = DefArt$PLACE.REG

DefArt$PLACE.REG[DefArt$PLACE.REG=="Abroad-Other"] <- "ABROAD OTHER"
DefArt$PLACE.REG[DefArt$PLACE.REG=="Copenhagen"] <- "COPENHAGEN"
DefArt$PLACE.REG[DefArt$PLACE.REG=="E"] <- "EAST"
DefArt$PLACE.REG[DefArt$PLACE.REG=="NE"] <- "NORTH"
DefArt$PLACE.REG[DefArt$PLACE.REG=="N"] <- "NORTH"
DefArt$PLACE.REG[DefArt$PLACE.REG=="North-America"] <- "ABROAD OTHER"
DefArt$PLACE.REG[DefArt$PLACE.REG=="NW"] <- "NORTH"
DefArt$PLACE.REG[DefArt$PLACE.REG=="S"] <- "SOUTH"
DefArt$PLACE.REG[DefArt$PLACE.REG=="SW"] <- "1SOUTHWEST"
DefArt$PLACE.REG[DefArt$PLACE.REG=="W"] <- "WEST"
DefArt$PLACE.REG[DefArt$PLACE.REG=="Wf."] <- "WESTFJORDS"
DefArt$PLACE.REG[DefArt$PLACE.REG=="UNKNOWN"] <- NA

DefArt$PLACE.REG2[DefArt$PLACE.REG2=="Abroad-Other"] <- "ABROAD OTHER"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="Copenhagen"] <- "COPENHAGEN"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="E"] <- "EAST"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="NE"] <- "NORTH"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="N"] <- "NORTH"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="North-America"] <- "ABROAD OTHER"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="NW"] <- "NORTH"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="S"] <- "SOUTH"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="SW"] <- "1SOUTHWEST"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="W"] <- "WEST"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="Wf."] <- "WESTFJORDS"
DefArt$PLACE.REG2[DefArt$PLACE.REG2=="UNKNOWN"] <- "UNKNOWN"

DefArt$PLACE.REG3[DefArt$PLACE.REG3=="Abroad-Other"] <- "ABROAD"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="Copenhagen"] <- "ABROAD"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="E"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="NE"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="N"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="North-America"] <- "ABROAD"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="NW"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="S"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="SW"] <- "1SUÐVESTURLAND"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="W"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="Wf."] <- "LANDSBYGGÐ"
DefArt$PLACE.REG3[DefArt$PLACE.REG3=="UNKNOWN"] <- "UNKNOWN"

DefArt$PLACE.REG4[DefArt$PLACE.REG4=="Abroad-Other"] <- "ABROAD"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="Copenhagen"] <- "ABROAD"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="E"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="NE"] <- "1REYKJAVIKAKUREYRI"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="N"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="North-America"] <- "ABROAD"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="NW"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="S"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="SW"] <- "1REYKJAVIKAKUREYRI"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="W"] <- "LANDSBYGGÐ"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="Wf."] <- "LANDSBYGGÐ"
DefArt$PLACE.REG4[DefArt$PLACE.REG4=="UNKNOWN"] <- "UNKNOWN"
DefArt.19LCLVall = droplevels(subset(DefArt, CORPUS=="Private Letters 19LCLV"|CORPUS=="Newspapers"|CORPUS=="Essays"))
DefArt.19LCLVall = droplevels(subset(DefArt.19LCLVall, PhraseType=="DET-ADJ-NOM"|PhraseType=="DET-ADJ"))


DefArt.19LCLVall$Textasafn[DefArt.19LCLVall$CORPUS=="Private Letters 19LCLV"] <- "Sendibréf"
DefArt.19LCLVall$Textasafn[DefArt.19LCLVall$CORPUS=="Newspapers"] <- "Dagblöð og tímarit"
DefArt.19LCLVall$Textasafn[DefArt.19LCLVall$CORPUS=="Essays"] <- "Skólaritgerðir"

table(DefArt.19LCLVall$CORPUS)

table(DefArt$CORPUS)



DefArt$YEAR = as.numeric(DefArt$YEAR)
DefArt$PERIOD = as.numeric(DefArt$PERIOD)
DefArt$YEAR.TRI = as.numeric(DefArt$YEAR.TRI)
#DefArt$YEAR.FIX = as.numeric(DefArt$YEAR.FIX)
DefArt$NewCat = as.factor(DefArt$NewCat)
DefArt$ADJ.CAT = DefArt$NewCat
  
DefArt$PhraseType = as.factor(DefArt$PhraseType)
DefArt$PHRASE.TYPE = DefArt$PhraseType 

DefArt$AdjDeg = as.factor(DefArt$AdjDeg)
DefArt$ADJ.DEG = DefArt$AdjDeg 

DefArt$GENDER = as.factor(DefArt$GENDER)

DefArt$RANK.NR = as.factor(DefArt$RANK.NR) # incl. unknowns as cat
DefArt$SOCIAL.STAT = DefArt$RANK.NR # incl. unknowns as cat

DefArt$VARIANT = as.factor(DefArt$VARIANT)
DefArt$CORPUS.GEN = as.factor(DefArt$CORPUS.GEN)
DefArt$ORIG.REG = as.factor(DefArt$ORIG.REG)
DefArt$ORIGIN.REGION = DefArt$ORIG.REG

DefArt$PLACE.REG = as.factor(DefArt$PLACE.REG)
DefArt$INDIVIDUAL = as.factor(DefArt$INDIVIDUAL)


# DefArt$NewCat <- factor(DefArt$NewCat, level=c("Evaluatives", "Descriptives/Other", "Nationalities/Origins", "Dates"))
DefArt$NewCat <- factor(DefArt$NewCat, level=c("EVAL", "DESC/OTHER", "NAT/ORIGIN", "DATE"))

DefArt$ADJ.CAT <- factor(DefArt$ADJ.CAT, level=c("DESC/OTHER", "EVAL", "NAT/ORIGIN", "DATE"))

DefArt$GENDER <- factor(DefArt$GENDER, level=c("MALE", "FEMALE"))
DefArt$SPEAKER.SEX = DefArt$GENDER

DefArt$PhraseType <- factor(DefArt$PhraseType, level=c("DET-ADJ", "DET-ADJ-NOM"))
DefArt$AdjDeg <- factor(DefArt$AdjDeg, level=c("POS", "CMP", "SUP"))
DefArt$ADJ.DEG <- factor(DefArt$ADJ.DEG, level=c("POS", "CMP", "SUP"))


DefArt$ORIG.REG = factor(DefArt$ORIG.REG, level=c("Southwest", "West", "Westfjords", "North", "East", "South", "Copenhagen", "Unknown"))

DefArt.19LCLV = droplevels(subset(DefArt, CORPUS=="Private Letters 19LCLV"))
DefArt.18OCR = droplevels(subset(DefArt, CORPUS=="Private Letters OCR 1800-1850"))
DefArt.LetC = droplevels(subset(DefArt, CORPUS=="Private Letters C"))
DefArt.News = droplevels(subset(DefArt, CORPUS=="Newspapers"))
#DefArt.Essays = subset(DefArt, CORPUS=="Essays")


DefArt.socsyn = droplevels(subset(DefArt, CORPUS.GEN=="Letters"))
DefArt.news = droplevels(subset(DefArt, CORPUS.GEN=="Newspapers"))


DefArt.socsyn.eval = subset(DefArt.socsyn, NewCat=="EVAL")
DefArt.socsyn.eval.pos = subset(DefArt.socsyn.eval, AdjDeg=="POS")

