V3news <- read.csv(file = "V3-AllCorpora.csv", stringsAsFactors=FALSE, sep = "\t", quote = "\"", na.strings=c("","NA"))

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
V3news = subset(V3news, CORPUSGEN=="Newspapers")


V3news$DECADE = V3news$YEAR
V3news$YEARRAW = V3news$YEAR
#V3news$YEAR <- cut(V3news$YEAR, breaks=c(1780,1799,1824,1849,1874,1899,1925), labels = FALSE)
V3news$DECADE <- cut(V3news$DECADE, breaks=c(1780,1789,1799,1809,1819,1829,1839,1849,1859,1869,1879,1889,1899,1909,1919,1925), labels = FALSE)

V3news$DECADE = as.character(V3news$DECADE)
# V3news$DECADE[V3news$DECADE==1] <- "1780s"
# V3news$DECADE[V3news$DECADE==2] <- "1790s"
# V3news$DECADE[V3news$DECADE==3] <- "1800s"
# V3news$DECADE[V3news$DECADE==4] <- "1810s"
# V3news$DECADE[V3news$DECADE==5] <- "1820s"
# V3news$DECADE[V3news$DECADE==6] <- "1830s"
# V3news$DECADE[V3news$DECADE==7] <- "1840s"
# V3news$DECADE[V3news$DECADE==8] <- "1850s"
# V3news$DECADE[V3news$DECADE==9] <- "1860s"
# V3news$DECADE[V3news$DECADE==10] <- "1870s"
# V3news$DECADE[V3news$DECADE==11] <- "1880s"
# V3news$DECADE[V3news$DECADE==12] <- "1890s"
# V3news$DECADE[V3news$DECADE==13] <- "1900s"
# V3news$DECADE[V3news$DECADE==14] <- "1910s"
# V3news$DECADE[V3news$DECADE==15] <- "1920s"
#V3news$DECADE[V3news$DECADE==16] <- 16




V3news$YEAR <- as.numeric(as.character(V3news$YEAR))
V3news.time = V3news
V3news.time$DECADE = V3news.time$YEAR 

V3news$YEAR <- cut(V3news$YEAR, breaks=c(1799,1824,1849,1874,1899,1925), labels = FALSE)
#V3news$YEAR <- cut(V3news$YEAR, breaks=c(1780,1799,1824,1849,1874,1899,1925), labels = c("Before 1800", "1800-1824", "1825-1849", "1850-1874", "1875-1899", "1900-1924"))

V3news.time$DECADE <- cut(V3news.time$DECADE, breaks=c(1800,1809,1819,1829,1839,1849,1859,1869,1879,1889,1899,1909,1919,1925), labels = FALSE)
table(V3news.time$DECADE,V3news.time$ENV)
prop.table(table(V3news.time$DECADE,V3news.time$ENV),1)


V3news$CLASS[V3news$CLASS=="0"] <- "OTHER"
V3news$CLASS[V3news$CLASS=="2"] <- "OTHER"
V3news$CLASS[V3news$CLASS=="A"] <- "ABE"
V3news$CLASS[V3news$CLASS=="B"] <- "ABE"
V3news$CLASS[V3news$CLASS=="C"] <- "CD"
V3news$CLASS[V3news$CLASS=="D"] <- "CD"
V3news$CLASS[V3news$CLASS=="E"] <- "ABE"
V3news$CLASS[V3news$CLASS=="E/Ass."] <- "ABE"
V3news$CLASS[V3news$CLASS=="O"] <- "OTHER"
V3news$CLASS[V3news$CLASS=="OTHER"] <- "OTHER"


V3news$MODAL[V3news$MODAL=="mega"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="munu"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="skulu"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="vilja"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="geta"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="eiga"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="fá"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="kunna"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="hljóta"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="verða"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="þurfa"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="ætla"] <- "MODAL"
V3news$MODAL[V3news$MODAL=="aux"] <- "AUX"
V3news$MODAL[V3news$MODAL=="lex"] <- "LEX"

V3news$NEG[V3news$NEG=="1"] <- "NEG"
V3news$NEG[V3news$NEG=="0"] <- "SADV"

V3news$HEYCOCK.CLASS = V3news$HEYCOCK
# Recode HEYCOCK and CPTYPE into three-way vars
#V3news$HEYCOCK[V3news$HEYCOCK=="Decl" & V3news$CLASS=="ABE"] <- "Typical V2"
#V3news$HEYCOCK[V3news$HEYCOCK=="Decl" & V3news$CLASS=="CD"] <- "Typical Non-V2"
#V3news$HEYCOCK[V3news$HEYCOCK=="Decl" & V3news$CLASS=="OTHER"] <- "Mixed"
V3news$HEYCOCK[V3news$HEYCOCK=="Decl"] <- "V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Result"] <- "V2"
V3news$HEYCOCK[V3news$HEYCOCK=="ConsDeg"] <- "V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Cause"] <- "V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Adv" & V3news$CP.SUBTYPE=="CMP"] <- "NON-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Adv"] <- "MIXED"
V3news$HEYCOCK[V3news$HEYCOCK=="Purpose"] <- "MIXED" #"Typical Non-V2" #"Mixed"
V3news$HEYCOCK[V3news$HEYCOCK=="IndQu"] <- "NON-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Rel"] <- "NON-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Cond"] <- "NON-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Cmp"] <- "NON-V2"

V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Decl" & V3news$CLASS=="ABE"] <- "Typical V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Decl" & V3news$CLASS=="CD"] <- "Typical Non-V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Decl" & V3news$CLASS=="OTHER"] <- "Mixed"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Decl"] <- "Typical V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Result"] <- "Typical V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="ConsDeg"] <- "Typical V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Cause"] <- "Typical V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Adv" & V3news$CP.SUBTYPE=="CMP"] <- "Typical Non-V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Adv"] <- "Mixed"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Purpose"] <- "Mixed" #"Typical Non-V2" #"Mixed"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="IndQu"] <- "Typical Non-V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Rel"] <- "Typical Non-V2"
V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Cond"] <- "Typical Non-V2"
#V3news$HEYCOCK.CLASS[V3news$HEYCOCK.CLASS=="Cmp"] <- "Typical Non-V2"

# Recode HEYCOCK and CPTYPE into three-way vars
#V3news$HEYCOCK[V3news$HEYCOCK=="Decl" & V3news$CLASS=="ABE"] <- "Typical V2"
#V3news$HEYCOCK[V3news$HEYCOCK=="Decl" & V3news$CLASS=="CD"] <- "Typical Non-V2"
#V3news$HEYCOCK[V3news$HEYCOCK=="Decl" & V3news$CLASS=="OTHER"] <- "Mixed"
V3news$HEYCOCK[V3news$HEYCOCK=="Decl"] <- "Typical V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Result"] <- "Typical V2"
V3news$HEYCOCK[V3news$HEYCOCK=="ConsDeg"] <- "Typical V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Cause"] <- "Typical V2"
#V3news$HEYCOCK[V3news$HEYCOCK=="Adv" & V3news$CP.SUBTYPE=="CMP"] <- "Typical Non-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Adv"] <- "Mixed"
V3news$HEYCOCK[V3news$HEYCOCK=="Purpose"] <- "Mixed" #"Typical Non-V2" #"Mixed"
V3news$HEYCOCK[V3news$HEYCOCK=="IndQu"] <- "Typical Non-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Rel"] <- "Typical Non-V2"
V3news$HEYCOCK[V3news$HEYCOCK=="Cond"] <- "Typical Non-V2"
#V3news$HEYCOCK[V3news$HEYCOCK=="Cmp"] <- "Typical Non-V2"


V3news$ORIGINREGION.TRI = V3news$ORIGINREGION
V3news$ORIGINREGION.BI = V3news$ORIGINREGION
V3news$ORIGINREGION.TRIb = V3news$ORIGINREGION

# V3news$ORIGINREGION[V3news$ORIGINREGION=="N"] <- "North"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="NE"] <- "North"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="NW"] <- "North"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="E"] <- "East"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="S"] <- "South"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="SW"] <- "Southwest"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="W"] <- "West"
# V3news$ORIGINREGION[V3news$ORIGINREGION=="Wf"] <- "West"


V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="N"] <- "North"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="NE"] <- "North"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="NW"] <- "North"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="E"] <- "Other"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="S"] <- "Other"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="SW"] <- "Southwest"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="W"] <- "Other"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="Wf"] <- "Other"
V3news$ORIGINREGION.TRI[V3news$ORIGINREGION.TRI=="DK"] <- "DK"

V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="N"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="NE"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="NW"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="E"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="S"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="SW"] <- "Southwest"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="W"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="Wf"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="Wf"] <- "Other"
V3news$ORIGINREGION.BI[V3news$ORIGINREGION.BI=="DK"] <- "DK"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="N"] <- "Other"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="NE"] <- "North"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="NW"] <- "Other"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="E"] <- "Other"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="S"] <- "Other"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="SW"] <- "Southwest"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="W"] <- "Other"
# V3news$ORIGINREGION.TRIb[V3news$ORIGINREGION.TRIb=="Wf."] <- "Other"



