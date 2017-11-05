setwd("C:\\Users\\DeNovo\\Dropbox\\NIS-Small\\LVAD")
setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD")

data <- read.csv("unweightedLVAD.csv")


setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD")
library(ggplot2)


#remove admissions that do not provide full hospitalization data

fulldata <- data[!((!is.na(data$p1) & data$p1day == -99) |
			(!is.na(data$p2) & data$p2day == -99) |
			(!is.na(data$p3) & data$p3day == -99) |
			(!is.na(data$p4) & data$p4day == -99) |
			(!is.na(data$p5) & data$p5day == -99) |
			(!is.na(data$p6) & data$p6day == -99) |
			(!is.na(data$p7) & data$p7day == -99) |
			(!is.na(data$p8) & data$p8day == -99) |
			(!is.na(data$p9) & data$p9day == -99) |
			(!is.na(data$p10) & data$p10day == -99) |
			(!is.na(data$p11) & data$p11day == -99) |
			(!is.na(data$p12) & data$p12day == -99) |
			(!is.na(data$p13) & data$p13day == -99) |
			(!is.na(data$p14) & data$p14day == -99) |
			(!is.na(data$p15) & data$p15day == -99))
			,]

fulldata <- fulldata[fulldata$los >= 0,] 
summary(fulldata) #median hospital day with LVAD is 30 days. 


#fulldata[is.na(fulldata$p1),]$p1 <- " "   #none of these hospitalizations don't have at least one procedure
fulldata[is.na(fulldata$p2),]$p2 <- " "
fulldata[is.na(fulldata$p3),]$p3 <- " "
fulldata[is.na(fulldata$p4),]$p4 <- " "
fulldata[is.na(fulldata$p5),]$p5 <- " "
fulldata[is.na(fulldata$p6),]$p6 <- " "
fulldata[is.na(fulldata$p7),]$p7 <- " "
fulldata[is.na(fulldata$p8),]$p8 <- " "
fulldata[is.na(fulldata$p9),]$p9 <- " "
fulldata[is.na(fulldata$p10),]$p10 <- " "
fulldata[is.na(fulldata$p11),]$p11 <- " "
fulldata[is.na(fulldata$p12),]$p12 <- " "
fulldata[is.na(fulldata$p13),]$p13 <- " "
fulldata[is.na(fulldata$p14),]$p14 <- " "
fulldata[is.na(fulldata$p15),]$p15 <- " "

fulldata$dayofLVAD <- -99
fulldata[fulldata$p1 == "3766",]$dayofLVAD <- fulldata[fulldata$p1 == "3766",]$p1day
fulldata[fulldata$p2 == "3766",]$dayofLVAD <- fulldata[fulldata$p2 == "3766",]$p2day
fulldata[fulldata$p3 == "3766",]$dayofLVAD <- fulldata[fulldata$p3 == "3766",]$p3day
fulldata[fulldata$p4 == "3766",]$dayofLVAD <- fulldata[fulldata$p4 == "3766",]$p4day
fulldata[fulldata$p5 == "3766",]$dayofLVAD <- fulldata[fulldata$p5 == "3766",]$p5day
fulldata[fulldata$p6 == "3766",]$dayofLVAD <- fulldata[fulldata$p6 == "3766",]$p6day
fulldata[fulldata$p7 == "3766",]$dayofLVAD <- fulldata[fulldata$p7 == "3766",]$p7day
fulldata[fulldata$p8 == "3766",]$dayofLVAD <- fulldata[fulldata$p8 == "3766",]$p8day
fulldata[fulldata$p9 == "3766",]$dayofLVAD <- fulldata[fulldata$p9 == "3766",]$p9day
fulldata[fulldata$p10 == "3766",]$dayofLVAD <- fulldata[fulldata$p10 == "3766",]$p10day
fulldata[fulldata$p11 == "3766",]$dayofLVAD <- fulldata[fulldata$p11 == "3766",]$p11day
fulldata[fulldata$p12 == "3766",]$dayofLVAD <- fulldata[fulldata$p12 == "3766",]$p12day
fulldata[fulldata$p13 == "3766",]$dayofLVAD <- fulldata[fulldata$p13 == "3766",]$p13day
fulldata[fulldata$p14 == "3766",]$dayofLVAD <- fulldata[fulldata$p14 == "3766",]$p14day
fulldata[fulldata$p15 == "3766",]$dayofLVAD <- fulldata[fulldata$p15 == "3766",]$p15day

fulldata <- fulldata[fulldata$dayofLVAD >= 0,]
write.csv(fulldata, "unweightedLVAD-fullDates.csv")

summary(fulldata$dayofLVAD)



#### MANUAL EDIT TO DETERMINE WHICH EARLIEST LVAD DAY ###

fulldata <- read.csv("unweightedLVAD-fullDates-CountLVAD-fixed.csv")


procedures <- c(fulldata$p1, fulldata$p2, fulldata$p3, fulldata$p4, fulldata$p5, fulldata$p6, fulldata$p7, fulldata$p8, fulldata$p9,
	fulldata$p10, fulldata$p11, fulldata$p12, fulldata$p13, fulldata$p14, fulldata$p15)

days <- c(fulldata$p1day, fulldata$p2day, fulldata$p3day, fulldata$p4day, fulldata$p5day, fulldata$p6day, fulldata$p7day,
 fulldata$p8day, fulldata$p9day, fulldata$p10day, fulldata$p11day, fulldata$p12day, fulldata$p13day, fulldata$p14day, fulldata$p15day)

daysFromLVAD <- c(fulldata$p1day - fulldata$dayofLVAD, fulldata$p2day  - fulldata$dayofLVAD, fulldata$p3day  - fulldata$dayofLVAD,
 fulldata$p4day - fulldata$dayofLVAD, fulldata$p5day - fulldata$dayofLVAD, fulldata$p6day - fulldata$dayofLVAD, fulldata$p7day - fulldata$dayofLVAD,
 fulldata$p8day - fulldata$dayofLVAD, fulldata$p9day - fulldata$dayofLVAD, fulldata$p10day - fulldata$dayofLVAD, fulldata$p11day - fulldata$dayofLVAD,
 fulldata$p12day - fulldata$dayofLVAD, fulldata$p13day - fulldata$dayofLVAD, fulldata$p14day - fulldata$dayofLVAD, fulldata$p15day - fulldata$dayofLVAD)

procs <- cbind(procedures, days, daysFromLVAD)
procDF <- as.data.frame(procs)
procDF$days <- as.integer(procDF$days)
procDF$daysFromLVAD<- as.integer(procDF$daysFromLVAD)
procDF <- procDF[procDF$procedures != " ",]
procDF <- procDF[!is.na(procDF$procedures),]

str(procDF)
sort(table(procDF$procedures))
procDF$counter <- 1

library(plyr)
newdata <- ddply(procDF, c("procedures"), summarize, meanDay = mean(days), meanDayFromLVAD = mean(daysFromLVAD), n = sum(counter))
write.csv(newdata, "tableProcedures.csv")


procDF$cat <- "None"
procDF[procDF$procedures == "3766",]$cat <- "LVAD"
procDF[procDF$procedures %in% c("3751", "375"),]$cat <- "Heart Transplant"
procDF[procDF$procedures %in% c("3403", "3764", "3479", "341", "3749", "3712"),]$cat <- "Reoperation"
procDF[procDF$procedures %in% c("3961", "3761", "9604", "9672", "9671", "3995"),]$cat <- "Support"
procDF[procDF$procedures %in% c("4513", "4523", "9904", "9907", "9905", "9909"),]$cat <- "Bleed Related"
procDF[procDF$procedures %in% c("8964", "3723", "3721", "8856", "3891", "3893"),]$cat <- "Cath"
table(procDF$cat)

qplot(procDF$daysFromLVAD, fill = procDF$cat) + facet_wrap()

excludeInitialLVAD <- procDF[!(procDF$daysFromLVAD == 0 & procDF$procedures == "3766"),]

p = ggplot(excludeInitialLVAD , aes(x=daysFromLVAD,fill = cat)) + geom_density(alpha = 0.3)
p + facet_grid(cat ~ ., scales = "free_y")
ggsave("TimelineAsDistribution.png")

p = ggplot(excludeInitialLVAD , aes(x=daysFromLVAD,fill = cat)) + geom_bar(alpha = 0.9, binwidth = 1)
p + facet_grid(cat ~ ., scales = "free_y") +xlim(-30, 100)
ggsave("TimelineAsBarGraph.png")




###### SEPARATE BY LVAD to OHT TIMING #########


data <- read.csv("unweightedLVAD-fullDates-CountLVAD-fixed-OverAndEqual18.csv")


data[is.na(data$p1),]$p1 <- " " 
data[is.na(data$p2),]$p2 <- " " 
data[is.na(data$p3),]$p3 <- " " 
data[is.na(data$p4),]$p4 <- " " 
data[is.na(data$p5),]$p5 <- " " 
data[is.na(data$p6),]$p6 <- " " 
data[is.na(data$p7),]$p7 <- " " 
data[is.na(data$p8),]$p8 <- " " 
data[is.na(data$p9),]$p9 <- " " 
data[is.na(data$p10),]$p10 <- " " 
data[is.na(data$p11),]$p11 <- " " 
data[is.na(data$p12),]$p12 <- " " 
data[is.na(data$p13),]$p13 <- " " 
data[is.na(data$p14),]$p14 <- " " 
data[is.na(data$p15),]$p15 <- " " 

data$hasOHT <- "375" == data$p1 | "375" == data$p2 | "375" == data$p3 | "375" ==  data$p4 |
		"375" == data$p5 | "375" == data$p6 | "375" == data$p7 | "375" == data$p8 | "375" == data$p9 |
		"375" == data$p10 | "375" == data$p11 | "375" ==  data$p12 | "375" == data$p13 |
		"375" == data$p14 | "375" == data$p15 | "3751" == data$p1 | "3751" == data$p2 | "3751" == data$p3 | 
		"3751" ==  data$p4 | "3751" == data$p5 | "3751" == data$p6 | "3751" == data$p7 | "3751" == data$p8 | 
		"3751" == data$p9 | "3751" == data$p10 | "3751" == data$p11 | "3751" ==  data$p12 | "3751" == data$p13 |
		"3751" == data$p14 | "3751" == data$p15 

sum(data$hasOHT)

data$dayofOHT <- -99

data["375" == data$p1,]$dayofOHT <- data["375" == data$p1,]$p1day 
data["375" == data$p2,]$dayofOHT <- data["375" == data$p2,]$p2day 
data["375" == data$p3,]$dayofOHT <- data["375" == data$p3,]$p3day 
data["375" == data$p4,]$dayofOHT <- data["375" == data$p4,]$p4day 
data["375" == data$p5,]$dayofOHT <- data["375" == data$p5,]$p5day 
data["375" == data$p6,]$dayofOHT <- data["375" == data$p6,]$p6day 
data["375" == data$p7,]$dayofOHT <- data["375" == data$p7,]$p7day 
data["375" == data$p8,]$dayofOHT <- data["375" == data$p8,]$p8day 
data["375" == data$p9,]$dayofOHT <- data["375" == data$p9,]$p9day 
data["375" == data$p10,]$dayofOHT <- data["375" == data$p10,]$p10day 
data["375" == data$p11,]$dayofOHT <- data["375" == data$p11,]$p11day 
data["375" == data$p12,]$dayofOHT <- data["375" == data$p12,]$p12day 
data["375" == data$p13,]$dayofOHT <- data["375" == data$p13,]$p13day 
data["375" == data$p14,]$dayofOHT <- data["375" == data$p14,]$p14day 
data["375" == data$p15,]$dayofOHT <- data["375" == data$p15,]$p15day 

data["3751" == data$p1,]$dayofOHT <- data["3751" == data$p1,]$p1day 
data["3751" == data$p2,]$dayofOHT <- data["3751" == data$p2,]$p2day 
data["3751" == data$p3,]$dayofOHT <- data["3751" == data$p3,]$p3day 
data["3751" == data$p4,]$dayofOHT <- data["3751" == data$p4,]$p4day 
data["3751" == data$p5,]$dayofOHT <- data["3751" == data$p5,]$p5day 
data["3751" == data$p6,]$dayofOHT <- data["3751" == data$p6,]$p6day 
data["3751" == data$p7,]$dayofOHT <- data["3751" == data$p7,]$p7day 
data["3751" == data$p8,]$dayofOHT <- data["3751" == data$p8,]$p8day 
data["3751" == data$p9,]$dayofOHT <- data["3751" == data$p9,]$p9day 
data["3751" == data$p10,]$dayofOHT <- data["3751" == data$p10,]$p10day 
data["3751" == data$p11,]$dayofOHT <- data["3751" == data$p11,]$p11day 
data["3751" == data$p12,]$dayofOHT <- data["3751" == data$p12,]$p12day 
data["3751" == data$p13,]$dayofOHT <- data["3751" == data$p13,]$p13day 
data["3751" == data$p14,]$dayofOHT <- data["3751" == data$p14,]$p14day 
data["3751" == data$p15,]$dayofOHT <- data["3751" == data$p15,]$p15day 


data$numDiags <- 0
data["     " != data$d1,]$numDiags <- data["     " != data$d1,]$numDiags + 1
data["     " != data$d2,]$numDiags <- data["     " != data$d2,]$numDiags + 1
data["     " != data$d3,]$numDiags <- data["     " != data$d3,]$numDiags + 1
data["     " != data$d4,]$numDiags <- data["     " != data$d4,]$numDiags + 1
data["     " != data$d5,]$numDiags <- data["     " != data$d5,]$numDiags + 1
data["     " != data$d6,]$numDiags <- data["     " != data$d6,]$numDiags + 1
data["     " != data$d7,]$numDiags <- data["     " != data$d7,]$numDiags + 1
data["     " != data$d8,]$numDiags <- data["     " != data$d8,]$numDiags + 1
data["     " != data$d9,]$numDiags <- data["     " != data$d9,]$numDiags + 1
data["     " != data$d10,]$numDiags <- data["     " != data$d10,]$numDiags + 1
data["     " != data$d11,]$numDiags <- data["     " != data$d11,]$numDiags + 1
data["     " != data$d12,]$numDiags <- data["     " != data$d12,]$numDiags + 1
data["     " != data$d13,]$numDiags <- data["     " != data$d13,]$numDiags + 1
data["     " != data$d14,]$numDiags <- data["     " != data$d14,]$numDiags + 1
data["     " != data$d15,]$numDiags <- data["     " != data$d15,]$numDiags + 1
summary(data$numDiags)


data$timeDiff <- data$dayofOHT - data$dayofLVAD

#transplants <- data[-99 != data$dayofOHT,]
transplants <- data[data$hasOHT,]


transplants$timeDiff <- transplants$dayofOHT - transplants$dayofLVAD
transplants <- transplants[transplants$timeDiff >= 0,] # Removed everyone who got LVAD after transplant
str(transplants)

summary(transplants$timeDiff)
qplot(transplants$timeDiff, binwidth = 1)
transplants$quartile <- -99
transplants[ 0 <= transplants$timeDiff & transplants$timeDiff < 8, ]$quartile <- 1
transplants[ 8 <= transplants$timeDiff & transplants$timeDiff < 32, ]$quartile <- 2
transplants[ 32 <= transplants$timeDiff & transplants$timeDiff < 66, ]$quartile <- 3
transplants[ 66 <= transplants$timeDiff & transplants$timeDiff <= 306, ]$quartile <- 4


qplot(transplants$timeDiff, binwidth = 1)
#ggsave("Time Difference between LVAD and OHT.png")

qplot(transplants$timeDiff, binwidth = 7)
#ggsave("Time Difference between LVAD and OHT - Binned by every 7 days.png")

table(transplants$quartile)

summary(transplants[transplants$quartile == 1,]$age)
sd(transplants[transplants$quartile == 1,]$age)
summary(transplants[transplants$quartile == 2,]$age)
sd(transplants[transplants$quartile == 2,]$age)
summary(transplants[transplants$quartile == 3,]$age)
sd(transplants[transplants$quartile == 3,]$age)
summary(transplants[transplants$quartile == 4,]$age)
sd(transplants[transplants$quartile == 4,]$age)

t.test(transplants[transplants$quartile == 4,]$age, transplants[transplants$quartile == 1,]$age)
t.test(transplants[transplants$quartile == 2,]$age, transplants[transplants$quartile == 1,]$age)
t.test(transplants[transplants$quartile == 3,]$age, transplants[transplants$quartile == 1,]$age)

t.test(transplants[transplants$quartile != 1,]$age, transplants[transplants$quartile == 1,]$age)

summary(transplants[transplants$quartile == 1,]$los)
sd(transplants[transplants$quartile == 1,]$los)
summary(transplants[transplants$quartile == 2,]$los)
sd(transplants[transplants$quartile == 2,]$los)
summary(transplants[transplants$quartile == 3,]$los)
sd(transplants[transplants$quartile == 3,]$los)
summary(transplants[transplants$quartile == 4,]$los)
sd(transplants[transplants$quartile == 4,]$los)

t.test(transplants[transplants$quartile == 4,]$los, transplants[transplants$quartile == 1,]$los)
t.test(transplants[transplants$quartile == 2,]$los, transplants[transplants$quartile == 1,]$los)
t.test(transplants[transplants$quartile == 3,]$los, transplants[transplants$quartile == 1,]$los)

t.test(transplants[transplants$quartile != 1,]$los, transplants[transplants$quartile == 1,]$los)

table(transplants[transplants$quartile == 1,]$died)
prop.table(table(transplants[transplants$quartile == 1,]$died))
table(transplants[transplants$quartile == 2,]$died)
prop.table(table(transplants[transplants$quartile == 2,]$died))
table(transplants[transplants$quartile == 3,]$died)
prop.table(table(transplants[transplants$quartile == 3,]$died))
table(transplants[transplants$quartile == 4,]$died)
prop.table(table(transplants[transplants$quartile == 4,]$died))

table(data[ !data$hasOHT,]$died)
prop.table(table(data[ !data$hasOHT,]$died))

chisq.test( matrix(c(11,30,15,108), ncol = 2))
# JUST POST 2006
chisq.test( matrix(c(3,16,4,39), ncol = 2), simulate.p.value = TRUE)
# JUST PRE 2006
chisq.test( matrix(c(8,14,9,69), ncol = 2), simulate.p.value = TRUE)



table(transplants[transplants$quartile == 1,]$sex)
prop.table(table(transplants[transplants$quartile == 1,]$sex))
table(transplants[transplants$quartile == 2,]$sex)
prop.table(table(transplants[transplants$quartile == 2,]$sex))
table(transplants[transplants$quartile == 3,]$sex)
prop.table(table(transplants[transplants$quartile == 3,]$sex))
table(transplants[transplants$quartile == 4,]$sex)
prop.table(table(transplants[transplants$quartile == 4,]$sex))


table(transplants[transplants$quartile == 1,]$race)
prop.table(table(transplants[transplants$quartile == 1,]$race))
table(transplants[transplants$quartile == 2,]$race)
prop.table(table(transplants[transplants$quartile == 2,]$race))
table(transplants[transplants$quartile == 3,]$race)
prop.table(table(transplants[transplants$quartile == 3,]$race))
table(transplants[transplants$quartile == 4,]$race)
prop.table(table(transplants[transplants$quartile == 4,]$race))

table(transplants[transplants$quartile == 1,]$zip)
prop.table(table(transplants[transplants$quartile == 1,]$zip))
table(transplants[transplants$quartile == 2,]$zip)
prop.table(table(transplants[transplants$quartile == 2,]$zip))
table(transplants[transplants$quartile == 3,]$zip)
prop.table(table(transplants[transplants$quartile == 3,]$zip))
table(transplants[transplants$quartile == 4,]$zip)
prop.table(table(transplants[transplants$quartile == 4,]$zip))



summary(transplants[transplants$quartile == 1,]$numDiags)
sd(transplants[transplants$quartile == 1,]$numDiags)
summary(transplants[transplants$quartile == 2,]$numDiags)
sd(transplants[transplants$quartile == 2,]$numDiags)
summary(transplants[transplants$quartile == 3,]$numDiags)
sd(transplants[transplants$quartile == 3,]$numDiags)
summary(transplants[transplants$quartile == 4,]$numDiags)
sd(transplants[transplants$quartile == 4,]$numDiags)

t.test(transplants[transplants$quartile != 1,]$numDiags, transplants[transplants$quartile == 1,]$numDiags)

####### FIND TIME FROM OHT TO DISCHARGE #######
str(transplants)

transplants$OHTtoDC <- transplants$los - transplants$dayofOHT
summary(transplants$OHTtoDC)

summary(transplants[transplants$quartile == 1,]$OHTtoDC)
sd(transplants[transplants$quartile == 1,]$OHTtoDC)
summary(transplants[transplants$quartile == 2,]$OHTtoDC)
sd(transplants[transplants$quartile == 2,]$OHTtoDC)
summary(transplants[transplants$quartile == 3,]$OHTtoDC)
sd(transplants[transplants$quartile == 3,]$OHTtoDC)
summary(transplants[transplants$quartile == 4,]$OHTtoDC)
sd(transplants[transplants$quartile == 4,]$OHTtoDC)

t.test(transplants[transplants$quartile == 1,]$OHTtoDC, transplants[transplants$quartile != 1,]$OHTtoDC)

summary(transplants[transplants$quartile == 1 & transplants$died == 0,]$OHTtoDC)
sd(transplants[transplants$quartile == 1 & transplants$died == 0,]$OHTtoDC)
summary(transplants[transplants$quartile == 2 & transplants$died == 0,]$OHTtoDC)
sd(transplants[transplants$quartile == 2 & transplants$died == 0,]$OHTtoDC)
summary(transplants[transplants$quartile == 3 & transplants$died == 0,]$OHTtoDC)
sd(transplants[transplants$quartile == 3 & transplants$died == 0,]$OHTtoDC)
summary(transplants[transplants$quartile == 4 & transplants$died == 0,]$OHTtoDC)
sd(transplants[transplants$quartile == 4 & transplants$died == 0,]$OHTtoDC)

t.test(transplants[transplants$quartile == 1 & transplants$died == 0,]$OHTtoDC, transplants[transplants$quartile != 1 & transplants$died == 0,]$OHTtoDC)


prop.test(x = c(sum(transplants[transplants$quartile == 1,]$sex),sum(transplants[transplants$quartile != 1,]$sex)), 
		n = c(length(transplants[transplants$quartile == 1,]$sex),length(transplants[transplants$quartile != 1,]$sex)),
		correct = TRUE)

table(transplants$race)

prop.test(x = c(sum(transplants[transplants$quartile == 1,]$race == 1),sum(transplants[transplants$quartile != 1,]$race == 1)), 
		n = c(length(transplants[transplants$quartile == 1,]$race ),length(transplants[transplants$quartile != 1,]$race )),
		correct = TRUE)

prop.test(x = c(sum(transplants[transplants$quartile == 1,]$zip  == 1),sum(transplants[transplants$quartile != 1,]$zip == 1)), 
		n = c(length(transplants[transplants$quartile == 1,]$zip ),length(transplants[transplants$quartile != 1,]$zip )),
		correct = TRUE)





###### COMPARISION WITHOUT OHT ######

data$timeDiff <- data$dayofOHT - data$dayofLVAD
data$quartile <- -99
data[ 0 <= data$timeDiff & data$timeDiff < 8, ]$quartile <- 1
data[ 8 <= data$timeDiff & data$timeDiff < 32, ]$quartile <- 2
data[ 32 <= data$timeDiff & data$timeDiff < 66, ]$quartile <- 3
data[ 66 <= data$timeDiff & data$timeDiff <= 306, ]$quartile <- 4
table(data$quartile)

t.test(data[data$quartile == - 99,]$died, data[data$quartile != - 99 & data$quartile != 1,]$died)
t.test(data[data$quartile == - 99,]$died, data[data$quartile == 1,]$died)




t.test(data[data$quartile == -99,]$age, transplants[transplants$quartile != -99,]$age)
t.test(data[data$quartile == -99,]$numDiags , transplants[transplants$quartile != -99,]$numDiags )

prop.test(x = c(sum(data[data$quartile == -99,]$sex),sum(data[data$quartile != -99,]$sex)), 
		n = c(length(data[data$quartile == -99,]$sex),length(data[data$quartile != -99,]$sex)),
		correct = TRUE)

table(transplants$race)

prop.test(x = c(sum(data[data$quartile == -99,]$race == 1),sum(data[data$quartile != -99,]$race == 1)), 
		n = c(length(data[data$quartile == -99,]$race ),length(data[data$quartile != -99,]$race )),
		correct = TRUE)

prop.test(x = c(sum(data[data$quartile == -99,]$zip  == 1),sum(data[data$quartile != -99,]$zip == 1)), 
		n = c(length(data[data$quartile == -99,]$zip ),length(data[data$quartile != -99,]$zip )),
		correct = TRUE)




###### Swan Analysis ########

data$hasSwan <- "8964" == data$p1 | "8964" == data$p2 | "8964" == data$p3 | "8964" ==  data$p4 |
		"8964" == data$p5 | "8964" == data$p6 | "8964" == data$p7 | "8964" == data$p8 | "8964" == data$p9 |
		"8964" == data$p10 | "8964" == data$p11 | "8964" ==  data$p12 | "8964" == data$p13 |
		"8964" == data$p14 | "8964" == data$p15 


summary(data[data$hasSwan,]$dayofLVAD)
summary(data[!data$hasSwan,]$dayofLVAD)

t.test(data[data$hasSwan,]$dayofLVAD, data[!data$hasSwan,]$dayofLVAD)

summary(data[data$hasSwan,]$died)
summary(data[!data$hasSwan,]$died)

t.test(data[data$hasSwan,]$died, data[!data$hasSwan,]$died)

summary(data[data$hasSwan,]$los)
sd(data[data$hasSwan,]$los)
summary(data[data$hasSwan,]$died)
table(data[data$hasSwan,]$died)

summary(data$los)
sd(data$los)
summary(data$died)
table(data$died)

summary(data[data$hasSwan & data$hasOHT,]$los)
summary(data[data$hasSwan & data$hasOHT,]$died)


########## 8/7/2015 ######## Age relationship

data$decade <- round_any(data$age, 5)
qplot(data = data[data$died >= 0,],x = decade, weight =died) + geom_bar()
data$counter <- 1

data[data$decade >= 80,]$decade <- 80

ageData <- ddply(data, .(decade), summarise, mortality = sum(died), total = sum(counter))
ageData$mortRate <- ageData$mortality/ageData$total
qplot( ageData$decade, ageData$mortRate)

ageRelation <- lm(data = ageData, mortRate  ~ decade)
summary(ageRelation)

t.test(data[data$sex == 0,]$died, data[data$sex == 1,]$died)

t.test(data[data$race == 1,]$died, data[data$race == 2,]$died)

summary(data$dayofLVAD)
sd(data$dayofLVAD)


t.test(data[data$dayofLVAD == 1 | data$dayofLVAD == 0,]$died, data[!(data$dayofLVAD == 1 | data$dayofLVAD == 0),]$died)


sum(data$hasSwan)

t.test(data[data$hasSwan,]$died, data[!data$hasSwan,]$died)
t.test(data[data$hasSwan,]$los, data[!data$hasSwan,]$los)
t.test(data[data$hasSwan,]$dayofLVAD, data[!data$hasSwan,]$dayofLVAD)

sum(data$hasOHT)

length(unique(data$hospital))

qplot(data$year)


swantrend <- ddply(data, .(year), summarise, swan = sum(hasSwan), total = sum(counter))
swantrend$prop <- swantrend$swan/swantrend$total
qplot(data = swantrend, year, prop) + geom_smooth()
summary(lm(prop~year, data = swantrend))

qplot(data = data, year, dayofLVAD)
qplot(data = data, year, dayofOHT)

t.test(data[data$hasOHT,]$died, data[!data$hasOHT,]$died)


###### YEARLY TRENDS #####

str(data)
data$count <- 1
yearlydata <- ddply(data, .(year), summarize, sumCount = sum(count), 
						propDied = sum(died)/sum(count)*1.0)

summary(lm(sumCount ~year, data = yearlydata ))
summary(lm(propDied ~ year, data = yearlydata[yearlydata$year %in% 1998:2006,] ))
summary(lm(propDied ~ year, data = yearlydata[yearlydata$year %in% 2007:2011,] ))




##### 3/24/2016 Analysis after Dr. Banerjee's comments

##### Mortality trends post2006 for OHT s/p LVAD

transplantsAfter2006 <- transplants[transplants$year >= 2006,]

table(transplantsAfter2006[transplantsAfter2006$quartile == 1,]$died)
prop.table(table(transplantsAfter2006[transplantsAfter2006$quartile == 1,]$died))
table(transplantsAfter2006[transplantsAfter2006$quartile == 2,]$died)
prop.table(table(transplantsAfter2006[transplantsAfter2006$quartile == 2,]$died))
table(transplantsAfter2006[transplantsAfter2006$quartile == 3,]$died)
prop.table(table(transplantsAfter2006[transplantsAfter2006$quartile == 3,]$died))
table(transplantsAfter2006[transplantsAfter2006$quartile == 4,]$died)
prop.table(table(transplantsAfter2006[transplantsAfter2006$quartile == 4,]$died))

table(data[ !data$hasOHT & data$year>=2006,]$died)
prop.table(table(data[ !data$hasOHT& data$year>=2006,]$died))


##### Mortality trends pre006 for OHT s/p LVAD

transplantsPre2006 <- transplants[transplants$year < 2006,]
str(transplantsPre2006 )
table(transplantsPre2006[transplantsPre2006$quartile == 1,]$died)
prop.table(table(transplantsPre2006[transplantsPre2006$quartile == 1,]$died))
table(transplantsPre2006[transplantsPre2006$quartile == 2,]$died)
prop.table(table(transplantsPre2006[transplantsPre2006$quartile == 2,]$died))
table(transplantsPre2006[transplantsPre2006$quartile == 3,]$died)
prop.table(table(transplantsPre2006[transplantsPre2006$quartile == 3,]$died))
table(transplantsPre2006[transplantsPre2006$quartile == 4,]$died)
prop.table(table(transplantsPre2006[transplantsPre2006$quartile == 4,]$died))

table(data[ !data$hasOHT & data$year<2006,]$died)
prop.table(table(data[ !data$hasOHT& data$year<2006,]$died))

str(transplants)
transplants$count <- 1
summarizedTransplant <- ddply(transplants, .(timeDiff), summarize, propdied = sum(died)/sum(count))
str(summarizedTransplant)
qplot(data = summarizedTransplant, timeDiff, propdied, geom = "line")
#ggsave("proportionMortalityByDay.png")

summarizedTransplant <- ddply(transplants, .(week = floor(timeDiff/7)), summarize, propdied = sum(died)/sum(count))
str(summarizedTransplant)
qplot(data = summarizedTransplant, week, propdied, geom = "line")
#ggsave("proportionMortalityByWeek.png")

##### Not Transplanted Summary Statistics ####

notTransplant <- rbind(data[!data$hasOHT,], data[data$hasOHT & data$timeDiff < 0,])
str(notTransplant)
summary(notTransplant)
table(notTransplant$died)
sd(notTransplant$los)
sd(notTransplant$age)
table(notTransplant$sex)

table(notTransplant$race)
82 + 265
(82 + 265)/2036
table(notTransplant$race)/2036

table(notTransplant$zip)
table(notTransplant$zip) / 2036

sd(notTransplant$numDiags)

( 392 - 19 ) / 2036
( 310 - 13 ) / 2036
( 301 - 10 ) / 2036
( 144 - 7 ) / 2036
( 96 - 0 ) / 2036

( 392 - 19 )
( 310 - 13 )
( 301 - 10 )
( 144 - 7 )
( 96 - 0 ) 



2181 - 164

17 / 2036
(2181 - 164) / 2036
2 / 2036

38 - 6
229 - 18
1931 - 140

(38 - 6) / 2036
(229 - 18) / 2036
(1931 - 140) / 2036

(165 - 5)
(2033 - 159)

(165 - 5) / 2036
(2033 - 159) / 2036


data$hasARF <- "5845" == data$p1 | "5845" == data$p2 | "5845" == data$p3 | "5845" ==  data$p4 |
		"5845" == data$p5 | "5845" == data$p6 | "5845" == data$p7 | "5845" == data$p8 | "5845" == data$p9 |
		"5845" == data$p10 | "5845" == data$p11 | "5845" ==  data$p12 | "5845" == data$p13 |
		"5845" == data$p14 | "5845" == data$p15 | "5846" == data$p1 | "5846" == data$p2 | "5846" == data$p3 | 
		"5846" ==  data$p4 | "5846" == data$p5 | "5846" == data$p6 | "5846" == data$p7 | "5846" == data$p8 | 
		"5846" == data$p9 | "5846" == data$p10 | "5846" == data$p11 | "5846" ==  data$p12 | "5846" == data$p13 |
		"5846" == data$p14 | "5846" == data$p15 | "5847" == data$p1 | "5847" == data$p2 | "5847" == data$p3 | 
		"5847" ==  data$p4 | "5847" == data$p5 | "5847" == data$p6 | "5847" == data$p7 | "5847" == data$p8 | 
		"5847" == data$p9 | "5847" == data$p10 | "5847" == data$p11 | "5847" ==  data$p12 | "5847" == data$p13 |
		"5847" == data$p14 | "5847" == data$p15 | "5848" == data$p1 | "5848" == data$p2 | "5848" == data$p3 | 
		"5848" ==  data$p4 | "5848" == data$p5 | "5848" == data$p6 | "5848" == data$p7 | "5848" == data$p8 | 
		"5848" == data$p9 | "5848" == data$p10 | "5848" == data$p11 | "5848" ==  data$p12 | "5848" == data$p13 |
		"5848" == data$p14 | "5848" == data$p15 | "5849" == data$p1 | "5849" == data$p2 | "5849" == data$p3 | 
		"5849" ==  data$p4 | "5849" == data$p5 | "5849" == data$p6 | "5849" == data$p7 | "5849" == data$p8 | 
		"5849" == data$p9 | "5849" == data$p10 | "5849" == data$p11 | "5849" ==  data$p12 | "5849" == data$p13 |
		"5849" == data$p14 | "5849" == data$p15 | "5845" == data$d1 | "5845" == data$d2 | "5845" == data$d3 | "5845" ==  data$d4 |
		"5845" == data$d5 | "5845" == data$d6 | "5845" == data$d7 | "5845" == data$d8 | "5845" == data$d9 |
		"5845" == data$d10 | "5845" == data$d11 | "5845" ==  data$d12 | "5845" == data$d13 |
		"5845" == data$d14 | "5845" == data$d15 | "5846" == data$d1 | "5846" == data$d2 | "5846" == data$d3 | 
		"5846" ==  data$d4 | "5846" == data$d5 | "5846" == data$d6 | "5846" == data$d7 | "5846" == data$d8 | 
		"5846" == data$d9 | "5846" == data$d10 | "5846" == data$d11 | "5846" ==  data$d12 | "5846" == data$d13 |
		"5846" == data$d14 | "5846" == data$d15 | "5847" == data$d1 | "5847" == data$d2 | "5847" == data$d3 | 
		"5847" ==  data$d4 | "5847" == data$d5 | "5847" == data$d6 | "5847" == data$d7 | "5847" == data$d8 | 
		"5847" == data$d9 | "5847" == data$d10 | "5847" == data$d11 | "5847" ==  data$d12 | "5847" == data$d13 |
		"5847" == data$d14 | "5847" == data$d15 | "5848" == data$d1 | "5848" == data$d2 | "5848" == data$d3 | 
		"5848" ==  data$d4 | "5848" == data$d5 | "5848" == data$d6 | "5848" == data$d7 | "5848" == data$d8 | 
		"5848" == data$d9 | "5848" == data$d10 | "5848" == data$d11 | "5848" ==  data$d12 | "5848" == data$d13 |
		"5848" == data$d14 | "5848" == data$d15 | "5849" == data$d1 | "5849" == data$d2 | "5849" == data$d3 | 
		"5849" ==  data$d4 | "5849" == data$d5 | "5849" == data$d6 | "5849" == data$d7 | "5849" == data$d8 | 
		"5849" == data$d9 | "5849" == data$d10 | "5849" == data$d11 | "5849" ==  data$d12 | "5849" == data$d13 |
		"5849" == data$d14 | "5849" == data$d15 

data$hasReoperation <- "3403" == data$p1 | "3403" == data$p2 | "3403" == data$p3 | "3403" ==  data$p4 |
		"3403" == data$p5 | "3403" == data$p6 | "3403" == data$p7 | "3403" == data$p8 | "3403" == data$p9 |
		"3403" == data$p10 | "3403" == data$p11 | "3403" ==  data$p12 | "3403" == data$p13 |
		"3403" == data$p14 | "3403" == data$p15 | "3764" == data$p1 | "3764" == data$p2 | "3764" == data$p3 | 
		"3764" ==  data$p4 | "3764" == data$p5 | "3764" == data$p6 | "3764" == data$p7 | "3764" == data$p8 | 
		"3764" == data$p9 | "3764" == data$p10 | "3764" == data$p11 | "3764" ==  data$p12 | "3764" == data$p13 |
		"3764" == data$p14 | "3764" == data$p15 | "3479" == data$p1 | "3479" == data$p2 | "3479" == data$p3 | 
		"3479" ==  data$p4 | "3479" == data$p5 | "3479" == data$p6 | "3479" == data$p7 | "3479" == data$p8 | 
		"3479" == data$p9 | "3479" == data$p10 | "3479" == data$p11 | "3479" ==  data$p12 | "3479" == data$p13 |
		"3479" == data$p14 | "3479" == data$p15 | "341" == data$p1 | "341" == data$p2 | "341" == data$p3 | 
		"341" ==  data$p4 | "341" == data$p5 | "341" == data$p6 | "341" == data$p7 | "341" == data$p8 | 
		"341" == data$p9 | "341" == data$p10 | "341" == data$p11 | "341" ==  data$p12 | "341" == data$p13 |
		"341" == data$p14 | "341" == data$p15 | "3749" == data$p1 | "3749" == data$p2 | "3749" == data$p3 | 
		"3749" ==  data$p4 | "3749" == data$p5 | "3749" == data$p6 | "3749" == data$p7 | "3749" == data$p8 | 
		"3749" == data$p9 | "3749" == data$p10 | "3749" == data$p11 | "3749" ==  data$p12 | "3749" == data$p13 |
		"3749" == data$p14 | "3749" == data$p15 | "3403" == data$d1 | "3403" == data$d2 | "3403" == data$d3 | "3403" ==  data$d4 |
		"3403" == data$d5 | "3403" == data$d6 | "3403" == data$d7 | "3403" == data$d8 | "3403" == data$d9 |
		"3403" == data$d10 | "3403" == data$d11 | "3403" ==  data$d12 | "3403" == data$d13 |
		"3403" == data$d14 | "3403" == data$d15 | "3764" == data$d1 | "3764" == data$d2 | "3764" == data$d3 | 
		"3764" ==  data$d4 | "3764" == data$d5 | "3764" == data$d6 | "3764" == data$d7 | "3764" == data$d8 | 
		"3764" == data$d9 | "3764" == data$d10 | "3764" == data$d11 | "3764" ==  data$d12 | "3764" == data$d13 |
		"3764" == data$d14 | "3764" == data$d15 | "3479" == data$d1 | "3479" == data$d2 | "3479" == data$d3 | 
		"3479" ==  data$d4 | "3479" == data$d5 | "3479" == data$d6 | "3479" == data$d7 | "3479" == data$d8 | 
		"3479" == data$d9 | "3479" == data$d10 | "3479" == data$d11 | "3479" ==  data$d12 | "3479" == data$d13 |
		"3479" == data$d14 | "3479" == data$d15 | "341" == data$d1 | "341" == data$d2 | "341" == data$d3 | 
		"341" ==  data$d4 | "341" == data$d5 | "341" == data$d6 | "341" == data$d7 | "341" == data$d8 | 
		"341" == data$d9 | "341" == data$d10 | "341" == data$d11 | "341" ==  data$d12 | "341" == data$d13 |
		"341" == data$d14 | "341" == data$d15 | "3749" == data$d1 | "3749" == data$d2 | "3749" == data$d3 | 
		"3749" ==  data$d4 | "3749" == data$d5 | "3749" == data$d6 | "3749" == data$d7 | "3749" == data$d8 | 
		"3749" == data$d9 | "3749" == data$d10 | "3749" == data$d11 | "3749" ==  data$d12 | "3749" == data$d13 |
		"3749" == data$d14 | "3749" == data$d15 

data$hasBleed <- "4513" == data$p1 | "4513" == data$p2 | "4513" == data$p3 | "4513" ==  data$p4 |
		"4513" == data$p5 | "4513" == data$p6 | "4513" == data$p7 | "4513" == data$p8 | "4513" == data$p9 |
		"4513" == data$p10 | "4513" == data$p11 | "4513" ==  data$p12 | "4513" == data$p13 |
		"4513" == data$p14 | "4513" == data$p15 | "4523" == data$p1 | "4523" == data$p2 | "4523" == data$p3 | 
		"4523" ==  data$p4 | "4523" == data$p5 | "4523" == data$p6 | "4523" == data$p7 | "4523" == data$p8 | 
		"4523" == data$p9 | "4523" == data$p10 | "4523" == data$p11 | "4523" ==  data$p12 | "4523" == data$p13 |
		"4523" == data$p14 | "4523" == data$p15 | "4513" == data$p1 | "4513" == data$p2 | "4513" == data$p3 | 
		"4513" ==  data$p4 | "4513" == data$p5 | "4513" == data$p6 | "4513" == data$p7 | "4513" == data$p8 | 
		"4513" == data$p9 | "4513" == data$p10 | "4513" == data$p11 | "4513" ==  data$p12 | "4513" == data$p13 |
		"4513" == data$p14 | "4513" == data$p15 | "9904" == data$p1 | "9904" == data$p2 | "9904" == data$p3 | 
		"9904" ==  data$p4 | "9904" == data$p5 | "9904" == data$p6 | "9904" == data$p7 | "9904" == data$p8 | 
		"9904" == data$p9 | "9904" == data$p10 | "9904" == data$p11 | "9904" ==  data$p12 | "9904" == data$p13 |
		"9904" == data$p14 | "9904" == data$p15 | "9905" == data$p1 | "9905" == data$p2 | "9905" == data$p3 | 
		"9905" ==  data$p4 | "9905" == data$p5 | "9905" == data$p6 | "9905" == data$p7 | "9905" == data$p8 | 
		"9905" == data$p9 | "9905" == data$p10 | "9905" == data$p11 | "9905" ==  data$p12 | "9905" == data$p13 |
		"9905" == data$p14 | "9905" == data$p15 | "9907" == data$p1 | "9907" == data$p2 | "9907" == data$p3 | 
		"9907" ==  data$p4 | "9907" == data$p5 | "9907" == data$p6 | "9907" == data$p7 | "9907" == data$p8 | 
		"9907" == data$p9 | "9907" == data$p10 | "9907" == data$p11 | "9907" ==  data$p12 | "9907" == data$p13 |
		"9907" == data$p14 | "9907" == data$p15 | "9909" == data$p1 | "9909" == data$p2 | "9909" == data$p3 | 
		"9909" ==  data$p4 | "9909" == data$p5 | "9909" == data$p6 | "9909" == data$p7 | "9909" == data$p8 | 
		"9909" == data$p9 | "9909" == data$p10 | "9909" == data$p11 | "9909" ==  data$p12 | "9909" == data$p13 |
		"9909" == data$p14 | "9909" == data$p15 | "4513" == data$d1 | "4513" == data$d2 | "4513" == data$d3 | "4513" ==  data$d4 |
		"4513" == data$d5 | "4513" == data$d6 | "4513" == data$d7 | "4513" == data$d8 | "4513" == data$d9 |
		"4513" == data$d10 | "4513" == data$d11 | "4513" ==  data$d12 | "4513" == data$d13 |
		"4513" == data$d14 | "4513" == data$d15 | "4523" == data$d1 | "4523" == data$d2 | "4523" == data$d3 | 
		"4523" ==  data$d4 | "4523" == data$d5 | "4523" == data$d6 | "4523" == data$d7 | "4523" == data$d8 | 
		"4523" == data$d9 | "4523" == data$d10 | "4523" == data$d11 | "4523" ==  data$d12 | "4523" == data$d13 |
		"4523" == data$d14 | "4523" == data$d15 | "4513" == data$d1 | "4513" == data$d2 | "4513" == data$d3 | 
		"4513" ==  data$d4 | "4513" == data$d5 | "4513" == data$d6 | "4513" == data$d7 | "4513" == data$d8 | 
		"4513" == data$d9 | "4513" == data$d10 | "4513" == data$d11 | "4513" ==  data$d12 | "4513" == data$d13 |
		"4513" == data$d14 | "4513" == data$d15 | "9904" == data$d1 | "9904" == data$d2 | "9904" == data$d3 | 
		"9904" ==  data$d4 | "9904" == data$d5 | "9904" == data$d6 | "9904" == data$d7 | "9904" == data$d8 | 
		"9904" == data$d9 | "9904" == data$d10 | "9904" == data$d11 | "9904" ==  data$d12 | "9904" == data$d13 |
		"9904" == data$d14 | "9904" == data$d15 | "9905" == data$d1 | "9905" == data$d2 | "9905" == data$d3 | 
		"9905" ==  data$d4 | "9905" == data$d5 | "9905" == data$d6 | "9905" == data$d7 | "9905" == data$d8 | 
		"9905" == data$d9 | "9905" == data$d10 | "9905" == data$d11 | "9905" ==  data$d12 | "9905" == data$d13 |
		"9905" == data$d14 | "9905" == data$d15 | "9907" == data$d1 | "9907" == data$d2 | "9907" == data$d3 | 
		"9907" ==  data$d4 | "9907" == data$d5 | "9907" == data$d6 | "9907" == data$d7 | "9907" == data$d8 | 
		"9907" == data$d9 | "9907" == data$d10 | "9907" == data$d11 | "9907" ==  data$d12 | "9907" == data$d13 |
		"9907" == data$d14 | "9907" == data$d15 | "9909" == data$d1 | "9909" == data$d2 | "9909" == data$d3 | 
		"9909" ==  data$d4 | "9909" == data$d5 | "9909" == data$d6 | "9909" == data$d7 | "9909" == data$d8 | 
		"9909" == data$d9 | "9909" == data$d10 | "9909" == data$d11 | "9909" ==  data$d12 | "9909" == data$d13 |
		"9909" == data$d14 | "9909" == data$d15 

data$hasRespiratoryFailure <- "51881" == data$p1 | "51881" == data$p2 | "51881" == data$p3 | "51881" ==  data$p4 |
		"51881" == data$p5 | "51881" == data$p6 | "51881" == data$p7 | "51881" == data$p8 | "51881" == data$p9 |
		"51881" == data$p10 | "51881" == data$p11 | "51881" ==  data$p12 | "51881" == data$p13 |
		"51881" == data$p14 | "51881" == data$p15 | "51881" == data$d1 | "51881" == data$d2 | "51881" == data$d3 | "51881" ==  data$d4 |
		"51881" == data$d5 | "51881" == data$d6 | "51881" == data$d7 | "51881" == data$d8 | "51881" == data$d9 |
		"51881" == data$d10 | "51881" == data$d11 | "51881" ==  data$d12 | "51881" == data$d13 |
		"51881" == data$d14 | "51881" == data$d15 

data$hasSepsis <- "99591" == data$p1 | "99591" == data$p2 | "99591" == data$p3 | "99591" ==  data$p4 |
		"99591" == data$p5 | "99591" == data$p6 | "99591" == data$p7 | "99591" == data$p8 | "99591" == data$p9 |
		"99591" == data$p10 | "99591" == data$p11 | "99591" ==  data$p12 | "99591" == data$p13 |
		"99591" == data$p14 | "99591" == data$p15 | "99592" == data$p1 | "99592" == data$p2 | "99592" == data$p3 | 
		"99592" ==  data$p4 | "99592" == data$p5 | "99592" == data$p6 | "99592" == data$p7 | "99592" == data$p8 | 
		"99592" == data$p9 | "99592" == data$p10 | "99592" == data$p11 | "99592" ==  data$p12 | "99592" == data$p13 |
		"99592" == data$p14 | "99592" == data$p15 | "99591" == data$d1 | "99591" == data$d2 | "99591" == data$d3 | "99591" ==  data$d4 |
		"99591" == data$d5 | "99591" == data$d6 | "99591" == data$d7 | "99591" == data$d8 | "99591" == data$d9 |
		"99591" == data$d10 | "99591" == data$d11 | "99591" ==  data$d12 | "99591" == data$d13 |
		"99591" == data$d14 | "99591" == data$d15 | "99592" == data$d1 | "99592" == data$d2 | "99592" == data$d3 | 
		"99592" ==  data$d4 | "99592" == data$d5 | "99592" == data$d6 | "99592" == data$d7 | "99592" == data$d8 | 
		"99592" == data$d9 | "99592" == data$d10 | "99592" == data$d11 | "99592" ==  data$d12 | "99592" == data$d13 |
		"99592" == data$d14 | "99592" == data$d15 

data$hasCardiacComp <- "9971" == data$p1 | "9971" == data$p2 | "9971" == data$p3 | "9971" ==  data$p4 |
		"9971" == data$p5 | "9971" == data$p6 | "9971" == data$p7 | "9971" == data$p8 | "9971" == data$p9 |
		"9971" == data$p10 | "9971" == data$p11 | "9971" ==  data$p12 | "9971" == data$p13 |
		"9971" == data$p14 | "9971" == data$p15 | "4294" == data$p1 | "4294" == data$p2 | "4294" == data$p3 | 
		"4294" ==  data$p4 | "4294" == data$p5 | "4294" == data$p6 | "4294" == data$p7 | "4294" == data$p8 | 
		"4294" == data$p9 | "4294" == data$p10 | "4294" == data$p11 | "4294" ==  data$p12 | "4294" == data$p13 |
		"4294" == data$p14 | "4294" == data$p15 | "9971" == data$d1 | "9971" == data$d2 | "9971" == data$d3 | "9971" ==  data$d4 |
		"9971" == data$d5 | "9971" == data$d6 | "9971" == data$d7 | "9971" == data$d8 | "9971" == data$d9 |
		"9971" == data$d10 | "9971" == data$d11 | "9971" ==  data$d12 | "9971" == data$d13 |
		"9971" == data$d14 | "9971" == data$d15 | "4294" == data$d1 | "4294" == data$d2 | "4294" == data$d3 | 
		"4294" ==  data$d4 | "4294" == data$d5 | "4294" == data$d6 | "4294" == data$d7 | "4294" == data$d8 | 
		"4294" == data$d9 | "4294" == data$d10 | "4294" == data$d11 | "4294" ==  data$d12 | "4294" == data$d13 |
		"4294" == data$d14 | "4294" == data$d15 

data$hasLiverFailure <- "570" == data$p1 | "570" == data$p2 | "570" == data$p3 | "570" ==  data$p4 |
		"570" == data$p5 | "570" == data$p6 | "570" == data$p7 | "570" == data$p8 | "570" == data$p9 |
		"570" == data$p10 | "570" == data$p11 | "570" ==  data$p12 | "570" == data$p13 |
		"570" == data$p14 | "570" == data$p15 | "570" == data$d1 | "570" == data$d2 | "570" == data$d3 | "570" ==  data$d4 |
		"570" == data$d5 | "570" == data$d6 | "570" == data$d7 | "570" == data$d8 | "570" == data$d9 |
		"570" == data$d10 | "570" == data$d11 | "570" ==  data$d12 | "570" == data$d13 |
		"570" == data$d14 | "570" == data$d15 

data$hasDeviceFailure <- "99609" == data$p1 | "99609" == data$p2 | "99609" == data$p3 | "99609" ==  data$p4 |
		"99609" == data$p5 | "99609" == data$p6 | "99609" == data$p7 | "99609" == data$p8 | "99609" == data$p9 |
		"99609" == data$p10 | "99609" == data$p11 | "99609" ==  data$p12 | "99609" == data$p13 |
		"99609" == data$p14 | "99609" == data$p15 | "99609" == data$d1 | "99609" == data$d2 | "99609" == data$d3 | "99609" ==  data$d4 |
		"99609" == data$d5 | "99609" == data$d6 | "99609" == data$d7 | "99609" == data$d8 | "99609" == data$d9 |
		"99609" == data$d10 | "99609" == data$d11 | "99609" ==  data$d12 | "99609" == data$d13 |
		"99609" == data$d14 | "99609" == data$d15 

data$hasStroke <- "43491" == data$p1 | "43491" == data$p2 | "43491" == data$p3 | "43491" ==  data$p4 |
		"43491" == data$p5 | "43491" == data$p6 | "43491" == data$p7 | "43491" == data$p8 | "43491" == data$p9 |
		"43491" == data$p10 | "43491" == data$p11 | "43491" ==  data$p12 | "43491" == data$p13 |
		"43491" == data$p14 | "43491" == data$p15 | "43491" == data$d1 | "43491" == data$d2 | "43491" == data$d3 | "43491" ==  data$d4 |
		"43491" == data$d5 | "43491" == data$d6 | "43491" == data$d7 | "43491" == data$d8 | "43491" == data$d9 |
		"43491" == data$d10 | "43491" == data$d11 | "43491" ==  data$d12 | "43491" == data$d13 |
		"43491" == data$d14 | "43491" == data$d15 



str(data[data$quartile == -99, ])
summary(data[data$quartile == -99, ])

963
803
780
518
233
234
224
62
53
963/2036
803/2036
780/2036
518/2036
233/2036
234/2036
224/2036
62/2036
53/2036


summary(data)
1051
918
817
563
252
256
236
66
55
1051/2200
918/2200
817/2200
563/2200
252/2200
256/2200
236/2200
66/2200
55/2200

data$timeDiff <- data$dayofOHT - data$dayofLVAD
data$quartile <- -99
data[ 0 <= data$timeDiff & data$timeDiff < 8, ]$quartile <- 1
data[ 8 <= data$timeDiff & data$timeDiff < 32, ]$quartile <- 2
data[ 32 <= data$timeDiff & data$timeDiff < 66, ]$quartile <- 3
data[ 66 <= data$timeDiff & data$timeDiff <= 306, ]$quartile <- 4

str(data[data$quartile == 1,])
summary(data[data$quartile == 1,])
24
28
7
8
2
7
3
0
1
24/41
28/41
7/41
8/41
2/41
7/41
3/41
0/41
1/41


str(data[data$quartile != 1 &data$quartile != -99, ])
summary(data[data$quartile != 1 &data$quartile != -99, ])
64
87
30
37
17
15
9
4
1
64/123
87/123
30/123
37/123
17/123
15/123
9/123
4/123
1/123

#### 5/14/2016 ####
### Demographics Before And After 2006 ###

before <- data[data$year < 2006,]
after <- data[data$year >= 2006,]
str(before)
str(after)

sum(before$died)
sum(before$died)/length(before$died)
sum(before$timeDiff > -1)
sum(before$timeDiff > -1)/length(before$died)
mean(before$los)
sd(before$los)
mean(before$age)
sd(before$age)
table(before$sex)
table(before$sex)/length(before$died)
table(before$race)
table(before$race)/length(before$died)
table(before$zip)
table(before$zip)/length(before$died)
mean(before$numDiags)
sd(before$numDiags)

sum(after$died)
sum(after$died)/length(after$died)
sum(after$timeDiff > -1)
sum(after$timeDiff > -1)/length(after$died)
mean(after$los)
sd(after$los)
mean(after$age)
sd(after$age)
table(after$sex)
table(after$sex)/length(after$died)
table(after$race)
table(after$race)/length(after$died)
table(after$zip)
table(after$zip)/length(after$died)
mean(after$numDiags)
sd(after$numDiags)

t.test(before$died, after$died)
t.test(before$timeDiff > -1, after$timeDiff > -1)
t.test(before$los, after$los)
t.test(before$age, after$age)
t.test(before$numDiags, after$numDiags)

prop.test(x = c(sum(before$died),sum(after$died)), 
		n = c(length(!before$died),length(!after$died)),correct = TRUE)

prop.test(x = c(sum(before$timeDiff > -1),sum(after$timeDiff > -1)), 
		n = c(length(!(before$timeDiff > -1)),length(!(after$timeDiff > -1))),correct = TRUE)

prop.test(x = c(sum(before$sex),sum(after$sex)), 
		n = c(length(!before$sex),length(!after$sex)),correct = TRUE)

chisq.test( matrix(c(3,16,4,39), ncol = 2), simulate.p.value = TRUE)

data$before2006 <- data$year < 2006
racelm <- lm(data$race ~ data$before2006)
summary(racelm)
anova(racelm)


ziplm <- lm(data$zip ~ data$before2006)
summary(ziplm )
anova(ziplm )

#GLM
str(transplants)
transplants$early <- transplants$quartile == 1
transplants$before2006 <- transplants$year < 2006

transplants[transplants$zip == -9,]$zip <- NA
transplants[transplants$race  == -9,]$zip <- NA
mortalityGLM <-lm(data = transplants, died ~ year + sex + zip + race + early)
AIC(mortalityGLM) #85
BIC(mortalityGLM) #105
mortalityGLM <-lm(data = transplants, died ~ sex + zip + race + early + numDiags)
AIC(mortalityGLM) #87
BIC(mortalityGLM) #108
mortalityGLM <-lm(data = transplants, died ~ year + sex + zip + race + early + numDiags)
AIC(mortalityGLM) #86
BIC(mortalityGLM) #109
mortalityGLM <-lm(data = transplants, died ~ before2006 + sex + zip + race + early + numDiags)
AIC(mortalityGLM) #87
BIC(mortalityGLM) #110
mortalityGLM <-lm(data = transplants, died ~ age + before2006 + sex + zip + race + early + numDiags)
AIC(mortalityGLM) #87
BIC(mortalityGLM) #113
summary(mortalityGLM)
mortalityGLM <-lm(data = transplants, died ~ before2006 + numDiags  + early + age)
AIC(mortalityGLM) #134
BIC(mortalityGLM) #153
summary(mortalityGLM)

sum(transplants[transplants$sex == 1,]$died)
length(transplants[transplants$sex == 1,]$died)
sum(transplants[transplants$sex == 0,]$died)
length(transplants[transplants$sex == 0,]$died)


demographics <- read.csv("LVADDemographics-ForAnalysis.csv", stringsAsFactors = TRUE)
str(demographics)
table(demographics[demographics$year < 2006,]$location)
table(demographics[demographics$year < 2006,]$location)/length(demographics[demographics$year < 2006,]$location)
table(demographics[demographics$year < 2006,]$bedsize)
table(demographics[demographics$year < 2006,]$bedsize)/length(demographics[demographics$year < 2006,]$bedsize)
table(demographics[demographics$year < 2006,]$teach)
table(demographics[demographics$year < 2006,]$teach)/length(demographics[demographics$year < 2006,]$teach)

table(demographics[demographics$year >= 2006,]$location)
table(demographics[demographics$year >= 2006,]$location)/length(demographics[demographics$year >= 2006,]$location)
table(demographics[demographics$year >= 2006,]$bedsize)
table(demographics[demographics$year >= 2006,]$bedsize)/length(demographics[demographics$year >= 2006,]$bedsize)
table(demographics[demographics$year >= 2006,]$teach)
table(demographics[demographics$year >= 2006,]$teach)/length(demographics[demographics$year >= 2006,]$teach)


chisq.test( matrix(c(5,583,1,12,1598,1), ncol = 2))
chisq.test( matrix(c(20,67,501,1,18,162,1430,1), ncol = 2))
chisq.test( matrix(c(75,513,1,90,1520,1), ncol = 2))


demographics[is.na(demographics$p1),]$p1 <- " " 
demographics[is.na(demographics$p2),]$p2 <- " " 
demographics[is.na(demographics$p3),]$p3 <- " " 
demographics[is.na(demographics$p4),]$p4 <- " " 
demographics[is.na(demographics$p5),]$p5 <- " " 
demographics[is.na(demographics$p6),]$p6 <- " " 
demographics[is.na(demographics$p7),]$p7 <- " " 
demographics[is.na(demographics$p8),]$p8 <- " " 
demographics[is.na(demographics$p9),]$p9 <- " " 
demographics[is.na(demographics$p10),]$p10 <- " " 
demographics[is.na(demographics$p11),]$p11 <- " " 
demographics[is.na(demographics$p12),]$p12 <- " " 
demographics[is.na(demographics$p13),]$p13 <- " " 
demographics[is.na(demographics$p14),]$p14 <- " " 
demographics[is.na(demographics$p15),]$p15 <- " " 



demographics$Diabetes <- ("250" == substr(demographics$p1,1,3) | "250" == substr(demographics$p2,1,3) | "250" == substr(demographics$p3,1,3) | "250" ==  substr(demographics$p4,1,3) |
		"250" == substr(demographics$p5,1,3) | "250" == substr(demographics$p6,1,3) | "250" == substr(demographics$p7,1,3) | "250" == substr(demographics$p8,1,3) | "250" == substr(demographics$p9,1,3) |
		"250" == substr(demographics$p10,1,3) | "250" == substr(demographics$p11,1,3) | "250" ==  substr(demographics$p12,1,3) | "250" == substr(demographics$p13,1,3) | "250" ==  substr(demographics$d4,1,3) |
		"250" == substr(demographics$p14,1,3) | "250" == substr(demographics$p15,1,3) | "250" == substr(demographics$d1,1,3) | "250" == substr(demographics$d2,1,3) | "250" == substr(demographics$d3,1,3) | 
		"250" == substr(demographics$d5,1,3) | "250" == substr(demographics$d6,1,3) | "250" == substr(demographics$d7,1,3) | "250" == substr(demographics$d8,1,3) | "250" == substr(demographics$d9,1,3) |
		"250" == substr(demographics$d10,1,3) | "250" == substr(demographics$d11,1,3) | "250" ==  substr(demographics$d12,1,3) | "250" == substr(demographics$d13,1,3) |
		"250" == substr(demographics$d14,1,3) | "250" == substr(demographics$d15,1,3) )


demographics$Hyperlipidemia <- ("272" == substr(demographics$p1,1,3) | "272" == substr(demographics$p2,1,3) | "272" == substr(demographics$p3,1,3) | "272" ==  substr(demographics$p4,1,3) |
		"272" == substr(demographics$p5,1,3) | "272" == substr(demographics$p6,1,3) | "272" == substr(demographics$p7,1,3) | "272" == substr(demographics$p8,1,3) | "272" == substr(demographics$p9,1,3) |
		"272" == substr(demographics$p10,1,3) | "272" == substr(demographics$p11,1,3) | "272" ==  substr(demographics$p12,1,3) | "272" == substr(demographics$p13,1,3) | "272" ==  substr(demographics$d4,1,3) |
		"272" == substr(demographics$p14,1,3) | "272" == substr(demographics$p15,1,3) | "272" == substr(demographics$d1,1,3) | "272" == substr(demographics$d2,1,3) | "272" == substr(demographics$d3,1,3) | 
		"272" == substr(demographics$d5,1,3) | "272" == substr(demographics$d6,1,3) | "272" == substr(demographics$d7,1,3) | "272" == substr(demographics$d8,1,3) | "272" == substr(demographics$d9,1,3) |
		"272" == substr(demographics$d10,1,3) | "272" == substr(demographics$d11,1,3) | "272" ==  substr(demographics$d12,1,3) | "272" == substr(demographics$d13,1,3) |
		"272" == substr(demographics$d14,1,3) | "272" == substr(demographics$d15,1,3) )



demographics$Hypertension <- ("401" == substr(demographics$p1,1,3) | "401" == substr(demographics$p2,1,3) | "401" == substr(demographics$p3,1,3) | "401" ==  substr(demographics$p4,1,3) |
		"401" == substr(demographics$p5,1,3) | "401" == substr(demographics$p6,1,3) | "401" == substr(demographics$p7,1,3) | "401" == substr(demographics$p8,1,3) | "401" == substr(demographics$p9,1,3) |
		"401" == substr(demographics$p10,1,3) | "401" == substr(demographics$p11,1,3) | "401" ==  substr(demographics$p12,1,3) | "401" == substr(demographics$p13,1,3) | "401" ==  substr(demographics$d4,1,3) |
		"401" == substr(demographics$p14,1,3) | "401" == substr(demographics$p15,1,3) | "401" == substr(demographics$d1,1,3) | "401" == substr(demographics$d2,1,3) | "401" == substr(demographics$d3,1,3) | 
		"401" == substr(demographics$d5,1,3) | "401" == substr(demographics$d6,1,3) | "401" == substr(demographics$d7,1,3) | "401" == substr(demographics$d8,1,3) | "401" == substr(demographics$d9,1,3) |
		"401" == substr(demographics$d10,1,3) | "401" == substr(demographics$d11,1,3) | "401" ==  substr(demographics$d12,1,3) | "401" == substr(demographics$d13,1,3) |
		"401" == substr(demographics$d14,1,3) | "401" == substr(demographics$d15,1,3) )

demographics$Hypertension <- ("401" == substr(demographics$p1,1,3) | "401" == substr(demographics$p2,1,3) | "401" == substr(demographics$p3,1,3) | "401" ==  substr(demographics$p4,1,3) |
		"401" == substr(demographics$p5,1,3) | "401" == substr(demographics$p6,1,3) | "401" == substr(demographics$p7,1,3) | "401" == substr(demographics$p8,1,3) | "401" == substr(demographics$p9,1,3) |
		"401" == substr(demographics$p10,1,3) | "401" == substr(demographics$p11,1,3) | "401" ==  substr(demographics$p12,1,3) | "401" == substr(demographics$p13,1,3) | "401" ==  substr(demographics$d4,1,3) |
		"401" == substr(demographics$p14,1,3) | "401" == substr(demographics$p15,1,3) | "401" == substr(demographics$d1,1,3) | "401" == substr(demographics$d2,1,3) | "401" == substr(demographics$d3,1,3) | 
		"401" == substr(demographics$d5,1,3) | "401" == substr(demographics$d6,1,3) | "401" == substr(demographics$d7,1,3) | "401" == substr(demographics$d8,1,3) | "401" == substr(demographics$d9,1,3) |
		"401" == substr(demographics$d10,1,3) | "401" == substr(demographics$d11,1,3) | "401" ==  substr(demographics$d12,1,3) | "401" == substr(demographics$d13,1,3) |
		"401" == substr(demographics$d14,1,3) | "401" == substr(demographics$d15,1,3) )

demographics$Smoking <- ("V1582" == substr(demographics$p1,1,5) | "V1582" == substr(demographics$p2,1,5) | "V1582" == substr(demographics$p3,1,5) | "V1582" ==  substr(demographics$p4,1,5) |
		"V1582" == substr(demographics$p5,1,5) | "V1582" == substr(demographics$p6,1,5) | "V1582" == substr(demographics$p7,1,5) | "V1582" == substr(demographics$p8,1,5) | "V1582" == substr(demographics$p9,1,5) |
		"V1582" == substr(demographics$p10,1,5) | "V1582" == substr(demographics$p11,1,5) | "V1582" ==  substr(demographics$p12,1,5) | "V1582" == substr(demographics$p13,1,5) | "V1582" ==  substr(demographics$d4,1,5) |
		"V1582" == substr(demographics$p14,1,5) | "V1582" == substr(demographics$p15,1,5) | "V1582" == substr(demographics$d1,1,5) | "V1582" == substr(demographics$d2,1,5) | "V1582" == substr(demographics$d5,1,5) | 
		"V1582" == substr(demographics$d5,1,5) | "V1582" == substr(demographics$d6,1,5) | "V1582" == substr(demographics$d7,1,5) | "V1582" == substr(demographics$d8,1,5) | "V1582" == substr(demographics$d9,1,5) |
		"V1582" == substr(demographics$d10,1,5) | "V1582" == substr(demographics$d11,1,5) | "V1582" ==  substr(demographics$d12,1,5) | "V1582" == substr(demographics$d15,1,5) |
		"V1582" == substr(demographics$d14,1,5) | "V1582" == substr(demographics$d15,1,5) ) | ("3051" == substr(demographics$p1,1,4) | "3051" == substr(demographics$p2,1,4) | "3051" == substr(demographics$p3,1,4) | "3051" ==  substr(demographics$p4,1,4) |
		"3051" == substr(demographics$p5,1,4) | "3051" == substr(demographics$p6,1,4) | "3051" == substr(demographics$p7,1,4) | "3051" == substr(demographics$p8,1,4) | "3051" == substr(demographics$p9,1,4) |
		"3051" == substr(demographics$p10,1,4) | "3051" == substr(demographics$p11,1,4) | "3051" ==  substr(demographics$p12,1,4) | "3051" == substr(demographics$p13,1,4) | "3051" ==  substr(demographics$d3,1,4) |
		"3051" == substr(demographics$p14,1,4) | "3051" == substr(demographics$p15,1,4) | "3051" == substr(demographics$d1,1,4) | "3051" == substr(demographics$d2,1,4) | "3051" == substr(demographics$d5,1,4) | 
		"3051" == substr(demographics$d4,1,4) | "3051" == substr(demographics$d6,1,4) | "3051" == substr(demographics$d7,1,4) | "3051" == substr(demographics$d8,1,4) | "3051" == substr(demographics$d9,1,4) |
		"3051" == substr(demographics$d10,1,4) | "3051" == substr(demographics$d11,1,4) | "3051" ==  substr(demographics$d12,1,4) | "3051" == substr(demographics$d13,1,4) |
		"3051" == substr(demographics$d14,1,4) | "3051" == substr(demographics$d15,1,4) )

demographics$Obesity <- ("278" == substr(demographics$p1,1,3) | "278" == substr(demographics$p2,1,3) | "278" == substr(demographics$p3,1,3) | "278" ==  substr(demographics$p4,1,3) |
		"278" == substr(demographics$p5,1,3) | "278" == substr(demographics$p6,1,3) | "278" == substr(demographics$p7,1,3) | "278" == substr(demographics$p8,1,3) | "278" == substr(demographics$p9,1,3) |
		"278" == substr(demographics$p10,1,3) | "278" == substr(demographics$p11,1,3) | "278" ==  substr(demographics$p12,1,3) | "278" == substr(demographics$p13,1,3) | "278" ==  substr(demographics$d4,1,3) |
		"278" == substr(demographics$p14,1,3) | "278" == substr(demographics$p15,1,3) | "278" == substr(demographics$d1,1,3) | "278" == substr(demographics$d2,1,3) | "278" == substr(demographics$d3,1,3) | 
		"278" == substr(demographics$d5,1,3) | "278" == substr(demographics$d6,1,3) | "278" == substr(demographics$d7,1,3) | "278" == substr(demographics$d8,1,3) | "278" == substr(demographics$d9,1,3) |
		"278" == substr(demographics$d10,1,3) | "278" == substr(demographics$d11,1,3) | "278" ==  substr(demographics$d12,1,3) | "278" == substr(demographics$d13,1,3) |
		"278" == substr(demographics$d14,1,3) | "278" == substr(demographics$d15,1,3) )


sum(demographics$Diabetes)
sum(demographics$Hyperlipidemia )
sum(demographics$Hypertension )
sum(demographics$Smoking )
sum(demographics$Obesity )


sum(demographics$Diabetes) /2200
sum(demographics$Hyperlipidemia ) /2200
sum(demographics$Hypertension ) /2200
sum(demographics$Smoking ) /2200
sum(demographics$Obesity ) /2200



sum(demographics[demographics$year < 2006,]$Diabetes)
sum(demographics[demographics$year < 2006,]$Hyperlipidemia )
sum(demographics[demographics$year < 2006,]$Hypertension )
sum(demographics[demographics$year < 2006,]$Smoking )
sum(demographics[demographics$year < 2006,]$Obesity )


sum(demographics[demographics$year < 2006,]$Diabetes) /589
sum(demographics[demographics$year < 2006,]$Hyperlipidemia ) /589
sum(demographics[demographics$year < 2006,]$Hypertension ) /589
sum(demographics[demographics$year < 2006,]$Smoking ) /589
sum(demographics[demographics$year < 2006,]$Obesity ) /589


sum(demographics[demographics$year >= 2006,]$Diabetes)
sum(demographics[demographics$year >= 2006,]$Hyperlipidemia )
sum(demographics[demographics$year >= 2006,]$Hypertension )
sum(demographics[demographics$year >= 2006,]$Smoking )
sum(demographics[demographics$year >= 2006,]$Obesity )


sum(demographics[demographics$year >= 2006,]$Diabetes) /1611
sum(demographics[demographics$year >= 2006,]$Hyperlipidemia ) /1611
sum(demographics[demographics$year >= 2006,]$Hypertension ) /1611
sum(demographics[demographics$year >= 2006,]$Smoking ) /1611
sum(demographics[demographics$year >= 2006,]$Obesity ) /1611

before <- demographics[demographics$year < 2006,]
after <- demographics[demographics$year >= 2006,]
prop.test(x = c(sum(before$Diabetes),sum(after$Diabetes)), 
		n = c(length(!before$Diabetes),length(!after$Diabetes)),correct = TRUE)
prop.test(x = c(sum(before$Hyperlipidemia),sum(after$Hyperlipidemia)), 
		n = c(length(!before$Hyperlipidemia),length(!after$Hyperlipidemia)),correct = TRUE)
prop.test(x = c(sum(before$Hypertension),sum(after$Hypertension)), 
		n = c(length(!before$Hypertension),length(!after$Hypertension)),correct = TRUE)
prop.test(x = c(sum(before$Smoking),sum(after$Smoking)), 
		n = c(length(!before$Smoking),length(!after$Smoking)),correct = TRUE)
prop.test(x = c(sum(before$Obesity ),sum(after$Obesity )), 
		n = c(length(!before$Obesity ),length(!after$Obesity )),correct = TRUE)



### FULL VS SAMPLE ###




data <- read.csv("unweightedLVAD.csv")
data$X.1 <- 1:3312
data2 <- read.csv("unweightedLVAD-fullDates.csv")
data <- data[!data$X.1 %in% data2$X.1,]
data <- data[data$age>=18,]
data2 <- data2[data2$age>=18,]
str(data)
str(data2)

prop.test(x = c(sum(data$died), sum(data2$died)), n = c(sum(!data$died), sum(!data2$died)), correct = TRUE)
prop.test(x = c(164,  267 ), n = c(2200 - 164, 3219 - 267), correct = TRUE)
t.test(data[data$los > -1,]$los, data2[data2$los > -1,]$los)
t.test(data[data$age > -1,]$age, data2[data2$age > -1,]$age)



chisq.test( matrix(c(1659,541,2434,772), ncol = 2))
chisq.test( matrix(c(1274,352,142,51,5,376,1742,444,169,57,11,783), ncol = 2))
chisq.test( matrix(c(475,491,552,631,51,637,738,832,932,67), ncol = 2))



chisq.test( matrix(c(392,535,2200 - 392, 3219 - 535), ncol = 2))

chisq.test( matrix(c(310,420,2200 - 310, 3219 - 420), ncol = 2))

chisq.test( matrix(c(301,418,2200 - 301, 3219 - 418), ncol = 2))

chisq.test( matrix(c(144,201,2200 - 144, 3219 - 201), ncol = 2))

chisq.test( matrix(c(96,130,2200 - 96, 3219 - 130), ncol = 2))

#http://www.graphpad.com/quickcalcs/ttest2/