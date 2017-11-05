setwd("C:\\Users\\DeNovo\\Dropbox\\NIS-Small\\LVAD")
setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD")

data <- read.csv("unweightedPVAD.csv")


setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD")
library(ggplot2)
library(stringr)


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



data$dayofPVAD <- -99


data["3768" == str_trim(data$p1),]$dayofPVAD <- data["3768" == str_trim(data$p1),]$p1day 
data["3768" == str_trim(data$p2),]$dayofPVAD <- data["3768" == str_trim(data$p2),]$p2day 
data["3768" == str_trim(data$p3),]$dayofPVAD <- data["3768" == str_trim(data$p3),]$p3day 
data["3768" == str_trim(data$p4),]$dayofPVAD <- data["3768" == str_trim(data$p4),]$p4day 
data["3768" == str_trim(data$p5),]$dayofPVAD <- data["3768" == str_trim(data$p5),]$p5day 
data["3768" == str_trim(data$p6),]$dayofPVAD <- data["3768" == str_trim(data$p6),]$p6day 
data["3768" == str_trim(data$p7),]$dayofPVAD <- data["3768" == str_trim(data$p7),]$p7day 
data["3768" == str_trim(data$p8),]$dayofPVAD <- data["3768" == str_trim(data$p8),]$p8day 
data["3768" == str_trim(data$p9),]$dayofPVAD <- data["3768" == str_trim(data$p9),]$p9day 
data["3768" == str_trim(data$p10),]$dayofPVAD <- data["3768" == str_trim(data$p10),]$p10day 
data["3768" == str_trim(data$p11),]$dayofPVAD <- data["3768" == str_trim(data$p11),]$p11day 
data["3768" == str_trim(data$p12),]$dayofPVAD <- data["3768" == str_trim(data$p12),]$p12day 
data["3768" == str_trim(data$p13),]$dayofPVAD <- data["3768" == str_trim(data$p13),]$p13day 
data["3768" == str_trim(data$p14),]$dayofPVAD <- data["3768" == str_trim(data$p14),]$p14day 
data["3768" == str_trim(data$p15),]$dayofPVAD <- data["3768" == str_trim(data$p15),]$p15day 

length(data$p1day )
sum(data$p1day == -99 & data$p2day == -99 & data$p3day == -99 & data$p4day == -99 & data$p5day == -99 & 
data$p6day == -99 & data$p7day == -99 & data$p8day == -99 & data$p9day == -99 & data$p10day == -99 & 
data$p11day == -99 & data$p12day == -99 & data$p13day == -99 & data$p14day == -99 & data$p15day == -99)

data <- data[data$dayofPVAD >=0 ,]
length(data$p1day )



data$hasIABP <- "3761" == data$p1 | "3761" == data$p2 | "3761" == data$p3 | 
		"3761" ==  data$p4 | "3761" == data$p5 | "3761" == data$p6 | "3761" == data$p7 | "3761" == data$p8 | 
		"3761" == data$p9 | "3761" == data$p10 | "3761" == data$p11 | "3761" ==  data$p12 | "3761" == data$p13 |
		"3761" == data$p14 | "3761" == data$p15 

data$hasShock <- "78551" == data$d1 | "78551" == data$d2 | "78551" == data$d3 | 
		"78551" ==  data$d4 | "78551" == data$d5 | "78551" == data$d6 | "78551" == data$d7 | "78551" == data$d8 | 
		"78551" == data$d9 | "78551" == data$d10 | "78551" == data$d11 | "78551" ==  data$d12 | "78551" == data$d13 |
		"78551" == data$d14 | "78551" == data$d15 

data$hasAMI <- "410" == substring(data$d1,1,3) | "410" == substring(data$d2,1,3) | "410" == substring(data$d3,1,3) | 
		"410" ==  substring(data$d4,1,3) | "410" == substring(data$d5,1,3) | "410" == substring(data$d6,1,3) | "410" == substring(data$d7,1,3) | "410" == substring(data$d8,1,3) | 
		"410" == substring(data$d9,1,3) | "410" == substring(data$d10,1,3) | "410" == substring(data$d11,1,3) | "410" ==  substring(data$d12,1,3) | "410" == substring(data$d13,1,3) |
		"410" == substring(data$d14,1,3) | "410" == substring(data$d15,1,3) 

data<- data[data$age >=18 & !data$hasIABP,]
length(data$p1day )

pvad <- data



data <- read.csv("unweightedIABP.csv")
data<- data[data$age >=18,]

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

length(data$p1day )

sum(data$p1day == -99 & data$p2day == -99 & data$p3day == -99 & data$p4day == -99 & data$p5day == -99 & 
data$p6day == -99 & data$p7day == -99 & data$p8day == -99 & data$p9day == -99 & data$p10day == -99 & 
data$p11day == -99 & data$p12day == -99 & data$p13day == -99 & data$p14day == -99 & data$p15day == -99)

data <- data[!(data$p1day == -99 & data$p2day == -99 & data$p3day == -99 & data$p4day == -99 & data$p5day == -99 & 
data$p6day == -99 & data$p7day == -99 & data$p8day == -99 & data$p9day == -99 & data$p10day == -99 & 
data$p11day == -99 & data$p12day == -99 & data$p13day == -99 & data$p14day == -99 & data$p15day == -99),]

length(data$p1day )

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



data$hasShock <- "78551" == data$d1 | "78551" == data$d2 | "78551" == data$d3 | 
		"78551" ==  data$d4 | "78551" == data$d5 | "78551" == data$d6 | "78551" == data$d7 | "78551" == data$d8 | 
		"78551" == data$d9 | "78551" == data$d10 | "78551" == data$d11 | "78551" ==  data$d12 | "78551" == data$d13 |
		"78551" == data$d14 | "78551" == data$d15 

data$hasAMI <- "410" == substring(data$d1,1,3) | "410" == substring(data$d2,1,3) | "410" == substring(data$d3,1,3) | 
		"410" ==  substring(data$d4,1,3) | "410" == substring(data$d5,1,3) | "410" == substring(data$d6,1,3) | "410" == substring(data$d7,1,3) | "410" == substring(data$d8,1,3) | 
		"410" == substring(data$d9,1,3) | "410" == substring(data$d10,1,3) | "410" == substring(data$d11,1,3) | "410" ==  substring(data$d12,1,3) | "410" == substring(data$d13,1,3) |
		"410" == substring(data$d14,1,3) | "410" == substring(data$d15,1,3) 

data$hasPVAD <- "3768" == data$p1 | "3768" == data$p2 | "3768" == data$p3 | 
		"3768" ==  data$p4 | "3768" == data$p5 | "3768" == data$p6 | "3768" == data$p7 | "3768" == data$p8 | 
		"3768" == data$p9 | "3768" == data$p10 | "3768" == data$p11 | "3768" ==  data$p12 | "3768" == data$p13 |
		"3768" == data$p14 | "3768" == data$p15 

data<- data[data$age >=18 & !data$hasPVAD,]
iabp <- data

pvad <- pvad[pvad$hasShock | pvad$hasAMI,]
length(pvad$p1day)
length(pvad[pvad$hasShock,]$p1day)
length(pvad[!pvad$hasShock & pvad$hasAMI,]$p1day)
iabp <- iabp[iabp$hasShock | iabp$hasAMI,]
length(iabp$p1day)
length(iabp[iabp$hasShock,]$p1day)
length(iabp[!iabp$hasShock & iabp$hasAMI,]$p1day)


542+97839

t.test(pvad$age, iabp$age)
t.test(pvad$numDiags, iabp$numDiags )

prop.test(x = c(sum(pvad$died), sum(iabp$died)), 
n = c(length(!pvad$died), length(!iabp$died)), correct = TRUE)

sum(pvad$died)
sum(iabp$died)


prop.test(x = c(sum(pvad$sex), sum(iabp$sex)), 
n = c(length(!pvad$died), length(!iabp$died)), correct = TRUE)

prop.test(x = c(sum(pvad$race == 1), sum(iabp$race == 1)), 
n = c(length(!pvad$died), length(!iabp$died)), correct = TRUE)

sum(pvad$sex)
155/543
