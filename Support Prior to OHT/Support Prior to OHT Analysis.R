setwd("C:\\Users\\DeNovo\\Dropbox\\NIS-Small\\LVAD")
setwd("C:\\Users\\David\\Dropbox\\StatisticalAnalysis\\NIS-Small\\LVAD\\")

data <- read.csv("unweightedOHT-total.csv")


#setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD")
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



data$hasOHT <- "375" == str_trim(data$p1) | "375" == str_trim(data$p2) | "375" == str_trim(data$p3) | "375" ==  str_trim(data$p4) |
		"375" == str_trim(data$p5) | "375" == str_trim(data$p6) | "375" == str_trim(data$p7) | "375" == str_trim(data$p8) | "375" == str_trim(data$p9) |
		"375" == str_trim(data$p10) | "375" == str_trim(data$p11) | "375" ==  str_trim(data$p12) | "375" == str_trim(data$p13) |
		"375" == str_trim(data$p14) | "375" == str_trim(data$p15) | "3751" == str_trim(data$p1) | "3751" == str_trim(data$p2) | "3751" == str_trim(data$p3) | 
		"3751" ==  str_trim(data$p4) | "3751" == str_trim(data$p5) | "3751" == str_trim(data$p6) | "3751" == str_trim(data$p7) | "3751" == str_trim(data$p8) | 
		"3751" == str_trim(data$p9) | "3751" == str_trim(data$p10) | "3751" == str_trim(data$p11) | "3751" ==  str_trim(data$p12) | "3751" == str_trim(data$p13) |
		"3751" == str_trim(data$p14) | "3751" == str_trim(data$p15) 

data = data[data$hasOHT,]

# We are not including combined heart-lung transplants
#	| "336" == str_trim(data$p1) | "336" == str_trim(data$p2) | "336" == str_trim(data$p3) | 
#		"336" ==  str_trim(data$p4) | "336" == str_trim(data$p5) | "336" == str_trim(data$p6) | "336" == str_trim(data$p7) | "336" == str_trim(data$p8) | 
#		"336" == str_trim(data$p9) | "336" == str_trim(data$p10) | "336" == str_trim(data$p11) | "336" ==  str_trim(data$p12) | "336" == str_trim(data$p13) |
#		"336" == str_trim(data$p14) | "336" == str_trim(data$p15) 


data$hasECMO <- "3961" == str_trim(data$p1) | "3961" == str_trim(data$p2) | "3961" == str_trim(data$p3) | "3961" ==  str_trim(data$p4) |
		"3961" == str_trim(data$p5) | "3961" == str_trim(data$p6) | "3961" == str_trim(data$p7) | "3961" == str_trim(data$p8) | "3961" == str_trim(data$p9) |
		"3961" == str_trim(data$p10) | "3961" == str_trim(data$p11) | "3961" ==  str_trim(data$p12) | "3961" == str_trim(data$p13) |
		"3961" == str_trim(data$p14) | "3961" == data$p15

data$hasIABP <- "3761" == str_trim(data$p1) | "3761" == str_trim(data$p2) | "3761" == str_trim(data$p3) | "3761" ==  str_trim(data$p4) |
		"3761" == str_trim(data$p5) | "3761" == str_trim(data$p6) | "3761" == str_trim(data$p7) | "3761" == str_trim(data$p8) | "3761" == str_trim(data$p9) |
		"3761" == str_trim(data$p10) | "3761" == str_trim(data$p11) | "3761" ==  str_trim(data$p12) | "3761" == str_trim(data$p13) |
		"3761" == str_trim(data$p14) | "3761" == data$p15

data$hasPVAD <- "3768" == str_trim(data$p1) | "3768" == str_trim(data$p2) | "3768" == str_trim(data$p3) | "3768" ==  str_trim(data$p4) |
		"3768" == str_trim(data$p5) | "3768" == str_trim(data$p6) | "3768" == str_trim(data$p7) | "3768" == str_trim(data$p8) | "3768" == str_trim(data$p9) |
		"3768" == str_trim(data$p10) | "3768" == str_trim(data$p11) | "3768" ==  str_trim(data$p12) | "3768" == str_trim(data$p13) |
		"3768" == str_trim(data$p14) | "3768" == data$p15

data$hasARF <- "5845" == str_trim(data$p1) | "5845" == str_trim(data$p2) | "5845" == str_trim(data$p3) | "5845" ==  str_trim(data$p4) |
		"5845" == str_trim(data$p5) | "5845" == str_trim(data$p6) | "5845" == str_trim(data$p7) | "5845" == str_trim(data$p8) | "5845" == str_trim(data$p9) |
		"5845" == str_trim(data$p10) | "5845" == str_trim(data$p11) | "5845" ==  str_trim(data$p12) | "5845" == str_trim(data$p13) |
		"5845" == str_trim(data$p14) | "5845" == str_trim(data$p15) | "5846" == str_trim(data$p1) | "5846" == str_trim(data$p2) | "5846" == str_trim(data$p3) | 
		"5846" ==  str_trim(data$p4) | "5846" == str_trim(data$p5) | "5846" == str_trim(data$p6) | "5846" == str_trim(data$p7) | "5846" == str_trim(data$p8) | 
		"5846" == str_trim(data$p9) | "5846" == str_trim(data$p10) | "5846" == str_trim(data$p11) | "5846" ==  str_trim(data$p12) | "5846" == str_trim(data$p13) |
		"5846" == str_trim(data$p14) | "5846" == str_trim(data$p15) | "5847" == str_trim(data$p1) | "5847" == str_trim(data$p2) | "5847" == str_trim(data$p3) | 
		"5847" ==  str_trim(data$p4) | "5847" == str_trim(data$p5) | "5847" == str_trim(data$p6) | "5847" == str_trim(data$p7) | "5847" == str_trim(data$p8) | 
		"5847" == str_trim(data$p9) | "5847" == str_trim(data$p10) | "5847" == str_trim(data$p11) | "5847" ==  str_trim(data$p12) | "5847" == str_trim(data$p13) |
		"5847" == str_trim(data$p14) | "5847" == str_trim(data$p15) | "5848" == str_trim(data$p1) | "5848" == str_trim(data$p2) | "5848" == str_trim(data$p3) | 
		"5848" ==  str_trim(data$p4) | "5848" == str_trim(data$p5) | "5848" == str_trim(data$p6) | "5848" == str_trim(data$p7) | "5848" == str_trim(data$p8) | 
		"5848" == str_trim(data$p9) | "5848" == str_trim(data$p10) | "5848" == str_trim(data$p11) | "5848" ==  str_trim(data$p12) | "5848" == str_trim(data$p13) |
		"5848" == str_trim(data$p14) | "5848" == str_trim(data$p15) | "5849" == str_trim(data$p1) | "5849" == str_trim(data$p2) | "5849" == str_trim(data$p3) | 
		"5849" ==  str_trim(data$p4) | "5849" == str_trim(data$p5) | "5849" == str_trim(data$p6) | "5849" == str_trim(data$p7) | "5849" == str_trim(data$p8) | 
		"5849" == str_trim(data$p9) | "5849" == str_trim(data$p10) | "5849" == str_trim(data$p11) | "5849" ==  str_trim(data$p12) | "5849" == str_trim(data$p13) |
		"5849" == str_trim(data$p14) | "5849" == str_trim(data$p15) | "5845" == data$d1 | "5845" == data$d2 | "5845" == data$d3 | "5845" ==  data$d4 |
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

data$hasReoperation <- "3403" == str_trim(data$p1) | "3403" == str_trim(data$p2) | "3403" == str_trim(data$p3) | "3403" ==  str_trim(data$p4) |
		"3403" == str_trim(data$p5) | "3403" == str_trim(data$p6) | "3403" == str_trim(data$p7) | "3403" == str_trim(data$p8) | "3403" == str_trim(data$p9) |
		"3403" == str_trim(data$p10) | "3403" == str_trim(data$p11) | "3403" ==  str_trim(data$p12) | "3403" == str_trim(data$p13) |
		"3403" == str_trim(data$p14) | "3403" == str_trim(data$p15)  | "3479" == str_trim(data$p1) | "3479" == str_trim(data$p2) | "3479" == str_trim(data$p3) | 
		"3479" ==  str_trim(data$p4) | "3479" == str_trim(data$p5) | "3479" == str_trim(data$p6) | "3479" == str_trim(data$p7) | "3479" == str_trim(data$p8) | 
		"3479" == str_trim(data$p9) | "3479" == str_trim(data$p10) | "3479" == str_trim(data$p11) | "3479" ==  str_trim(data$p12) | "3479" == str_trim(data$p13) |
		"3479" == str_trim(data$p14) | "3479" == str_trim(data$p15) | "341" == str_trim(data$p1) | "341" == str_trim(data$p2) | "341" == str_trim(data$p3) | 
		"341" ==  str_trim(data$p4) | "341" == str_trim(data$p5) | "341" == str_trim(data$p6) | "341" == str_trim(data$p7) | "341" == str_trim(data$p8) | 
		"341" == str_trim(data$p9) | "341" == str_trim(data$p10) | "341" == str_trim(data$p11) | "341" ==  str_trim(data$p12) | "341" == str_trim(data$p13) |
		"341" == str_trim(data$p14) | "341" == str_trim(data$p15) | "3749" == str_trim(data$p1) | "3749" == str_trim(data$p2) | "3749" == str_trim(data$p3) | 
		"3749" ==  str_trim(data$p4) | "3749" == str_trim(data$p5) | "3749" == str_trim(data$p6) | "3749" == str_trim(data$p7) | "3749" == str_trim(data$p8) | 
		"3749" == str_trim(data$p9) | "3749" == str_trim(data$p10) | "3749" == str_trim(data$p11) | "3749" ==  str_trim(data$p12) | "3749" == str_trim(data$p13) |
		"3749" == str_trim(data$p14) | "3749" == str_trim(data$p15) | "3403" == data$d1 | "3403" == data$d2 | "3403" == data$d3 | "3403" ==  data$d4 |
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


transfusionCodes <- c("99811","99812","56881","5967","59970","59971", "7863", "7847","4590", 
"53021", "4560","5307","53082","7581","7580","5789","45620","53100","53101", "53120", "53121",
"53140", "53141", "53160", "53161", "53200","53201", "53220", "53221", "53240", "53241", "53260",
"53261","53300","53301","53320","53321","53340","53341","53360","53361","53400","53401","53420",
"53421","53440","53441","53460","53461","53501","53511","53521","53531","53541","53551","53561",
"53571","53783","56202","56203","56212","56213","5693","56985","53784","56986")


strokeCodes <- c("430","431","4320","4321","4329", "4340","43491",
"4330","4331","4332","4333","4334","4335","4336","4337","4338","4339",
"4340","4341","4342","4343","4344","4345","4346","4347","4348","4349")

cardiacCodes <- c("9971", "4320","4233","4260")

data$hasTransfusion <- data$d1 %in% transfusionCodes | data$d2 %in% transfusionCodes | data$d3 %in% transfusionCodes | 
data$d4 %in% transfusionCodes | data$d5 %in% transfusionCodes | data$d6 %in% transfusionCodes | 
data$d7 %in% transfusionCodes | data$d8 %in% transfusionCodes | data$d9 %in% transfusionCodes |
 data$d10 %in% transfusionCodes | data$d11 %in% transfusionCodes | data$d12 %in% transfusionCodes | 
data$d13 %in% transfusionCodes | data$d14 %in% transfusionCodes | data$d15 %in% transfusionCodes 

data$hasStroke <- data$d1 %in% strokeCodes | data$d2 %in% strokeCodes | data$d3 %in% strokeCodes | data$d4 %in% strokeCodes | 
data$d5 %in% strokeCodes | data$d6 %in% strokeCodes | data$d7 %in% strokeCodes | 
data$d8 %in% strokeCodes | data$d9 %in% strokeCodes | data$d10 %in% strokeCodes | 
data$d11 %in% strokeCodes | data$d12 %in% strokeCodes | data$d13 %in% strokeCodes | 
data$d14 %in% strokeCodes | data$d15 %in% strokeCodes 

data$cardiacComp <- data$d1 %in% cardiacCodes | data$d2 %in% cardiacCodes | data$d3 %in% cardiacCodes | data$d4 %in% cardiacCodes | 
data$d5 %in% cardiacCodes | data$d6 %in% cardiacCodes | data$d7 %in% cardiacCodes | 
data$d8 %in% cardiacCodes | data$d9 %in% cardiacCodes | data$d10 %in% cardiacCodes | 
data$d11 %in% cardiacCodes | data$d12 %in% cardiacCodes | data$d13 %in% cardiacCodes | 
data$d14 %in% cardiacCodes | data$d15 %in% cardiacCodes 


#data$hasScope <- "4513" == str_trim(data$p1) | "4513" == str_trim(data$p2) | "4513" == str_trim(data$p3) | "4513" ==  str_trim(data$p4) |
#		"4513" == str_trim(data$p5) | "4513" == str_trim(data$p6) | "4513" == str_trim(data$p7) | "4513" == str_trim(data$p8) | "4513" == str_trim(data$p9) |
#		"4513" == str_trim(data$p10) | "4513" == str_trim(data$p11) | "4513" ==  str_trim(data$p12) | "4513" == str_trim(data$p13) |
#		"4513" == str_trim(data$p14) | "4513" == str_trim(data$p15) | "4523" == str_trim(data$p1) | "4523" == str_trim(data$p2) | "4523" == str_trim(data$p3) | 
#		"4523" ==  str_trim(data$p4) | "4523" == str_trim(data$p5) | "4523" == str_trim(data$p6) | "4523" == str_trim(data$p7) | "4523" == str_trim(data$p8) | 
#		"4523" == str_trim(data$p9) | "4523" == str_trim(data$p10) | "4523" == str_trim(data$p11) | "4523" ==  str_trim(data$p12) | "4523" == str_trim(data$p13) |
#		"4523" == str_trim(data$p14) | "4523" == str_trim(data$p15) | "4513" == str_trim(data$p1) | "4513" == str_trim(data$p2) | "4513" == str_trim(data$p3) | 
#		"4513" ==  str_trim(data$p4) | "4513" == str_trim(data$p5) | "4513" == str_trim(data$p6) | "4513" == str_trim(data$p7) | "4513" == str_trim(data$p8) | 
#		"4513" == str_trim(data$p9) | "4513" == str_trim(data$p10) | "4513" == str_trim(data$p11) | "4513" ==  str_trim(data$p12) | "4513" == str_trim(data$p13) |
#		"4513" == str_trim(data$p14) | "4513" == str_trim(data$p15) | "9904" == str_trim(data$p1) | "4513" == data$d1 | "4513" == data$d2 | "4513" == data$d3 | "4513" ==  data$d4 |
#		"4513" == data$d5 | "4513" == data$d6 | "4513" == data$d7 | "4513" == data$d8 | "4513" == data$d9 |
#		"4513" == data$d10 | "4513" == data$d11 | "4513" ==  data$d12 | "4513" == data$d13 |
#		"4513" == data$d14 | "4513" == data$d15 | "4523" == data$d1 | "4523" == data$d2 | "4523" == data$d3 | 
#		"4523" ==  data$d4 | "4523" == data$d5 | "4523" == data$d6 | "4523" == data$d7 | "4523" == data$d8 | 
#		"4523" == data$d9 | "4523" == data$d10 | "4523" == data$d11 | "4523" ==  data$d12 | "4523" == data$d13 |
#		"4523" == data$d14 | "4523" == data$d15 | "4513" == data$d1 | "4513" == data$d2 | "4513" == data$d3 | 
#		"4513" ==  data$d4 | "4513" == data$d5 | "4513" == data$d6 | "4513" == data$d7 | "4513" == data$d8 | 
#		"4513" == data$d9 | "4513" == data$d10 | "4513" == data$d11 | "4513" ==  data$d12 | "4513" == data$d13 |
#		"4513" == data$d14 | "4513" == data$d15 

data$hasRespiratoryFailure <- "51881" == str_trim(data$p1) | "51881" == str_trim(data$p2) | "51881" == str_trim(data$p3) | "51881" ==  str_trim(data$p4) |
		"51881" == str_trim(data$p5) | "51881" == str_trim(data$p6) | "51881" == str_trim(data$p7) | "51881" == str_trim(data$p8) | "51881" == str_trim(data$p9) |
		"51881" == str_trim(data$p10) | "51881" == str_trim(data$p11) | "51881" ==  str_trim(data$p12) | "51881" == str_trim(data$p13) |
		"51881" == str_trim(data$p14) | "51881" == str_trim(data$p15) | "51881" == data$d1 | "51881" == data$d2 | "51881" == data$d3 | "51881" ==  data$d4 |
		"51881" == data$d5 | "51881" == data$d6 | "51881" == data$d7 | "51881" == data$d8 | "51881" == data$d9 |
		"51881" == data$d10 | "51881" == data$d11 | "51881" ==  data$d12 | "51881" == data$d13 |
		"51881" == data$d14 | "51881" == data$d15 

data$hasSepsis <-  "99591" == data$d1 | "99591" == data$d2 | "99591" == data$d3 | "99591" ==  data$d4 |
		"99591" == data$d5 | "99591" == data$d6 | "99591" == data$d7 | "99591" == data$d8 | "99591" == data$d9 |
		"99591" == data$d10 | "99591" == data$d11 | "99591" ==  data$d12 | "99591" == data$d13 |
		"99591" == data$d14 | "99591" == data$d15 | "99592" == data$d1 | "99592" == data$d2 | "99592" == data$d3 | 
		"99592" ==  data$d4 | "99592" == data$d5 | "99592" == data$d6 | "99592" == data$d7 | "99592" == data$d8 | 
		"99592" == data$d9 | "99592" == data$d10 | "99592" == data$d11 | "99592" ==  data$d12 | "99592" == data$d13 |
		"99592" == data$d14 | "99592" == data$d15 


data$hasLiverFailure <- "570" == str_trim(data$p1) | "570" == str_trim(data$p2) | "570" == str_trim(data$p3) | "570" ==  str_trim(data$p4) |
		"570" == str_trim(data$p5) | "570" == str_trim(data$p6) | "570" == str_trim(data$p7) | "570" == str_trim(data$p8) | "570" == str_trim(data$p9) |
		"570" == str_trim(data$p10) | "570" == str_trim(data$p11) | "570" ==  str_trim(data$p12) | "570" == str_trim(data$p13) |
		"570" == str_trim(data$p14) | "570" == str_trim(data$p15) | "570" == data$d1 | "570" == data$d2 | "570" == data$d3 | "570" ==  data$d4 |
		"570" == data$d5 | "570" == data$d6 | "570" == data$d7 | "570" == data$d8 | "570" == data$d9 |
		"570" == data$d10 | "570" == data$d11 | "570" ==  data$d12 | "570" == data$d13 |
		"570" == data$d14 | "570" == data$d15 




summary(data)
head(data[!data$hasOHT,])



data$dayofOHT <- -99

data["375" == str_trim(data$p1),]$dayofOHT <- data["375" == str_trim(data$p1),]$p1day 
data["375" == str_trim(data$p2),]$dayofOHT <- data["375" == str_trim(data$p2),]$p2day 
data["375" == str_trim(data$p3),]$dayofOHT <- data["375" == str_trim(data$p3),]$p3day 
data["375" == str_trim(data$p4),]$dayofOHT <- data["375" == str_trim(data$p4),]$p4day 
data["375" == str_trim(data$p5),]$dayofOHT <- data["375" == str_trim(data$p5),]$p5day 
data["375" == str_trim(data$p6),]$dayofOHT <- data["375" == str_trim(data$p6),]$p6day 
data["375" == str_trim(data$p7),]$dayofOHT <- data["375" == str_trim(data$p7),]$p7day 
data["375" == str_trim(data$p8),]$dayofOHT <- data["375" == str_trim(data$p8),]$p8day 
data["375" == str_trim(data$p9),]$dayofOHT <- data["375" == str_trim(data$p9),]$p9day 
data["375" == str_trim(data$p10),]$dayofOHT <- data["375" == str_trim(data$p10),]$p10day 
data["375" == str_trim(data$p11),]$dayofOHT <- data["375" == str_trim(data$p11),]$p11day 
data["375" == str_trim(data$p12),]$dayofOHT <- data["375" == str_trim(data$p12),]$p12day 
data["375" == str_trim(data$p13),]$dayofOHT <- data["375" == str_trim(data$p13),]$p13day 
data["375" == str_trim(data$p14),]$dayofOHT <- data["375" == str_trim(data$p14),]$p14day 
data["375" == str_trim(data$p15),]$dayofOHT <- data["375" == str_trim(data$p15),]$p15day 

data["3751" == str_trim(data$p1),]$dayofOHT <- data["3751" == str_trim(data$p1),]$p1day 
data["3751" == str_trim(data$p2),]$dayofOHT <- data["3751" == str_trim(data$p2),]$p2day 
data["3751" == str_trim(data$p3),]$dayofOHT <- data["3751" == str_trim(data$p3),]$p3day 
data["3751" == str_trim(data$p4),]$dayofOHT <- data["3751" == str_trim(data$p4),]$p4day 
data["3751" == str_trim(data$p5),]$dayofOHT <- data["3751" == str_trim(data$p5),]$p5day 
data["3751" == str_trim(data$p6),]$dayofOHT <- data["3751" == str_trim(data$p6),]$p6day 
data["3751" == str_trim(data$p7),]$dayofOHT <- data["3751" == str_trim(data$p7),]$p7day 
data["3751" == str_trim(data$p8),]$dayofOHT <- data["3751" == str_trim(data$p8),]$p8day 
data["3751" == str_trim(data$p9),]$dayofOHT <- data["3751" == str_trim(data$p9),]$p9day 
data["3751" == str_trim(data$p10),]$dayofOHT <- data["3751" == str_trim(data$p10),]$p10day 
data["3751" == str_trim(data$p11),]$dayofOHT <- data["3751" == str_trim(data$p11),]$p11day 
data["3751" == str_trim(data$p12),]$dayofOHT <- data["3751" == str_trim(data$p12),]$p12day 
data["3751" == str_trim(data$p13),]$dayofOHT <- data["3751" == str_trim(data$p13),]$p13day 
data["3751" == str_trim(data$p14),]$dayofOHT <- data["3751" == str_trim(data$p14),]$p14day 
data["3751" == str_trim(data$p15),]$dayofOHT <- data["3751" == str_trim(data$p15),]$p15day 



data$dayofECMO <- -99

data["3961" == str_trim(data$p1),]$dayofECMO <- data["3961" == str_trim(data$p1),]$p1day 
data["3961" == str_trim(data$p2),]$dayofECMO <- data["3961" == str_trim(data$p2),]$p2day 
data["3961" == str_trim(data$p3),]$dayofECMO <- data["3961" == str_trim(data$p3),]$p3day 
data["3961" == str_trim(data$p4),]$dayofECMO <- data["3961" == str_trim(data$p4),]$p4day 
data["3961" == str_trim(data$p5),]$dayofECMO <- data["3961" == str_trim(data$p5),]$p5day 
data["3961" == str_trim(data$p6),]$dayofECMO <- data["3961" == str_trim(data$p6),]$p6day 
data["3961" == str_trim(data$p7),]$dayofECMO <- data["3961" == str_trim(data$p7),]$p7day 
data["3961" == str_trim(data$p8),]$dayofECMO <- data["3961" == str_trim(data$p8),]$p8day 
data["3961" == str_trim(data$p9),]$dayofECMO <- data["3961" == str_trim(data$p9),]$p9day 
data["3961" == str_trim(data$p10),]$dayofECMO <- data["3961" == str_trim(data$p10),]$p10day 
data["3961" == str_trim(data$p11),]$dayofECMO <- data["3961" == str_trim(data$p11),]$p11day 
data["3961" == str_trim(data$p12),]$dayofECMO <- data["3961" == str_trim(data$p12),]$p12day 
data["3961" == str_trim(data$p13),]$dayofECMO <- data["3961" == str_trim(data$p13),]$p13day 
data["3961" == str_trim(data$p14),]$dayofECMO <- data["3961" == str_trim(data$p14),]$p14day 
data["3961" == str_trim(data$p15),]$dayofECMO <- data["3961" == str_trim(data$p15),]$p15day 



data$dayofIABP <- -99

data["3761" == str_trim(data$p1),]$dayofIABP <- data["3761" == str_trim(data$p1),]$p1day 
data["3761" == str_trim(data$p2),]$dayofIABP <- data["3761" == str_trim(data$p2),]$p2day 
data["3761" == str_trim(data$p3),]$dayofIABP <- data["3761" == str_trim(data$p3),]$p3day 
data["3761" == str_trim(data$p4),]$dayofIABP <- data["3761" == str_trim(data$p4),]$p4day 
data["3761" == str_trim(data$p5),]$dayofIABP <- data["3761" == str_trim(data$p5),]$p5day 
data["3761" == str_trim(data$p6),]$dayofIABP <- data["3761" == str_trim(data$p6),]$p6day 
data["3761" == str_trim(data$p7),]$dayofIABP <- data["3761" == str_trim(data$p7),]$p7day 
data["3761" == str_trim(data$p8),]$dayofIABP <- data["3761" == str_trim(data$p8),]$p8day 
data["3761" == str_trim(data$p9),]$dayofIABP <- data["3761" == str_trim(data$p9),]$p9day 
data["3761" == str_trim(data$p10),]$dayofIABP <- data["3761" == str_trim(data$p10),]$p10day 
data["3761" == str_trim(data$p11),]$dayofIABP <- data["3761" == str_trim(data$p11),]$p11day 
data["3761" == str_trim(data$p12),]$dayofIABP <- data["3761" == str_trim(data$p12),]$p12day 
data["3761" == str_trim(data$p13),]$dayofIABP <- data["3761" == str_trim(data$p13),]$p13day 
data["3761" == str_trim(data$p14),]$dayofIABP <- data["3761" == str_trim(data$p14),]$p14day 
data["3761" == str_trim(data$p15),]$dayofIABP <- data["3761" == str_trim(data$p15),]$p15day 


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

data$preECMO <- data$dayofECMO >= 0 & (data$dayofOHT - data$dayofECMO) > 0

data$preIABP <- data$dayofIABP >= 0 & (data$dayofOHT - data$dayofIABP) > 0

data$prePVAD <- data$dayofPVAD >= 0 & (data$dayofOHT - data$dayofPVAD) > 0


data$postECMO <- data$dayofECMO >= 0 & (data$dayofOHT - data$dayofECMO) < 0

data$postIABP <- data$dayofIABP >= 0 & (data$dayofOHT - data$dayofIABP) < 0

data$postPVAD <- data$dayofPVAD >= 0 & (data$dayofOHT - data$dayofPVAD) < 0


summary(data)


table(data$dayofOHT)
table(data$dayofIABP )
table(data$dayofECMO )
table(data$dayofPVAD )
sum((data$dayofOHT >= 0)) / sum(data$hasOHT)
sum((data$dayofIABP >= 0)) / sum(data$hasIABP)
sum((data$dayofECMO >= 0)) / sum(data$hasECMO)
sum((data$dayofPVAD >= 0)) / sum(data$hasPVAD)



str(data)

summary(data[data$age > 0,]$age)
sd(data[data$age > 0,]$age)
summary(data[data$sex >= 0,]$sex)
table(data[data$sex >= 0,]$sex)

sum(data$hasOHT)
sum(data$died == 1)
sum(data$died == 1)/sum(data$hasOHT)

table(data$race)
table(data$race)/6892

table(data$bedsize)
table(data$bedsize)/6892

table(data$zip)
table(data$zip)/6892



summary(data[data$dayofOHT > 0,]$dayofOHT)

sum(data$preIABP)
sum(data$prePVAD)
sum(data$preECMO)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasOHT)
456/6892

sum(data$preIABP)/456
sum(data$prePVAD)/456
sum(data$preECMO)/456

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasOHT)

#1036 - 956
#80 more patients

253 + 4 + 102
5381 - (253 + 4 + 102)
(253 + 4 + 102)/5381


sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$died == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$died == 1)/6436

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$died == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$died == 1)/456

prop.test(c(39, 456), 
c(402,6436), correct = TRUE)


sum(data$postIABP)
sum(data$postPVAD)
sum(data$postECMO)

sum(data$preIABP)
sum(data$prePVAD)
sum(data$preECMO)


sum(data$preIABP & data$prePVAD)
sum(data$prePVAD & data$preECMO)
sum(data$preECMO & data$preIABP)

sum(data$preECMO & data$preIABP & data$prePVAD)


data$postSupport <- data$postIABP | data$postPVAD | data$postECMO

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)/6436

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)/456

prop.test(c(4, 456), 
c(90,6436), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)), 
c(456,6436), correct = TRUE)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)), 
c(456,6436), correct = TRUE)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)), 
c(456,6436), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)), 
c(456,6436), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)), 
c(456,6436), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)), 
c(456,6436), correct = TRUE)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)), 
c(456,6436), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)/456
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)/6436
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)), 
c(456,6436), correct = TRUE)

#sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasScope == 1)
#sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasScope == 1)
#prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasScope == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasScope == 1)), 
#c(337,5022), correct = TRUE)



summary(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)
sd(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)

summary(data[(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)
sd(data[(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)

t.test(data[(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los, data[!(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)




summary(data[ data$age >= 0,]$age)
sd(data[data$age >= 0,]$age)

summary(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$age >= 0,]$age)
sd(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$age >= 0,]$age)

summary(data[(data$preIABP | data$prePVAD | data$preECMO) & data$age >= 0,]$age)
sd(data[(data$preIABP | data$prePVAD | data$preECMO) & data$age >= 0,]$age)



summary(data[(data$preIABP ) & data$age >= 0,]$age)
sd(data[(data$preIABP ) & data$age >= 0,]$age)

summary(data[(data$prePVAD) & data$age >= 0,]$age)
sd(data[(data$prePVAD ) & data$age >= 0,]$age)

summary(data[( data$preECMO) & data$age >= 0,]$age)
sd(data[(data$preECMO) & data$age >= 0,]$age)


t.test(data[(data$preIABP | data$prePVAD | data$preECMO) & data$age >= 0,]$age, data[!(data$preIABP | data$prePVAD | data$preECMO) & data$age >= 0,]$age)





table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$sex)
table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$sex)/6435

table(data[(data$preIABP | data$prePVAD | data$preECMO),]$sex)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$sex)/456

prop.test(c(339,4621), 
c(456,6435), correct = TRUE)



table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$race)
table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$race) / (6436)

table(data[(data$preIABP | data$prePVAD | data$preECMO),]$race)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$race) / (456)

#chisq.test(data$support, data$race)

table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$zip)
table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$zip) / (6436)

table(data[(data$preIABP | data$prePVAD | data$preECMO),]$zip)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$zip) / (456 )

#chisq.test(data$support, data$zip)

#write.csv(data, "SupportBeforeOHT-total.csv")


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
sd(data$numDiags)


summary(data[!(data$preIABP | data$prePVAD | data$preECMO),]$numDiags)
sd(data[!(data$preIABP | data$prePVAD | data$preECMO),]$numDiags)

summary(data[(data$preIABP | data$prePVAD | data$preECMO),]$numDiags)
sd(data[(data$preIABP | data$prePVAD | data$preECMO),]$numDiags)







demographics = data

demographics$Diabetes <- ("250" == substr(demographics$p1,1,3) | "250" == substr(demographics$p2,1,3) | "250" == substr(demographics$p3,1,3) | "250" ==  substr(demographics$p4,1,3) |
		"250" == substr(demographics$p5,1,3) | "250" == substr(demographics$p6,1,3) | "250" == substr(demographics$p7,1,3) | "250" == substr(demographics$p8,1,3) | "250" == substr(demographics$p9,1,3) |
		"250" == substr(demographics$p10,1,3) | "250" == substr(demographics$p11,1,3) | "250" ==  substr(demographics$p12,1,3) | "250" == substr(demographics$p13,1,3) | "250" ==  substr(demographics$d4,1,3) |
		"250" == substr(demographics$p14,1,3) | "250" == substr(demographics$p15,1,3) | "250" == substr(demographics$d1,1,3) | "250" == substr(demographics$d2,1,3) | "250" == substr(demographics$d3,1,3) | 
		"250" == substr(demographics$d5,1,3) | "250" == substr(demographics$d6,1,3) | "250" == substr(demographics$d7,1,3) | "250" == substr(demographics$d8,1,3) | "250" == substr(demographics$d9,1,3) |
		"250" == substr(demographics$d10,1,3) | "250" == substr(demographics$d11,1,3) | "250" ==  substr(demographics$d12,1,3) | "250" == substr(demographics$d13,1,3) |
		"250" == substr(demographics$d14,1,3) | "250" == substr(demographics$d15,1,3) ) | ("249" == substr(demographics$p1,1,3) | "249" == substr(demographics$p2,1,3) | "249" == substr(demographics$p3,1,3) | "249" ==  substr(demographics$p4,1,3) |
		"249" == substr(demographics$p5,1,3) | "249" == substr(demographics$p6,1,3) | "249" == substr(demographics$p7,1,3) | "249" == substr(demographics$p8,1,3) | "249" == substr(demographics$p9,1,3) |
		"249" == substr(demographics$p10,1,3) | "249" == substr(demographics$p11,1,3) | "249" ==  substr(demographics$p12,1,3) | "249" == substr(demographics$p13,1,3) | "249" ==  substr(demographics$d4,1,3) |
		"249" == substr(demographics$p14,1,3) | "249" == substr(demographics$p15,1,3) | "249" == substr(demographics$d1,1,3) | "249" == substr(demographics$d2,1,3) | "249" == substr(demographics$d3,1,3) | 
		"249" == substr(demographics$d5,1,3) | "249" == substr(demographics$d6,1,3) | "249" == substr(demographics$d7,1,3) | "249" == substr(demographics$d8,1,3) | "249" == substr(demographics$d9,1,3) |
		"249" == substr(demographics$d10,1,3) | "249" == substr(demographics$d11,1,3) | "249" ==  substr(demographics$d12,1,3) | "249" == substr(demographics$d13,1,3) |
		"249" == substr(demographics$d14,1,3) | "249" == substr(demographics$d15,1,3) )

demographics$PVD <- ("440" == substr(demographics$p1,1,3) | "440" == substr(demographics$p2,1,3) | "440" == substr(demographics$p3,1,3) | "440" ==  substr(demographics$p4,1,3) |
		"440" == substr(demographics$p5,1,3) | "440" == substr(demographics$p6,1,3) | "440" == substr(demographics$p7,1,3) | "440" == substr(demographics$p8,1,3) | "440" == substr(demographics$p9,1,3) |
		"440" == substr(demographics$p10,1,3) | "440" == substr(demographics$p11,1,3) | "440" ==  substr(demographics$p12,1,3) | "440" == substr(demographics$p13,1,3) | "440" ==  substr(demographics$d4,1,3) |
		"440" == substr(demographics$p14,1,3) | "440" == substr(demographics$p15,1,3) | "440" == substr(demographics$d1,1,3) | "440" == substr(demographics$d2,1,3) | "440" == substr(demographics$d3,1,3) | 
		"440" == substr(demographics$d5,1,3) | "440" == substr(demographics$d6,1,3) | "440" == substr(demographics$d7,1,3) | "440" == substr(demographics$d8,1,3) | "440" == substr(demographics$d9,1,3) |
		"440" == substr(demographics$d10,1,3) | "440" == substr(demographics$d11,1,3) | "440" ==  substr(demographics$d12,1,3) | "440" == substr(demographics$d13,1,3) |
		"440" == substr(demographics$d14,1,3) | "440" == substr(demographics$d15,1,3) ) | ("443" == substr(demographics$p1,1,3) | "443" == substr(demographics$p2,1,3) | "443" == substr(demographics$p3,1,3) | "443" ==  substr(demographics$p4,1,3) |
		"443" == substr(demographics$p5,1,3) | "443" == substr(demographics$p6,1,3) | "443" == substr(demographics$p7,1,3) | "443" == substr(demographics$p8,1,3) | "443" == substr(demographics$p9,1,3) |
		"443" == substr(demographics$p10,1,3) | "443" == substr(demographics$p11,1,3) | "443" ==  substr(demographics$p12,1,3) | "443" == substr(demographics$p13,1,3) | "443" ==  substr(demographics$d4,1,3) |
		"443" == substr(demographics$p14,1,3) | "443" == substr(demographics$p15,1,3) | "443" == substr(demographics$d1,1,3) | "443" == substr(demographics$d2,1,3) | "443" == substr(demographics$d3,1,3) | 
		"443" == substr(demographics$d5,1,3) | "443" == substr(demographics$d6,1,3) | "443" == substr(demographics$d7,1,3) | "443" == substr(demographics$d8,1,3) | "443" == substr(demographics$d9,1,3) |
		"443" == substr(demographics$d10,1,3) | "443" == substr(demographics$d11,1,3) | "443" ==  substr(demographics$d12,1,3) | "443" == substr(demographics$d13,1,3) |
		"443" == substr(demographics$d14,1,3) | "443" == substr(demographics$d15,1,3) ) | ("4471" == substr(demographics$p1,1,4) | "4471" == substr(demographics$p2,1,4) | "4471" == substr(demographics$p3,1,4) | "4471" ==  substr(demographics$p4,1,4) |
		"4471" == substr(demographics$p5,1,4) | "4471" == substr(demographics$p6,1,4) | "4471" == substr(demographics$p7,1,4) | "4471" == substr(demographics$p8,1,4) | "4471" == substr(demographics$p9,1,4) |
		"4471" == substr(demographics$p10,1,4) | "4471" == substr(demographics$p11,1,4) | "4471" ==  substr(demographics$p12,1,4) | "4471" == substr(demographics$p13,1,4) | "4471" ==  substr(demographics$d4,1,4) |
		"4471" == substr(demographics$p14,1,4) | "4471" == substr(demographics$p15,1,4) | "4471" == substr(demographics$d1,1,4) | "4471" == substr(demographics$d2,1,4) | "4471" == substr(demographics$d3,1,4) | 
		"4471" == substr(demographics$d5,1,4) | "4471" == substr(demographics$d6,1,4) | "4471" == substr(demographics$d7,1,4) | "4471" == substr(demographics$d8,1,4) | "4471" == substr(demographics$d9,1,4) |
		"4471" == substr(demographics$d10,1,4) | "4471" == substr(demographics$d11,1,4) | "4471" ==  substr(demographics$d12,1,4) | "4471" == substr(demographics$d13,1,4) |
		"4471" == substr(demographics$d14,1,4) | "4471" == substr(demographics$d15,1,4) ) | ("V434" == substr(demographics$p1,1,4) | "V434" == substr(demographics$p2,1,4) | "V434" == substr(demographics$p3,1,4) | "V434" ==  substr(demographics$p4,1,4) |
		"V434" == substr(demographics$p5,1,4) | "V434" == substr(demographics$p6,1,4) | "V434" == substr(demographics$p7,1,4) | "V434" == substr(demographics$p8,1,4) | "V434" == substr(demographics$p9,1,4) |
		"V434" == substr(demographics$p10,1,4) | "V434" == substr(demographics$p11,1,4) | "V434" ==  substr(demographics$p12,1,4) | "V434" == substr(demographics$p13,1,4) | "V434" ==  substr(demographics$d4,1,4) |
		"V434" == substr(demographics$p14,1,4) | "V434" == substr(demographics$p15,1,4) | "V434" == substr(demographics$d1,1,4) | "V434" == substr(demographics$d2,1,4) | "V434" == substr(demographics$d3,1,4) | 
		"V434" == substr(demographics$d5,1,4) | "V434" == substr(demographics$d6,1,4) | "V434" == substr(demographics$d7,1,4) | "V434" == substr(demographics$d8,1,4) | "V434" == substr(demographics$d9,1,4) |
		"V434" == substr(demographics$d10,1,4) | "V434" == substr(demographics$d11,1,4) | "V434" ==  substr(demographics$d12,1,4) | "V434" == substr(demographics$d13,1,4) |
		"V434" == substr(demographics$d14,1,4) | "V434" == substr(demographics$d15,1,4) )

demographics$CKD <- ("585" == substr(demographics$p1,1,3) | "585" == substr(demographics$p2,1,3) | "585" == substr(demographics$p3,1,3) | "585" ==  substr(demographics$p4,1,3) |
		"585" == substr(demographics$p5,1,3) | "585" == substr(demographics$p6,1,3) | "585" == substr(demographics$p7,1,3) | "585" == substr(demographics$p8,1,3) | "585" == substr(demographics$p9,1,3) |
		"585" == substr(demographics$p10,1,3) | "585" == substr(demographics$p11,1,3) | "585" ==  substr(demographics$p12,1,3) | "585" == substr(demographics$p13,1,3) | "585" ==  substr(demographics$d4,1,3) |
		"585" == substr(demographics$p14,1,3) | "585" == substr(demographics$p15,1,3) | "585" == substr(demographics$d1,1,3) | "585" == substr(demographics$d2,1,3) | "585" == substr(demographics$d3,1,3) | 
		"585" == substr(demographics$d5,1,3) | "585" == substr(demographics$d6,1,3) | "585" == substr(demographics$d7,1,3) | "585" == substr(demographics$d8,1,3) | "585" == substr(demographics$d9,1,3) |
		"585" == substr(demographics$d10,1,3) | "585" == substr(demographics$d11,1,3) | "585" ==  substr(demographics$d12,1,3) | "585" == substr(demographics$d13,1,3) |
		"585" == substr(demographics$d14,1,3) | "585" == substr(demographics$d15,1,3) ) | ("V45" == substr(demographics$p1,1,3) | "V45" == substr(demographics$p2,1,3) | "V45" == substr(demographics$p3,1,3) | "V45" ==  substr(demographics$p4,1,3) |
		"V45" == substr(demographics$p5,1,3) | "V45" == substr(demographics$p6,1,3) | "V45" == substr(demographics$p7,1,3) | "V45" == substr(demographics$p8,1,3) | "V45" == substr(demographics$p9,1,3) |
		"V45" == substr(demographics$p10,1,3) | "V45" == substr(demographics$p11,1,3) | "V45" ==  substr(demographics$p12,1,3) | "V45" == substr(demographics$p13,1,3) | "V45" ==  substr(demographics$d4,1,3) |
		"V45" == substr(demographics$p14,1,3) | "V45" == substr(demographics$p15,1,3) | "V45" == substr(demographics$d1,1,3) | "V45" == substr(demographics$d2,1,3) | "V45" == substr(demographics$d3,1,3) | 
		"V45" == substr(demographics$d5,1,3) | "V45" == substr(demographics$d6,1,3) | "V45" == substr(demographics$d7,1,3) | "V45" == substr(demographics$d8,1,3) | "V45" == substr(demographics$d9,1,3) |
		"V45" == substr(demographics$d10,1,3) | "V45" == substr(demographics$d11,1,3) | "V45" ==  substr(demographics$d12,1,3) | "V45" == substr(demographics$d13,1,3) |
		"V45" == substr(demographics$d14,1,3) | "V45" == substr(demographics$d15,1,3) ) | ("V56" == substr(demographics$p1,1,3) | "V56" == substr(demographics$p2,1,3) | "V56" == substr(demographics$p3,1,3) | "V56" ==  substr(demographics$p4,1,3) |
		"V56" == substr(demographics$p5,1,3) | "V56" == substr(demographics$p6,1,3) | "V56" == substr(demographics$p7,1,3) | "V56" == substr(demographics$p8,1,3) | "V56" == substr(demographics$p9,1,3) |
		"V56" == substr(demographics$p10,1,3) | "V56" == substr(demographics$p11,1,3) | "V56" ==  substr(demographics$p12,1,3) | "V56" == substr(demographics$p13,1,3) | "V56" ==  substr(demographics$d4,1,3) |
		"V56" == substr(demographics$p14,1,3) | "V56" == substr(demographics$p15,1,3) | "V56" == substr(demographics$d1,1,3) | "V56" == substr(demographics$d2,1,3) | "V56" == substr(demographics$d3,1,3) | 
		"V56" == substr(demographics$d5,1,3) | "V56" == substr(demographics$d6,1,3) | "V56" == substr(demographics$d7,1,3) | "V56" == substr(demographics$d8,1,3) | "V56" == substr(demographics$d9,1,3) |
		"V56" == substr(demographics$d10,1,3) | "V56" == substr(demographics$d11,1,3) | "V56" ==  substr(demographics$d12,1,3) | "V56" == substr(demographics$d13,1,3) |
		"V56" == substr(demographics$d14,1,3) | "V56" == substr(demographics$d15,1,3) )


demographics$Hypertension <- ("401" == substr(demographics$p1,1,3) | "401" == substr(demographics$p2,1,3) | "401" == substr(demographics$p3,1,3) | "401" ==  substr(demographics$p4,1,3) |
		"401" == substr(demographics$p5,1,3) | "401" == substr(demographics$p6,1,3) | "401" == substr(demographics$p7,1,3) | "401" == substr(demographics$p8,1,3) | "401" == substr(demographics$p9,1,3) |
		"401" == substr(demographics$p10,1,3) | "401" == substr(demographics$p11,1,3) | "401" ==  substr(demographics$p12,1,3) | "401" == substr(demographics$p13,1,3) | "401" ==  substr(demographics$d4,1,3) |
		"401" == substr(demographics$p14,1,3) | "401" == substr(demographics$p15,1,3) | "401" == substr(demographics$d1,1,3) | "401" == substr(demographics$d2,1,3) | "401" == substr(demographics$d3,1,3) | 
		"401" == substr(demographics$d5,1,3) | "401" == substr(demographics$d6,1,3) | "401" == substr(demographics$d7,1,3) | "401" == substr(demographics$d8,1,3) | "401" == substr(demographics$d9,1,3) |
		"401" == substr(demographics$d10,1,3) | "401" == substr(demographics$d11,1,3) | "401" ==  substr(demographics$d12,1,3) | "401" == substr(demographics$d13,1,3) |
		"401" == substr(demographics$d14,1,3) | "401" == substr(demographics$d15,1,3) ) | ("402" == substr(demographics$p1,1,3) | "402" == substr(demographics$p2,1,3) | "402" == substr(demographics$p3,1,3) | "402" ==  substr(demographics$p4,1,3) |
		"402" == substr(demographics$p5,1,3) | "402" == substr(demographics$p6,1,3) | "402" == substr(demographics$p7,1,3) | "402" == substr(demographics$p8,1,3) | "402" == substr(demographics$p9,1,3) |
		"402" == substr(demographics$p10,1,3) | "402" == substr(demographics$p11,1,3) | "402" ==  substr(demographics$p12,1,3) | "402" == substr(demographics$p13,1,3) | "402" ==  substr(demographics$d4,1,3) |
		"402" == substr(demographics$p14,1,3) | "402" == substr(demographics$p15,1,3) | "402" == substr(demographics$d1,1,3) | "402" == substr(demographics$d2,1,3) | "402" == substr(demographics$d3,1,3) | 
		"402" == substr(demographics$d5,1,3) | "402" == substr(demographics$d6,1,3) | "402" == substr(demographics$d7,1,3) | "402" == substr(demographics$d8,1,3) | "402" == substr(demographics$d9,1,3) |
		"402" == substr(demographics$d10,1,3) | "402" == substr(demographics$d11,1,3) | "402" ==  substr(demographics$d12,1,3) | "402" == substr(demographics$d13,1,3) |
		"402" == substr(demographics$d14,1,3) | "402" == substr(demographics$d15,1,3) ) | ("403" == substr(demographics$p1,1,3) | "403" == substr(demographics$p2,1,3) | "403" == substr(demographics$p3,1,3) | "403" ==  substr(demographics$p4,1,3) |
		"403" == substr(demographics$p5,1,3) | "403" == substr(demographics$p6,1,3) | "403" == substr(demographics$p7,1,3) | "403" == substr(demographics$p8,1,3) | "403" == substr(demographics$p9,1,3) |
		"403" == substr(demographics$p10,1,3) | "403" == substr(demographics$p11,1,3) | "403" ==  substr(demographics$p12,1,3) | "403" == substr(demographics$p13,1,3) | "403" ==  substr(demographics$d4,1,3) |
		"403" == substr(demographics$p14,1,3) | "403" == substr(demographics$p15,1,3) | "403" == substr(demographics$d1,1,3) | "403" == substr(demographics$d2,1,3) | "403" == substr(demographics$d3,1,3) | 
		"403" == substr(demographics$d5,1,3) | "403" == substr(demographics$d6,1,3) | "403" == substr(demographics$d7,1,3) | "403" == substr(demographics$d8,1,3) | "403" == substr(demographics$d9,1,3) |
		"403" == substr(demographics$d10,1,3) | "403" == substr(demographics$d11,1,3) | "403" ==  substr(demographics$d12,1,3) | "403" == substr(demographics$d13,1,3) |
		"403" == substr(demographics$d14,1,3) | "403" == substr(demographics$d15,1,3) ) | ("404" == substr(demographics$p1,1,3) | "404" == substr(demographics$p2,1,3) | "404" == substr(demographics$p3,1,3) | "404" ==  substr(demographics$p4,1,3) |
		"404" == substr(demographics$p5,1,3) | "404" == substr(demographics$p6,1,3) | "404" == substr(demographics$p7,1,3) | "404" == substr(demographics$p8,1,3) | "404" == substr(demographics$p9,1,3) |
		"404" == substr(demographics$p10,1,3) | "404" == substr(demographics$p11,1,3) | "404" ==  substr(demographics$p12,1,3) | "404" == substr(demographics$p13,1,3) | "404" ==  substr(demographics$d4,1,3) |
		"404" == substr(demographics$p14,1,3) | "404" == substr(demographics$p15,1,3) | "404" == substr(demographics$d1,1,3) | "404" == substr(demographics$d2,1,3) | "404" == substr(demographics$d3,1,3) | 
		"404" == substr(demographics$d5,1,3) | "404" == substr(demographics$d6,1,3) | "404" == substr(demographics$d7,1,3) | "404" == substr(demographics$d8,1,3) | "404" == substr(demographics$d9,1,3) |
		"404" == substr(demographics$d10,1,3) | "404" == substr(demographics$d11,1,3) | "404" ==  substr(demographics$d12,1,3) | "404" == substr(demographics$d13,1,3) |
		"404" == substr(demographics$d14,1,3) | "404" == substr(demographics$d15,1,3) ) | ("405" == substr(demographics$p1,1,3) | "405" == substr(demographics$p2,1,3) | "405" == substr(demographics$p3,1,3) | "405" ==  substr(demographics$p4,1,3) |
		"405" == substr(demographics$p5,1,3) | "405" == substr(demographics$p6,1,3) | "405" == substr(demographics$p7,1,3) | "405" == substr(demographics$p8,1,3) | "405" == substr(demographics$p9,1,3) |
		"405" == substr(demographics$p10,1,3) | "405" == substr(demographics$p11,1,3) | "405" ==  substr(demographics$p12,1,3) | "405" == substr(demographics$p13,1,3) | "405" ==  substr(demographics$d4,1,3) |
		"405" == substr(demographics$p14,1,3) | "405" == substr(demographics$p15,1,3) | "405" == substr(demographics$d1,1,3) | "405" == substr(demographics$d2,1,3) | "405" == substr(demographics$d3,1,3) | 
		"405" == substr(demographics$d5,1,3) | "405" == substr(demographics$d6,1,3) | "405" == substr(demographics$d7,1,3) | "405" == substr(demographics$d8,1,3) | "405" == substr(demographics$d9,1,3) |
		"405" == substr(demographics$d10,1,3) | "405" == substr(demographics$d11,1,3) | "405" ==  substr(demographics$d12,1,3) | "405" == substr(demographics$d13,1,3) |
		"405" == substr(demographics$d14,1,3) | "405" == substr(demographics$d15,1,3) ) | ("4372" == substr(demographics$p1,1,4) | "4372" == substr(demographics$p2,1,4) | "4372" == substr(demographics$p3,1,4) | "4372" ==  substr(demographics$p4,1,4) |
		"4372" == substr(demographics$p5,1,4) | "4372" == substr(demographics$p6,1,4) | "4372" == substr(demographics$p7,1,4) | "4372" == substr(demographics$p8,1,4) | "4372" == substr(demographics$p9,1,4) |
		"4372" == substr(demographics$p10,1,4) | "4372" == substr(demographics$p11,1,4) | "4372" ==  substr(demographics$p12,1,4) | "4372" == substr(demographics$p13,1,4) | "4372" ==  substr(demographics$d4,1,4) |
		"4372" == substr(demographics$p14,1,4) | "4372" == substr(demographics$p15,1,4) | "4372" == substr(demographics$d1,1,4) | "4372" == substr(demographics$d2,1,4) | "4372" == substr(demographics$d3,1,4) | 
		"4372" == substr(demographics$d5,1,4) | "4372" == substr(demographics$d6,1,4) | "4372" == substr(demographics$d7,1,4) | "4372" == substr(demographics$d8,1,4) | "4372" == substr(demographics$d9,1,4) |
		"4372" == substr(demographics$d10,1,4) | "4372" == substr(demographics$d11,1,4) | "4372" ==  substr(demographics$d12,1,4) | "4372" == substr(demographics$d13,1,4) |
		"4372" == substr(demographics$d14,1,4) | "4372" == substr(demographics$d15,1,4) )


demographics$IschemicHD <- ("410" == substr(demographics$p1,1,3) | "410" == substr(demographics$p2,1,3) | "410" == substr(demographics$p3,1,3) | "410" ==  substr(demographics$p4,1,3) |
		"410" == substr(demographics$p5,1,3) | "410" == substr(demographics$p6,1,3) | "410" == substr(demographics$p7,1,3) | "410" == substr(demographics$p8,1,3) | "410" == substr(demographics$p9,1,3) |
		"410" == substr(demographics$p10,1,3) | "410" == substr(demographics$p11,1,3) | "410" ==  substr(demographics$p12,1,3) | "410" == substr(demographics$p13,1,3) | "410" ==  substr(demographics$d4,1,3) |
		"410" == substr(demographics$p14,1,3) | "410" == substr(demographics$p15,1,3) | "410" == substr(demographics$d1,1,3) | "410" == substr(demographics$d2,1,3) | "410" == substr(demographics$d3,1,3) | 
		"410" == substr(demographics$d5,1,3) | "410" == substr(demographics$d6,1,3) | "410" == substr(demographics$d7,1,3) | "410" == substr(demographics$d8,1,3) | "410" == substr(demographics$d9,1,3) |
		"410" == substr(demographics$d10,1,3) | "410" == substr(demographics$d11,1,3) | "410" ==  substr(demographics$d12,1,3) | "410" == substr(demographics$d13,1,3) |
		"410" == substr(demographics$d14,1,3) | "410" == substr(demographics$d15,1,3) ) | ("411" == substr(demographics$p1,1,3) | "411" == substr(demographics$p2,1,3) | "411" == substr(demographics$p3,1,3) | "411" ==  substr(demographics$p4,1,3) |
		"411" == substr(demographics$p5,1,3) | "411" == substr(demographics$p6,1,3) | "411" == substr(demographics$p7,1,3) | "411" == substr(demographics$p8,1,3) | "411" == substr(demographics$p9,1,3) |
		"411" == substr(demographics$p10,1,3) | "411" == substr(demographics$p11,1,3) | "411" ==  substr(demographics$p12,1,3) | "411" == substr(demographics$p13,1,3) | "411" ==  substr(demographics$d4,1,3) |
		"411" == substr(demographics$p14,1,3) | "411" == substr(demographics$p15,1,3) | "411" == substr(demographics$d1,1,3) | "411" == substr(demographics$d2,1,3) | "411" == substr(demographics$d3,1,3) | 
		"411" == substr(demographics$d5,1,3) | "411" == substr(demographics$d6,1,3) | "411" == substr(demographics$d7,1,3) | "411" == substr(demographics$d8,1,3) | "411" == substr(demographics$d9,1,3) |
		"411" == substr(demographics$d10,1,3) | "411" == substr(demographics$d11,1,3) | "411" ==  substr(demographics$d12,1,3) | "411" == substr(demographics$d13,1,3) |
		"411" == substr(demographics$d14,1,3) | "411" == substr(demographics$d15,1,3) ) |  ("412" == substr(demographics$p1,1,3) | "412" == substr(demographics$p2,1,3) | "412" == substr(demographics$p3,1,3) | "412" ==  substr(demographics$p4,1,3) |
		"412" == substr(demographics$p5,1,3) | "412" == substr(demographics$p6,1,3) | "412" == substr(demographics$p7,1,3) | "412" == substr(demographics$p8,1,3) | "412" == substr(demographics$p9,1,3) |
		"412" == substr(demographics$p10,1,3) | "412" == substr(demographics$p11,1,3) | "412" ==  substr(demographics$p12,1,3) | "412" == substr(demographics$p13,1,3) | "412" ==  substr(demographics$d4,1,3) |
		"412" == substr(demographics$p14,1,3) | "412" == substr(demographics$p15,1,3) | "412" == substr(demographics$d1,1,3) | "412" == substr(demographics$d2,1,3) | "412" == substr(demographics$d3,1,3) | 
		"412" == substr(demographics$d5,1,3) | "412" == substr(demographics$d6,1,3) | "412" == substr(demographics$d7,1,3) | "412" == substr(demographics$d8,1,3) | "412" == substr(demographics$d9,1,3) |
		"412" == substr(demographics$d10,1,3) | "412" == substr(demographics$d11,1,3) | "412" ==  substr(demographics$d12,1,3) | "412" == substr(demographics$d13,1,3) |
		"412" == substr(demographics$d14,1,3) | "412" == substr(demographics$d15,1,3) ) |  ("413" == substr(demographics$p1,1,3) | "413" == substr(demographics$p2,1,3) | "413" == substr(demographics$p3,1,3) | "413" ==  substr(demographics$p4,1,3) |
		"413" == substr(demographics$p5,1,3) | "413" == substr(demographics$p6,1,3) | "413" == substr(demographics$p7,1,3) | "413" == substr(demographics$p8,1,3) | "413" == substr(demographics$p9,1,3) |
		"413" == substr(demographics$p10,1,3) | "413" == substr(demographics$p11,1,3) | "413" ==  substr(demographics$p12,1,3) | "413" == substr(demographics$p13,1,3) | "413" ==  substr(demographics$d4,1,3) |
		"413" == substr(demographics$p14,1,3) | "413" == substr(demographics$p15,1,3) | "413" == substr(demographics$d1,1,3) | "413" == substr(demographics$d2,1,3) | "413" == substr(demographics$d3,1,3) | 
		"413" == substr(demographics$d5,1,3) | "413" == substr(demographics$d6,1,3) | "413" == substr(demographics$d7,1,3) | "413" == substr(demographics$d8,1,3) | "413" == substr(demographics$d9,1,3) |
		"413" == substr(demographics$d10,1,3) | "413" == substr(demographics$d11,1,3) | "413" ==  substr(demographics$d12,1,3) | "413" == substr(demographics$d13,1,3) |
		"413" == substr(demographics$d14,1,3) | "413" == substr(demographics$d15,1,3) ) |  ("414" == substr(demographics$p1,1,3) | "414" == substr(demographics$p2,1,3) | "414" == substr(demographics$p3,1,3) | "414" ==  substr(demographics$p4,1,3) |
		"414" == substr(demographics$p5,1,3) | "414" == substr(demographics$p6,1,3) | "414" == substr(demographics$p7,1,3) | "414" == substr(demographics$p8,1,3) | "414" == substr(demographics$p9,1,3) |
		"414" == substr(demographics$p10,1,3) | "414" == substr(demographics$p11,1,3) | "414" ==  substr(demographics$p12,1,3) | "414" == substr(demographics$p13,1,3) | "414" ==  substr(demographics$d4,1,3) |
		"414" == substr(demographics$p14,1,3) | "414" == substr(demographics$p15,1,3) | "414" == substr(demographics$d1,1,3) | "414" == substr(demographics$d2,1,3) | "414" == substr(demographics$d3,1,3) | 
		"414" == substr(demographics$d5,1,3) | "414" == substr(demographics$d6,1,3) | "414" == substr(demographics$d7,1,3) | "414" == substr(demographics$d8,1,3) | "414" == substr(demographics$d9,1,3) |
		"414" == substr(demographics$d10,1,3) | "414" == substr(demographics$d11,1,3) | "414" ==  substr(demographics$d12,1,3) | "414" == substr(demographics$d13,1,3) |
		"414" == substr(demographics$d14,1,3) | "414" == substr(demographics$d15,1,3) ) |  ("V4582" == substr(demographics$p1,1,5) | "V4582" == substr(demographics$p2,1,5) | "V4582" == substr(demographics$p3,1,5) | "V4582" ==  substr(demographics$p4,1,5) |
		"V4582" == substr(demographics$p5,1,5) | "V4582" == substr(demographics$p6,1,5) | "V4582" == substr(demographics$p7,1,5) | "V4582" == substr(demographics$p8,1,5) | "V4582" == substr(demographics$p9,1,5) |
		"V4582" == substr(demographics$p10,1,5) | "V4582" == substr(demographics$p11,1,5) | "V4582" ==  substr(demographics$p12,1,5) | "V4582" == substr(demographics$p13,1,5) | "V4582" ==  substr(demographics$d4,1,5) |
		"V4582" == substr(demographics$p14,1,5) | "V4582" == substr(demographics$p15,1,5) | "V4582" == substr(demographics$d1,1,5) | "V4582" == substr(demographics$d2,1,5) | "V4582" == substr(demographics$d3,1,5) | 
		"V4582" == substr(demographics$d5,1,5) | "V4582" == substr(demographics$d6,1,5) | "V4582" == substr(demographics$d7,1,5) | "V4582" == substr(demographics$d8,1,5) | "V4582" == substr(demographics$d9,1,5) |
		"V4582" == substr(demographics$d10,1,5) | "V4582" == substr(demographics$d11,1,5) | "V4582" ==  substr(demographics$d12,1,5) | "V4582" == substr(demographics$d13,1,5) |
		"V4582" == substr(demographics$d14,1,5) | "V4582" == substr(demographics$d15,1,5) ) |  ("V4581" == substr(demographics$p1,1,5) | "V4581" == substr(demographics$p2,1,5) | "V4581" == substr(demographics$p3,1,5) | "V4581" ==  substr(demographics$p4,1,5) |
		"V4581" == substr(demographics$p5,1,5) | "V4581" == substr(demographics$p6,1,5) | "V4581" == substr(demographics$p7,1,5) | "V4581" == substr(demographics$p8,1,5) | "V4581" == substr(demographics$p9,1,5) |
		"V4581" == substr(demographics$p10,1,5) | "V4581" == substr(demographics$p11,1,5) | "V4581" ==  substr(demographics$p12,1,5) | "V4581" == substr(demographics$p13,1,5) | "V4581" ==  substr(demographics$d4,1,5) |
		"V4581" == substr(demographics$p14,1,5) | "V4581" == substr(demographics$p15,1,5) | "V4581" == substr(demographics$d1,1,5) | "V4581" == substr(demographics$d2,1,5) | "V4581" == substr(demographics$d3,1,5) | 
		"V4581" == substr(demographics$d5,1,5) | "V4581" == substr(demographics$d6,1,5) | "V4581" == substr(demographics$d7,1,5) | "V4581" == substr(demographics$d8,1,5) | "V4581" == substr(demographics$d9,1,5) |
		"V4581" == substr(demographics$d10,1,5) | "V4581" == substr(demographics$d11,1,5) | "V4581" ==  substr(demographics$d12,1,5) | "V4581" == substr(demographics$d13,1,5) |
		"V4581" == substr(demographics$d14,1,5) | "V4581" == substr(demographics$d15,1,5) )

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
sum(demographics$IschemicHD )
sum(demographics$Hypertension )
sum(demographics$CKD )
sum(demographics$PVD )
sum(demographics$Smoking )
sum(demographics$Obesity )


sum(demographics$Diabetes) / 6892
sum(demographics$IschemicHD ) / 6892
sum(demographics$Hypertension ) / 6892
sum(demographics$CKD ) / 6892
sum(demographics$PVD ) / 6892
sum(demographics$Smoking ) / 6892
sum(demographics$Obesity ) / 6892

data = demographics 


data$numComorbid = data$Diabetes + data$IschemicHD + data$Hypertension + data$CKD + data$PVD + data$Smoking + data$Obesity
table(data$numComorbid)

table(data$numDiags - data$numComorbid)

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Diabetes)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Diabetes)
sum(data$Diabetes)

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Diabetes)/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Diabetes)/456
sum(data$Diabetes)/5381

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$IschemicHD )
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$IschemicHD )
sum(data$IschemicHD )

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$IschemicHD )/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$IschemicHD )/456
sum(data$IschemicHD )/5381


sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Hypertension )
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Hypertension )
sum(data$Hypertension )

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Hypertension )/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Hypertension )/456
sum(data$Hypertension )/5381


sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$CKD )
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$CKD )
sum(data$CKD )

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$CKD )/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$CKD )/456
sum(data$CKD )/5381

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$PVD )
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$PVD )
sum(data$PVD )

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$PVD )/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$PVD )/456
sum(data$PVD )/5381



sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Smoking )
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Smoking )
sum(data$Smoking )

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Smoking )/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Smoking )/456
sum(data$Smoking )/5381

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Obesity )
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Obesity )
sum(data$Obesity )

sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$Obesity )/6436
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$Obesity )/456
sum(data$Obesity )/5381


prop.test(c(69, 1278), 
c(456,6436), correct = TRUE)
prop.test(c(194, 2760), 
c(456,6436), correct = TRUE)
prop.test(c(106, 1943), 
c(456,6436), correct = TRUE)
prop.test(c(119, 2169), 
c(456,6436), correct = TRUE)
prop.test(c(8, 103), 
c(456,6436), correct = TRUE)
prop.test(c(16, 354), 
c(456,6436), correct = TRUE)
prop.test(c(11, 192), 
c(456,6436), correct = TRUE)




# Hospital Characteristics

table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$location)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$location)
table(data$location)

8/(4979)
4971/(4979)

prop.test(c(0, 8), 
c(337,5022), correct = TRUE)

table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$bedsize)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$bedsize)
table(data$bedsize)


table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$bedsize)/6436
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$bedsize)/456
table(data$bedsize)/6892

#chisq.test(data$support, data$bedsize)

prop.test(c(0, 8), 
c(337,5022), correct = TRUE)



prop.test(c(14, 432), 
c(337,5022), correct = TRUE)


table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$bedsize)/(5022-65)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$bedsize)/337
table(data$bedsize)/(126+796+4394)

table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$teach)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$teach)
table(data$teach)

table(data[!(data$preIABP | data$prePVAD | data$preECMO),]$teach)/(5022-65)
table(data[(data$preIABP | data$prePVAD | data$preECMO),]$teach)/337
table(data$teach)/(126+796+4394)




library(plyr)
data$support <- "No"
data$count <- 1
data[data$preECMO,]$support <- "Yes" #"ECMO"
data[data$preIABP,]$support <- "Yes" #"IABP"
data[data$prePVAD,]$support <- "Yes" #"PVAD"

# Time Trends
qplot(data = data, x = year, binwidth = 1) + facet_wrap(~support, scale = "free_y") 

data$support <- "No"
data$count <- 1

data <- allData
data[data$preECMO,]$support <- "ECMO"
data[data$preIABP,]$support <- "IABP"
data[data$prePVAD,]$support <- "PVAD"


data[data$preECMO,]$support <- "Yes" #"ECMO"
data[data$preIABP,]$support <- "Yes" #"IABP"
data[data$prePVAD,]$support <- "Yes" #"PVAD"


data$support <- ""
data[data$preECMO,]$support <- paste(data[data$preECMO,]$support,"ECMO")
data[data$preIABP,]$support <- paste(data[data$preIABP,]$support,"IABP")
data[data$prePVAD,]$support <- paste(data[data$prePVAD,]$support,"PVAD")



data$preSupport <- data$preECMO | data$preIABP | data$ prePVAD




ggplot(data = data, aes(x = year)) + geom_histogram( aes(fill = preSupport), position = "fill", binwidth = 1) + theme_bw() + 
scale_fill_grey(start = 0.5, end = 0.75, name = "", labels=c("No Temporary Mechanical Circulatory Support", "Temporary Mechanical Circulatory Support")) + 
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
 
#ggsave("V2-NumberOfTransplantsOverTime.png", width = 12, height = 4.5)


summaryData <- ddply(data[data$died >= 0,], .(year, preSupport), summarize, total = sum(count), support = support[1], preSupport = preSupport[1],
	mortality = sum(died)*1.0/sum(count), 
	hasReoperation= sum(hasReoperation) * 1.0/sum(count), 
	hasARF= sum(hasARF) * 1.0 / sum(count), 
	hasStroke = sum(hasStroke ) * 1.0 / sum(count), 
	hasLiverFailure = sum(hasLiverFailure ) * 1.0 / sum(count), 
	hasSepsis = sum(hasSepsis ) * 1.0 / sum(count), 
	hasRespiratoryFailure = sum(hasRespiratoryFailure ) * 1.0 / sum(count), 
	preECMO = sum(preECMO),
	preIABP = sum(preIABP),
	prePVAD = sum(prePVAD))

qplot(data = summaryData, x = year, y = total, color = preSupport, geom = c("line", "point")) + 
geom_smooth( method = "lm", level = 0.67) + theme_bw()
#ggsave("TotalTrendOverTime.png") 

qplot(data = summaryData, x = year, y = mortality, color = preSupport, geom = c("line", "point")) + 
geom_smooth( method = "lm", level = 0.67) + theme_bw() + 
scale_color_brewer( palette = "Set1", name = "", labels=c("No Temporary Mechanical Circulatory Support", "Temporary Mechanical Circulatory Support")) + 
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#ggsave("V2-MortalityTrendOverTime.png", width = 12, height = 4.5) 



qplot(data = summaryData, x = year, y = hasARF, color = preSupport, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67)  + theme_bw() + 
scale_color_brewer( palette = "Set1",  name = "", labels=c("No Temporary Mechanical Circulatory Support", "Temporary Mechanical Circulatory Support")) +
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#ggsave("V3-RenalFailureTrendOverTime.png", width = 12, height = 4.5)

qplot(data = summaryData, x = year, y = hasStroke , color = preSupport, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) + theme_bw() + 
scale_color_brewer( palette = "Set1", name = "", labels=c("No Temporary Mechanical Circulatory Support", "Temporary Mechanical Circulatory Support")) +
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#ggsave("v3-StrokeTrendOverTime.png", width = 10, height = 4.5)



qplot(data = summaryData, x = year, y = hasARF, color = preSupport, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) + 
theme_bw() + scale_color_brewer( palette = "Set1", name = "Mechanical\nCirculatory\nSupport") + 
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#ggsave("RenalFailureTrendOverTime.png", width = 10, height = 4.5)

qplot(data = summaryData, x = year, y = hasStroke , color = preSupport, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) +
theme_bw() + scale_color_brewer( palette = "Set1", name = "Mechanical\nCirculatory\nSupport") + 
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#ggsave("StrokeTrendOverTime.png", width = 10, height = 4.5)

summaryData <- ddply(data[data$died >= 0,], .(year, support), summarize, total = sum(count), support = support[1], preSupport = preSupport[1],
	mortality = sum(died)*1.0/sum(count), 
	hasReoperation= sum(hasReoperation) * 1.0/sum(count), 
	hasARF= sum(hasARF) * 1.0 / sum(count), 
	hasStroke = sum(hasStroke ) * 1.0 / sum(count), 
	hasLiverFailure = sum(hasLiverFailure ) * 1.0 / sum(count), 
	hasSepsis = sum(hasSepsis ) * 1.0 / sum(count), 
	hasRespiratoryFailure = sum(hasRespiratoryFailure ) * 1.0 / sum(count), 
	preECMO = sum(preECMO),
	preIABP = sum(preIABP),
	prePVAD = sum(prePVAD))


#write.csv(summaryData, "breakdownByYearAndTypeOfSupportv2.csv")

qplot(data = summaryData, x = year, y = total, color = support, geom = c("line", "point")) + geom_smooth( method = "lm") + facet_wrap( ~ support, scale = "free_y")
#ggsave("NumbersTrendOverTimeFaceted.png")

qplot(data = summaryData, x = year, y = mortality, color = support, geom = c("line", "point")) + geom_smooth( method = "lm") + facet_wrap( ~ support, scale = "free_y")
#ggsave("MortalityTrendOverTimeFaceted.png")

qplot(data = summaryData, x = year, y = mortality, color = support, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) 
#ggsave("MortalityTrendOverTime.png")


summaryIABP <- ddply(data[data$died >= 0 & data$preSupport ,], .(year), summarize, type = "IABP", total = sum(preIABP))
summaryECMO <- ddply(data[data$died >= 0 & data$preSupport ,], .(year), summarize, type = "ECMO", total = sum(preECMO))
summaryPVAD <- ddply(data[data$died >= 0 & data$preSupport ,], .(year), summarize, type = "PVAD", total = sum(prePVAD))

allSummary <- rbind(summaryIABP, summaryECMO, summaryPVAD)

qplot(data = allSummary, x = year, y = total, color = type, geom = c("line", "point")) + #geom_smooth( method = "lm", level = 0.67) +
theme_bw() + scale_color_brewer( palette = "Set1", name = "Type of\nMechanical\nCirculatory\nSupport") + 
xlab("Year") + ylab("Proportion of Patients") + 
scale_x_continuous(breaks = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#ggsave("TypeTrendOverTime.png", width = 10, height = 4.5)



qplot(data = summaryData, x = year, y = hasReoperation, color = support, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) 
#ggsave("ReoperationTrendOverTime.png")

qplot(data = summaryData, x = year, y = hasARF, color = support, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) 
#ggsave("RenalFailureTrendOverTime.png")

qplot(data = summaryData, x = year, y = hasStroke , color = support, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) 
#ggsave("StrokeTrendOverTime.png")

qplot(data = summaryData, x = year, y = hasLiverFailure , color = support, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) 
#ggsave("LiverFailureTrendOverTime.png")


qplot(data = summaryData, x = year, y = hasSepsis , color = support, geom = c("line", "point")) + geom_smooth( method = "lm", level = 0.67) 
#ggsave("SepsisTrendOverTime.png")




model1 <- glm(data = summaryData, total ~ year)
summary(model1)
model2 <- glm(data = summaryData[summaryData$support == "None",], total ~ year)
summary(model2)
model3 <- glm(data = summaryData[summaryData$support == "Yes",], total ~ year)
summary(model3)


sum(data[(data$preIABP | data$prePVAD | data$preECMO) & (data$year == "1998"|data$year == "1999"|data$year == "2000"),]$hasOHT)



sum(data[(data$preIABP | data$prePVAD | data$preECMO) & (data$year == "2009"|data$year == "2010"|data$year == "2011"),]$hasOHT)

51/3
99/3

data <- data[data$los >= 0,]
library(survival)
data$SurvObj <- with(data, Surv(los, died == 1))
data$preSupport <- 0
data[data$preIABP | data$prePVAD | data$preECMO,]$preSupport <- 1
km.by.support <- survfit(SurvObj ~ preSupport , data = data, conf.type = "log-log")
plot(km.by.support)




table(data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$location)/ length(data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$sex)
table(data[data$hasIABP,]$location)/ length(data[data$hasIABP,]$sex)
table(data[data$hasPVAD,]$location)/ length(data[data$hasPVAD,]$sex)
table(data[data$preECMO,]$location)/ length(data[data$preECMO,]$sex)
table(data$location)/ length(data$sex)

a <- lm(data = data, died ~ year + age + pay1 + sex + zip + race + dayofOHT + numDiags + Diabetes + Hyperlipidemia + Hypertension + Smoking + Obesity)
summary(a)
AIC(a)

b <- lm(data = data, died ~ age + pay1 + sex + zip  + dayofOHT + numDiags + Diabetes + Hyperlipidemia + Hypertension + Smoking + Obesity)
summary(b)
AIC(b)

c <- lm(data = data, died ~ age + zip + race + numDiags + dayofOHT )
summary(c)
AIC(c)


prop.test(x = c(sum(data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$died), sum(data[data$hasIABP,]$died)), 
n = c(sum(!data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$died), sum(!data[data$hasIABP,]$died)), correct = TRUE)

prop.test(x = c(sum(data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$died), sum(data[data$hasPVAD,]$died)), 
n = c(sum(!data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$died), sum(!data[data$hasPVAD,]$died)), correct = TRUE)

prop.test(x = c(sum(data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$died), sum(data[data$preECMO,]$died)), 
n = c(sum(!data[!(data$hasIABP | data$hasPVAD | data$preECMO),]$died), sum(!data[data$preECMO,]$died)), correct = TRUE)


data2 = data[!(data$preIABP | data$prePVAD | data$preECMO),]
data3 = data[(data$preIABP | data$prePVAD | data$preECMO),]
t.test(data3[data3$age>=0,]$age, data2[data2$age>=0,]$age)
t.test(data3[data3$los>=0,]$los, data2[data2$los>=0,]$los)
t.test(data3$numDiags, data2$numDiags)

t.test(data3$IschemicHD , data2$IschemicHD )
t.test(data3$Hypertension, data2$Hypertension)
t.test(data3$Diabetes , data2$Diabetes )
t.test(data3$Smoking , data2$Smoking )
t.test(data3$Obesity , data2$Obesity )


data$preSupport <- (data$preIABP | data$prePVAD | data$preECMO)

summary(aov(preSupport ~ race, data = data))
summary(aov(preSupport ~ zip, data = data))
summary(aov(preSupport ~ bedsize, data = data))





t.test(data3$sex , data2$sex )


prop.test(data3$race , data2$race )


model1 <- glm(data = summaryData, total ~ year)
summary(model1)
model2 <- glm(data = summaryData[summaryData$support == "None",], total ~ year)
summary(model2)
model3 <- glm(data = summaryData[summaryData$support == "ECMO",], total ~ year)
summary(model3)
model4 <- glm(data = summaryData[summaryData$support == "IABP",], total ~ year)
summary(model4)
model5 <- glm(data = summaryData[summaryData$support == "PVAD",], total ~ year)
summary(model5)


model6 <- glm(data = summaryData[summaryData$support != "None",], total ~ year)
summary(model6)


str(summaryData[summaryData$support == "PVAD",])
sum(summaryData[summaryData$support == "PVAD",]$total)
sum(summaryData[summaryData$support == "PVAD" & summaryData$year >= 2008,]$total)
77/110









########### Separating Data by Era and Doing Comparison Analysis #############
allData <- data
data <- allData[allData$year <= 2006,]
#data <- allData[allData$year > 2006,]


sum(allData[(allData$preIABP | allData$prePVAD | allData$preECMO),]$count)
sum(allData[!(allData$preIABP | allData$prePVAD | allData$preECMO),]$count)
sum(allData$count)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)

data[data$sex == -9,]$sex = 0
data[data$race != 1,]$race = 2
data$raceFactor <- factor(data$race)
data$sexFactor <- factor(data$sex)
data$payFactor <- factor(data$pay1)
data$zipFactor <- factor(data$zip)

data$era <- ""
data[data$year >= 2007,]$era <- "later"
data[data$year < 2007,]$era <- "earlier"

data$decade <- floor(data$age/10)*10


####### Death Model ####### 6/4/2017

deathModel <- glm(data = data, died ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(deathModel )
exp(cbind(OR = coef(deathModel ), confint(deathModel )))


# Stroke Model 6/4/17

strokeModel <- glm(data = data, hasStroke ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(strokeModel )
exp(cbind(OR = coef(strokeModel ), confint(strokeModel )))


# Stroke Model 6/4/17

strokeModel <- glm(data = data, hasStroke ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(strokeModel )
exp(cbind(OR = coef(strokeModel ), confint(strokeModel )))


# Renal Failure Model 6/4/17

arfModel <- glm(data = data, hasARF ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(arfModel )
exp(cbind(OR = coef(arfModel ), confint(arfModel )))





####### Death Model ####### 10/18/2017

deathModel <- glm(data = data, died ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(deathModel )
exp(cbind(OR = coef(deathModel ), confint(deathModel )))


# Stroke Model 10/18/2017

strokeModel <- glm(data = data, hasStroke ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(strokeModel )
exp(cbind(OR = coef(strokeModel ), confint(strokeModel )))


# Stroke Model 10/18/2017

strokeModel <- glm(data = data, hasStroke ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(strokeModel )
exp(cbind(OR = coef(strokeModel ), confint(strokeModel )))


# Renal Failure Model 10/18/2017

arfModel <- glm(data = data, hasARF ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(arfModel )
exp(cbind(OR = coef(arfModel ), confint(arfModel )))










model <- glm(data = data, died ~ support + Diabetes + PVD + CKD + Hypertension + IschemicHD + Smoking + Obesity + age + raceFactor + sexFactor )
a <- glm(data = data, died ~ year + age + payFactor + sexFactor + zip + raceFactor + dayofOHT + numDiags + support + Diabetes + PVD + CKD + Hypertension + IschemicHD + Smoking + Obesity + age + raceFactor + sexFactor)
AIC(a)
summary(a)

a <- glm(data = data, died ~ year + age + sexFactor + raceFactor + dayofOHT + numDiags + support + Diabetes + PVD + CKD + Hypertension + IschemicHD + Smoking + Obesity)
AIC(a)
summary(a)
exp(cbind(OR = coef(a), confint(a)))

a <- glm(data = data, died ~ year + age + sexFactor + raceFactor + dayofOHT + numDiags + support + Diabetes + PVD + IschemicHD + Smoking + Obesity)
AIC(a)
summary(a)

a <- glm(data = data, died ~ year + age + sexFactor + raceFactor + dayofOHT + numDiags + support + PVD)
AIC(a)
summary(a)

a <- glm(data = data, died ~ age + sexFactor + raceFactor + numDiags + support + PVD + Obesity + Hypertension + CKD)
AIC(a)
summary(a)
exp(cbind(OR = coef(a), confint(a)))

a <- glm(data = data, died ~ age + sexFactor + year + numDiags + support + Diabetes + PVD + Obesity + Hypertension + CKD)
AIC(a)
summary(a)
exp(cbind(OR = coef(a), confint(a)))

a <- glm(data = data, died ~ age + sexFactor + raceFactor + preECMO + preIABP + prePVAD + Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD + year )
AIC(a)
summary(a)
exp(cbind(OR = coef(a), confint(a)))




summary(data[(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)
sd(data[(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)
summary(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)
sd(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)

t.test(data[(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los, data[!(data$preIABP | data$prePVAD | data$preECMO) & data$los >= 0,]$los)



sum(data[(data$preIABP | data$prePVAD | data$preECMO) & data$died>= 0,]$died)
sum(data[(data$preIABP | data$prePVAD | data$preECMO) & data$died>= 0,]$died)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$died>= 0,]$died)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO) & data$died>= 0,]$died)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)

t.test(data[(data$preIABP | data$prePVAD | data$preECMO) & data$died>= 0,]$died, data[!(data$preIABP | data$prePVAD | data$preECMO) & data$died>= 0,]$died)


model <- glm(data = data, died ~ support + Diabetes + PVD + CKD + Hypertension + IschemicHD + Smoking + Obesity + age + race + sex)
summary(model)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$postSupport == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)

t.test(data[(data$preIABP | data$prePVAD | data$preECMO),]$postSupport , data[!(data$preIABP | data$prePVAD | data$preECMO),]$postSupport )




sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasARF == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasLiverFailure == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasRespiratoryFailure == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$cardiacComp == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasSepsis == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasStroke == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)

sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasReoperation == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)


sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)
sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)/sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count)
sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)/sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)
prop.test(c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1), sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$hasTransfusion == 1)), 
c(sum(data[(data$preIABP | data$prePVAD | data$preECMO),]$count),sum(data[!(data$preIABP | data$prePVAD | data$preECMO),]$count)), correct = TRUE)






sum(data[data$died>= 0,]$died)/sum(data$count)


#data[data$preECMO,]$support <- "ECMO"
#data[data$preIABP,]$support <- "IABP"
#data[data$prePVAD,]$support <- "PVAD"

# Verifying and confirming dip's analysis
ecmo <- data[data$preECMO,]
str(ecmo)
#132
sum(ecmo$died)
#14

iabp <- data[data$preIABP,]
str(iabp )
#342
sum(iabp$died)
#18

pvad <- data[data$prePVAD,]
str(pvad )
#21
sum(pvad$died)
#0

ecmoAndiabp <- data[data$preECMO &  data$preIABP,]
str(ecmoAndiabp )
#27
sum(ecmoAndiabp$died)
#1

ecmoAndpvad <- data[data$preECMO &  data$prePVAD,]
str(ecmoAndpvad )
#3
sum(ecmoAndpvad$died)
#0

iabpAndpvad <- data[data$preIABP &  data$prePVAD,]
str(iabpAndpvad )
#9
sum(iabpAndpvad $died)
#0


all <- data[data$preECMO & data$prePVAD & data$preIABP,]
str(all)
#0

support <- data[data$preECMO | data$prePVAD | data$preIABP,]
str(support)
#456


justPre <- data[data$preSupport,]
str(justPre)

justPre <- data

justPre[justPre$dayofECMO == -99,]$dayofECMO<- 9999
justPre[justPre$dayofIABP== -99,]$dayofIABP<- 9999
justPre[justPre$dayofPVAD== -99,]$dayofPVAD<- 9999
justPre$minDay <- pmin(justPre$dayofECMO, justPre$dayofIABP, justPre$dayofPVAD) 
justPre$dayofOHT
justPre$durationOfSupport <- justPre$dayofOHT - justPre$minDay

summary(justPre$durationOfSupport)
sd(justPre$durationOfSupport)


summary(justPre[justPre$year <= 2006,]$durationOfSupport)
sd(justPre[justPre$year <= 2006,]$durationOfSupport)

summary(justPre[justPre$year <= 2006,]$durationOfSupport)
sd(justPre[justPre$year <= 2006,]$durationOfSupport)

data <- justPre
data[!data$preSupport,]$durationOfSupport <- NA


summary(justPre$durationOfSupport)
sd(justPre$durationOfSupport)
qplot(justPre$durationOfSupport, binwidth = 1) +xlim(0,60)+
theme_bw() + xlab("Duration of Mechanical Support") + ylab("Number of Patients")  
#ggsave("DurationOfMechanicalSupport.png", width = 6.5, height = 4.5)


deathModel <- glm(data = data, died ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
Diabetes + PVD + Obesity + Hypertension + Smoking + CKD + IschemicHD +
era + numDiags)
summary(deathModel )
exp(cbind(OR = coef(deathModel ), confint(deathModel )))




####### Death Model ####### 10/18/2017

deathModel <- glm(data = data, died ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD + 
era + numDiags)
summary(deathModel )
exp(cbind(OR = coef(deathModel ), confint(deathModel )))

# Stroke Model 10/18/2017

strokeModel <- glm(data = data, hasStroke ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
era + numDiags)
summary(strokeModel )
exp(cbind(OR = coef(strokeModel ), confint(strokeModel )))


# Renal Failure Model 10/18/2017

arfModel <- glm(data = data, hasARF ~ decade + race + 
 sexFactor + preECMO + preIABP + prePVAD +
era + numDiags)
summary(arfModel )
exp(cbind(OR = coef(arfModel ), confint(arfModel )))



summary(data[(data$preIABP ) & data$los >= 0,]$los)
sd(data[(data$preIABP ) & data$los >= 0,]$los)

summary(data[(data$prePVAD) & data$los >= 0,]$los)
sd(data[(data$prePVAD ) & data$los >= 0,]$los)

summary(data[( data$preECMO) & data$los >= 0,]$los)
sd(data[(data$preECMO) & data$los >= 0,]$los)




summary(data[(data$preIABP ) & data$los >= 0,]$los - data[(data$preIABP ) & data$los >= 0,]$dayofOHT)
sd(data[(data$preIABP ) & data$los >= 0,]$los - data[(data$preIABP ) & data$los >= 0,]$dayofOHT)

summary(data[(data$prePVAD) & data$los >= 0,]$los - data[(data$prePVAD) & data$los >= 0,]$dayofOHT)
sd(data[(data$prePVAD ) & data$los >= 0,]$los - data[(data$prePVAD) & data$los >= 0,]$dayofOHT)

summary(data[( data$preECMO) & data$los >= 0,]$los - data[(data$preECMO) & data$los >= 0,]$dayofOHT)
sd(data[(data$preECMO) & data$los >= 0,]$los - data[(data$preECMO) & data$los >= 0,]$dayofOHT)





summary(data[(data$preIABP ) & data$los >= 0,]$durationOfSupport)
sd(data[(data$preIABP ) & data$los >= 0,]$durationOfSupport)

summary(data[(data$prePVAD) & data$los >= 0,]$durationOfSupport)
sd(data[(data$prePVAD ) & data$los >= 0,]$durationOfSupport)

summary(data[( data$preECMO) & data$los >= 0,]$durationOfSupport)
sd(data[(data$preECMO) & data$los >= 0,]$durationOfSupport)






table(data[(data$preIABP ),]$sex)
table(data[(data$preIABP ),]$sex)/342


table(data[(data$prePVAD),]$sex)
table(data[(data$prePVAD),]$sex)/21


table(data[( data$preECMO),]$sex)
table(data[( data$preECMO),]$sex)/132







table(data[(data$preIABP ),]$race)
table(data[(data$preIABP ),]$race)/342


table(data[(data$prePVAD),]$race)
table(data[(data$prePVAD),]$race)/21


table(data[( data$preECMO),]$race)
table(data[( data$preECMO),]$race)/132



table(data[(data$preIABP ),]$zip)
table(data[(data$preIABP ),]$zip)/342


table(data[(data$prePVAD),]$zip)
table(data[(data$prePVAD),]$zip)/21


table(data[( data$preECMO),]$zip)
table(data[( data$preECMO),]$zip)/132



sum(data[(data$preIABP),]$Diabetes)
sum(data[(data$prePVAD),]$Diabetes)
sum(data[(data$preECMO),]$Diabetes)
sum(data[(data$preIABP),]$Diabetes)/342
sum(data[(data$prePVAD),]$Diabetes)/21
sum(data[(data$preECMO),]$Diabetes)/132


sum(data[(data$preIABP),]$Diabetes)
sum(data[(data$prePVAD),]$Diabetes)
sum(data[(data$preECMO),]$Diabetes)
sum(data[(data$preIABP),]$Diabetes)/342
sum(data[(data$prePVAD),]$Diabetes)/21
sum(data[(data$preECMO),]$Diabetes)/132


sum(data[(data$preIABP),]$IschemicHD )
sum(data[(data$prePVAD),]$IschemicHD )
sum(data[(data$preECMO),]$IschemicHD )
sum(data[(data$preIABP),]$IschemicHD )/342
sum(data[(data$prePVAD),]$IschemicHD )/21
sum(data[(data$preECMO),]$IschemicHD )/132



sum(data[(data$preIABP),]$Hypertension )
sum(data[(data$prePVAD),]$Hypertension )
sum(data[(data$preECMO),]$Hypertension )
sum(data[(data$preIABP),]$Hypertension )/342
sum(data[(data$prePVAD),]$Hypertension )/21
sum(data[(data$preECMO),]$Hypertension )/132


sum(data[(data$preIABP),]$CKD )
sum(data[(data$prePVAD),]$CKD )
sum(data[(data$preECMO),]$CKD )
sum(data[(data$preIABP),]$CKD )/342
sum(data[(data$prePVAD),]$CKD )/21
sum(data[(data$preECMO),]$CKD )/132

sum(data[(data$preIABP),]$PVD )
sum(data[(data$prePVAD),]$PVD )
sum(data[(data$preECMO),]$PVD )
sum(data[(data$preIABP),]$PVD )/342
sum(data[(data$prePVAD),]$PVD )/21
sum(data[(data$preECMO),]$PVD )/132

sum(data[(data$preIABP),]$Smoking )
sum(data[(data$prePVAD),]$Smoking )
sum(data[(data$preECMO),]$Smoking )
sum(data[(data$preIABP),]$Smoking )/342
sum(data[(data$prePVAD),]$Smoking )/21
sum(data[(data$preECMO),]$Smoking )/132


sum(data[(data$preIABP),]$Obesity )
sum(data[(data$prePVAD),]$Obesity )
sum(data[(data$preECMO),]$Obesity )
sum(data[(data$preIABP),]$Obesity )/342
sum(data[(data$prePVAD),]$Obesity )/21
sum(data[(data$preECMO),]$Obesity )/132

table(data[(data$preIABP),]$bedsize)
table(data[(data$prePVAD),]$bedsize)
table(data[(data$preECMO),]$bedsize)
table(data[(data$preIABP),]$bedsize)/342
table(data[(data$prePVAD),]$bedsize)/21
table(data[(data$preECMO),]$bedsize)/132


