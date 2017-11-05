setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD\\IABP vs PVAD")
data <- read.csv("PVAD for R.csv", stringsAsFactors = FALSE)

library(ggplot2)

data$race <- factor(data$race )
data$zip <- factor(data$zip )
data$hospital <- factor(data$hospital )
data$sex <- factor(data$sex )
data$volume <- "Medium"
data[data$Volume.year >= 11.5,]$volume <- "High"
data[data$Volume.year <= 3.375,]$volume <- "Low"




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



data$Diabetes <- ("250" == substr(data$p1,1,3) | "250" == substr(data$p2,1,3) | "250" == substr(data$p3,1,3) | "250" ==  substr(data$p4,1,3) |
		"250" == substr(data$p5,1,3) | "250" == substr(data$p6,1,3) | "250" == substr(data$p7,1,3) | "250" == substr(data$p8,1,3) | "250" == substr(data$p9,1,3) |
		"250" == substr(data$p10,1,3) | "250" == substr(data$p11,1,3) | "250" ==  substr(data$p12,1,3) | "250" == substr(data$p13,1,3) | "250" ==  substr(data$d4,1,3) |
		"250" == substr(data$p14,1,3) | "250" == substr(data$p15,1,3) | "250" == substr(data$d1,1,3) | "250" == substr(data$d2,1,3) | "250" == substr(data$d3,1,3) | 
		"250" == substr(data$d5,1,3) | "250" == substr(data$d6,1,3) | "250" == substr(data$d7,1,3) | "250" == substr(data$d8,1,3) | "250" == substr(data$d9,1,3) |
		"250" == substr(data$d10,1,3) | "250" == substr(data$d11,1,3) | "250" ==  substr(data$d12,1,3) | "250" == substr(data$d13,1,3) |
		"250" == substr(data$d14,1,3) | "250" == substr(data$d15,1,3) ) | ("249" == substr(data$p1,1,3) | "249" == substr(data$p2,1,3) | "249" == substr(data$p3,1,3) | "249" ==  substr(data$p4,1,3) |
		"249" == substr(data$p5,1,3) | "249" == substr(data$p6,1,3) | "249" == substr(data$p7,1,3) | "249" == substr(data$p8,1,3) | "249" == substr(data$p9,1,3) |
		"249" == substr(data$p10,1,3) | "249" == substr(data$p11,1,3) | "249" ==  substr(data$p12,1,3) | "249" == substr(data$p13,1,3) | "249" ==  substr(data$d4,1,3) |
		"249" == substr(data$p14,1,3) | "249" == substr(data$p15,1,3) | "249" == substr(data$d1,1,3) | "249" == substr(data$d2,1,3) | "249" == substr(data$d3,1,3) | 
		"249" == substr(data$d5,1,3) | "249" == substr(data$d6,1,3) | "249" == substr(data$d7,1,3) | "249" == substr(data$d8,1,3) | "249" == substr(data$d9,1,3) |
		"249" == substr(data$d10,1,3) | "249" == substr(data$d11,1,3) | "249" ==  substr(data$d12,1,3) | "249" == substr(data$d13,1,3) |
		"249" == substr(data$d14,1,3) | "249" == substr(data$d15,1,3) )

data$PVD <- ("440" == substr(data$p1,1,3) | "440" == substr(data$p2,1,3) | "440" == substr(data$p3,1,3) | "440" ==  substr(data$p4,1,3) |
		"440" == substr(data$p5,1,3) | "440" == substr(data$p6,1,3) | "440" == substr(data$p7,1,3) | "440" == substr(data$p8,1,3) | "440" == substr(data$p9,1,3) |
		"440" == substr(data$p10,1,3) | "440" == substr(data$p11,1,3) | "440" ==  substr(data$p12,1,3) | "440" == substr(data$p13,1,3) | "440" ==  substr(data$d4,1,3) |
		"440" == substr(data$p14,1,3) | "440" == substr(data$p15,1,3) | "440" == substr(data$d1,1,3) | "440" == substr(data$d2,1,3) | "440" == substr(data$d3,1,3) | 
		"440" == substr(data$d5,1,3) | "440" == substr(data$d6,1,3) | "440" == substr(data$d7,1,3) | "440" == substr(data$d8,1,3) | "440" == substr(data$d9,1,3) |
		"440" == substr(data$d10,1,3) | "440" == substr(data$d11,1,3) | "440" ==  substr(data$d12,1,3) | "440" == substr(data$d13,1,3) |
		"440" == substr(data$d14,1,3) | "440" == substr(data$d15,1,3) ) | ("443" == substr(data$p1,1,3) | "443" == substr(data$p2,1,3) | "443" == substr(data$p3,1,3) | "443" ==  substr(data$p4,1,3) |
		"443" == substr(data$p5,1,3) | "443" == substr(data$p6,1,3) | "443" == substr(data$p7,1,3) | "443" == substr(data$p8,1,3) | "443" == substr(data$p9,1,3) |
		"443" == substr(data$p10,1,3) | "443" == substr(data$p11,1,3) | "443" ==  substr(data$p12,1,3) | "443" == substr(data$p13,1,3) | "443" ==  substr(data$d4,1,3) |
		"443" == substr(data$p14,1,3) | "443" == substr(data$p15,1,3) | "443" == substr(data$d1,1,3) | "443" == substr(data$d2,1,3) | "443" == substr(data$d3,1,3) | 
		"443" == substr(data$d5,1,3) | "443" == substr(data$d6,1,3) | "443" == substr(data$d7,1,3) | "443" == substr(data$d8,1,3) | "443" == substr(data$d9,1,3) |
		"443" == substr(data$d10,1,3) | "443" == substr(data$d11,1,3) | "443" ==  substr(data$d12,1,3) | "443" == substr(data$d13,1,3) |
		"443" == substr(data$d14,1,3) | "443" == substr(data$d15,1,3) ) | ("4471" == substr(data$p1,1,4) | "4471" == substr(data$p2,1,4) | "4471" == substr(data$p3,1,4) | "4471" ==  substr(data$p4,1,4) |
		"4471" == substr(data$p5,1,4) | "4471" == substr(data$p6,1,4) | "4471" == substr(data$p7,1,4) | "4471" == substr(data$p8,1,4) | "4471" == substr(data$p9,1,4) |
		"4471" == substr(data$p10,1,4) | "4471" == substr(data$p11,1,4) | "4471" ==  substr(data$p12,1,4) | "4471" == substr(data$p13,1,4) | "4471" ==  substr(data$d4,1,4) |
		"4471" == substr(data$p14,1,4) | "4471" == substr(data$p15,1,4) | "4471" == substr(data$d1,1,4) | "4471" == substr(data$d2,1,4) | "4471" == substr(data$d3,1,4) | 
		"4471" == substr(data$d5,1,4) | "4471" == substr(data$d6,1,4) | "4471" == substr(data$d7,1,4) | "4471" == substr(data$d8,1,4) | "4471" == substr(data$d9,1,4) |
		"4471" == substr(data$d10,1,4) | "4471" == substr(data$d11,1,4) | "4471" ==  substr(data$d12,1,4) | "4471" == substr(data$d13,1,4) |
		"4471" == substr(data$d14,1,4) | "4471" == substr(data$d15,1,4) ) | ("V434" == substr(data$p1,1,4) | "V434" == substr(data$p2,1,4) | "V434" == substr(data$p3,1,4) | "V434" ==  substr(data$p4,1,4) |
		"V434" == substr(data$p5,1,4) | "V434" == substr(data$p6,1,4) | "V434" == substr(data$p7,1,4) | "V434" == substr(data$p8,1,4) | "V434" == substr(data$p9,1,4) |
		"V434" == substr(data$p10,1,4) | "V434" == substr(data$p11,1,4) | "V434" ==  substr(data$p12,1,4) | "V434" == substr(data$p13,1,4) | "V434" ==  substr(data$d4,1,4) |
		"V434" == substr(data$p14,1,4) | "V434" == substr(data$p15,1,4) | "V434" == substr(data$d1,1,4) | "V434" == substr(data$d2,1,4) | "V434" == substr(data$d3,1,4) | 
		"V434" == substr(data$d5,1,4) | "V434" == substr(data$d6,1,4) | "V434" == substr(data$d7,1,4) | "V434" == substr(data$d8,1,4) | "V434" == substr(data$d9,1,4) |
		"V434" == substr(data$d10,1,4) | "V434" == substr(data$d11,1,4) | "V434" ==  substr(data$d12,1,4) | "V434" == substr(data$d13,1,4) |
		"V434" == substr(data$d14,1,4) | "V434" == substr(data$d15,1,4) )

data$CKD <- ("585" == substr(data$p1,1,3) | "585" == substr(data$p2,1,3) | "585" == substr(data$p3,1,3) | "585" ==  substr(data$p4,1,3) |
		"585" == substr(data$p5,1,3) | "585" == substr(data$p6,1,3) | "585" == substr(data$p7,1,3) | "585" == substr(data$p8,1,3) | "585" == substr(data$p9,1,3) |
		"585" == substr(data$p10,1,3) | "585" == substr(data$p11,1,3) | "585" ==  substr(data$p12,1,3) | "585" == substr(data$p13,1,3) | "585" ==  substr(data$d4,1,3) |
		"585" == substr(data$p14,1,3) | "585" == substr(data$p15,1,3) | "585" == substr(data$d1,1,3) | "585" == substr(data$d2,1,3) | "585" == substr(data$d3,1,3) | 
		"585" == substr(data$d5,1,3) | "585" == substr(data$d6,1,3) | "585" == substr(data$d7,1,3) | "585" == substr(data$d8,1,3) | "585" == substr(data$d9,1,3) |
		"585" == substr(data$d10,1,3) | "585" == substr(data$d11,1,3) | "585" ==  substr(data$d12,1,3) | "585" == substr(data$d13,1,3) |
		"585" == substr(data$d14,1,3) | "585" == substr(data$d15,1,3) ) | ("V45" == substr(data$p1,1,3) | "V45" == substr(data$p2,1,3) | "V45" == substr(data$p3,1,3) | "V45" ==  substr(data$p4,1,3) |
		"V45" == substr(data$p5,1,3) | "V45" == substr(data$p6,1,3) | "V45" == substr(data$p7,1,3) | "V45" == substr(data$p8,1,3) | "V45" == substr(data$p9,1,3) |
		"V45" == substr(data$p10,1,3) | "V45" == substr(data$p11,1,3) | "V45" ==  substr(data$p12,1,3) | "V45" == substr(data$p13,1,3) | "V45" ==  substr(data$d4,1,3) |
		"V45" == substr(data$p14,1,3) | "V45" == substr(data$p15,1,3) | "V45" == substr(data$d1,1,3) | "V45" == substr(data$d2,1,3) | "V45" == substr(data$d3,1,3) | 
		"V45" == substr(data$d5,1,3) | "V45" == substr(data$d6,1,3) | "V45" == substr(data$d7,1,3) | "V45" == substr(data$d8,1,3) | "V45" == substr(data$d9,1,3) |
		"V45" == substr(data$d10,1,3) | "V45" == substr(data$d11,1,3) | "V45" ==  substr(data$d12,1,3) | "V45" == substr(data$d13,1,3) |
		"V45" == substr(data$d14,1,3) | "V45" == substr(data$d15,1,3) ) | ("V56" == substr(data$p1,1,3) | "V56" == substr(data$p2,1,3) | "V56" == substr(data$p3,1,3) | "V56" ==  substr(data$p4,1,3) |
		"V56" == substr(data$p5,1,3) | "V56" == substr(data$p6,1,3) | "V56" == substr(data$p7,1,3) | "V56" == substr(data$p8,1,3) | "V56" == substr(data$p9,1,3) |
		"V56" == substr(data$p10,1,3) | "V56" == substr(data$p11,1,3) | "V56" ==  substr(data$p12,1,3) | "V56" == substr(data$p13,1,3) | "V56" ==  substr(data$d4,1,3) |
		"V56" == substr(data$p14,1,3) | "V56" == substr(data$p15,1,3) | "V56" == substr(data$d1,1,3) | "V56" == substr(data$d2,1,3) | "V56" == substr(data$d3,1,3) | 
		"V56" == substr(data$d5,1,3) | "V56" == substr(data$d6,1,3) | "V56" == substr(data$d7,1,3) | "V56" == substr(data$d8,1,3) | "V56" == substr(data$d9,1,3) |
		"V56" == substr(data$d10,1,3) | "V56" == substr(data$d11,1,3) | "V56" ==  substr(data$d12,1,3) | "V56" == substr(data$d13,1,3) |
		"V56" == substr(data$d14,1,3) | "V56" == substr(data$d15,1,3) )


data$Hypertension <- ("401" == substr(data$p1,1,3) | "401" == substr(data$p2,1,3) | "401" == substr(data$p3,1,3) | "401" ==  substr(data$p4,1,3) |
		"401" == substr(data$p5,1,3) | "401" == substr(data$p6,1,3) | "401" == substr(data$p7,1,3) | "401" == substr(data$p8,1,3) | "401" == substr(data$p9,1,3) |
		"401" == substr(data$p10,1,3) | "401" == substr(data$p11,1,3) | "401" ==  substr(data$p12,1,3) | "401" == substr(data$p13,1,3) | "401" ==  substr(data$d4,1,3) |
		"401" == substr(data$p14,1,3) | "401" == substr(data$p15,1,3) | "401" == substr(data$d1,1,3) | "401" == substr(data$d2,1,3) | "401" == substr(data$d3,1,3) | 
		"401" == substr(data$d5,1,3) | "401" == substr(data$d6,1,3) | "401" == substr(data$d7,1,3) | "401" == substr(data$d8,1,3) | "401" == substr(data$d9,1,3) |
		"401" == substr(data$d10,1,3) | "401" == substr(data$d11,1,3) | "401" ==  substr(data$d12,1,3) | "401" == substr(data$d13,1,3) |
		"401" == substr(data$d14,1,3) | "401" == substr(data$d15,1,3) ) | ("402" == substr(data$p1,1,3) | "402" == substr(data$p2,1,3) | "402" == substr(data$p3,1,3) | "402" ==  substr(data$p4,1,3) |
		"402" == substr(data$p5,1,3) | "402" == substr(data$p6,1,3) | "402" == substr(data$p7,1,3) | "402" == substr(data$p8,1,3) | "402" == substr(data$p9,1,3) |
		"402" == substr(data$p10,1,3) | "402" == substr(data$p11,1,3) | "402" ==  substr(data$p12,1,3) | "402" == substr(data$p13,1,3) | "402" ==  substr(data$d4,1,3) |
		"402" == substr(data$p14,1,3) | "402" == substr(data$p15,1,3) | "402" == substr(data$d1,1,3) | "402" == substr(data$d2,1,3) | "402" == substr(data$d3,1,3) | 
		"402" == substr(data$d5,1,3) | "402" == substr(data$d6,1,3) | "402" == substr(data$d7,1,3) | "402" == substr(data$d8,1,3) | "402" == substr(data$d9,1,3) |
		"402" == substr(data$d10,1,3) | "402" == substr(data$d11,1,3) | "402" ==  substr(data$d12,1,3) | "402" == substr(data$d13,1,3) |
		"402" == substr(data$d14,1,3) | "402" == substr(data$d15,1,3) ) | ("403" == substr(data$p1,1,3) | "403" == substr(data$p2,1,3) | "403" == substr(data$p3,1,3) | "403" ==  substr(data$p4,1,3) |
		"403" == substr(data$p5,1,3) | "403" == substr(data$p6,1,3) | "403" == substr(data$p7,1,3) | "403" == substr(data$p8,1,3) | "403" == substr(data$p9,1,3) |
		"403" == substr(data$p10,1,3) | "403" == substr(data$p11,1,3) | "403" ==  substr(data$p12,1,3) | "403" == substr(data$p13,1,3) | "403" ==  substr(data$d4,1,3) |
		"403" == substr(data$p14,1,3) | "403" == substr(data$p15,1,3) | "403" == substr(data$d1,1,3) | "403" == substr(data$d2,1,3) | "403" == substr(data$d3,1,3) | 
		"403" == substr(data$d5,1,3) | "403" == substr(data$d6,1,3) | "403" == substr(data$d7,1,3) | "403" == substr(data$d8,1,3) | "403" == substr(data$d9,1,3) |
		"403" == substr(data$d10,1,3) | "403" == substr(data$d11,1,3) | "403" ==  substr(data$d12,1,3) | "403" == substr(data$d13,1,3) |
		"403" == substr(data$d14,1,3) | "403" == substr(data$d15,1,3) ) | ("404" == substr(data$p1,1,3) | "404" == substr(data$p2,1,3) | "404" == substr(data$p3,1,3) | "404" ==  substr(data$p4,1,3) |
		"404" == substr(data$p5,1,3) | "404" == substr(data$p6,1,3) | "404" == substr(data$p7,1,3) | "404" == substr(data$p8,1,3) | "404" == substr(data$p9,1,3) |
		"404" == substr(data$p10,1,3) | "404" == substr(data$p11,1,3) | "404" ==  substr(data$p12,1,3) | "404" == substr(data$p13,1,3) | "404" ==  substr(data$d4,1,3) |
		"404" == substr(data$p14,1,3) | "404" == substr(data$p15,1,3) | "404" == substr(data$d1,1,3) | "404" == substr(data$d2,1,3) | "404" == substr(data$d3,1,3) | 
		"404" == substr(data$d5,1,3) | "404" == substr(data$d6,1,3) | "404" == substr(data$d7,1,3) | "404" == substr(data$d8,1,3) | "404" == substr(data$d9,1,3) |
		"404" == substr(data$d10,1,3) | "404" == substr(data$d11,1,3) | "404" ==  substr(data$d12,1,3) | "404" == substr(data$d13,1,3) |
		"404" == substr(data$d14,1,3) | "404" == substr(data$d15,1,3) ) | ("405" == substr(data$p1,1,3) | "405" == substr(data$p2,1,3) | "405" == substr(data$p3,1,3) | "405" ==  substr(data$p4,1,3) |
		"405" == substr(data$p5,1,3) | "405" == substr(data$p6,1,3) | "405" == substr(data$p7,1,3) | "405" == substr(data$p8,1,3) | "405" == substr(data$p9,1,3) |
		"405" == substr(data$p10,1,3) | "405" == substr(data$p11,1,3) | "405" ==  substr(data$p12,1,3) | "405" == substr(data$p13,1,3) | "405" ==  substr(data$d4,1,3) |
		"405" == substr(data$p14,1,3) | "405" == substr(data$p15,1,3) | "405" == substr(data$d1,1,3) | "405" == substr(data$d2,1,3) | "405" == substr(data$d3,1,3) | 
		"405" == substr(data$d5,1,3) | "405" == substr(data$d6,1,3) | "405" == substr(data$d7,1,3) | "405" == substr(data$d8,1,3) | "405" == substr(data$d9,1,3) |
		"405" == substr(data$d10,1,3) | "405" == substr(data$d11,1,3) | "405" ==  substr(data$d12,1,3) | "405" == substr(data$d13,1,3) |
		"405" == substr(data$d14,1,3) | "405" == substr(data$d15,1,3) ) | ("4372" == substr(data$p1,1,4) | "4372" == substr(data$p2,1,4) | "4372" == substr(data$p3,1,4) | "4372" ==  substr(data$p4,1,4) |
		"4372" == substr(data$p5,1,4) | "4372" == substr(data$p6,1,4) | "4372" == substr(data$p7,1,4) | "4372" == substr(data$p8,1,4) | "4372" == substr(data$p9,1,4) |
		"4372" == substr(data$p10,1,4) | "4372" == substr(data$p11,1,4) | "4372" ==  substr(data$p12,1,4) | "4372" == substr(data$p13,1,4) | "4372" ==  substr(data$d4,1,4) |
		"4372" == substr(data$p14,1,4) | "4372" == substr(data$p15,1,4) | "4372" == substr(data$d1,1,4) | "4372" == substr(data$d2,1,4) | "4372" == substr(data$d3,1,4) | 
		"4372" == substr(data$d5,1,4) | "4372" == substr(data$d6,1,4) | "4372" == substr(data$d7,1,4) | "4372" == substr(data$d8,1,4) | "4372" == substr(data$d9,1,4) |
		"4372" == substr(data$d10,1,4) | "4372" == substr(data$d11,1,4) | "4372" ==  substr(data$d12,1,4) | "4372" == substr(data$d13,1,4) |
		"4372" == substr(data$d14,1,4) | "4372" == substr(data$d15,1,4) )


data$IschemicHD <- ("410" == substr(data$p1,1,3) | "410" == substr(data$p2,1,3) | "410" == substr(data$p3,1,3) | "410" ==  substr(data$p4,1,3) |
		"410" == substr(data$p5,1,3) | "410" == substr(data$p6,1,3) | "410" == substr(data$p7,1,3) | "410" == substr(data$p8,1,3) | "410" == substr(data$p9,1,3) |
		"410" == substr(data$p10,1,3) | "410" == substr(data$p11,1,3) | "410" ==  substr(data$p12,1,3) | "410" == substr(data$p13,1,3) | "410" ==  substr(data$d4,1,3) |
		"410" == substr(data$p14,1,3) | "410" == substr(data$p15,1,3) | "410" == substr(data$d1,1,3) | "410" == substr(data$d2,1,3) | "410" == substr(data$d3,1,3) | 
		"410" == substr(data$d5,1,3) | "410" == substr(data$d6,1,3) | "410" == substr(data$d7,1,3) | "410" == substr(data$d8,1,3) | "410" == substr(data$d9,1,3) |
		"410" == substr(data$d10,1,3) | "410" == substr(data$d11,1,3) | "410" ==  substr(data$d12,1,3) | "410" == substr(data$d13,1,3) |
		"410" == substr(data$d14,1,3) | "410" == substr(data$d15,1,3) ) | ("411" == substr(data$p1,1,3) | "411" == substr(data$p2,1,3) | "411" == substr(data$p3,1,3) | "411" ==  substr(data$p4,1,3) |
		"411" == substr(data$p5,1,3) | "411" == substr(data$p6,1,3) | "411" == substr(data$p7,1,3) | "411" == substr(data$p8,1,3) | "411" == substr(data$p9,1,3) |
		"411" == substr(data$p10,1,3) | "411" == substr(data$p11,1,3) | "411" ==  substr(data$p12,1,3) | "411" == substr(data$p13,1,3) | "411" ==  substr(data$d4,1,3) |
		"411" == substr(data$p14,1,3) | "411" == substr(data$p15,1,3) | "411" == substr(data$d1,1,3) | "411" == substr(data$d2,1,3) | "411" == substr(data$d3,1,3) | 
		"411" == substr(data$d5,1,3) | "411" == substr(data$d6,1,3) | "411" == substr(data$d7,1,3) | "411" == substr(data$d8,1,3) | "411" == substr(data$d9,1,3) |
		"411" == substr(data$d10,1,3) | "411" == substr(data$d11,1,3) | "411" ==  substr(data$d12,1,3) | "411" == substr(data$d13,1,3) |
		"411" == substr(data$d14,1,3) | "411" == substr(data$d15,1,3) ) |  ("412" == substr(data$p1,1,3) | "412" == substr(data$p2,1,3) | "412" == substr(data$p3,1,3) | "412" ==  substr(data$p4,1,3) |
		"412" == substr(data$p5,1,3) | "412" == substr(data$p6,1,3) | "412" == substr(data$p7,1,3) | "412" == substr(data$p8,1,3) | "412" == substr(data$p9,1,3) |
		"412" == substr(data$p10,1,3) | "412" == substr(data$p11,1,3) | "412" ==  substr(data$p12,1,3) | "412" == substr(data$p13,1,3) | "412" ==  substr(data$d4,1,3) |
		"412" == substr(data$p14,1,3) | "412" == substr(data$p15,1,3) | "412" == substr(data$d1,1,3) | "412" == substr(data$d2,1,3) | "412" == substr(data$d3,1,3) | 
		"412" == substr(data$d5,1,3) | "412" == substr(data$d6,1,3) | "412" == substr(data$d7,1,3) | "412" == substr(data$d8,1,3) | "412" == substr(data$d9,1,3) |
		"412" == substr(data$d10,1,3) | "412" == substr(data$d11,1,3) | "412" ==  substr(data$d12,1,3) | "412" == substr(data$d13,1,3) |
		"412" == substr(data$d14,1,3) | "412" == substr(data$d15,1,3) ) |  ("413" == substr(data$p1,1,3) | "413" == substr(data$p2,1,3) | "413" == substr(data$p3,1,3) | "413" ==  substr(data$p4,1,3) |
		"413" == substr(data$p5,1,3) | "413" == substr(data$p6,1,3) | "413" == substr(data$p7,1,3) | "413" == substr(data$p8,1,3) | "413" == substr(data$p9,1,3) |
		"413" == substr(data$p10,1,3) | "413" == substr(data$p11,1,3) | "413" ==  substr(data$p12,1,3) | "413" == substr(data$p13,1,3) | "413" ==  substr(data$d4,1,3) |
		"413" == substr(data$p14,1,3) | "413" == substr(data$p15,1,3) | "413" == substr(data$d1,1,3) | "413" == substr(data$d2,1,3) | "413" == substr(data$d3,1,3) | 
		"413" == substr(data$d5,1,3) | "413" == substr(data$d6,1,3) | "413" == substr(data$d7,1,3) | "413" == substr(data$d8,1,3) | "413" == substr(data$d9,1,3) |
		"413" == substr(data$d10,1,3) | "413" == substr(data$d11,1,3) | "413" ==  substr(data$d12,1,3) | "413" == substr(data$d13,1,3) |
		"413" == substr(data$d14,1,3) | "413" == substr(data$d15,1,3) ) |  ("414" == substr(data$p1,1,3) | "414" == substr(data$p2,1,3) | "414" == substr(data$p3,1,3) | "414" ==  substr(data$p4,1,3) |
		"414" == substr(data$p5,1,3) | "414" == substr(data$p6,1,3) | "414" == substr(data$p7,1,3) | "414" == substr(data$p8,1,3) | "414" == substr(data$p9,1,3) |
		"414" == substr(data$p10,1,3) | "414" == substr(data$p11,1,3) | "414" ==  substr(data$p12,1,3) | "414" == substr(data$p13,1,3) | "414" ==  substr(data$d4,1,3) |
		"414" == substr(data$p14,1,3) | "414" == substr(data$p15,1,3) | "414" == substr(data$d1,1,3) | "414" == substr(data$d2,1,3) | "414" == substr(data$d3,1,3) | 
		"414" == substr(data$d5,1,3) | "414" == substr(data$d6,1,3) | "414" == substr(data$d7,1,3) | "414" == substr(data$d8,1,3) | "414" == substr(data$d9,1,3) |
		"414" == substr(data$d10,1,3) | "414" == substr(data$d11,1,3) | "414" ==  substr(data$d12,1,3) | "414" == substr(data$d13,1,3) |
		"414" == substr(data$d14,1,3) | "414" == substr(data$d15,1,3) ) |  ("V4582" == substr(data$p1,1,5) | "V4582" == substr(data$p2,1,5) | "V4582" == substr(data$p3,1,5) | "V4582" ==  substr(data$p4,1,5) |
		"V4582" == substr(data$p5,1,5) | "V4582" == substr(data$p6,1,5) | "V4582" == substr(data$p7,1,5) | "V4582" == substr(data$p8,1,5) | "V4582" == substr(data$p9,1,5) |
		"V4582" == substr(data$p10,1,5) | "V4582" == substr(data$p11,1,5) | "V4582" ==  substr(data$p12,1,5) | "V4582" == substr(data$p13,1,5) | "V4582" ==  substr(data$d4,1,5) |
		"V4582" == substr(data$p14,1,5) | "V4582" == substr(data$p15,1,5) | "V4582" == substr(data$d1,1,5) | "V4582" == substr(data$d2,1,5) | "V4582" == substr(data$d3,1,5) | 
		"V4582" == substr(data$d5,1,5) | "V4582" == substr(data$d6,1,5) | "V4582" == substr(data$d7,1,5) | "V4582" == substr(data$d8,1,5) | "V4582" == substr(data$d9,1,5) |
		"V4582" == substr(data$d10,1,5) | "V4582" == substr(data$d11,1,5) | "V4582" ==  substr(data$d12,1,5) | "V4582" == substr(data$d13,1,5) |
		"V4582" == substr(data$d14,1,5) | "V4582" == substr(data$d15,1,5) ) |  ("V4581" == substr(data$p1,1,5) | "V4581" == substr(data$p2,1,5) | "V4581" == substr(data$p3,1,5) | "V4581" ==  substr(data$p4,1,5) |
		"V4581" == substr(data$p5,1,5) | "V4581" == substr(data$p6,1,5) | "V4581" == substr(data$p7,1,5) | "V4581" == substr(data$p8,1,5) | "V4581" == substr(data$p9,1,5) |
		"V4581" == substr(data$p10,1,5) | "V4581" == substr(data$p11,1,5) | "V4581" ==  substr(data$p12,1,5) | "V4581" == substr(data$p13,1,5) | "V4581" ==  substr(data$d4,1,5) |
		"V4581" == substr(data$p14,1,5) | "V4581" == substr(data$p15,1,5) | "V4581" == substr(data$d1,1,5) | "V4581" == substr(data$d2,1,5) | "V4581" == substr(data$d3,1,5) | 
		"V4581" == substr(data$d5,1,5) | "V4581" == substr(data$d6,1,5) | "V4581" == substr(data$d7,1,5) | "V4581" == substr(data$d8,1,5) | "V4581" == substr(data$d9,1,5) |
		"V4581" == substr(data$d10,1,5) | "V4581" == substr(data$d11,1,5) | "V4581" ==  substr(data$d12,1,5) | "V4581" == substr(data$d13,1,5) |
		"V4581" == substr(data$d14,1,5) | "V4581" == substr(data$d15,1,5) )

data$Smoking <- ("V1582" == substr(data$p1,1,5) | "V1582" == substr(data$p2,1,5) | "V1582" == substr(data$p3,1,5) | "V1582" ==  substr(data$p4,1,5) |
		"V1582" == substr(data$p5,1,5) | "V1582" == substr(data$p6,1,5) | "V1582" == substr(data$p7,1,5) | "V1582" == substr(data$p8,1,5) | "V1582" == substr(data$p9,1,5) |
		"V1582" == substr(data$p10,1,5) | "V1582" == substr(data$p11,1,5) | "V1582" ==  substr(data$p12,1,5) | "V1582" == substr(data$p13,1,5) | "V1582" ==  substr(data$d4,1,5) |
		"V1582" == substr(data$p14,1,5) | "V1582" == substr(data$p15,1,5) | "V1582" == substr(data$d1,1,5) | "V1582" == substr(data$d2,1,5) | "V1582" == substr(data$d5,1,5) | 
		"V1582" == substr(data$d5,1,5) | "V1582" == substr(data$d6,1,5) | "V1582" == substr(data$d7,1,5) | "V1582" == substr(data$d8,1,5) | "V1582" == substr(data$d9,1,5) |
		"V1582" == substr(data$d10,1,5) | "V1582" == substr(data$d11,1,5) | "V1582" ==  substr(data$d12,1,5) | "V1582" == substr(data$d15,1,5) |
		"V1582" == substr(data$d14,1,5) | "V1582" == substr(data$d15,1,5) ) | ("3051" == substr(data$p1,1,4) | "3051" == substr(data$p2,1,4) | "3051" == substr(data$p3,1,4) | "3051" ==  substr(data$p4,1,4) |
		"3051" == substr(data$p5,1,4) | "3051" == substr(data$p6,1,4) | "3051" == substr(data$p7,1,4) | "3051" == substr(data$p8,1,4) | "3051" == substr(data$p9,1,4) |
		"3051" == substr(data$p10,1,4) | "3051" == substr(data$p11,1,4) | "3051" ==  substr(data$p12,1,4) | "3051" == substr(data$p13,1,4) | "3051" ==  substr(data$d3,1,4) |
		"3051" == substr(data$p14,1,4) | "3051" == substr(data$p15,1,4) | "3051" == substr(data$d1,1,4) | "3051" == substr(data$d2,1,4) | "3051" == substr(data$d5,1,4) | 
		"3051" == substr(data$d4,1,4) | "3051" == substr(data$d6,1,4) | "3051" == substr(data$d7,1,4) | "3051" == substr(data$d8,1,4) | "3051" == substr(data$d9,1,4) |
		"3051" == substr(data$d10,1,4) | "3051" == substr(data$d11,1,4) | "3051" ==  substr(data$d12,1,4) | "3051" == substr(data$d13,1,4) |
		"3051" == substr(data$d14,1,4) | "3051" == substr(data$d15,1,4) )

data$Obesity <- ("278" == substr(data$p1,1,3) | "278" == substr(data$p2,1,3) | "278" == substr(data$p3,1,3) | "278" ==  substr(data$p4,1,3) |
		"278" == substr(data$p5,1,3) | "278" == substr(data$p6,1,3) | "278" == substr(data$p7,1,3) | "278" == substr(data$p8,1,3) | "278" == substr(data$p9,1,3) |
		"278" == substr(data$p10,1,3) | "278" == substr(data$p11,1,3) | "278" ==  substr(data$p12,1,3) | "278" == substr(data$p13,1,3) | "278" ==  substr(data$d4,1,3) |
		"278" == substr(data$p14,1,3) | "278" == substr(data$p15,1,3) | "278" == substr(data$d1,1,3) | "278" == substr(data$d2,1,3) | "278" == substr(data$d3,1,3) | 
		"278" == substr(data$d5,1,3) | "278" == substr(data$d6,1,3) | "278" == substr(data$d7,1,3) | "278" == substr(data$d8,1,3) | "278" == substr(data$d9,1,3) |
		"278" == substr(data$d10,1,3) | "278" == substr(data$d11,1,3) | "278" ==  substr(data$d12,1,3) | "278" == substr(data$d13,1,3) |
		"278" == substr(data$d14,1,3) | "278" == substr(data$d15,1,3) )

str(data)


 
(sum(data[data$died>=0,]$age >= 65 & data[data$died>=0,]$died == 1)/ sum(data[data$died>=0,]$age >= 65 ))
(sum(data[data$died>=0,]$age < 65 & data[data$died>=0,]$died == 1)/ sum(data[data$died>=0,]$age < 65))

(sum(data[data$died>=0,]$age >= 65 & data[data$died>=0,]$died == 1)/ sum(data[data$died>=0,]$age >= 65 ))/
(sum(data[data$died>=0,]$age < 65 & data[data$died>=0,]$died == 1)/ sum(data[data$died>=0,]$age < 65))


modelUnadjusted <- glm(died ~ age>=65, 
data = data[data$died>=0,], family = "binomial")
summary(modelUnadjusted ) 
exp(cbind(OR = coef(modelUnadjusted ), confint(modelUnadjusted )))

modelUnadjusted <- glm(died ~ age, 
data = data[data$died>=0,], family = "binomial")
summary(modelUnadjusted ) 
exp(cbind(OR = coef(modelUnadjusted ), confint(modelUnadjusted )))


model <- glm(died ~ age + race + zip + sex + volume + Hypertension + CKD + Diabetes + PVD + IschemicHD + Smoking + Obesity, 
data = data[data$died>=0,], family = "binomial")
summary(model) 

exp(cbind(OR = coef(model), confint(model)))

