setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD\\IABP vs PVAD")
all.counts <- read.csv("PVAD for R.csv", stringsAsFactors = FALSE)

data<-all.counts[all.counts$year<2012 & all.counts$died>-1,]
Low <- all.counts[all.counts$Volume.year <= 3.375 & all.counts$year<2012,]
Medium <-all.counts[all.counts$Volume.year >3.375 & all.counts$Volume.year<11.5 & all.counts$died >-1 
                    & all.counts$year<2012, ]
High <-all.counts[all.counts$Volume.year >= 11.5 & all.counts$year<2012, ]

demographics <- data.frame(variable = character(0), Low = numeric(0), Medium = numeric(0), High = numeric(0) 
                           , stringsAsFactors = FALSE)


#Mortality calculation
demographics[1,] <- c("Mortality", (sum(Low$died) / nrow(Low)) * 100, (sum(Medium$died) / nrow(Medium)) * 100,
                      (sum(High$died) / nrow(High)) * 100)
demographics


mortality <- data.frame(
  hospital.volume = factor(c("Low","Medium", "High"), levels=c("Low","Medium", "High")),
  percent.mortality = c((sum(Low$died) / nrow(Low)) * 100, (sum(Medium$died) / nrow(Medium)) * 100,
                 (sum(High$died) / nrow(High)) * 100))



#Age calculation
demographics[2,] <- c("Age.mean", mean(Low$age), mean(Medium$age), mean(High$age))
demographics[3,] <- c("Age.sd", sd(Low$age), sd(Medium$age), sd(High$age))

#Gender Calculation
demographics[4,] <- c("Female Gender", (sum(Low$sex) / nrow(Low)) * 100, (sum(Medium$sex) / nrow(Medium)) * 100, 
                      (sum(High$sex) / nrow(High)) * 100)

#Race Calculation
demographics[5,] <- c("Race")
demographics[6,] <- c("White",(length(which(Low$race == 1)) / nrow(Low)) * 100,
                      (length(which(Medium$race == 1)) / nrow(Medium)) * 100,
                      (length(which(High$race == 1)) / nrow(High)) * 100)
demographics[7,] <- c("Black",(length(which(Low$race == 2)) / nrow(Low)) * 100, 
                      (length(which(Medium$race == 2)) / nrow(Medium)) * 100,
                      (length(which(High$race == 2)) / nrow(High)) * 100)
demographics[8,] <- c ("Hispanic",(length(which(Low$race == 3)) / nrow(Low)) * 100, 
                       (length(which(Medium$race == 3)) / nrow(Medium)) * 100,
                       (length(which(High$race == 3)) / nrow(High)) * 100)
demographics [9,] <- c("Asian/Pacific Islander",(length(which(Low$race == 4)) / nrow(Low)) * 100, 
                       (length(which(Medium$race == 4)) / nrow(Medium)) * 100,
                       (length(which(High$race == 4)) / nrow(High)) * 100)
demographics [10,] <- c("Native American",(length(which(Low$race == 5)) / nrow(Low)) * 100, 
                        (length(which(Medium$race == 5)) / nrow(Medium)) * 100,
                        (length(which(High$race == 5)) / nrow(High)) * 100)
demographics [11,] <- c("Other",(length(which(Low$race == 6 | Low$race == -9 | Low$race == -8)) / nrow(Low)) * 100, 
                        (length(which(Medium$race == 6 | Medium$race == -9 | Medium$race == -8)) / nrow(Medium)) * 100,
                        (length(which(High$race == 6 | High$race == -9 | High$race == -8)) / nrow(High)) * 100)


#Socioeconomic status calculation 
demographics [12,] <- c("Socioeconomic status")
demographics[13,] <- c("$1-$24,999",(length(which(Low$zip == 1)) / nrow(Low)) *100, 
                       (length(which(Medium$zip == 1)) / nrow(Medium)) *100,
                       (length(which(High$zip == 1)) / nrow(High)) *100)

demographics[14,] <- c("$25,000-$34,999",(length(which(Low$zip == 2)) / nrow(Low)) *100,
                       (length(which(Medium$zip == 2)) / nrow(Medium)) *100, 
                       (length(which(High$zip == 2)) / nrow(High)) *100)

demographics[15,] <- c("35,000-$44,999", (length(which(Low$zip == 3)) / nrow(Low)) *100, 
                       (length(which(Medium$zip == 3)) / nrow(Medium)) *100,
                       (length(which(High$zip == 3)) / nrow(High)) *100)

demographics[16,] <- c(">$45,000",(length(which(Low$zip == 4)) / nrow(Low)) *100, 
                       (length(which(Medium$zip == 4)) / nrow(Medium)) *100,
                       (length(which(High$zip == 4)) / nrow(High)) *100)
demographics[17,] <- c("Unknown",(length(which(Low$zip == -9 | Low$zip == -8)) / nrow(Low)) *100,
                       (length(which(Medium$zip == -9 | Medium$zip == -8)) / nrow(Medium)) *100,
                       (length(which(High$zip == -9 | High$zip == -8)) / nrow(High)) *100)

#Length of stay
demographics[18,] <- c("Length of Stay.mean", mean(Low$los[Low$los > -1 & Low$died == 0]),
                       mean(Medium$los[Medium$los > -1 & Medium$died == 0]),
                       mean(High$los[High$los > -1 & High$died == 0]))

demographics[19,] <- c("Length of Stay.sd", sd(Low$los[Low$los > -1 & Low$died == 0]),
                       sd(Medium$los[Medium$los > -1 & Medium$died == 0]),
                       sd(High$los[High$los > -1 & High$died == 0]))

#Year of procedure
demographics[20,] <- c("Year")
demographics[21,] <- c("1998", (length(which(Low$year == 1998)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 1998)) / nrow(Medium)) * 100,
                       (length(which(High$year == 1998)) / nrow(High)) * 100)
demographics[22,] <- c("1999", (length(which(Low$year == 1999)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 1999)) / nrow(Medium)) * 100,
                       (length(which(High$year == 1999)) / nrow(High)) * 100)
demographics[23,] <- c("2000", (length(which(Low$year == 2000)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2000)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2000)) / nrow(High)) * 100)
demographics[24,] <- c("2001", (length(which(Low$year == 2001)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2001)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2001)) / nrow(High)) * 100)
demographics[25,] <- c("2002", (length(which(Low$year == 2002)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2002)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2002)) / nrow(High)) * 100)
demographics[26,] <- c("2003", (length(which(Low$year == 2003)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2003)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2003)) / nrow(High)) * 100)
demographics[27,] <- c("2004", (length(which(Low$year == 2004)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2004)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2004)) / nrow(High)) * 100)
demographics[28,] <- c("2005", (length(which(Low$year == 2005)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2005)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2005)) / nrow(High)) * 100)
demographics[29,] <- c("2006", (length(which(Low$year == 2006)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2006)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2006)) / nrow(High)) * 100)
demographics[30,] <- c("2007", (length(which(Low$year == 2007)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2007)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2007)) / nrow(High)) * 100)
demographics[31,] <- c("2008", (length(which(Low$year == 2008)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2008)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2008)) / nrow(High)) * 100)
demographics[32,] <- c("2009", (length(which(Low$year == 2009)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2009)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2009)) / nrow(High)) * 100)
demographics[33,] <- c("2010", (length(which(Low$year == 2010)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2010)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2010)) / nrow(High)) * 100)
demographics[34,] <- c("2011", (length(which(Low$year == 2011)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2011)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2011)) / nrow(High)) * 100)
demographics[35,] <- c("2012", (length(which(Low$year == 2012)) / nrow(Low)) * 100,
                       (length(which(Medium$year == 2012)) / nrow(Medium)) * 100,
                       (length(which(High$year == 2012)) / nrow(High)) * 100)

###Region of hospital
demographics[36,] <- c("Region of hospital")
demographics[37,] <- c("Northeast")
demographics[38,] <- c("Midwest")
demographics[39,] <- c("South")
demographics[40,] <- c("West")
demographics[41,] <- c("Unknown")

###Location of hospital
###Bedsize of hospital


#######COMORBIDITIES
comorbidities <- data.frame(variable = character(0), Low = numeric(0), Medium = numeric(0), High = numeric(0) 
                            , stringsAsFactors = FALSE)

Low.d <- Low[,15:29]
Medium.d <-Medium[,15:29]
High.d <- High[,15:29]

#Acute Myocardial Infarction
comorbidities[1,]<-c("Acute Myocardial Infarction",
                     ((sum(rowSums(Low.d >= 41000 & Low.d <= 41090, na.rm = T) >0)) / nrow(Low.d)) * 100,
                     ((sum(rowSums(Medium.d >= 41000 & Medium.d <= 41090, na.rm = T) >0)) / nrow(Medium.d)) * 100,
                     ((sum(rowSums(High.d >= 41000 & High.d <= 41090, na.rm = T) >0)) / nrow(High.d)) * 100)


#Hypertension
comorbidities[2,] <- c("Hypertension",
                       ((sum(rowSums((Low.d >= 4010 & Low.d <= 4019) | (Low.d == 4020) | (Low.d >= 40200 & Low.d <= 40291) 
                                     | (Low.d == 4030) | (Low.d >= 40300 & Low.d <= 40391) | (Low.d == 4040) 
                                     | (Low.d >= 40400 & Low.d <= 40493) | (Low.d == 4050) | (Low.d >= 40501 & Low.d <= 40591) 
                                     | (Low.d == 4372), na.rm = T) >0)) / nrow(Low.d)) * 100,
                       ((sum(rowSums((Medium.d >= 4010 & Medium.d <= 4019) | (Medium.d == 4020) | (Medium.d >= 40200 & Medium.d <= 40291) 
                                     | (Medium.d == 4030) | (Medium.d >= 40300 & Medium.d <= 40391) | (Medium.d == 4040) 
                                     | (Medium.d >= 40400 & Medium.d <= 40493) | (Medium.d == 4050) | (Medium.d >= 40501 & Medium.d <= 40591) 
                                     | (Medium.d == 4372), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                       ((sum(rowSums((High.d >= 4010 & High.d <= 4019) | (High.d == 4020) | (High.d >= 40200 & High.d <= 40291) 
                                     | (High.d == 4030) | (High.d >= 40300 & High.d <= 40391) | (High.d == 4040) 
                                     | (High.d >= 40400 & High.d <= 40493) | (High.d == 4050) | (High.d >= 40501 & High.d <= 40591) 
                                     | (High.d == 4372), na.rm = T) >0)) / nrow(High.d)) * 100)

#Diabetes
comorbidities[3,] <-c("Diabetes",
                      ((sum(rowSums((Low.d >= 25000 & Low.d <= 25093) | (Low.d >= 24900 & Low.d <= 24991), na.rm = T) >0)) / nrow(Low.d)) * 100,
                      ((sum(rowSums((Medium.d >= 25000 & Medium.d <= 25093) | (Medium.d >= 24900 & Medium.d <= 24991), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                      ((sum(rowSums((High.d >= 25000 & High.d <= 25093) | (High.d >= 24900 & High.d <= 24991), na.rm = T) >0)) / nrow(High.d)) * 100)


#Pre-existing renal dysfunction
comorbidities[4,] <- c("Renal Dysfunction",
                       ((sum(rowSums((Low.d == 5853) | (Low.d == 5854) | (Low.d == 5855) | (Low.d == 5856) | (Low.d == 5859)
                                     | (Low.d == "V420") | (Low.d == "V451") | (Low.d == "V4511") | (Low.d == "V4512")
                                     | (Low.d == "V560") | (Low.d == "V561") | (Low.d == "V562") | (Low.d == "V563")
                                     | (Low.d == "V5631") | (Low.d == "V5632") | (Low.d == "V568"), na.rm = T) >0)) / nrow(Low.d)) * 100,
                       ((sum(rowSums((Medium.d == 5853) | (Medium.d == 5854) | (Medium.d == 5855) | (Medium.d == 5856) | (Medium.d == 5859)
                                     | (Medium.d == "V420") | (Medium.d == "V451") | (Medium.d == "V4511") | (Medium.d == "V4512")
                                     | (Medium.d == "V560") | (Medium.d == "V561") | (Medium.d == "V562") | (Medium.d == "V563")
                                     | (Medium.d == "V5631") | (Medium.d == "V5632") | (Medium.d == "V568"), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                       ((sum(rowSums((High.d == 5853) | (High.d == 5854) | (High.d == 5855) | (High.d == 5856) | (High.d == 5859)
                                     | (High.d == "V420") | (High.d == "V451") | (High.d == "V4511") | (High.d == "V4512")
                                     | (High.d == "V560") | (High.d == "V561") | (High.d == "V562") | (High.d == "V563")
                                     | (High.d == "V5631") | (High.d == "V5632") | (High.d == "V568"), na.rm = T) >0)) / nrow(High.d)) * 100)





#Chronic Ischemic Heart Disease
comorbidities[5,] <-c("Chronic Ischemic Heart Disease", 
                      ((sum(rowSums((Low.d >= 4110 & Low.d <= 4118) | (Low.d == 412) | (Low.d >= 4130 & Low.d <=4139) 
                                    | (Low.d >= 4140 & Low.d <= 4149) | (Low.d == "V4582"), na.rm = T) >0)) / nrow(Low.d)) * 100,
                      ((sum(rowSums((Medium.d >= 4110 & Medium.d <= 4118) | (Medium.d == 412) | (Medium.d >= 4130 & Medium.d <=4139) 
                                    | (Medium.d >= 4140 & Medium.d <= 4149) | (Medium.d == "V4582"), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                      ((sum(rowSums((High.d >= 4110 & High.d <= 4118) | (High.d == 412) | (High.d >= 4130 & High.d <=4139) 
                                    | (High.d >= 4140 & High.d <= 4149) | (High.d == "V4582"), na.rm = T) >0)) / nrow(High.d)) * 100)


#History of CABG
comorbidities[6,] <-c("History of CABG", 
                      ((sum(rowSums((Low.d == "V4581"), na.rm = T) >0)) / nrow(Low.d)) * 100,
                      ((sum(rowSums((Medium.d == "V4581"), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                      ((sum(rowSums((High.d == "V4581"), na.rm = T) >0)) / nrow(High.d)) * 100)

#Ischemic cardiomyopathy
comorbidities[7,] <-c("Ischemic cardiomyopathy",
                      ((sum(rowSums((Low.d >= 4100 & Low.d <= 4109) | (Low.d == 412) | (Low.d >= 4110 & Low.d <=4118) 
                                    | (Low.d >= 4130 & Low.d <= 4139) | (Low.d >= 4140 & Low.d <= 4149)
                                    | (Low.d == "V4582") | (Low.d == "V4581"), na.rm = T) >0)) / nrow(Low.d)) * 100,
                      ((sum(rowSums((Medium.d >= 4100 & Medium.d <= 4109) | (Medium.d == 412) | (Medium.d >= 4110 & Medium.d <=4118) 
                                    | (Medium.d >= 4130 & Medium.d <= 4139) | (Medium.d >= 4140 & Medium.d <= 4149)
                                    | (Medium.d == "V4582") | (Medium.d == "V4581"), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                      ((sum(rowSums((High.d >= 4100 & High.d <= 4109) | (High.d == 412) | (High.d >= 4110 & High.d <=4118) 
                                    | (High.d >= 4130 & High.d <= 4139) | (High.d >= 4140 & High.d <= 4149)
                                    | (High.d == "V4582") | (High.d == "V4581"), na.rm = T) >0)) / nrow(High.d)) * 100)


#History of other cardiac surgery
comorbidities[8,] <-c("History of other cardiac surgery", 
                      ((sum(rowSums((Low.d == "V151") | (Low.d == "V433"), na.rm = T) >0)) / nrow(Low.d)) * 100,
                      ((sum(rowSums((Medium.d == "V151") | (Medium.d == "V433"), na.rm = T) >0)) / nrow(Medium.d)) * 100,
                      ((sum(rowSums((High.d == "V151") | (High.d == "V433"), na.rm = T) >0)) / nrow(High.d)) * 100)

#Peripheral vascular disease
comorbidities[9,] <-c("Peripheral vascular disease",
                      ((sum(rowSums((Low.d >= 4400 & Low.d <= 4409) | (Low.d == 4431) | (Low.d == 4438) |(Low.d == 44381) 
                                    |(Low.d == 44382) |(Low.d == 44389) | (Low.d == 4439) |(Low.d == 4471) |(Low.d == "V434"),
                                    na.rm = T) >0)) / nrow(Low.d)) * 100,
                      ((sum(rowSums((Medium.d >= 4400 & Medium.d <= 4409) | (Medium.d == 4431) | (Medium.d == 4438) |(Medium.d == 44381) 
                                    |(Medium.d == 44382) |(Medium.d == 44389) | (Medium.d == 4439) |(Medium.d == 4471) |(Medium.d == "V434"),
                                    na.rm = T) >0)) / nrow(Medium.d)) * 100,
                      ((sum(rowSums((High.d >= 4400 & High.d <= 4409) | (High.d == 4431) | (High.d == 4438) |(High.d == 44381) 
                                    |(High.d == 44382) |(High.d == 44389) | (High.d == 4439) |(High.d == 4471) |(High.d == "V434"),
                                    na.rm = T) >0)) / nrow(High.d)) * 100)


#######COMPLICATIONS
complications <- data.frame(variable = character(0), Low = numeric(0), Medium = numeric(0), High = numeric(0) 
                            , stringsAsFactors = FALSE)

Low.p <- Low[, 30:44]
Medium.p <- Medium[, 30:44]
High.p <- High[, 30:44]

#Major Bleeding
complications[1,]<-c("Bleeding",
                     ((sum(rowSums((Low.p == 99811) | (Low.p == 99812) | (Low.p == 99811) | (Low.p == 99812) 
                                   | (Low.p == 9900) | (Low.p == 9902) | (Low.p == 9903) | (Low.p == 9904)
                                   | (Low.p == 56881) | (Low.p == 5967) | (Low.p == 59970) | (Low.p == 59971)
                                   | (Low.p == 7863) | (Low.p == 7847) | (Low.p == 4590) | (Low.p == 5967)
                                   | (Low.p == 59970) | (Low.p == 59971) | (Low.p == 7863) | (Low.p == 7847)
                                   | (Low.p == 4590) | (Low.p == 9900) | (Low.p == 9902) | (Low.p == 9903)
                                   | (Low.p == 9904) | (Low.p == 430) | (Low.p == 431) | (Low.p == 4320)
                                   | (Low.p == 4321) | (Low.p == 4329)| (Low.p == 53021) | (Low.p == 4560)
                                   | (Low.p == 5307) | (Low.p == 53082) | (Low.p == 5780) | (Low.p == 5781)
                                   | (Low.p == 5789) | (Low.p == 45620) | (Low.p == 53100) | (Low.p == 53101)
                                   | (Low.p == 53120) | (Low.p == 53121) | (Low.p == 53140) | (Low.p == 53141)
                                   | (Low.p == 53160) | (Low.p == 53161) | (Low.p == 53200) | (Low.p == 53221)
                                   | (Low.p == 53240) | (Low.p == 53241) | (Low.p == 53260) | (Low.p == 53261)
                                   | (Low.p == 53300) | (Low.p == 53301) | (Low.p == 53320) | (Low.p == 53321)
                                   | (Low.p == 53340) | (Low.p == 53341) | (Low.p == 53360) | (Low.p == 53361) 
                                   | (Low.p == 53400) | (Low.p == 53401) | (Low.p == 53420) | (Low.p == 53421)
                                   | (Low.p == 53440) | (Low.p == 53441) | (Low.p == 53460) | (Low.p == 53461)
                                   | (Low.p == 53501) | (Low.p == 53511) | (Low.p == 53521) | (Low.p == 53531)
                                   | (Low.p == 53541) | (Low.p == 53551) | (Low.p == 53561) | (Low.p == 53571)
                                   | (Low.p == 53783) | (Low.p == 56202) | (Low.p == 56203) | (Low.p == 56212)
                                   | (Low.p == 56213) | (Low.p == 5693) | (Low.p == 56985) | (Low.p == 53784)
                                   | (Low.p == 56986), na.rm = T) >0)) / nrow(Low.p)) * 100,
                     ((sum(rowSums((Medium.p == 99811) | (Medium.p == 99812) | (Medium.p == 99811) | (Medium.p == 99812) 
                                   | (Medium.p == 9900) | (Medium.p == 9902) | (Medium.p == 9903) | (Medium.p == 9904)
                                   | (Medium.p == 56881) | (Medium.p == 5967) | (Medium.p == 59970) | (Medium.p == 59971)
                                   | (Medium.p == 7863) | (Medium.p == 7847) | (Medium.p == 4590) | (Medium.p == 5967)
                                   | (Medium.p == 59970) | (Medium.p == 59971) | (Medium.p == 7863) | (Medium.p == 7847)
                                   | (Medium.p == 4590) | (Medium.p == 9900) | (Medium.p == 9902) | (Medium.p == 9903)
                                   | (Medium.p == 9904) | (Medium.p == 430) | (Medium.p == 431) | (Medium.p == 4320)
                                   | (Medium.p == 4321) | (Medium.p == 4329)| (Medium.p == 53021) | (Medium.p == 4560)
                                   | (Medium.p == 5307) | (Medium.p == 53082) | (Medium.p == 5780) | (Medium.p == 5781)
                                   | (Medium.p == 5789) | (Medium.p == 45620) | (Medium.p == 53100) | (Medium.p == 53101)
                                   | (Medium.p == 53120) | (Medium.p == 53121) | (Medium.p == 53140) | (Medium.p == 53141)
                                   | (Medium.p == 53160) | (Medium.p == 53161) | (Medium.p == 53200) | (Medium.p == 53221)
                                   | (Medium.p == 53240) | (Medium.p == 53241) | (Medium.p == 53260) | (Medium.p == 53261)
                                   | (Medium.p == 53300) | (Medium.p == 53301) | (Medium.p == 53320) | (Medium.p == 53321)
                                   | (Medium.p == 53340) | (Medium.p == 53341) | (Medium.p == 53360) | (Medium.p == 53361) 
                                   | (Medium.p == 53400) | (Medium.p == 53401) | (Medium.p == 53420) | (Medium.p == 53421)
                                   | (Medium.p == 53440) | (Medium.p == 53441) | (Medium.p == 53460) | (Medium.p == 53461)
                                   | (Medium.p == 53501) | (Medium.p == 53511) | (Medium.p == 53521) | (Medium.p == 53531)
                                   | (Medium.p == 53541) | (Medium.p == 53551) | (Medium.p == 53561) | (Medium.p == 53571)
                                   | (Medium.p == 53783) | (Medium.p == 56202) | (Medium.p == 56203) | (Medium.p == 56212)
                                   | (Medium.p == 56213) | (Medium.p == 5693) | (Medium.p == 56985) | (Medium.p == 53784)
                                   | (Medium.p == 56986), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                     ((sum(rowSums((High.p == 99811) | (High.p == 99812) | (High.p == 99811) | (High.p == 99812) 
                                   | (High.p == 9900) | (High.p == 9902) | (High.p == 9903) | (High.p == 9904)
                                   | (High.p == 56881) | (High.p == 5967) | (High.p == 59970) | (High.p == 59971)
                                   | (High.p == 7863) | (High.p == 7847) | (High.p == 4590) | (High.p == 5967)
                                   | (High.p == 59970) | (High.p == 59971) | (High.p == 7863) | (High.p == 7847)
                                   | (High.p == 4590) | (High.p == 9900) | (High.p == 9902) | (High.p == 9903)
                                   | (High.p == 9904) | (High.p == 430) | (High.p == 431) | (High.p == 4320)
                                   | (High.p == 4321) | (High.p == 4329)| (High.p == 53021) | (High.p == 4560)
                                   | (High.p == 5307) | (High.p == 53082) | (High.p == 5780) | (High.p == 5781)
                                   | (High.p == 5789) | (High.p == 45620) | (High.p == 53100) | (High.p == 53101)
                                   | (High.p == 53120) | (High.p == 53121) | (High.p == 53140) | (High.p == 53141)
                                   | (High.p == 53160) | (High.p == 53161) | (High.p == 53200) | (High.p == 53221)
                                   | (High.p == 53240) | (High.p == 53241) | (High.p == 53260) | (High.p == 53261)
                                   | (High.p == 53300) | (High.p == 53301) | (High.p == 53320) | (High.p == 53321)
                                   | (High.p == 53340) | (High.p == 53341) | (High.p == 53360) | (High.p == 53361) 
                                   | (High.p == 53400) | (High.p == 53401) | (High.p == 53420) | (High.p == 53421)
                                   | (High.p == 53440) | (High.p == 53441) | (High.p == 53460) | (High.p == 53461)
                                   | (High.p == 53501) | (High.p == 53511) | (High.p == 53521) | (High.p == 53531)
                                   | (High.p == 53541) | (High.p == 53551) | (High.p == 53561) | (High.p == 53571)
                                   | (High.p == 53783) | (High.p == 56202) | (High.p == 56203) | (High.p == 56212)
                                   | (High.p == 56213) | (High.p == 5693) | (High.p == 56985) | (High.p == 53784)
                                   | (High.p == 56986), na.rm = T) >0)) / nrow(High.p)) * 100)

#Infectious complications
complications[2,]<-c("Infectious", 
                     ((sum(rowSums((Low.p == 380) | (Low.p == 381) | (Low.p == 3810) | (Low.p == 3811) 
                                   | (Low.p == 3812) | (Low.p == 3819) | (Low.p == 382) | (Low.p == 383)
                                   | (Low.p == 3840) | (Low.p == 3841) | (Low.p == 3842) | (Low.p == 3843)
                                   | (Low.p == 3844) | (Low.p == 3849) | (Low.p == 388) | (Low.p == 389)
                                   | (Low.p == 78552) | (Low.p == 78559) | (Low.p == 99591) | (Low.p == 99592)
                                   | (Low.p == 9980) | (Low.p == 99800) | (Low.p == 99802) | (Low.p == 9993)
                                   | (Low.p == 99662) | (Low.p == 99931) | (Low.p == 99932) | (Low.p == 99660)
                                   | (Low.p == 99661) | (Low.p == 99851)| (Low.p == 99859), na.rm = T) >0)) / nrow(Low.p)) * 100,
                     ((sum(rowSums((Medium.p == 380) | (Medium.p == 381) | (Medium.p == 3810) | (Medium.p == 3811) 
                                   | (Medium.p == 3812) | (Medium.p == 3819) | (Medium.p == 382) | (Medium.p == 383)
                                   | (Medium.p == 3840) | (Medium.p == 3841) | (Medium.p == 3842) | (Medium.p == 3843)
                                   | (Medium.p == 3844) | (Medium.p == 3849) | (Medium.p == 388) | (Medium.p == 389)
                                   | (Medium.p == 78552) | (Medium.p == 78559) | (Medium.p == 99591) | (Medium.p == 99592)
                                   | (Medium.p == 9980) | (Medium.p == 99800) | (Medium.p == 99802) | (Medium.p == 9993)
                                   | (Medium.p == 99662) | (Medium.p == 99931) | (Medium.p == 99932) | (Medium.p == 99660)
                                   | (Medium.p == 99661) | (Medium.p == 99851)| (Medium.p == 99859), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                     ((sum(rowSums((High.p == 380) | (High.p == 381) | (High.p == 3810) | (High.p == 3811) 
                                   | (High.p == 3812) | (High.p == 3819) | (High.p == 382) | (High.p == 383)
                                   | (High.p == 3840) | (High.p == 3841) | (High.p == 3842) | (High.p == 3843)
                                   | (High.p == 3844) | (High.p == 3849) | (High.p == 388) | (High.p == 389)
                                   | (High.p == 78552) | (High.p == 78559) | (High.p == 99591) | (High.p == 99592)
                                   | (High.p == 9980) | (High.p == 99800) | (High.p == 99802) | (High.p == 9993)
                                   | (High.p == 99662) | (High.p == 99931) | (High.p == 99932) | (High.p == 99660)
                                   | (High.p == 99661) | (High.p == 99851)| (High.p == 99859), na.rm = T) >0)) / nrow(High.p)) * 100)

#Cardiac complications
complications[3,] <- c("Cardiac", 
                       ((sum(rowSums((Low.p == 9971) | (Low.p == 4260) | (Low.p == 3771) | (Low.p == 3772) 
                                     | (Low.p == 3778) | (Low.p == 3780) | (Low.p == 3781) | (Low.p == 3782)
                                     | (Low.p == 3783) | (Low.p == 3964) | (Low.p == 4230) | (Low.p == 4233)
                                     , na.rm = T) >0)) / nrow(Low.p)) * 100,
                       ((sum(rowSums((Medium.p == 9971) | (Medium.p == 4260) | (Medium.p == 3771) | (Medium.p == 3772) 
                                     | (Medium.p == 3778) | (Medium.p == 3780) | (Medium.p == 3781) | (Medium.p == 3782)
                                     | (Medium.p == 3783) | (Medium.p == 3964) | (Medium.p == 4230) | (Medium.p == 4233)
                                     , na.rm = T) >0)) / nrow(Medium.p)) * 100,
                       ((sum(rowSums((High.p == 9971) | (High.p == 4260) | (High.p == 3771) | (High.p == 3772) 
                                     | (High.p == 3778) | (High.p == 3780) | (High.p == 3781) | (High.p == 3782)
                                     | (High.p == 3783) | (High.p == 3964) | (High.p == 4230) | (High.p == 4233)
                                     , na.rm = T) >0)) / nrow(High.p)) * 100)
#Thromboembolic complications
complications[4,] <- c("Thromboembolic", 
                       ((sum(rowSums((Low.p >= 4330 & Low.p <= 4339) | (Low.p >= 4340 & Low.p <= 4349) 
                                     | (Low.p == 5570) | (Low.p >= 36230 & Low.p <=36234 ) | (Low.p == 59381) 
                                     | (Low.p == 4440) | (Low.p == 4441) | (Low.p == 4442) | (Low.p == 4448) 
                                     | (Low.p == 4449) | (Low.p == 5734) | (Low.p == 28959) | (Low.p == 99670)
                                     | (Low.p == 99672) | (Low.p == 99702) | (Low.p >= 4350 & Low.p <= 4353)
                                     | (Low.p == 4358) | (Low.p == 4359) | (Low.p == "V1254") | (Low.p == 9970) 
                                     | (Low.p == 99700) | (Low.p == 99701) | (Low.p == 45111) | (Low.p == 45119)
                                     | (Low.p == 4512) | (Low.p == 45181) | (Low.p == 4519) | (Low.p == 45340)
                                     | (Low.p == 45341) | (Low.p == 45342) | (Low.p == 4538) | (Low.p == 4539)
                                     | (Low.p == 4151) | (Low.p == 41511) | (Low.p == 41513) | (Low.p == 41519)
                                     , na.rm = T) >0)) / nrow(Low.p)) * 100,
                       ((sum(rowSums((Medium.p >= 4330 & Medium.p <= 4339) | (Medium.p >= 4340 & Medium.p <= 4349) 
                                     | (Medium.p == 5570) | (Medium.p >= 36230 & Medium.p <=36234 ) | (Medium.p == 59381) 
                                     | (Medium.p == 4440) | (Medium.p == 4441) | (Medium.p == 4442) | (Medium.p == 4448) 
                                     | (Medium.p == 4449) | (Medium.p == 5734) | (Medium.p == 28959) | (Medium.p == 99670)
                                     | (Medium.p == 99672) | (Medium.p == 99702) | (Medium.p >= 4350 & Medium.p <= 4353)
                                     | (Medium.p == 4358) | (Medium.p == 4359) | (Medium.p == "V1254") | (Medium.p == 9970) 
                                     | (Medium.p == 99700) | (Medium.p == 99701) | (Medium.p == 45111) | (Medium.p == 45119)
                                     | (Medium.p == 4512) | (Medium.p == 45181) | (Medium.p == 4519) | (Medium.p == 45340)
                                     | (Medium.p == 45341) | (Medium.p == 45342) | (Medium.p == 4538) | (Medium.p == 4539)
                                     | (Medium.p == 4151) | (Medium.p == 41511) | (Medium.p == 41513) | (Medium.p == 41519)
                                     , na.rm = T) >0)) / nrow(Medium.p)) * 100,
                       ((sum(rowSums((High.p >= 4330 & High.p <= 4339) | (High.p >= 4340 & High.p <= 4349) 
                                     | (High.p == 5570) | (High.p >= 36230 & High.p <=36234 ) | (High.p == 59381) 
                                     | (High.p == 4440) | (High.p == 4441) | (High.p == 4442) | (High.p == 4448) 
                                     | (High.p == 4449) | (High.p == 5734) | (High.p == 28959) | (High.p == 99670)
                                     | (High.p == 99672) | (High.p == 99702) | (High.p >= 4350 & High.p <= 4353)
                                     | (High.p == 4358) | (High.p == 4359) | (High.p == "V1254") | (High.p == 9970) 
                                     | (High.p == 99700) | (High.p == 99701) | (High.p == 45111) | (High.p == 45119)
                                     | (High.p == 4512) | (High.p == 45181) | (High.p == 4519) | (High.p == 45340)
                                     | (High.p == 45341) | (High.p == 45342) | (High.p == 4538) | (High.p == 4539)
                                     | (High.p == 4151) | (High.p == 41511) | (High.p == 41513) | (High.p == 41519)
                                     , na.rm = T) >0)) / nrow(High.p)) * 100)

#Vascular complications
complications[5,] <- c("Vascular",
                       ((sum(rowSums((Low.p >= 900 & Low.p <= 904) | (Low.p == 9982) | (Low.p == 9992) 
                                     | (Low.p == 9977) | (Low.p == 99771) | (Low.p == 99772) | (Low.p == 99779)
                                     | (Low.p == 9972), na.rm = T) >0)) / nrow(Low.p)) * 100,
                       ((sum(rowSums((Medium.p >= 900 & Medium.p <= 904) | (Medium.p == 9982) | (Medium.p == 9992) 
                                     | (Medium.p == 9977) | (Medium.p == 99771) | (Medium.p == 99772) | (Medium.p == 99779)
                                     | (Medium.p == 9972), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                       ((sum(rowSums((High.p >= 900 & High.p <= 904) | (High.p == 9982) | (High.p == 9992) 
                                     | (High.p == 9977) | (High.p == 99771) | (High.p == 99772) | (High.p == 99779)
                                     | (High.p == 9972), na.rm = T) >0)) / nrow(High.p)) * 100)

#Respiratory complications
complications[6,] <- c("Respiratory",
                       ((sum(rowSums((Low.p >= 900 & Low.p <= 904) | (Low.p == 9982) | (Low.p == 9992) 
                                     | (Low.p == 9977) | (Low.p == 99771) | (Low.p == 99772) | (Low.p == 99779)
                                     | (Low.p == 9972), na.rm = T) >0)) / nrow(Low.p)) * 100,
                       ((sum(rowSums((Medium.p >= 900 & Medium.p <= 904) | (Medium.p == 9982) | (Medium.p == 9992) 
                                     | (Medium.p == 9977) | (Medium.p == 99771) | (Medium.p == 99772) | (Medium.p == 99779)
                                     | (Medium.p == 9972), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                       ((sum(rowSums((High.p >= 900 & High.p <= 904) | (High.p == 9982) | (High.p == 9992) 
                                     | (High.p == 9977) | (High.p == 99771) | (High.p == 99772) | (High.p == 99779)
                                     | (High.p == 9972), na.rm = T) >0)) / nrow(High.p)) * 100)

#Renal complications
complications[7,] <- c("Renal",
                       ((sum(rowSums((Low.p >= 5845 & Low.p <= 5849) | (Low.p == 586) | (Low.p == 9975) 
                                     | (Low.p == 3995) | (Low.p == 5498), na.rm = T) >0)) / nrow(Low.p)) * 100,
                       ((sum(rowSums((Medium.p >= 5845 & Medium.p <= 5849) | (Medium.p == 586) | (Medium.p == 9975) 
                                     | (Medium.p == 3995) | (Medium.p == 5498), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                       ((sum(rowSums((High.p >= 5845 & High.p <= 5849) | (High.p == 586) | (High.p == 9975) 
                                     | (High.p == 3995) | (High.p == 5498), na.rm = T) >0)) / nrow(High.p)) * 100)
#Mechanical complications
complications[8,] <-c("Mechanical",
                      ((sum(rowSums((Low.p == 9960) | (Low.p == 99600) | (Low.p == 99609), na.rm = T) >0)) / nrow(Low.p)) * 100,
                      ((sum(rowSums((Medium.p == 9960) | (Medium.p == 99600) | (Medium.p == 99609), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                      ((sum(rowSums((High.p == 9960) | (High.p == 99600) | (High.p == 99609), na.rm = T) >0)) / nrow(High.p)) * 100)

#Wound dehiscence
complications[9,] <-c("Wound Dehiscence",
                      ((sum(rowSums((Low.p == 9983) | (Low.p == 99830) | (Low.p == 99831) | (Low.p == 99832)
                                    , na.rm = T) >0)) / nrow(Low.p)) * 100,
                      ((sum(rowSums((Medium.p == 9983) | (Medium.p == 99830) | (Medium.p == 99831) | (Medium.p == 99832)
                                    , na.rm = T) >0)) / nrow(Medium.p)) * 100,
                      ((sum(rowSums((High.p == 9983) | (High.p == 99830) | (High.p == 99831) | (High.p == 99832)
                                    , na.rm = T) >0)) / nrow(High.p)) * 100)
#Hemolytic anemia
complications[10,]<- c("Hemolytic anemia",
                       ((sum(rowSums((Low.p == 2832) | (Low.p == 28310) | (Low.p == 28319) | (Low.p == 2839), na.rm = T) >0)) / nrow(Low.p)) * 100,
                       ((sum(rowSums((Medium.p == 2832) | (Medium.p == 28310) | (Medium.p == 28319) | (Medium.p == 2839), na.rm = T) >0)) / nrow(Medium.p)) * 100,
                       ((sum(rowSums((High.p == 2832) | (High.p == 28310) | (High.p == 28319) | (High.p == 2839), na.rm = T) >0)) / nrow(High.p)) * 100)

###Write to csv
write.csv(demographics, file = "demographics.csv")
write.csv(comorbidities, file = "comorbidities.csv")
write.csv(complications, file = "complications.csv")

###Histogram Plot
ggplot(data=all.counts[all.counts$year<2012 & all.counts$year>=2007,], aes(all.counts$Volume.year[all.counts$year<2012 & all.counts$year>=2007])) + 
  geom_histogram(breaks=seq(0, 32, by = 1), 
                 binwidth = 1,
                 col="black", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Distribution of PVAD volume/year across U.S. hospitals") +
  labs(x="PVAD volume/year", y="Hospital count") + 
  xlim(c(0,35)) + 
  ylim(c(0,175)) +
  scale_x_continuous(breaks = round(seq(0, 32, by = 1),1), expand = c(0, 0)) +
  scale_y_continuous(breaks = round(seq(0, 175, by = 25),1), expand = c(0, 0)) +
  annotate ("segment", x = 3.375, y = 0, xend = 3.375, yend = 175, size = 1, colour = "red") + 
  annotate("text", label = "Quartile 1 \n3.375      ", x = 4.575, y = 170, size = 3, colour = "red") +
  annotate ("segment", x = 7, y = 0, xend = 7, yend = 175, size = 1, colour = "red") + 
  annotate("text", label = "Median \n7.000  ", x = 8, y = 170, size = 3, colour = "red") +
  annotate ("segment", x = 11.5, y = 0, xend = 11.5, yend = 175, size = 1, colour = "red") + 
  annotate("text", label = "Quartile 3\n 11.500     ", x = 12.7, y = 170, size = 3, colour = "red") +
  #annotate("text", label = "Low", x = 1.69, y = 157.5, size = 4, colour = "black") +
  #annotate("text", label = "Medium", x = 7, y = 157.5, size = 4, colour = "black") +
  #annotate("text", label = "High", x = 21.75, y = 157.5, size = 4, colour = "black") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))


###Bargraph Plot
ggplot(data=mortality, aes(x=hospital.volume, y=percent.mortality, fill=percent.mortality)) +
  geom_bar(stat="identity") +
  labs(title="Percent mortality by hospital volume") +
  labs(x = "PVAD volume/year", y = "Percent mortality") +
  labs(fill = "Percent mortality") +
  ylim(c(20,35)) +
  scale_y_continuous(breaks = round(seq(20, 35, by = 1),1)) +
  coord_cartesian(ylim=c(20,35)) +
  annotate("segment", x = 1, y = 34, xend= 3, yend = 34, size = 1, colour = "black") +
  annotate("text", label = "p = 0.0352", x = 2, y = 34.25, size = 4, colour = "black") +
  annotate("segment", x = 1, y = 33, xend= 2, yend = 33, size = 1, colour = "black") +
  annotate("text", label = "p = 0.0206", x = 1.5, y = 33.25, size = 4, colour = "black") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) +
  theme(text = element_text(size=15))

###Odds Ratios
library(fmsb)
age.65.or.older.OR <- oddsratio(as.numeric(sum(data$age >= 65 & data$died == 1)), 
                                as.numeric(sum(data$age < 65 & data$died == 1)),
                                as.numeric(sum(data$age >= 65 & data$died == 0)), 
                                as.numeric(sum(data$age < 65 & data$died == 0)), conf.level = 0.95)

female.OR <- oddsratio(as.numeric(sum(data$sex ==1 & data$died == 1)),
                       as.numeric(sum(data$sex !=1 & data$died == 1)),
                       as.numeric(sum(data$sex ==1 & data$died == 0)), 
                       as.numeric(sum(data$sex !=1 & data$died == 0)), conf.level = 0.95)

race.OR <- oddsratio(as.numeric(sum(data$race ==1 & data$died == 1)), 
                     as.numeric(sum(data$race !=1 & data$died == 1)),
                     as.numeric(sum(data$race ==1 & data$died == 0)), 
                     as.numeric(sum(data$race !=1 & data$died == 0)), conf.level = 0.95)

income.OR <- oddsratio(as.numeric(sum(data$zip ==1 & data$died == 1)), 
                       as.numeric(sum(data$zip !=1 & data$died == 1)), 
                       as.numeric(sum(data$zip ==1 & data$died == 0)),
                       as.numeric(sum(data$zip !=1 & data$died == 0)), conf.level = 0.95)
year.2008.or.later.OR <- oddsratio(as.numeric(sum(data$year >=2008 & data$died == 1)), 
                                   as.numeric(sum(data$year <2008 & data$died == 1)), 
                                   as.numeric(sum(data$year >=2008 & data$died == 0)),
                                   as.numeric(sum(data$year <2008 & data$died == 0)), conf.level = 0.95)

##Comorbidities OR
data.d <-data[,c(15:29)]
AMI.OR <- oddsratio(as.numeric(sum((rowSums(data.d >= 41000 & data.d <= 41090, na.rm = T) >0) & data$died == 1)),
                    as.numeric(sum(!(rowSums(data.d >= 41000 & data.d <= 41090, na.rm = T) >0) & data$died == 1)),
                    as.numeric(sum((rowSums(data.d >= 41000 & data.d <= 41090, na.rm = T) >0) & data$died == 0)), 
                    as.numeric(sum(!(rowSums(data.d >= 41000 & data.d <= 41090, na.rm = T) >0) & data$died == 0)), 
                    conf.level = 0.95)
Hypertension.OR <- oddsratio(as.numeric(sum((rowSums((data.d >= 4010 & data.d <= 4019) | (data.d == 4020) | (data.d >= 40200 & data.d <= 40291) 
                                            | (data.d == 4030) | (data.d >= 40300 & data.d <= 40391) | (data.d == 4040) 
                                            | (data.d >= 40400 & data.d <= 40493) | (data.d == 4050) | (data.d >= 40501 & data.d <= 40591) 
                                            | (data.d == 4372), na.rm = T) >0) & data$died == 1)),
                    as.numeric(sum(!(rowSums((data.d >= 4010 & data.d <= 4019) | (data.d == 4020) | (data.d >= 40200 & data.d <= 40291) 
                                            | (data.d == 4030) | (data.d >= 40300 & data.d <= 40391) | (data.d == 4040) 
                                            | (data.d >= 40400 & data.d <= 40493) | (data.d == 4050) | (data.d >= 40501 & data.d <= 40591) 
                                            | (data.d == 4372), na.rm = T) >0) & data$died == 1)),
                    as.numeric(sum((rowSums((data.d >= 4010 & data.d <= 4019) | (data.d == 4020) | (data.d >= 40200 & data.d <= 40291) 
                                            | (data.d == 4030) | (data.d >= 40300 & data.d <= 40391) | (data.d == 4040) 
                                            | (data.d >= 40400 & data.d <= 40493) | (data.d == 4050) | (data.d >= 40501 & data.d <= 40591) 
                                            | (data.d == 4372), na.rm = T) >0) & data$died == 0)),
                    as.numeric(sum(!(rowSums((data.d >= 4010 & data.d <= 4019) | (data.d == 4020) | (data.d >= 40200 & data.d <= 40291) 
                                            | (data.d == 4030) | (data.d >= 40300 & data.d <= 40391) | (data.d == 4040) 
                                            | (data.d >= 40400 & data.d <= 40493) | (data.d == 4050) | (data.d >= 40501 & data.d <= 40591) 
                                            | (data.d == 4372), na.rm = T) >0) & data$died == 0)), conf.level = 0.95)
Diabetes.OR <- oddsratio(as.numeric(sum((rowSums((data.d >= 25000 & data.d <= 25093) 
                                                | (data.d >= 24900 & data.d <= 24991), na.rm = T) >0) & data$died == 1)),
                         as.numeric(sum(!(rowSums((data.d >= 25000 & data.d <= 25093) 
                                                | (data.d >= 24900 & data.d <= 24991), na.rm = T) >0) & data$died == 1)),
                         as.numeric(sum((rowSums((data.d >= 25000 & data.d <= 25093) 
                                                | (data.d >= 24900 & data.d <= 24991), na.rm = T) >0) & data$died == 0)),
                         as.numeric(sum(!(rowSums((data.d >= 25000 & data.d <= 25093) 
                                                | (data.d >= 24900 & data.d <= 24991), na.rm = T) >0) & data$died == 0)), conf.level = 0.95)

RenalDys.OR <- oddsratio(as.numeric(sum((rowSums((data.d == 5853) | (data.d == 5854) | (data.d == 5855) | (data.d == 5856) | (data.d == 5859)
                                                 | (data.d == "V420") | (data.d == "V451") | (data.d == "V4511") | (data.d == "V4512")
                                                 | (data.d == "V560") | (data.d == "V561") | (data.d == "V562") | (data.d == "V563")
                                                 | (data.d == "V5631") | (data.d == "V5632") | (data.d == "V568"), na.rm = T) >0) & data$died ==1)),
                         as.numeric(sum(!(rowSums((data.d == 5853) | (data.d == 5854) | (data.d == 5855) | (data.d == 5856) | (data.d == 5859)
                                                 | (data.d == "V420") | (data.d == "V451") | (data.d == "V4511") | (data.d == "V4512")
                                                 | (data.d == "V560") | (data.d == "V561") | (data.d == "V562") | (data.d == "V563")
                                                 | (data.d == "V5631") | (data.d == "V5632") | (data.d == "V568"), na.rm = T) >0) & data$died ==1)),
                         as.numeric(sum((rowSums((data.d == 5853) | (data.d == 5854) | (data.d == 5855) | (data.d == 5856) | (data.d == 5859)
                                                 | (data.d == "V420") | (data.d == "V451") | (data.d == "V4511") | (data.d == "V4512")
                                                 | (data.d == "V560") | (data.d == "V561") | (data.d == "V562") | (data.d == "V563")
                                                 | (data.d == "V5631") | (data.d == "V5632") | (data.d == "V568"), na.rm = T) >0) & data$died ==0)),
                         as.numeric(sum(!(rowSums((data.d == 5853) | (data.d == 5854) | (data.d == 5855) | (data.d == 5856) | (data.d == 5859)
                                                 | (data.d == "V420") | (data.d == "V451") | (data.d == "V4511") | (data.d == "V4512")
                                                 | (data.d == "V560") | (data.d == "V561") | (data.d == "V562") | (data.d == "V563")
                                                 | (data.d == "V5631") | (data.d == "V5632") | (data.d == "V568"), na.rm = T) >0) & data$died ==0)), conf.level = 0.95)

IHD.OR <- oddsratio(as.numeric(sum((rowSums((data.d >= 4110 & data.d <= 4118) | (data.d == 412) | (data.d >= 4130 & data.d <=4139) 
                                            | (data.d >= 4140 & data.d <= 4149) | (data.d == "V4582"), na.rm = T) >0) & data$died == 1)),
                    as.numeric(sum(!(rowSums((data.d >= 4110 & data.d <= 4118) | (data.d == 412) | (data.d >= 4130 & data.d <=4139) 
                                            | (data.d >= 4140 & data.d <= 4149) | (data.d == "V4582"), na.rm = T) >0) & data$died == 1)),
                    as.numeric(sum((rowSums((data.d >= 4110 & data.d <= 4118) | (data.d == 412) | (data.d >= 4130 & data.d <=4139) 
                                            | (data.d >= 4140 & data.d <= 4149) | (data.d == "V4582"), na.rm = T) >0) & data$died == 0)),
                    as.numeric(sum(!(rowSums((data.d >= 4110 & data.d <= 4118) | (data.d == 412) | (data.d >= 4130 & data.d <=4139) 
                                            | (data.d >= 4140 & data.d <= 4149) | (data.d == "V4582"), na.rm = T) >0) & data$died == 0)), conf.level = 0.95)

CABG.OR <- oddsratio(as.numeric(sum((rowSums((data.d == "V4581"), na.rm = T) >0) & data$died == 1)),
                                as.numeric(sum(!(rowSums((data.d == "V4581"), na.rm = T) >0) & data$died == 1)),
                                as.numeric(sum((rowSums((data.d == "V4581"), na.rm = T) >0) & data$died == 0)),
                                as.numeric(sum(!(rowSums((data.d == "V4581"), na.rm = T) >0) & data$died == 0)), conf.level = 0.95)


PVD.OR <- oddsratio(as.numeric(sum((rowSums((data.d >= 4400 & data.d <= 4409) | (data.d == 4431) | (data.d == 4438) |(data.d == 44381) 
                                             |(data.d == 44382) |(data.d == 44389) | (data.d == 4439) |(data.d == 4471) |(data.d == "V434"),
                                             na.rm = T) >0) & data$died == 1)),
                     as.numeric(sum(!(rowSums((data.d >= 4400 & data.d <= 4409) | (data.d == 4431) | (data.d == 4438) |(data.d == 44381) 
                                             |(data.d == 44382) |(data.d == 44389) | (data.d == 4439) |(data.d == 4471) |(data.d == "V434"),
                                             na.rm = T) >0) & data$died == 1)),
                     as.numeric(sum((rowSums((data.d >= 4400 & data.d <= 4409) | (data.d == 4431) | (data.d == 4438) |(data.d == 44381) 
                                             |(data.d == 44382) |(data.d == 44389) | (data.d == 4439) |(data.d == 4471) |(data.d == "V434"),
                                             na.rm = T) >0) & data$died == 0)),
                     as.numeric(sum(!(rowSums((data.d >= 4400 & data.d <= 4409) | (data.d == 4431) | (data.d == 4438) |(data.d == 44381) 
                                             |(data.d == 44382) |(data.d == 44389) | (data.d == 4439) |(data.d == 4471) |(data.d == "V434"),
                                             na.rm = T) >0) & data$died == 0)), conf.level = 0.95)

###Complications
data.p <- data[,30:44]

Bleeding.OR <- oddsratio(as.numeric(sum((rowSums((data.p == 99811) | (data.p == 99812) | (data.p == 99811) | (data.p == 99812) 
                                      | (data.p == 9900) | (data.p == 9902) | (data.p == 9903) | (data.p == 9904)
                                      | (data.p == 56881) | (data.p == 5967) | (data.p == 59970) | (data.p == 59971)
                                      | (data.p == 7863) | (data.p == 7847) | (data.p == 4590) | (data.p == 5967)
                                      | (data.p == 59970) | (data.p == 59971) | (data.p == 7863) | (data.p == 7847)
                                      | (data.p == 4590) | (data.p == 9900) | (data.p == 9902) | (data.p == 9903)
                                      | (data.p == 9904) | (data.p == 430) | (data.p == 431) | (data.p == 4320)
                                      | (data.p == 4321) | (data.p == 4329)| (data.p == 53021) | (data.p == 4560)
                                      | (data.p == 5307) | (data.p == 53082) | (data.p == 5780) | (data.p == 5781)
                                      | (data.p == 5789) | (data.p == 45620) | (data.p == 53100) | (data.p == 53101)
                                      | (data.p == 53120) | (data.p == 53121) | (data.p == 53140) | (data.p == 53141)
                                      | (data.p == 53160) | (data.p == 53161) | (data.p == 53200) | (data.p == 53221)
                                      | (data.p == 53240) | (data.p == 53241) | (data.p == 53260) | (data.p == 53261)
                                      | (data.p == 53300) | (data.p == 53301) | (data.p == 53320) | (data.p == 53321)
                                      | (data.p == 53340) | (data.p == 53341) | (data.p == 53360) | (data.p == 53361) 
                                      | (data.p == 53400) | (data.p == 53401) | (data.p == 53420) | (data.p == 53421)
                                      | (data.p == 53440) | (data.p == 53441) | (data.p == 53460) | (data.p == 53461)
                                      | (data.p == 53501) | (data.p == 53511) | (data.p == 53521) | (data.p == 53531)
                                      | (data.p == 53541) | (data.p == 53551) | (data.p == 53561) | (data.p == 53571)
                                      | (data.p == 53783) | (data.p == 56202) | (data.p == 56203) | (data.p == 56212)
                                      | (data.p == 56213) | (data.p == 5693) | (data.p == 56985) | (data.p == 53784)
                                      | (data.p == 56986), na.rm = T) >0) & data$died == 1)), 
as.numeric(sum(!(rowSums((data.p == 99811) | (data.p == 99812) | (data.p == 99811) | (data.p == 99812) 
                        | (data.p == 9900) | (data.p == 9902) | (data.p == 9903) | (data.p == 9904)
                        | (data.p == 56881) | (data.p == 5967) | (data.p == 59970) | (data.p == 59971)
                        | (data.p == 7863) | (data.p == 7847) | (data.p == 4590) | (data.p == 5967)
                        | (data.p == 59970) | (data.p == 59971) | (data.p == 7863) | (data.p == 7847)
                        | (data.p == 4590) | (data.p == 9900) | (data.p == 9902) | (data.p == 9903)
                        | (data.p == 9904) | (data.p == 430) | (data.p == 431) | (data.p == 4320)
                        | (data.p == 4321) | (data.p == 4329)| (data.p == 53021) | (data.p == 4560)
                        | (data.p == 5307) | (data.p == 53082) | (data.p == 5780) | (data.p == 5781)
                        | (data.p == 5789) | (data.p == 45620) | (data.p == 53100) | (data.p == 53101)
                        | (data.p == 53120) | (data.p == 53121) | (data.p == 53140) | (data.p == 53141)
                        | (data.p == 53160) | (data.p == 53161) | (data.p == 53200) | (data.p == 53221)
                        | (data.p == 53240) | (data.p == 53241) | (data.p == 53260) | (data.p == 53261)
                        | (data.p == 53300) | (data.p == 53301) | (data.p == 53320) | (data.p == 53321)
                        | (data.p == 53340) | (data.p == 53341) | (data.p == 53360) | (data.p == 53361) 
                        | (data.p == 53400) | (data.p == 53401) | (data.p == 53420) | (data.p == 53421)
                        | (data.p == 53440) | (data.p == 53441) | (data.p == 53460) | (data.p == 53461)
                        | (data.p == 53501) | (data.p == 53511) | (data.p == 53521) | (data.p == 53531)
                        | (data.p == 53541) | (data.p == 53551) | (data.p == 53561) | (data.p == 53571)
                        | (data.p == 53783) | (data.p == 56202) | (data.p == 56203) | (data.p == 56212)
                        | (data.p == 56213) | (data.p == 5693) | (data.p == 56985) | (data.p == 53784)
                        | (data.p == 56986), na.rm = T) >0) & data$died == 1)),
as.numeric(sum((rowSums((data.p == 99811) | (data.p == 99812) | (data.p == 99811) | (data.p == 99812) 
                        | (data.p == 9900) | (data.p == 9902) | (data.p == 9903) | (data.p == 9904)
                        | (data.p == 56881) | (data.p == 5967) | (data.p == 59970) | (data.p == 59971)
                        | (data.p == 7863) | (data.p == 7847) | (data.p == 4590) | (data.p == 5967)
                        | (data.p == 59970) | (data.p == 59971) | (data.p == 7863) | (data.p == 7847)
                        | (data.p == 4590) | (data.p == 9900) | (data.p == 9902) | (data.p == 9903)
                        | (data.p == 9904) | (data.p == 430) | (data.p == 431) | (data.p == 4320)
                        | (data.p == 4321) | (data.p == 4329)| (data.p == 53021) | (data.p == 4560)
                        | (data.p == 5307) | (data.p == 53082) | (data.p == 5780) | (data.p == 5781)
                        | (data.p == 5789) | (data.p == 45620) | (data.p == 53100) | (data.p == 53101)
                        | (data.p == 53120) | (data.p == 53121) | (data.p == 53140) | (data.p == 53141)
                        | (data.p == 53160) | (data.p == 53161) | (data.p == 53200) | (data.p == 53221)
                        | (data.p == 53240) | (data.p == 53241) | (data.p == 53260) | (data.p == 53261)
                        | (data.p == 53300) | (data.p == 53301) | (data.p == 53320) | (data.p == 53321)
                        | (data.p == 53340) | (data.p == 53341) | (data.p == 53360) | (data.p == 53361) 
                        | (data.p == 53400) | (data.p == 53401) | (data.p == 53420) | (data.p == 53421)
                        | (data.p == 53440) | (data.p == 53441) | (data.p == 53460) | (data.p == 53461)
                        | (data.p == 53501) | (data.p == 53511) | (data.p == 53521) | (data.p == 53531)
                        | (data.p == 53541) | (data.p == 53551) | (data.p == 53561) | (data.p == 53571)
                        | (data.p == 53783) | (data.p == 56202) | (data.p == 56203) | (data.p == 56212)
                        | (data.p == 56213) | (data.p == 5693) | (data.p == 56985) | (data.p == 53784)
                        | (data.p == 56986), na.rm = T) >0) & data$died == 0)),
as.numeric(sum(!(rowSums((data.p == 99811) | (data.p == 99812) | (data.p == 99811) | (data.p == 99812) 
                        | (data.p == 9900) | (data.p == 9902) | (data.p == 9903) | (data.p == 9904)
                        | (data.p == 56881) | (data.p == 5967) | (data.p == 59970) | (data.p == 59971)
                        | (data.p == 7863) | (data.p == 7847) | (data.p == 4590) | (data.p == 5967)
                        | (data.p == 59970) | (data.p == 59971) | (data.p == 7863) | (data.p == 7847)
                        | (data.p == 4590) | (data.p == 9900) | (data.p == 9902) | (data.p == 9903)
                        | (data.p == 9904) | (data.p == 430) | (data.p == 431) | (data.p == 4320)
                        | (data.p == 4321) | (data.p == 4329)| (data.p == 53021) | (data.p == 4560)
                        | (data.p == 5307) | (data.p == 53082) | (data.p == 5780) | (data.p == 5781)
                        | (data.p == 5789) | (data.p == 45620) | (data.p == 53100) | (data.p == 53101)
                        | (data.p == 53120) | (data.p == 53121) | (data.p == 53140) | (data.p == 53141)
                        | (data.p == 53160) | (data.p == 53161) | (data.p == 53200) | (data.p == 53221)
                        | (data.p == 53240) | (data.p == 53241) | (data.p == 53260) | (data.p == 53261)
                        | (data.p == 53300) | (data.p == 53301) | (data.p == 53320) | (data.p == 53321)
                        | (data.p == 53340) | (data.p == 53341) | (data.p == 53360) | (data.p == 53361) 
                        | (data.p == 53400) | (data.p == 53401) | (data.p == 53420) | (data.p == 53421)
                        | (data.p == 53440) | (data.p == 53441) | (data.p == 53460) | (data.p == 53461)
                        | (data.p == 53501) | (data.p == 53511) | (data.p == 53521) | (data.p == 53531)
                        | (data.p == 53541) | (data.p == 53551) | (data.p == 53561) | (data.p == 53571)
                        | (data.p == 53783) | (data.p == 56202) | (data.p == 56203) | (data.p == 56212)
                        | (data.p == 56213) | (data.p == 5693) | (data.p == 56985) | (data.p == 53784)
                        | (data.p == 56986), na.rm = T) >0) & data$died == 0)), conf.level = .95)
Renal.OR <- oddsratio(as.numeric(sum((rowSums((data.p >= 5845 & data.p <= 5849) | (data.p == 586) | (data.p == 9975) 
                                             | (data.p == 3995) | (data.p == 5498), na.rm = T) >0) & data$died == 1)),
                      as.numeric(sum(!(rowSums((data.p >= 5845 & data.p <= 5849) | (data.p == 586) | (data.p == 9975) 
                                              | (data.p == 3995) | (data.p == 5498), na.rm = T) >0) & data$died == 1)),
                      as.numeric(sum((rowSums((data.p >= 5845 & data.p <= 5849) | (data.p == 586) | (data.p == 9975) 
                                              | (data.p == 3995) | (data.p == 5498), na.rm = T) >0) & data$died == 0)),
                      as.numeric(sum(!(rowSums((data.p >= 5845 & data.p <= 5849) | (data.p == 586) | (data.p == 9975) 
                                              | (data.p == 3995) | (data.p == 5498), na.rm = T) >0) & data$died == 0)), conf.level = 0.95)


Cardiac.OR <- oddsratio(as.numeric(sum((rowSums((data.p == 9971) | (data.p == 4260) | (data.p == 3771) | (data.p == 3772) 
                                                | (data.p == 3778) | (data.p == 3780) | (data.p == 3781) | (data.p == 3782)
                                                | (data.p == 3783) | (data.p == 3964) | (data.p == 4230) | (data.p == 4233)
                                                , na.rm = T) >0) & data$died == 1)),
                        as.numeric(sum(!(rowSums((data.p == 9971) | (data.p == 4260) | (data.p == 3771) | (data.p == 3772) 
                                                | (data.p == 3778) | (data.p == 3780) | (data.p == 3781) | (data.p == 3782)
                                                | (data.p == 3783) | (data.p == 3964) | (data.p == 4230) | (data.p == 4233)
                                                , na.rm = T) >0) & data$died == 1)), 
                        as.numeric(sum((rowSums((data.p == 9971) | (data.p == 4260) | (data.p == 3771) | (data.p == 3772) 
                                                | (data.p == 3778) | (data.p == 3780) | (data.p == 3781) | (data.p == 3782)
                                                | (data.p == 3783) | (data.p == 3964) | (data.p == 4230) | (data.p == 4233)
                                                , na.rm = T) >0) & data$died == 0)), 
                        as.numeric(sum(!(rowSums((data.p == 9971) | (data.p == 4260) | (data.p == 3771) | (data.p == 3772) 
                                                | (data.p == 3778) | (data.p == 3780) | (data.p == 3781) | (data.p == 3782)
                                                | (data.p == 3783) | (data.p == 3964) | (data.p == 4230) | (data.p == 4233)
                                                , na.rm = T) >0) & data$died == 0)), conf.level = 0.95)

Mechanical.OR <- oddsratio(as.numeric(sum((rowSums((data.p == 9960) | (data.p == 99600) 
                                          | (data.p == 99609), na.rm = T) >0) & data$died == 1)),
                           as.numeric(sum(!(rowSums((data.p == 9960) | (data.p == 99600) 
                                          | (data.p == 99609), na.rm = T) >0) & data$died == 1)),
                           as.numeric(sum((rowSums((data.p == 9960) | (data.p == 99600) 
                                          | (data.p == 99609), na.rm = T) >0) & data$died == 0)),
                           as.numeric(sum(!(rowSums((data.p == 9960) | (data.p == 99600) 
                                          | (data.p == 99609), na.rm = T) >0) & data$died == 0)), conf.level = 0.95)

Low.Volume.OR <- oddsratio(as.numeric(sum(data$Volume.year <= 3.375 & data$died == 1)),
                           as.numeric(sum(data$Volume.year > 3.375 & data$died == 1)),
                           as.numeric(sum(data$Volume.year <= 3.375 & data$died == 0)),
                           as.numeric(sum(data$Volume.year > 3.375 & data$died == 0)))
  
  all.counts[all.counts$Volume.year <= 3.375 & all.counts$year<2012,]
Medium <-all.counts[all.counts$Volume.year >3.375 & all.counts$Volume.year<11.5 & all.counts$died >-1 
                    & all.counts$year<2012, ]
High <-all.counts[all.counts$Volume.year >= 11.5 & all.counts$year<2012, ]



Female gender
Comorbidity list(
Complication list
Demographics
Hospital volume
)

all.counts$race <- factor(all.counts$race )
all.counts$zip <- factor(all.counts$zip )
all.counts$hospital <- factor(all.counts$hospital )
all.counts$sex <- factor(all.counts$sex )
all.counts$volume <- "Medium"
all.counts[all.counts$Volume.year >= 11.5,]$volume <- "High"
all.counts[all.counts$Volume.year <= 3.375,]$volume <- "Low"

model <- glm(died ~ age + race + zip + sex + volume, 
data = all.counts[all.counts$died>=0,], family = "binomial")
summary(model) 

exp(cbind(OR = coef(model), confint(model)))

