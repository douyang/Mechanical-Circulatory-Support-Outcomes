setwd("C:\\Users\\David\\Dropbox\\NIS-Small\\LVAD\\")

data <- read.csv("unweightedLVAD-fullDates-CountLVAD-fixed.csv", stringsAsFactors = FALSE)

str(data)
data2 <- data[data$age >= 18,]

write.csv(data2, "unweightedLVAD-fullDates-CountLVAD-fixed-Over18.csv")

fulldata <- data2

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
write.csv(newdata, "tableProceduresGreaterThan18.csv")


procDF$cat <- "None"
procDF[procDF$procedures == "3766",]$cat <- "LVAD"
procDF[procDF$procedures %in% c("3751", "375"),]$cat <- "Heart Transplant"
procDF[procDF$procedures %in% c("3403", "3764", "3479", "341", "3749", "3712"),]$cat <- "Reoperation"
procDF[procDF$procedures %in% c("3961", "3761", "9604", "9672", "9671", "3995"),]$cat <- "Support"
procDF[procDF$procedures %in% c("4513", "4523", "9904", "9907", "9905", "9909"),]$cat <- "Bleed Related"
procDF[procDF$procedures %in% c("8964", "3723", "3721", "8856", "3891", "3893"),]$cat <- "Cath"
table(procDF$cat)

library(ggplot2)
qplot(procDF$daysFromLVAD, fill = procDF$cat) + facet_wrap()

excludeInitialLVAD <- procDF[!(procDF$daysFromLVAD == 0 & procDF$procedures == "3766"),]

p = ggplot(excludeInitialLVAD , aes(x=daysFromLVAD,fill = cat)) + geom_density(alpha = 0.3)
p + facet_grid(cat ~ ., scales = "free_y")
ggsave("TimelineAsDistribution.png")

p = ggplot(excludeInitialLVAD , aes(x=daysFromLVAD,fill = cat)) + geom_bar(alpha = 0.9, binwidth = 1)
p + facet_grid(cat ~ ., scales = "free_y") +xlim(-30, 100)
ggsave("TimelineAsBarGraph.png")