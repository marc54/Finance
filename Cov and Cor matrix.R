library(rJava)
library(xlsxjars)
library(xlsx)

weeklyData = read.xlsx("C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/data-DJ30-july2011-june2013.xlsx", 1, header=TRUE)
monthlyData = read.xlsx("C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/data-DJ30-july2011-june2013.xlsx", 2, header=TRUE)

weeklyData = data.matrix(weeklyData)
monthlyData = data.matrix(monthlyData)

weeklyData = subset(weeklyData, select = -c(Date) )
monthlyData = subset(monthlyData, select = -c(Date) )

weeklyDataBottom = weeklyData[-(nrow(weeklyData)),]
weeklyDataTop = weeklyData[-1,]

monthlyDataBottom = monthlyData[-(nrow(monthlyData)),]
monthlyDataTop = monthlyData[-1,]

weeklyLog = log(weeklyDataTop/weeklyDataBottom)
monthlyLog = log(monthlyDataTop/monthlyDataBottom)

write.xlsx(weeklyLog ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question16Output.xlsx", sheetName  = "WeeklyLog", append = TRUE)
write.xlsx(monthlyLog ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question16Output.xlsx", sheetName  = "MonthlyLog", append = TRUE)

weeklyCovariance = cov(weeklyLog)
monthlyCovariance = cov(monthlyLog)

weeklyCorrilation = cor(weeklyLog)
monthlyCorrilation = cor(monthlyLog)

write.xlsx(weeklyCovariance ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question16Output.xlsx", sheetName  = "Weekly Covariance", append = TRUE)
write.xlsx(weeklyCorrilation ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question16Output.xlsx", sheetName =  "Weekly Corrilation", append = TRUE)
write.xlsx(monthlyCovariance ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question16Output.xlsx", sheetName = "Monthly Covariance", append = TRUE)
write.xlsx(monthlyCorrilation ,"C:/Users/Marc Pfeiffer/Desktop/Baruch Pre-MFE/NLA/Homework 6/Question16Output.xlsx", sheetName ="Monthly Corrilation", append = TRUE)
             
