#1. Data Wrangling
#loading the RegularSeasonDetailedResults
RegularSeasonDetailedResults <- read.csv("RegularSeasonDetailedResults.csv")
View(RegularSeasonDetailedResults)
myData = RegularSeasonDetailedResults

#creating winningTeam dataset having data of winningTeam
winningTeam = myData[, c(1,3,4,9:21)]

#creating losingTeam dataset having data of losingTeam
losingTeam = myData[, c(1,5,6,22:34)]

View(winningTeam)
View(losingTeam)

#renaming the columns of Winning Team

colnames(winningTeam) = c("Season","team","score", "fgm","fga","fgm3","fga3","ftm","fta","or","dr","ast","to","stl","blk","pf")

#renaming the columns of losing team

colnames(losingTeam) = c("Season","team","score", "fgm","fga","fgm3","fga3","ftm","fta","or","dr","ast","to","stl","blk","pf")

#combing losingTeam and winningTeam datasets

myDataSet = rbind(winningTeam,losingTeam)
View(myDataSet)

#calculating average of all the statistics(from columns 3-16) 
#of myDataSet
myDataSet_avg = aggregate(myDataSet[, 3:16], list(myDataSet$team, myDataSet$Season), mean)
View(myDataSet_avg)
colnames(myDataSet_avg) = c("Team","Season","Avgscore", "Avgfgm","Avgfga","Avgfgm3","Avgfga3","Avgftm","Avgfta","Avgor","Avgdr","Avgast","Avgto","Avgstl","Avgblk","Avgpf")


#saving the final data in new dataset
write.csv(myDataSet_avg, "Avg_score.csv")