setwd("K:/Home/Pierre/Employment/Qualifications/Coursera Data Scientist/05 Reproducible Research/Test/RepData_PeerAssessment1")
activity <- read.csv("activity.csv")
cleanact <- activity[!is.na(activity$steps),]
perday<-count(cleanact,"date","steps")
hist(perday[,2], main="Histogram of Steps per Day", xlab="Steps per Day", col="green")
mean(perday[,2])
median(perday[,2])
ag<-aggregate(steps ~ interval,data=activity, mean)
plot(ag,type="l")
max(ag[,2])
ags<-sort.list(ag[,2], decreasing=TRUE)
ag[ags[1],1]

adjact<-join(activity,ag,by="interval")
adjact[is.na(adjact[,1]),1] <- adjact[is.na(adjact[,1]),4]
adjacty<-adjact[,1:3]
adjday<-count(adjacty,"date","steps")
adjday[is.na(adjday$freq)]<-0
mean(adjday$freq)
median(adjday$freq)

adjactx<-adjact[,c(1,2,3,2)]
colnames(adjactx)[4]<-"dayofweek"
adjactx[,4]<-weekdays(as.Date(adjactx[,2]))
adjactx<-adjactx[,c(1,2,3,4,4)]
colnames(adjactx)[5]<-"typeofday"
adjactx[,5]<-"weekday"
adjactx[adjactx[,4]=="Saturday"|adjactx[,4]=="Sunday",5]<-"weekend"
agx<-aggregate(steps ~ interval + typeofday, data=adjactx, mean)
xyplot(steps ~ interval | typeofday, data=agx, layout=c(1,2), xlab="Interval", ylab="Number of steps", type="l")