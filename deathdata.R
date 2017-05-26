#import data
data = read.csv("TABLE_III._Deaths_in_122_U.S._cities.csv")

#convert NA's to 0
data[is.na(data)] <- 0

unique(data$Reporting.Area)
#133 unique reporting areas

#major regions
regions <- unique(data$Reporting.Area)[1:9]

unique(data$MMWR.WEEK)
#all 2016, 39 weeks included

x11()

#plot 1 ###########################################################
#graph weekly deaths by major region

#add first line
region <- "New England"
datasmall <- data[data$Reporting.Area==region,]
#order by week
datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
plot(datasmall$MMWR.WEEK,
     datasmall$All.causes..by.age..years...All.Ages..,
     ylim=c(25,3500),
     type="l",
     col="blue",
     main="Weekly deaths by region",
     xlab="Week",
     ylab="# deaths")
#create vector of colors
colors <- c("blue","green","brown","cyan",
            "darkorchid","gold","firebrick",
            "darkolivegreen","deeppink")
for(i in 2:9){
  region <- regions[i]
  datasmall <- data[data$Reporting.Area==region,]
  #order by week
  datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
  points(datasmall$MMWR.WEEK,
         datasmall$All.causes..by.age..years...All.Ages..,
         col=colors[i],
         type="l")
}
#add legend
legend(25,3500,regions,lty=rep(1,9),lwd=rep(2.5,9),col=colors)

#peak in Pacific region in week 16
region <- "Pacific"
datasmall <- data[data$Reporting.Area==region,]
#order by week
datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
max(datasmall$All.causes..by.age..years...All.Ages..)
datasmall$MMWR.WEEK[datasmall$All.causes..by.age..years...All.Ages..>2500]

#consider age vs # deaths in pacific region
names(datasmall)

#plot 2 ###########################################################
#Consider only Pacific, plot different ages - 4 groups
agecols <- c(6,8,10,12)
ages <- names(datasmall)[agecols]
plot(datasmall$MMWR.WEEK,
     datasmall[,agecols[1]],
     ylim=c(25,2000),
     type="l",
     col="blue",
     main="Weekly deaths in Pacific region by age",
     xlab="Week",
     ylab="# deaths")
for(i in 2:4){
  points(datasmall$MMWR.WEEK,
         datasmall[,agecols[i]],
         col=colors[i],
         type="l")
}
#add legend
agesnice <- c(">65","45-64","25-44","1-24")
legend(30,2000,agesnice,lty=rep(1,4),lwd=rep(2.5,4),col=colors[1:4])

#plot 3 ###########################################################
#multiple plots for each age group
#change axis range accordingly
axismax=c(2000,750,200,100)
par(mfrow=c(2,2))
for(j in 1:4){
  #add first line
  region <- "New England"
  datasmall <- data[data$Reporting.Area==region,]
  #order by week
  datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
  plot(datasmall$MMWR.WEEK,
       datasmall[,agecols[j]],
       ylim=c(0,axismax[j]),
       type="l",
       col="blue",
       main=agesnice[j],
       xlab="Week",
       ylab="# deaths")
  #create vector of colors
  colors <- c("blue","green","brown","cyan",
              "darkorchid","gold","firebrick",
              "darkolivegreen","deeppink")
  for(i in 2:9){
    region <- regions[i]
    datasmall <- data[data$Reporting.Area==region,]
    #order by week
    datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
    points(datasmall$MMWR.WEEK,
           datasmall[,agecols[j]],
           col=colors[i],
           type="l")
  }
}

#add legend
legend(25,100,regions,lty=rep(1,9),lwd=rep(2.5,9),
       col=colors, cex=0.5)

#plot 4 ###########################################################
head(data)
summary(data)
subregions <- unique(data$Reporting.Area)[11:length(unique(data$Reporting.Area))]

regionmeans <- data.frame(subregions)
for(i in 1:(length(subregions))){
  regionmeans[i,2] <- mean(data[data$Reporting.Area==subregions[i],4])
}
names(regionmeans) <- c("subregion","avg_deaths")

max(regionmeans$avg_deaths)
#order by avg_deaths
regionmeans <- regionmeans[order(regionmeans$avg_deaths, 
                                 decreasing=TRUE),]
#plot top 5 most deaths
regions <- regionmeans$subregion[1:5]
par(mfrow=c(1,1))

#add first line
region <- regions[1]
datasmall <- data[data$Reporting.Area==region,]
#order by week
datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
plot(datasmall$MMWR.WEEK,
     datasmall$All.causes..by.age..years...All.Ages..,
     ylim=c(0,1500),
     type="l",
     col="blue",
     main="Weekly deaths by region",
     xlab="Week",
     ylab="# deaths")
#create vector of colors
colors <- c("blue","green","brown","cyan",
            "darkorchid","gold","firebrick",
            "darkolivegreen","deeppink")
for(i in 2:9){
  region <- regions[i]
  datasmall <- data[data$Reporting.Area==region,]
  #order by week
  datasmall <- datasmall[order(datasmall$MMWR.WEEK),]
  points(datasmall$MMWR.WEEK,
         datasmall$All.causes..by.age..years...All.Ages..,
         col=colors[i],
         type="l")
}
#add legend
legend(25,1500,regions,lty=rep(1,5),lwd=rep(2.5,5),col=colors)

