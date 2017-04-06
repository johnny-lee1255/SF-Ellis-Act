library(sjPlot)
library(miscTools)
library(zoo)



setwd("~/Dropbox/UCSD/POLI 170A")

pricesSF <- read.csv("prices.csv")
pricesCity <- read.csv("pricesCity.csv")
unemployment <- read.csv("unemployment.csv")

names(pricesSF)[8:dim(pricesSF)[2]] <-
  substr(as.character(names(pricesSF)[8:dim(pricesSF)[2]]),2,8) # fix column names
names(pricesCity)[8:dim(pricesCity)[2]] <-
  substr(as.character(names(pricesCity)[8:dim(pricesCity)[2]]),2,8) # fix column names

evictions <- read.csv("evictions.csv")
## The following csv file was created by using table() on the evictions data,
## then edited in Excel because it was easier to manipulate it there.

months <- as.yearmon(names(pricesSF)[8:dim(pricesSF)[2]],"%Y.%m")
evictionsEllis <- subset(evictions, Ellis.Act.WithDrawal == 1)
nofaultEvictions <- subset(evictions,nofault == 1)
faultEvictions <- subset(evictions,nofault == 0)
ownerEvictions <- subset(evictions,Owner.Move.In == 1)
paymentEvictions <-
  subset(evictions,Late.Payments == 1 | Non.Payment == 1)
breachEvictions <-
  subset(evictions,Breach == 1)
ellisFreq <- data.frame(date = as.yearmon(names(table(
  as.yearmon(evictionsEllis$Date)
))), Freq =  as.numeric(table(as.yearmon(
  evictionsEllis$Date
))))

dat2 <- data.frame(date = as.yearmon(seq(
  as.Date("1997-01-01"), by = "1 month", length.out = 228
)))
ellisFreq <- merge(ellisFreq,dat2,all = TRUE)
ellisFreq$Freq[is.na(ellisFreq$Freq)] <- 0

#How many notices have a positive change in price after 120 days:
sum(na.omit(evictionsEllis$deltaPrice > 0))/length(na.omit(evictionsEllis$deltaPrice))
sum(na.omit(evictions$deltaPrice > 0))/length(na.omit(evictions$deltaPrice))
sum(na.omit(nofaultEvictions$deltaPrice > 0))/length(na.omit(nofaultEvictions$deltaPrice))
sum(na.omit(ownerEvictions$deltaPrice > 0))/length(na.omit(ownerEvictions$deltaPrice))

#looking at the mean and medians of the change in price
median(na.omit(evictionsEllis$deltaPrice[evictionsEllis$deltaPrice > 0]))
mean(na.omit(evictionsEllis$deltaPrice[evictionsEllis$deltaPrice > 0]))
median(na.omit(evictions$deltaPrice[evictions$deltaPrice > 0]))
mean(na.omit(evictions$deltaPrice[evictions$deltaPrice > 0]))
median(na.omit(nofaultEvictions$deltaPrice[nofaultEvictions$deltaPrice > 0]))
mean(na.omit(nofaultEvictions$deltaPrice[nofaultEvictions$deltaPrice > 0]))
median(na.omit(ownerEvictions$deltaPrice[ownerEvictions$deltaPrice > 0]))
mean(na.omit(ownerEvictions$deltaPrice[ownerEvictions$deltaPrice > 0]))


## All Evictions Plot
plot(
  table(as.yearmon(evictions$Date)),
  xlab = "Month",ylab = "Number of Eviction per Month", type = "l",
  main = "All Evictions Frequency\nJan 1997 to Dec 2015"
  ,axes = F
)
axis(1,seq(1,228,12),labels = names(table(as.yearmon(evictions$Date)))[seq(1,228,12)])
axis(2)



## Ellis Evictions Plot
#pdf("EllisCount.pdf", height=6, width=6)

plot(
  ellisFreq$date, ellisFreq$Freq,
  xlab = "Month",ylab = "Number of Eviction per Month", type = "l",
  main = "Ellis Evictions Frequency\nJan 1997 to Dec 2015",
  axes = F
)
axis(1,ellisFreq$date[seq(1,228,12)])
axis(2)
#dev.off()

## All Evictions barplots, separated by fault/no fault
#pdf("EvictionsCount.pdf", height=6, width=6)
barplot(
  table(evictions$nofault,as.yearmon(evictions$Date)),main = "Number of Evictions",
  xlab = "Month",ylab = "Number of Evictions", col = c("red","darkblue")
)
legend(60,350,c("No Fault","Fault"),fill = c("red","darkblue"))
#dev.off()

## No Fault Eviction barplots, separated by Ellis/Other no faults
#pdf("NoFaultsCount.pdf", height=6, width=6)
barplot(
  table(
    nofaultEvictions$Ellis.Act.WithDrawal,as.yearmon(nofaultEvictions$Date)
  )
  ,main = "No Fault Evictions"
  ,xlab = "Month",ylab = "Number of Evictions", col = c("cadetblue","darkorange")
)
legend(50,250,c("Ellis","Other No Fault"),fill = c("darkorange","cadetblue"))
#dev.off()




## Median Home Value Plot
#pdf("MedianPrice.pdf", height=6, width=6)
plot(
  1:(dim(pricesCity)[2] - 7), pricesCity[,8:(dim(pricesCity)[2])],
  xlab = "Month",ylab = "Price in Dollars", type = "l",
  main = "Median Home Value\nApr 1996 to Dec 2015"
  ,axes = F
)
axis(1,seq(10,238,12),labels = months[seq(10,238,12)])
axis(2)
#dev.off()


Y_deltaPrices <-
  as.numeric(pricesCity[,21:244] - pricesCity[,17:240]) # 4 months change
X_unemployment <- unemployment$Rate[5:228]


X_NumOfEvictions <-
  as.numeric(table(as.yearmon(evictions$Date))[1:224])
regressionAll <-
  lm(Y_deltaPrices ~ X_NumOfEvictions + X_unemployment)
summary(regressionAll)
plot(
  Y_deltaPrices ~ X_NumOfEvictions, xlab = "Number of All Evictions", ylab =
    "Change in Price", main = "Change in Price vs Evictions \n Monthly"
)
abline(regressionAll)


X_NumOfEllis <- as.numeric(ellisFreq$Freq[1:224])
ellisRegression <- lm(Y_deltaPrices ~ X_NumOfEllis + X_unemployment)
summary(ellisRegression)
plot(
  Y_deltaPrices ~ X_NumOfEllis, xlab = "Number of Ellis Evictions", ylab =
    "Change in Price", main = "Change in Price vs Ellis Evictions \n Monthly"
)
abline(ellisRegression)


X_NumOfMoveIns <-
  as.numeric(table(as.yearmon(ownerEvictions$Date))[1:224])
moveinRegression <-
  lm(Y_deltaPrices ~ X_NumOfMoveIns + X_unemployment)
summary(moveinRegression)
plot(
  Y_deltaPrices ~ X_NumOfMoveIns, xlab = "Number of Owner Move-In Evictions", ylab =
    "Change in Price", main = "Change in Price vs Owner Move-In Evictions \n Monthly"
)
abline(moveinRegression)

ellisMoveInRegression <-
  lm (Y_deltaPrices ~  X_NumOfEllis + X_NumOfMoveIns + X_unemployment)

sjt.lm(
  regressionAll,ellisRegression,moveinRegression,ellisMoveInRegression,showConfInt = F,showStdError = T
)


modelEllis <-
  lm(evictions$deltaPrice ~ evictions$Ellis.Act.WithDrawal + evictions$unemployment)
modelOwner <-
  lm(evictions$deltaPrice ~ evictions$Owner.Move.In + evictions$unemployment)
modelPayment <-
  lm(
    evictions$deltaPrice ~ evictions$Late.Payments + evictions$Non.Payment + evictions$unemployment
  )
modelEllisMoveCap <-
  lm(
    evictions$deltaPrice ~ evictions$Ellis.Act.WithDrawal + evictions$Owner.Move.In + evictions$unemployment
  )

modelDevelopment <-
  lm(evictions$deltaPrice ~ evictions$Development + evictions$unemployment)

modelAll <-
  lm(
    evictions$deltaPrice ~ evictions$Ellis.Act.WithDrawal + evictions$Owner.Move.In+ evictions$Late.Payments + evictions$Non.Payment + evictions$Development + evictions$unemployment
  )

sjt.lm(
  modelEllis,modelOwner,modelPayment,modelEllisMoveCap,modelAll
  ,showConfInt = F,showStdError = T
)
