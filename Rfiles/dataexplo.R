#setwd('/path/to/Rnyc/Rfiles')
train <- read.csv(file = '../train/train.csv',sep = ",")
test <- read.csv(file = '../test/test.csv',sep = ",")

#install.packages('xts')
library(xts)


subtrain <- train[1:10000,]
subtest <- test[1:10000,]

#Question 1.1 Analyser cohérence train/test
print('Train pickup-longitude')
print(summary(train$pickup_longitude))
print('Test pick-up longitude')
print(summary(test$pickup_longitude))

print('Train pickup-latitude')
print(summary(train$pickup_latitude))
print('Test pickup-latitude')
print(summary(test$pickup_latitude))

print('Train passenger count')
print(summary(train$passenger_count))
print('Test passenger count')
print(summary(test$passenger_count))

long <- list(train$pickup_longitude, test$pickup_longitude)
names(long)=c(paste("Training set\n n=" , length(train$pickup_longitude) , sep=""), paste("Test set\n n=" , length(test$pickup_longitude) , sep=""))
boxplot(long)
title(" Boxplot of longitude")

lat <- list(train$pickup_latitude, test$pickup_latitude)
names(lat)=c(paste("Training set\n n=" , length(train$pickup_latitude) , sep=""), paste("Test set\n n=" , length(test$pickup_latitude) , sep=""))
boxplot(lat)
title(" Boxplot of latitude")

pass <- list(train$passenger_count, test$passenger_count)
names(pass)=c(paste("training set\n n=" , length(train$passenger_count) , sep=""), paste("Test set\n n=" , length(test$passenger_count) , sep=""))
boxplot(pass)
title(" Boxplot of passenger count")

# Question 1.1 Analyser
#Recuperer les dates/heures uniques pour train et test selon jours, mois, heures
dati <- substr(as.character(train$pickup_datetime), 0, 10)
monthy <- substr(as.character(train$pickup_datetime), 6, 7)
timi <- substr(as.character(train$pickup_datetime), 12, 13)

dati_test <- substr(as.character(test$pickup_datetime), 0, 10)
monthy_test <- substr(as.character(test$pickup_datetime), 6, 7)
timi_test <- substr(as.character(test$pickup_datetime), 12, 13)

#Plot la répartition journaliere de train et test
agg_dati<- aggregate(train, by = list(dati) , length)
plot.ts(x=agg_dati$id, xlab="day", ylab="number of rides")
title('Nombre quotidien de courses à NYC\nDataTrain')

agg_dati_test<- aggregate(test, by = list(dati_test) , length)
plot.ts(x=agg_dati_test$id, xlab="day", ylab="number of rides")
title('Nombre quotidien de courses à NYC\nDataTest')

#Plot la répartition mensuelle de train et test
agg_monthy<- aggregate(train, by = list(monthy) , length)
plot.ts(x=agg_monthy$id, xlab="month", ylab="number of rides")
title('Nombre de course totale datatrain\nréparti par mois')

agg_monthy_test<- aggregate(test, by = list(monthy_test) , length)
plot.ts(x=agg_monthy_test$id, xlab="month", ylab="number of rides")
title('Nombre de course totale datatest\nréparti par mois')

#Plot la répartition journaliere mensuelle et horaire de test
agg_timi<- aggregate(train, by = list(timi) , length)
plot.ts(x=agg_timi$id, xlab="hour", ylab="number of rides")
title('Nombre de course totale datatrain\nréparti par heure')

agg_timi_test<- aggregate(test, by = list(timi_test) , length)
plot.ts(x=agg_timi_test$id, xlab="hour", ylab="number of rides")
title('Nombre de course totale datatest\nréparti par heure')

dati <- weekdays(as.Date.character(dati))
dati[dati=="Lundi"] <- 1
dati[dati=="Mardi"] <- 2
dati[dati=="Mercredi"] <- 3
dati[dati=="Jeudi"] <- 4
dati[dati=="Vendredi"] <- 5
dati[dati=="Samedi"] <- 6
dati[dati=="Dimanche"] <- 7
dati <- as.integer(dati)

train$weekday <- dati

dati_test <- weekdays(as.Date.character(dati_test))
dati_test[dati_test=="Lundi"] <- 1
dati_test[dati_test=="Mardi"] <- 2
dati_test[dati_test=="Mercredi"] <- 3
dati_test[dati_test=="Jeudi"] <- 4
dati_test[dati_test=="Vendredi"] <- 5
dati_test[dati_test=="Samedi"] <- 6
dati_test[dati_test=="Dimanche"] <- 7
dati_test <- as.integer(dati_test)

test$weekday <- dati_test

agg_weekday_train <- aggregate(train, by = list(dati) , length)
plot.ts(x=agg_weekday_train$id, xlab="weekday", ylab="number of rides")
title('Nombre de course totale datatrain\nréparti par jour de la semaine')

agg_weekday_test <- aggregate(test, by = list(dati_test) , length)
plot.ts(x=agg_weekday_test$id, xlab="weekday", ylab="number of rides")
title('Nombre de course totale datatest\nréparti par jour de la semaine')

rm(agg_timi, agg_timi_test, agg_monthy, agg_monthy_test,
   agg_dati, agg_dati_test,dati, monthy, timi,
   dati_test, monthy_test, timi_test, long, lat, pass)

color.gradient <- function(x, colors=c("green","yellow","red"), colsteps=5) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

par(bg = 'black', col.lab="white", col.axis="white", col.main="white", col.sub="white")
plot(x=subtrain$pickup_longitude, y=subtrain$pickup_latitude,
            title("NYC map train data"), xlab='Longitude', ylab='Latitude', pch=18, cex=0.3,
            xlim = c(min(subtrain$pickup_longitude), max(subtrain$pickup_longitude)),
            ylim = c(min(subtrain$pickup_latitude), max(subtrain$pickup_latitude)),
            col =color.gradient(log(subtrain$trip_duration))
)

par(bg = 'black', col.lab="white", col.axis="white", col.main="white", col.sub="white")
plot(x=subtest$pickup_longitude, y=subtest$pickup_latitude,
                title("NYC map test data"), xlab='Longitude', ylab='Latitude', pch=18, cex=0.3,
                xlim = c(min(subtest$pickup_longitude), max(subtest$pickup_longitude)),
                ylim = c(min(subtest$pickup_latitude), max(subtest$pickup_latitude)),
                col ="white"
)

# Compute distance between pickup and dropoff points
# install.packages("geosphere")
library(geosphere)
distances <- distHaversine(p1= matrix(train$pickup_longitude, train$pickup_latitude, nrow=length(train$pickup_latitude), ncol=2),
              p2= matrix(train$dropoff_longitude, train$dropoff_latitude, nrow=length(train$dropoff_latitude), ncol=2))

train$distances <- distances


# Enrich Data with Weather
train$date <- as.Date(train$pickup_datetime, "%Y-%m-%d")
train$dropoff_datetime <- as.Date(train$dropoff_datetime, "%Y-%m-%d")
train$pickup_datetime <- NULL


weather <- read.csv('../train/weather.csv')
weather$date <- as.Date(x=weather$date, "%d-%m-%Y")
weather$precipitation <- as.integer(weather$precipitation)
weather$snow.fall <- as.integer(weather$snow.fall)
weather$snow.depth <- as.integer(weather$snow.depth)

train <- merge(x = train, y = weather, by = "date", all.x = TRUE)

#Question 1.2.2
par(bg = 'white', col.lab="black", col.axis="black", col.main="black", col.sub="black")
hist(x=log(train$trip_duration), breaks=100, xlab = 'log of trip duration',
     ylab="frequency", title='Histograme des fréquences durée des courses')


plot(x=log(sort(subtrain$trip_duration)) ,pch=18, cex=0.3, xlab="index id", ylab='log(trip duration)')
title("Duration of trips in NYC")

print(summary(train$trip_duration))
hist(log(train$trip_duration), breaks=1000)

train$z_vals <- (train$trip_duration - mean(train$trip_duration))/sd(train$trip_duration)

outliers <- train[
  which(train$z_vals >1.96 | train$pickup_longitude < -74.05 | train$pickup_latitude >41 |
      train$pickup_longitude > -73.7 | train$pickup_latitude < 40.6 | train$z_vals < -1.96),
  ]

par(bg = 'black', col.lab="white", col.axis="white", col.main="white", col.sub="white")
plot(x=outliers$pickup_longitude, y=outliers$pickup_latitude,
     title("NYC map outliers geo-data"), xlab='Longitude', ylab='Latitude', pch=18, cex=0.3,
     xlim = c(min(outliers$pickup_longitude), max(outliers$pickup_longitude)),
     ylim = c(min(outliers$pickup_latitude), max(outliers$pickup_latitude)),
     col ="white")

summary(z_vals[which(z_vals<1.96)])
summary(outliers$z_vals)

plot(x=outliers)
print(length(outliers)/length(train$trip_duration))

plot(sort(outliers$passenger_count), sort(outliers$z_vals), xlab="Nombre de passagers",
     ylab="Z score")
title("Evolution du Z score selon le nombre de passagers")

plot(x=outliers$pickup_longitude, y = outliers$pickup_latitude, xlab="longitude", ylab="latitude")
title("Pick up place for outliers")
plot(x=outliers$dropoff_longitude, y = outliers$dropoff_latitude, xlab="longitude", ylab="latitude")
title("Dropoff place for outliers")


# Remove the outliers from the train & test data set
# Plot the ne<ly selected values
train <- train[
  !(train$z_vals >1.96 | train$pickup_longitude < -74.05 | train$pickup_latitude >41 |
           train$pickup_longitude > -73.7 | train$pickup_latitude < 40.6 | train$z_vals < -1.96),
]

par(bg = 'black', col.lab="white", col.axis="white", col.main="white", col.sub="white")
plot(x=train$pickup_longitude, y=train$pickup_latitude,
               title("NYC map curated train data"), xlab='Longitude', ylab='Latitude', pch=18, cex=0.3,
               xlim = c(min(train$pickup_longitude), max(train$pickup_longitude)),
               ylim = c(min(train$pickup_latitude), max(train$pickup_latitude)),
               col =color.gradient(log(train$trip_duration))
     )

test <- test[
  !(test$pickup_longitude < -74.05 | test$pickup_latitude >41 |
      test$pickup_longitude > -73.7 | test$pickup_latitude < 40.6),
  ]

par(bg = 'black', col.lab="white", col.axis="white", col.main="white", col.sub="white")
plot(x=test$pickup_longitude, y=test$pickup_latitude,
     title("NYC map curated test data"), xlab='Longitude', ylab='Latitude', pch=18, cex=0.3,
     xlim = c(min(test$pickup_longitude), max(test$pickup_longitude)),
     ylim = c(min(test$pickup_latitude), max(test$pickup_latitude)),
     col ="white")

subtrain <- train[1:10000,]
subtest <- test[1:10000,]

#Prediction Part

X <- subset( subtrain, select = -c(id, dropoff_datetime ) )


X$store_and_fwd_flag <- as.character(X$store_and_fwd_flag)
X[X=="N"]<-0
X[X=="Y"]<-1

X$year <- substr(as.character(X$pickup_datetime), 0, 4)
X$year <- as.integer(X$year)

X$month <- substr(as.character(X$pickup_datetime), 6, 7)
X$month <- as.integer(X$month)

X$day <- substr(as.character(X$pickup_datetime), 9, 10)
X$day <- as.integer(X$day)

X$pickup_datetime <- as.Date.character(X$pickup_datetime)
X$id <- NULL
X$year <- NULL
X$z_vals <- NULL


fitControl <- trainControl(
  ## Repeated 5–fold CV 
  method = "repeatedcv",
  number = 1,
  ## repeated 10 times
  repeats = 1,
  verboseIter = TRUE,
  returnResamp = "all")


rrfFit <- train(log(trip_duration) ~ vendor_id+passenger_count+pickup_longitude+
                  pickup_latitude+dropoff_longitude+dropoff_latitude+store_and_fwd_flag+
                  month+day+weekday+distances+maximum.temperature+minimum.temperature+
                  average.temperature+precipitation+snow.fall+snow.depth, 
                data = X,
                method = 'ranger',
                # should be set high at least p/3
                tuneLength = 10, 
                trControl = fitControl,
                ## parameters passed onto the ranger function
                # the bigger the better.
                num.trees = 100,
                importance = "permutation")

print(max(rrfFit$results$Rsquared))

preds <- predict(object=rrfFit, newdata = X)

plt_y <- hist(log(X$trip_duration), breaks = 100)
pltpreds <- hist(preds, breaks = 100)

plot(plt_y, col=rgb(1,0,0,1))
plot(pltpreds, col=rgb(0,0,1,1), add=T)

#install.packages("scatterplot3d")
library(scatterplot3d)
s3d <- scatterplot3d(X$passenger_count,X$day,log(X$trip_duration), 
              pch=16, type="h", highlight.3d=TRUE,
              xlab = "Passenger count", ylab="Day in month", zlab = "log trip duration",
              main="3D plot of evolution of trip duration\n following the weekday and number of passenger")


fit <- lm(log(X$trip_duration) ~ X$passenger_count+X$weekday)
s3d$plane3d(fit)

