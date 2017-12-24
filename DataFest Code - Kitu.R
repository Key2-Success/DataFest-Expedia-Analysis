# setwd(wd)
library(dplyr)
library(ggplot2)
library(plyr)
library(knitr)
library(rworldmap)
library(car)
library(kmeans)

# mydata <- data.table::fread(file = "Kitu/College/Junior Year/Extracurriculars/DataFest/data.txt")
# dest <- data.table::fread("dest.txt")
# 
# smalldata <- sample_n(mydata, 50000)
bigdata <- sample_n(mydata, 1455588)
save(bigdata, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/bigdata.Rdata")
# save(smalldata, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/smalldata.Rdata")
  
load(file = "Kitu/College/Junior Year/Extracurriculars/DataFest/smalldata.Rdata")
dest <- data.table::fread("Kitu/College/Junior Year/Extracurriculars/DataFest/dest.txt")

# look at days...what time of the year booked...what time of year booked for...length between booking data
# and actual date...which days are popular...which locations are popular at what parts of the year and
# at what hotels...demographics of these people...how long is vacation...distance travelled in a year

# change from char to date
smalldata$date_time <- as.Date(smalldata$date_time)
smalldata$srch_ci <- as.Date(smalldata$srch_ci)
smalldata$hotel_country <- as.factor(smalldata$hotel_country)

# plot histogram of when booked
hist(x = smalldata$date_time, breaks = "months")

# plot histogram of when will check in
hist(x = smalldata$srch_ci, breaks = "months")

# ggplot histogram filled by destination and time
ggplot(data = smalldata, aes(srch_ci)) + geom_histogram(aes(fill = hotel_country)) + guides(fill = FALSE)

# table of destination 
table(smalldata$hotel_country)
sort(table(smalldata$hotel_country))

# saving data for user location country outside of USA and Canada
a <- c("UNITED STATES OF AMERICA", "CANADA")

# get data only on germany
b <- c("GERMANY")
u <- c("UNITED STATES OF AMERICA")
c <- c("CANADA")
usa <- mydata[mydata$user_location_country %in% u, ]
canada <- mydata[mydata$user_location_country %in% c, ]
germany <- mydata[mydata$user_location_country %in% b, ]
save(germany, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/germany.Rdata")
save(usa, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/usa.Rdata")
save(canada, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/canada.Rdata")

mydata2 <- mydata[!mydata$user_location_country %in% a , ]
save(mydata2, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/intl.Rdata")
write.csv(mydata2, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/intl.csv")

# changing into factor
mydata2$user_location_country <- as.factor(mydata2$user_location_country)
mydata2$user_id <- as.numeric(mydata2$user_id)

# sort by unique users in each user location country
sorted <- mydata %>% 
   group_by(., user_location_country) %>% 
   summarise(., users = n_distinct(user_id))

load(file = "Kitu/College/Junior Year/Extracurriculars/DataFest/sorted.Rdata")
sorted <- sorted2[ , 1:2]

# create map of user base...eventually do map of destination travel as well at popular destinations
data(sorted)
sPDF <- joinCountryData2Map(sorted, joinCode = "NAME", nameJoinColumn = "user_location_country")
mapDevice()
mapCountryData(sPDF, nameColumnToPlot = "users")

# change to numeric
sorted_test$populations <- as.numeric(sorted_test$populations)

# create new column for proportion
populations <- c(81410000, 127000000, 65140000, 9157000, 8380000, 31540000, 48230000, 60800000, 66810000, 3474000, 
                 4807850, 8281430, 1371000000, 50620000, 78665830, 46443994, 126958472, 10528391, 91508084, 3929141, 
                 67959359, 144096870, 23780000, 8611000, 7306000, 16940000, 1311000000, 31380000, 207800000, 17950000, 
                 16140000, 388019, 5851000, 54960000, 3892000, 2235000, 100700000, 16340000, 23381038, 10820000, 1360000, 31110000)

sorted_test <- sorted[1:42, ]
sorted_test <- cbind(populations, sorted_test)
sorted_test <- transform(sorted_test, percent = users/populations)
sorted_test <- sorted_test[order(-sorted_test$percent), ]
sorted_test$percent <- as.numeric(sorted_test$percent)
sorted_test$user_location_country <- as.factor(sorted_test$user_location_country)
save(sorted_test, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/sorted_42.Rdata")
save(sorted2, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/sorted2.Rdata")


sorted2$user_location_country <- as.factor(sorted2$user_location_country)
sorted2$users <- as.numeric(sorted2$users)
sorted2 <- sorted2[order(-sorted2$users), ]

# create heat map of proportion
# data(sorted_test)
mapDevice('x11')
sPDF <- joinCountryData2Map(sorted_test, joinCode = "NAME", nameJoinColumn = "user_location_country")
mapCountryData(sPDF, nameColumnToPlot = "percent")

avg_percent <- mean(sorted_test$percent)
sd_percent <- sd(sorted_test$percent)
sqrt_n <- sqrt(42)

for (i in 1:length(sorted_test$users))
{
  sorted_test[i, 5] <- (avg_percent - sorted_test[i, 4])/(sd_percent/sqrt_n)
}

mapCountryData(sPDF, nameColumnToPlot = "populations")

# correlation between population and user base
fit <- lm(populations~users, data = sorted_test)
plot(fit)
plot(x = sorted_test$users, y = sorted_test$populations)
abline(fit)
summary(fit)

# no germany
sorted_test_nogermany <- sorted_test[-c(1), ]
fit2 <- lm(populations~users, data = sorted_test_nogermany)
plot(fit2)
abline(lm(populations~users, data = sorted_test_nogermany))
plot(x = sorted_test_nogermany$users, y = sorted_test_nogermany$populaton)
summary(fit2) # r square is 0.467

# test percent and user base
fit3 <- lm(percent~users, data = sorted_test)
plot(fit3)
plot(x = sorted_test_nogermany$users, y = sorted_test_nogermany$percent)
abline(lm(percent~users, data = sorted_test))
summary(fit3) # r square is 0.06817

# model for germany
fitme <- glm(is_booking~is_mobile+is_package+srch_adults_cnt+srch_children_cnt
             +srch_rm_cnt+prop_is_branded+prop_starrating+distance_band+hist_price_band
             +popularity_band+cnt, family = "binomial", data = germany)
summary(fitme)

biggie <- glm(is_booking~is_mobile+is_package+srch_adults_cnt+srch_children_cnt
              +srch_rm_cnt+prop_is_branded+prop_starrating+distance_band+hist_price_band
              +popularity_band+cnt, family = "binomial", data = bigdata)
summary(biggie)

df_merge <- left_join(x = germany, y = dest, by = "srch_destination_id")

# converting to percent
for (i in 7:ncol(canada50_merged))
{
  for(j in 1:nrow(canada50_merged))
  {
    canada50_merged[j, i] <- (10^(canada50_merged[j, i]))*100
  }
}
save(merged2, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/merged_percent.Rdata")

germ_50 <- read.csv(file = "Kitu/College/Junior Year/Extracurriculars/DataFest/germanyTop50Destinations.csv")

load(file = "Kitu/College/Junior Year/Extracurriculars/DataFest/merged50.Rdata")

# left join germ_50 w dest
merged50 <- merge(germ_50, dest, by.x = "Destination.Search.ID", by.y = "srch_destination_id")

# knn
ofInterest <- merged2[c("popular_social_familyfriendly","popular_activity_excursions")]
km1 <- kmeans(ofInterest,3,nstart=100)
plot(ofInterest,col=(km1$cluster +1),pch=20, cex=2)
View(table(km1$cluster,ofInterest$srch_destination_id))



# stan subsets top 50 cities in canada

tb <- table(canada$srch_destination_id)
tb <- as.data.frame(tb)
tbTop50Canada <- subset.data.frame(tb, tb$Freq > 4710)

# stan subsets top 50 cities in USA
tb2 <- table(usa$srch_destination_id)
tb2 <- as.data.frame(tb2)
tbTop50USA <- subset.data.frame(tb2, tb2$Freq > 23500)

dest$srch_destination_id <- as.factor(dest$srch_destination_id)

usa50_merged <- left_join(tbTop50USA, dest, by = "srch_destination_id")
colnames(tbTop50USA) <- c("srch_destination_id", "freq")
colnames(tbTop50Canada) <- c("srch_destination_id", "freq")

canada50_merged <- left_join(tbTop50Canada, dest, by = "srch_destination_id")

# mohini turns up now

sumscan <- apply(X = canada50_merged[, 8:100], MARGIN = 2, FUN = sum)
sumsus <- apply(X = usa50_merged[, 8:100], MARGIN = 2, FUN = sum)
canada <- data.frame( popular = colnames(canada50_merged)[8:100], sum = sumscan)
youessayyy <- data.frame(popular = colnames(usa50_merged)[8:100], sum = sumsus)

save(canada, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/canada50.Rdata")
save(youessayyy, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/usa50.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/DataFest/germany50.Rdata")

merged2$srch_destination_name <- as.character(merged2$srch_destination_name)

# most popular cities hosting the events
whichcity <- function(x)
{
  a <- which(merged2$x == max(merged2$x))
  b <- merged2[a, 4]
  return (b)
}

whichcity(popular_activity_dining)

a <- which(merged2$popular_landscape_scenery == max(merged2$popular_landscape_scenery))
b <- merged2[a, 4]
b

  save(usa50_merged, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/usa50_merged.Rdata")
save(canada50_merged, file = "Kitu/College/Junior Year/Extracurriculars/DataFest/canada50_merged.Rdata")
