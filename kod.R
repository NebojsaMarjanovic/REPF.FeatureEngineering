data<-read.csv("cukarica-fetch_from_05.05.2023.csv", stringsAsFactors = F, sep = "|")

data$price[data$price<10000]<-NA

sum(is.na(data$price))
sum(data$price == "", na.rm = T)
sum(data$price == "-", na.rm = T)
sum(data$price == "0", na.rm = T)
sum(data$price == " ", na.rm = T)

table(data$price)

apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "N/A", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "-", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == " ", na.rm = T))

# NA : price(1)
# "" : heatingType(32), furnished(187), registered(71)

data$furnished<-NULL   #jer ima previse nedostajucih vrednosti
data$registered<-NULL #isto

data$createdAt<-NULL  #ne treba nam


data<-data[complete.cases(data[,4]), ]

length(unique(data$heatingType))  #10
length(unique(data$placeTitle))   #16

#menjas nedostajuce vrednosti za heatingType sa najdominantnijom vrednoscu
sort(table(data$heatingType))

data$heatingType[data$heatingType==""]<-"district"

str(data)

#pretvaras sve u numericke
data$heatingType<-as.numeric(as.factor(data$heatingType))
data$lastFloor<-as.numeric(as.factor(data$lastFloor))
data$placeTitle<-as.numeric(as.factor(data$placeTitle))
data$m2<-as.numeric(data$m2)
data$elevator<-as.numeric(data$elevator)
data$price<-as.numeric(data$price)
data$redactedFloor<-as.numeric(data$redactedFloor)

# sad gledamo korelacije
library(corrplot)
matrica <- cor(data)
matrica[,4] # izabrali smo 4 kolonu/varijablu price
corrplot(matrica, method = "number", type = "upper", diag = FALSE)

#pravimo train i test data setove
library(caret)
set.seed(1010)
indexes <- createDataPartition(data$price, p = 0.8, list = FALSE)
train.data <- data[indexes, ]
test.data <- data[-indexes, ]

#pravimo model
lm1 <- lm(price ~ m2+roomCount+elevator+placeTitle, data = train.data)
summary(lm1)

library(car)
vif(lm1)
sort(sqrt(vif(lm1)))

lm1.pred <- predict(lm1, newdata = test.data)
head(lm1.pred)
head(test.data$price)




# RSS = Residual Sum of Squares
# TSS = Total Sum of Squares
# sve ovo ispod je u cheatsheetu !!!

# residual je razlika izmedju stvarne i predvidjene vrednosti,
# nju sumiramo i kvadriramo da bismo dobili RSS:
RSS <- sum((lm1.pred - test.data$price)^2)

# razlika izmedju vrednosti u testu i srednje vrednosti u trainu,
# nju sumiramo i kvadriramo da bismo dobili TSS
TSS <- sum((mean(train.data$price) - test.data$price)^2)

# ovo je formula za RSQUARED
rsquared <- 1 - RSS / TSS
rsquared
# ukupan objasnjeni varijabilitet je 36.79%


summary(lm1)
# uporedjujemo sa rsquared nad trainom i nad testom i vidimo 
# da je veca na trainu
# ukupan objasnjeni varijabilitet je 36.79%, a na trainu je 39.16%



graphics.off()
par(mfrow = c(1,1)) # da imamo samo 1 red i 1 kolonu za grafove
par(mfrow = c(2,2)) # da imamo 2 reda i 2 kolone za grafove
plot(lm1)


test.data$price_pred<-lm1.pred
library(ggplot2)
ggplot(test.data) +
  geom_density(aes(x = price, color = 'actual')) +
  geom_density(aes(x = price_pred, color = 'predicted'))


##########################################################

#pravimo model
lm2 <- lm(price ~., data = train.data)
summary(lm2)

library(car)
vif(lm2)
sort(sqrt(vif(lm2)))

lm3 <-lm(price ~ m2+elevator+roomCount+placeTitle, data=train.data)
summary(lm3)

library(car)
vif(lm3)
sort(sqrt(vif(lm3)))

graphics.off()
par(mfrow = c(1,1)) # da imamo samo 1 red i 1 kolonu za grafove
par(mfrow = c(2,2)) # da imamo 2 reda i 2 kolone za grafove
plot(lm3)


lm3.pred <- predict(lm3, newdata = test.data)
head(lm3.pred)
head(test.data$price)


# mozete da da koristite ovaj ggplot, ali nije neophodno
test.data$price <- lm3.pred




# RSS = Residual Sum of Squares
# TSS = Total Sum of Squares
# sve ovo ispod je u cheatsheetu !!!

# residual je razlika izmedju stvarne i predvidjene vrednosti,
# nju sumiramo i kvadriramo da bismo dobili RSS:
RSS <- sum((lm3.pred - test.data$price)^2)

# razlika izmedju vrednosti u testu i srednje vrednosti u trainu,
# nju sumiramo i kvadriramo da bismo dobili TSS
TSS <- sum((mean(train.data$price) - test.data$price)^2)

# ovo je formula za RSQUARED
rsquared <- 1 - RSS / TSS
rsquared
# ukupan objasnjeni varijabilitet je 36.79%


summary(lm3)
# uporedjujemo sa rsquared nad trainom i nad testom i vidimo 
# da je veca na trainu
# ukupan objasnjeni varijabilitet je 36.79%, a na trainu je 39.16%









