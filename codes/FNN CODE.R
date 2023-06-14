install.packages("readxl")
install.packages("psych")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("tseries")
install.packages("timetk")
install.packages("caret")
install.packages("MASS")
install.packages("sets")
install.packages('FuzzyNumbers')
install_github('FuzzyNumbers', 'gagolews')

library(sets)
library(psych)
library(ggplot2)
library(gridExtra)
library(tseries)
library(timetk)
library(rlang)
library(caret)
library(readxl)
library(MASS)
library(lubridate)
library(magrittr)
library(rstatix)
library(base)
library(olsrr)
require(ggplot2)
library(caret)
library(ehaGoF)
library(Metrics)
library(NeuralNetTools)

####>>>>>>>>>>>>>>>>>>>>>>>>Loading data>>>>>>>>>>
### Fuzification of ANN
#setting the model based on number of weeks in an year
sets_options("universe", seq(0, 52, 1))
variables <- set(
  time = fuzzy_partition(varnames = c(period1 = 0, period2 = 11, period3 = 22, period4 = 35, period5 = 47),
                         sd = 2.0),
  pp = fuzzy_partition(varnames = c(dormant = 0, flowering_fruitset = 11, fruitset_RE = 22, fruit_development = 35, harvest = 47),
                       FUN = fuzzy_cone, radius = 6)
)

# Fuzzy rules
rules <- set(
  fuzzy_rule(time %is% period1, pp %is% dormant),
  fuzzy_rule(time %is% period2, pp %is% flowering_fruitset),
  fuzzy_rule(time %is% period3, pp %is% fruitset_RE),
  fuzzy_rule(time %is% period4, pp %is% fruit_development),
  fuzzy_rule(time %is% period5, pp %is% harvest)
)

model <- fuzzy_system(variables, rules)
print(model)
plot(model)
summary(model)

# generalising fuzzyfication to 172 weeks and using the values obtained to create fuzzy variable
# numbers represents the weeks at which each stage of plant physiology lasted
#install.packages("FuzzyNumbers")
library(FuzzyNumbers)
dormant <- TrapezoidalFuzzyNumber(1, 4, 4, 7)
flowering.fruitset <- TrapezoidalFuzzyNumber(8, 10.5, 10.5, 13)
fruitset.rapidexpansion <- TrapezoidalFuzzyNumber(14, 22.5, 22.5, 31)
fruitdevelopment <- TrapezoidalFuzzyNumber(32, 35.5, 35.5, 39)
harvest <- TrapezoidalFuzzyNumber(40, 46.5, 46.5, 53)
dormant1 <- TrapezoidalFuzzyNumber(54, 56.5, 56.5, 59)
flowering.fruitset1 <- TrapezoidalFuzzyNumber(60, 63, 63, 66)
fruitset.rapidexpansion1 <- TrapezoidalFuzzyNumber(67, 75, 75, 83)
fruitdevelopment1 <- TrapezoidalFuzzyNumber(84, 88, 88, 92)
harvest1 <- TrapezoidalFuzzyNumber(93, 99, 99, 105)
dormant2 <- TrapezoidalFuzzyNumber(106, 108.5, 108.5, 111)
flowering.fruitset2 <- TrapezoidalFuzzyNumber(112, 115, 115, 118)
fruitset.rapidexpansion2 <- TrapezoidalFuzzyNumber(119, 127, 127, 135)
fruitdevelopment2 <- TrapezoidalFuzzyNumber(136, 140, 140, 144)
harvest2 <- TrapezoidalFuzzyNumber(145, 151, 151, 157)
dormant3 <- TrapezoidalFuzzyNumber(158, 161, 161, 164)
flowering.fruitset3 <- TrapezoidalFuzzyNumber(165, 167.5, 167.5, 170)
fruitset.rapidexpansion3 <- TrapezoidalFuzzyNumber(171, 179, 179, 188)

evaluate(dormant, seq(1, 7, by=1))
evaluate(flowering.fruitset, seq(8, 13, by=1))
evaluate(fruitset.rapidexpansion, seq(14, 31, by=1))
evaluate(fruitdevelopment, seq(32, 39, by=1))
evaluate(harvest, seq(40, 53, by=1))
evaluate(dormant1, seq(54, 59, by=1))
evaluate(flowering.fruitset1, seq(60, 66, by=1))
evaluate(fruitset.rapidexpansion1, seq(67, 83, by=1))
evaluate(fruitdevelopment1, seq(84, 92, by=1))
evaluate(harvest1, seq(93, 105, by=1))
evaluate(dormant2, seq(106, 111, by=1))
evaluate(flowering.fruitset2, seq(112, 118, by=1))
evaluate(fruitset.rapidexpansion2, seq(119, 135, by=1))
evaluate(fruitdevelopment2, seq(136, 144, by=1))
evaluate(harvest2, seq(145, 157, by=1))
evaluate(dormant3, seq(158, 164, by=1))
evaluate(flowering.fruitset3, seq(165, 170, by=1))
evaluate(fruitset.rapidexpansion3, seq(171, 188, by=1))


par(mfrow=c(1,1))
plot(dormant, xlim=c(1,173),ylim=c(0,1.7), lwd = 2, main = "Membership grades of the fuzzified avocado plant physiology stages", ylab = "Membership  grade", xlab = "Time (weeks)")
plot(flowering.fruitset, add=TRUE, col=2, lty=1, lwd = 2)
plot(fruitset.rapidexpansion, add=TRUE, col=3, lty=1, lwd = 2)
plot(fruitdevelopment, add=TRUE, col=4, lty=1, lwd = 2)
plot(harvest, add=TRUE, col=5, lty=1, lwd = 2)
plot(dormant1, add=TRUE, col=1, lty=1, lwd = 2)
plot(flowering.fruitset1, add=TRUE, col=2, lty=1, lwd = 2)
plot(fruitset.rapidexpansion1, add=TRUE, col=3, lty=1, lwd = 2)
plot(fruitdevelopment1, add=TRUE, col=4, lty=1, lwd = 2)
plot(harvest1, add=TRUE, col=5, lty=1, lwd = 2)
plot(dormant2, add=TRUE, col=1, lty=1, lwd = 2)
plot(flowering.fruitset2, add=TRUE, col=2, lty=1, lwd = 2)
plot(fruitset.rapidexpansion2, add=TRUE, col=3, lty=1, lwd = 2)
plot(fruitdevelopment2, add=TRUE, col=4, lty=1, lwd = 2)
plot(harvest2, add=TRUE, col=5, lty=1, lwd = 2)
plot(dormant3, add=TRUE, col=1, lty=1, lwd = 2)
plot(flowering.fruitset3, add=TRUE, col=2, lty=1, lwd = 2)
plot(fruitset.rapidexpansion3, add=TRUE, col=3, lty=1, lwd = 2)
legend("topright", bty = "n", title="Avocado plant physiology stages",
       c("Dormant stage","Flowering and fruitset",
         "Fruitset and rapid expansion ","Fruit development",
         "Harvest"),
       fill=c("black","red",
              "green","blue",
              "cyan"))


# Importinng dataset with fuzzified variable
setwd("D:\\Eric\\Manuscripts")
kakuzi.data<-read_excel("dataset_insectpests.xlsx")
attach(kakuzi.data)
kakuzi.data[["pphysiology"]] = as.numeric(kakuzi.data[["pphysiology"]])

#.................. Functions................................
## Defining the function LAG: to lag the pest counts by one week 
LAG <- function(x,k){n = length(x)
xx=x
xx[1:(n-k)]=x[(k+1):n]
xx[(n-k+1):n]=NA
xx}

lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)]} #

# creating lagged variable for pest counts 
#########
fccj.yt_1<-lagpad(kakuzi.data$Jambo_Fruitfly_cc, 1)
fccnde.yt_1<-lagpad(kakuzi.data$Ndera_Fruitfly_cc, 1)
fbij.yt_1<-lagpad(kakuzi.data$Jambo_Fruitfly_bi, 1)
fbinde.yt_1<-lagpad(kakuzi.data$Ndera_Fruitfly_bi, 1)

# dataset for creating model on fruitfly (ceratitis species) in orchard A
jambo11.fcc <- cbind(kakuzi.data$Jambo_Fruitfly_cc, 
                     fccj.yt_1, 
                     kakuzi.data$pphysiology_fuzzy,
                     kakuzi.data$rainfall, 
                     kakuzi.data$ave.temp,
                     kakuzi.data$r_humidity)
colnames(jambo11.fcc) <- c("fcc.jambo", "fccjambo.yt_1",
                           "pphysiology",
                           "rainfall",
                           "average.temp", 
                           "relative.humidity")

jambo22.fcc <- data.frame(jambo11.fcc)
jambo33.fcc  <- na.omit(jambo22.fcc)

# partitioning data into training and test set
set.seed(2018) #
ind.fccj <-sample(2, nrow(jambo33.fcc), replace = T, prob = c(.7, .3))
training1.fccj <- jambo33.fcc[ind.fccj==1, 2:6]
test1.fccj <- jambo33.fcc[ind.fccj==2, 2:6]
trainingtarget1.fccj <- jambo33.fcc[ind.fccj==1, 1]
testtarget1.fccj <- jambo33.fcc[ind.fccj==2, 1]
training2.fccj <- as.matrix(trainingtarget1.fccj)
test2.fccj <- as.matrix(testtarget1.fccj)

# Normalizing the data 
maxtraining.fccj <- apply(training1.fccj, 2, max) 
maxtrainingtarget.fccj <- max(trainingtarget1.fccj)
mintraining.fccj <- apply(training1.fccj, 2, min) 
mintrainingtarget.fccj <- min(trainingtarget1.fccj)

maxtest.fccj <- apply(test1.fccj, 2, max) 
maxtesttarget.fccj <- max(testtarget1.fccj)

mintest.fccj <- apply(test1.fccj, 2, min) 
mintesttarget.fccj <- min(testtarget1.fccj)

training.fccj <- as.matrix(scale(training1.fccj, center = mintraining.fccj, scale = maxtraining.fccj - mintraining.fccj))
test.fccj <- as.matrix(scale(test1.fccj, center = mintest.fccj, scale = maxtest.fccj - mintest.fccj))

trainingtarget.fccj <- as.matrix(scale(trainingtarget1.fccj, center = mintrainingtarget.fccj, scale = maxtrainingtarget.fccj - mintrainingtarget.fccj))
testtarget.fccj <- as.matrix(scale(testtarget1.fccj, center = mintrainingtarget.fccj, scale = maxtrainingtarget.fccj - mintrainingtarget.fccj))
names(trainingtarget.fccj)[names(trainingtarget.fccj) == "V1"] <- "fcc.jambo"
names(testtarget.fccj)[names(testtarget.fccj) == "V1"] <- "fcc.jambo"

# the model
library(ANN2)
nn1fccj <- neuralnetwork(X = training.fccj,
                         y = trainingtarget.fccj, 
                         standardize = TRUE, 
                         hidden.layers = c(5),
                         regression = TRUE,
                         loss.type = "squared",
                         activ.functions = "sigmoid",
                         optim.type = 'adam', 
                         learn.rates = 0.012, 
                         n.epochs = 2000, 
                         val.prop = 0.2, 
                         verbose = TRUE,
                         random.seed = 2018)

pr.nn1fccj <- predict(nn1fccj, test.fccj)
pr.nn1_Tfccj <- predict(nn1fccj, training.fccj)
pr.nn_1fccj <- (pr.nn1fccj$predictions)*(max(jambo33.fcc$fcc.jambo)- min(jambo33.fcc$fcc.jambo))+min(jambo33.fcc$fcc.jambo)
pr.nn_1_Tfccj <- (pr.nn1_Tfccj$predictions)*(max(jambo33.fcc$fcc.jambo)-min(jambo33.fcc$fcc.jambo))+min(jambo33.fcc$fcc.jambo)
test.r1fccj <- (testtarget.fccj)*(max(jambo33.fcc$fcc.jambo)-min(jambo33.fcc$fcc.jambo))+min(jambo33.fcc$fcc.jambo)
train.r1fccj <- (trainingtarget.fccj)*(max(jambo33.fcc$fcc.jambo)-min(jambo33.fcc$fcc.jambo))+min(jambo33.fcc$fcc.jambo)
GoF(train.r1fccj, pr.nn_1_Tfccj)
GoF(test.r1fccj, pr.nn_1fccj)
nn1fccj$Rcpp_ANN$getParams()

# dataset for creating model on fruitfly (B. dorsalis) in orchard A
jambo11.fbi <- cbind(kakuzi.data$Jambo_Fruitfly_bi, 
                     fbij.yt_1, 
                     kakuzi.data$pphysiology_fuzzy,
                     kakuzi.data$rainfall, 
                     kakuzi.data$ave.temp,
                     kakuzi.data$r_humidity)
colnames(jambo11.fbi) <- c("fbi.jambo", "fbijambo.yt_1",
                           "pphysiology",
                           "rainfall",
                           "average.temp", 
                           "relative.humidity")

jambo22.fbi <- data.frame(jambo11.fbi)
jambo33.fbi  <- na.omit(jambo22.fbi)

# partitioning data into training and test set
set.seed(1002) # 
ind.fbij <-sample(2, nrow(jambo33.fbi), replace = T, prob = c(.7, .3))
training1.fbij <- jambo33.fbi[ind.fbij==1, 2:6]
test1.fbij <- jambo33.fbi[ind.fbij==2, 2:6]
trainingtarget1.fbij <- jambo33.fbi[ind.fbij==1, 1]
testtarget1.fbij <- jambo33.fbi[ind.fbij==2, 1]
training2.fbij <- as.matrix(trainingtarget1.fbij)
test2.fbij <- as.matrix(testtarget1.fbij)
# Normalize 
maxtraining.fbij <- apply(training1.fbij, 2, max) 
maxtrainingtarget.fbij <- max(trainingtarget1.fbij)
mintraining.fbij <- apply(training1.fbij, 2, min) 
mintrainingtarget.fbij <- min(trainingtarget1.fbij)

maxtest.fbij <- apply(test1.fbij, 2, max) 
maxtesttarget.fbij <- max(testtarget1.fbij)
mintest.fbij <- apply(test1.fbij, 2, min) 
mintesttarget.fbij <- min(testtarget1.fbij)
training.fbij <- as.matrix(scale(training1.fbij, center = mintraining.fbij, scale = maxtraining.fbij - mintraining.fbij))
test.fbij <- as.matrix(scale(test1.fbij, center = mintest.fbij, scale = maxtest.fbij - mintest.fbij))
trainingtarget.fbij <- as.matrix(scale(trainingtarget1.fbij, center = mintrainingtarget.fbij, scale = maxtrainingtarget.fbij - mintrainingtarget.fbij))
testtarget.fbij <- as.matrix(scale(testtarget1.fbij, center = mintrainingtarget.fbij, scale = maxtrainingtarget.fbij - mintrainingtarget.fbij))
names(trainingtarget.fbij)[names(trainingtarget.fbij) == "V1"] <- "fbi.jambo"
names(testtarget.fbij)[names(testtarget.fbij) == "V1"] <- "fbi.jambo"

# the model
library(ANN2)
nn1fbij <- neuralnetwork(X = training.fbij,
                         y = trainingtarget.fbij, 
                         hidden.layers = c(6),
                         regression = TRUE,
                         loss.type = "squared",
                         activ.functions = "sigmoid",
                         optim.type = 'rmsprop', 
                         learn.rates = 0.006, 
                         n.epochs = 2000, 
                         val.prop = 0.2, 
                         verbose = TRUE,
                         random.seed = 1002)

pr.nn1fbij <- predict(nn1fbij, test.fbij)
pr.nn1_Tfbij <- predict(nn1fbij, training.fbij)
pr.nn_1fbij <- (pr.nn1fbij$predictions)*(max(jambo33.fbi$fbi.jambo)- min(jambo33.fbi$fbi.jambo))+min(jambo33.fbi$fbi.jambo)
pr.nn_1_Tfbij <- (pr.nn1_Tfbij$predictions)*(max(jambo33.fbi$fbi.jambo)-min(jambo33.fbi$fbi.jambo))+min(jambo33.fbi$fbi.jambo)
test.r1fbij <- (testtarget.fbij)*(max(jambo33.fbi$fbi.jambo)-min(jambo33.fbi$fbi.jambo))+min(jambo33.fbi$fbi.jambo)
train.r1fbij <- (trainingtarget.fbij)*(max(jambo33.fbi$fbi.jambo)-min(jambo33.fbi$fbi.jambo))+min(jambo33.fbi$fbi.jambo)
GoF(train.r1fbij, pr.nn_1_Tfbij)
GoF(test.r1fbij, pr.nn_1fbij)
nn1fbij$Rcpp_ANN$getParams()

# dataset for creating model on fruitfly (ceratitis species) in orchard B
ndera11.fcc <- cbind(kakuzi.data$Ndera_Fruitfly_cc, 
                     fccnde.yt_1, 
                     kakuzi.data$pphysiology_fuzzy,
                     kakuzi.data$rainfall, 
                     kakuzi.data$ave.temp,
                     kakuzi.data$r_humidity)
colnames(ndera11.fcc) <- c("fcc.ndera", "fccndera.yt_1",
                           "pphysiology",
                           "rainfall",
                           "average.temp", 
                           "relative.humidity")

ndera22.fcc <- data.frame(ndera11.fcc)
ndera33.fcc  <- na.omit(ndera22.fcc)

# partitioning data into training and test set
set.seed(4046) 
ind.fccnde <-sample(2, nrow(ndera33.fcc), replace = T, prob = c(.7, .3))
training1.fccnde <- ndera33.fcc[ind.fccnde==1, 2:6]
test1.fccnde <- ndera33.fcc[ind.fccnde==2, 2:6]
trainingtarget1.fccnde <- ndera33.fcc[ind.fccnde==1, 1]
testtarget1.fccnde <- ndera33.fcc[ind.fccnde==2, 1]
training2.fccnde <- as.matrix(trainingtarget1.fccnde)
test2.fccnde <- as.matrix(testtarget1.fccnde)
# Normalize 
maxtraining.fccnde <- apply(training1.fccnde, 2, max) 
maxtrainingtarget.fccnde <- max(trainingtarget1.fccnde)
mintraining.fccnde <- apply(training1.fccnde, 2, min) 
mintrainingtarget.fccnde <- min(trainingtarget1.fccnde)

maxtest.fccnde <- apply(test1.fccnde, 2, max) 
maxtesttarget.fccnde <- max(testtarget1.fccnde)
mintest.fccnde <- apply(test1.fccnde, 2, min) 
mintesttarget.fccnde <- min(testtarget1.fccnde)
training.fccnde <- as.matrix(scale(training1.fccnde, center = mintraining.fccnde, scale = maxtraining.fccnde - mintraining.fccnde))
test.fccnde <- as.matrix(scale(test1.fccnde, center = mintest.fccnde, scale = maxtest.fccnde - mintest.fccnde))
trainingtarget.fccnde <- as.matrix(scale(trainingtarget1.fccnde, center = mintrainingtarget.fccnde, scale = maxtrainingtarget.fccnde - mintrainingtarget.fccnde))
testtarget.fccnde <- as.matrix(scale(testtarget1.fccnde, center = mintrainingtarget.fccnde, scale = maxtrainingtarget.fccnde - mintrainingtarget.fccnde))
names(trainingtarget.fccnde)[names(trainingtarget.fccnde) == "V1"] <- "fcc.ndera"
names(testtarget.fccnde)[names(testtarget.fccnde) == "V1"] <- "fcc.ndera"

# the model
library(ANN2)
nn1fccnde <- neuralnetwork(X = training.fccnde,
                           y = trainingtarget.fccnde, 
                           hidden.layers = c(5), 
                           standardize = TRUE,
                           regression = TRUE,
                           loss.type = "squared",
                           activ.functions = "sigmoid",
                           optim.type = 'rmsprop', 
                           learn.rates = 0.002, 
                           n.epochs = 2700,  
                           val.prop = 0.2, 
                           verbose = TRUE,
                           random.seed = 4046)

pr.nn1fccnde <- predict(nn1fccnde, test.fccnde)
pr.nn1_Tfccnde <- predict(nn1fccnde, training.fccnde)
pr.nn_1fccnde <- (pr.nn1fccnde$predictions)*(max(ndera33.fcc$fcc.ndera)- min(ndera33.fcc$fcc.ndera))+min(ndera33.fcc$fcc.ndera)
pr.nn_1_Tfccnde <- (pr.nn1_Tfccnde$predictions)*(max(ndera33.fcc$fcc.ndera)-min(ndera33.fcc$fcc.ndera))+min(ndera33.fcc$fcc.ndera)
test.r1fccnde <- (testtarget.fccnde)*(max(ndera33.fcc$fcc.ndera)-min(ndera33.fcc$fcc.ndera))+min(ndera33.fcc$fcc.ndera)
train.r1fccnde <- (trainingtarget.fccnde)*(max(ndera33.fcc$fcc.ndera)-min(ndera33.fcc$fcc.ndera))+min(ndera33.fcc$fcc.ndera)
GoF(train.r1fccnde, pr.nn_1_Tfccnde)
GoF(test.r1fccnde, pr.nn_1fccnde)
nn1fccnde$Rcpp_ANN$getParams()

# dataset for creating model on fruitfly (B. dorsalis) in orchard B
ndera11.fbi <- cbind(kakuzi.data$Ndera_Fruitfly_bi, 
                     fbinde.yt_1, 
                     kakuzi.data$pphysiology_fuzzy,
                     kakuzi.data$rainfall, 
                     kakuzi.data$ave.temp,
                     kakuzi.data$r_humidity)
colnames(ndera11.fbi) <- c("fbi.ndera", "fbindera.yt_1",
                           "pphysiology",
                           "rainfall",
                           "average.temp", 
                           "relative.humidity")

ndera22.fbi <- data.frame(ndera11.fbi)
ndera33.fbi  <- na.omit(ndera22.fbi)
str(ndera33.fbi )

# partitioning data into training and test set
set.seed(3029)
ind.fbinde <-sample(2, nrow(ndera33.fbi), replace = T, prob = c(.7, .3))
training1.fbinde <- ndera33.fbi[ind.fbinde==1, 2:6]
test1.fbinde <- ndera33.fbi[ind.fbinde==2, 2:6]
trainingtarget1.fbinde <- ndera33.fbi[ind.fbinde==1, 1]
testtarget1.fbinde <- ndera33.fbi[ind.fbinde==2, 1]
training2.fbinde <- as.matrix(trainingtarget1.fbinde)
test2.fbinde <- as.matrix(testtarget1.fbinde)
# Normalize 
maxtraining.fbinde <- apply(training1.fbinde, 2, max) 
maxtrainingtarget.fbinde <- max(trainingtarget1.fbinde)
mintraining.fbinde <- apply(training1.fbinde, 2, min) 
mintrainingtarget.fbinde <- min(trainingtarget1.fbinde)
maxtest.fbinde <- apply(test1.fbinde, 2, max) 
maxtesttarget.fbinde <- max(testtarget1.fbinde)
mintest.fbinde <- apply(test1.fbinde, 2, min) 
mintesttarget.fbinde <- min(testtarget1.fbinde)
training.fbinde <- as.matrix(scale(training1.fbinde, center = mintraining.fbinde, scale = maxtraining.fbinde - mintraining.fbinde))
test.fbinde <- as.matrix(scale(test1.fbinde, center = mintest.fbinde, scale = maxtest.fbinde - mintest.fbinde))
trainingtarget.fbinde <- as.matrix(scale(trainingtarget1.fbinde, center = mintrainingtarget.fbinde, scale = maxtrainingtarget.fbinde - mintrainingtarget.fbinde))
testtarget.fbinde <- as.matrix(scale(testtarget1.fbinde, center = mintrainingtarget.fbinde, scale = maxtrainingtarget.fbinde - mintrainingtarget.fbinde))
names(trainingtarget.fbinde)[names(trainingtarget.fbinde) == "V1"] <- "fbi.ndera"
names(testtarget.fbinde)[names(testtarget.fbinde) == "V1"] <- "fbi.ndera"

# the model
library(ANN2)
nn1fbinde <- neuralnetwork(X = training.fbinde,
                           y = trainingtarget.fbinde, 
                           hidden.layers = c(7),
                           regression = TRUE,
                           loss.type = "squared",
                           activ.functions = "sigmoid",
                           optim.type = 'rmsprop', 
                           rmsprop.decay = 0.999,
                           learn.rates = 0.01, 
                           n.epochs = 55000,  
                           val.prop = 0.2, 
                           verbose = TRUE,
                           random.seed = 3029)

print(nn1fbinde)
pr.nn1fbinde <- predict(nn1fbinde, test.fbinde)
pr.nn1_Tfbinde <- predict(nn1fbinde, training.fbinde)
pr.nn_1fbinde <- (pr.nn1fbinde$predictions)*(max(ndera33.fbi$fbi.ndera)- min(ndera33.fbi$fbi.ndera))+min(ndera33.fbi$fbi.ndera)
pr.nn_1_Tfbinde <- (pr.nn1_Tfbinde$predictions)*(max(ndera33.fbi$fbi.ndera)-min(ndera33.fbi$fbi.ndera))+min(ndera33.fbi$fbi.ndera)
test.r1fbinde <- (testtarget.fbinde)*(max(ndera33.fbi$fbi.ndera)-min(ndera33.fbi$fbi.ndera))+min(ndera33.fbi$fbi.ndera)
train.r1fbinde <- (trainingtarget.fbinde)*(max(ndera33.fbi$fbi.ndera)-min(ndera33.fbi$fbi.ndera))+min(ndera33.fbi$fbi.ndera)
GoF(train.r1fbinde, pr.nn_1_Tfbinde)
GoF(test.r1fbinde, pr.nn_1fbinde)
nn1fbinde$Rcpp_ANN$getParams()

# plots
# Calculating RMSE using rmse() 
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
RSq.fcc.j <- gofRSq(test.r1fccj, pr.nn_1fccj, dgt = 3); RSq.fcc.j
plot(test.r1fccj, round(pr.nn_1fccj, digits = 0), col='red', 
     main='Actual verses predicted population density of Ceratitis spp.', 
     ylab = "Predicted population density of Ceratitis spp.", 
     xlab = "Actual population density of Ceratitis spp.", 
     pch=18, cex=0.7, cex.main=0.8)
abline(0,1,lwd=2)
legend("topleft", "predicted", fill = "red")
mylabel = bquote(italic(R)^2 == .(format(RSq.fcc.j)))
text(x = 300, y = 2000, labels = mylabel)

plot(test.r1fccj, type="l", col="blue", lwd = 2,
     main= 'Predicted and actual Ceratitis spp. population densities against time', 
     ylab = " Population density of Ceratitis spp.", 
     xlab = "Time (weeks)", pch=18, cex=0.7, cex.main=0.8, ylim = c(-10,3500))
lines(pr.nn_1fccj, col="red",lty=1, lwd = 2)
legend("topleft",
       c("Actual","Predicted"),
       fill=c("blue","red"))
mtext('Orchard A', outer = TRUE, cex = 1.2)
par(mfrow=c(1,1))


# Calculating RMSE using rmse() 
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
RSq.fcc.nde <- gofRSq(test.r1fccnde, pr.nn_1fccnde, dgt = 3); RSq.fcc.nde
plot(test.r1fccnde, round(pr.nn_1fccnde, digits = 0), col='red', lwd = 2,
     main = 'Actual verses predicted population density of Ceratitis spp.', 
     ylab = "Predicted population density of Ceratitis spp.", 
     xlab = "Actual population density of Ceratitis spp.", 
     pch=18, cex=0.7, cex.main=0.8)
abline(0, 1,lwd=2)
legend("topleft", "predicted", fill = "red")
mylabel = bquote(italic(R)^2 == .(format(RSq.fcc.nde)))
text(x = 400, y = 1300, labels = mylabel)

plot(test.r1fccnde, type="l", col="blue", lwd = 2,
     main= 'Predicted and actual Ceratitis spp. population densities against time', 
     ylab = "Population density of Ceratitis spp.", 
     xlab = "Time (weeks)", pch=18, cex=0.7, cex.main=0.8, ylim = c(-10,3500))
lines(pr.nn_1fccnde, col="red",lty=1, lwd = 2,)
legend("topleft",
       c("Actual","Predicted"),
       fill=c("blue","red"))
mtext('Orchard B', outer = TRUE, cex = 1.2)
par(mfrow=c(1,1))


# Calculating RMSE using rmse() 
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
RSq.fbi.j <- gofRSq(test.r1fbij, pr.nn_1fbij, dgt = 3); RSq.fbi.j
# Calculating RMSE using rmse() 
plot(test.r1fbij, round(pr.nn_1fbij, digits = 0), col='red', 
     main='Actual verses predicted population density of B. dorsalis', 
     ylab = "Predicted population density of B. dorsalis", 
     xlab = "Actual population density of B. dorsalis", 
     pch=18, cex=0.7, cex.main=0.8)
abline(0,1,lwd=2)
legend("topleft", "predicted", fill = "red")
mylabel = bquote(italic(R)^2 == .(format(RSq.fbi.j)))
text(x = 50, y = 400, labels = mylabel)

plot(test.r1fbij, type="l", col="blue", lwd = 2,
     main='Predicted and actual B. dorsalis. population densities against time', 
     ylab = " Population density of B. dorsalis", 
     xlab = "Time (weeks)", pch=18, cex=0.7, cex.main=0.8, ylim = c(10,600))
lines(pr.nn_1fbij, col="red",lty=1, lwd = 2)
legend("topleft",
       c("Actual","Predicted"),
       fill=c("blue","red"))
mtext('Orchard A', outer = TRUE, cex = 1.2)
par(mfrow=c(1,1))


# Calculating RMSE using rmse() 
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
# Calculating RMSE using rmse() 
RSq.fbi.nde <- gofRSq(test.r1fbinde, pr.nn_1fbinde, dgt = 3); RSq.fbi.nde
plot(test.r1fbinde, round(pr.nn_1fbinde, digits = 0), col='red', 
     main='Actual verses predicted population density of B. dorsalis', 
     ylab = "Predicted population density of B. dorsalis", 
     xlab = "Actual population density of B. dorsalis", 
     pch=18, cex=0.7, cex.main=0.8)
abline(0,1,lwd=2)
legend("topleft", "predicted", fill = "red")
mylabel = bquote(italic(R)^2 == .(format(RSq.fbi.nde)))
text(x = 50, y = 150, labels = mylabel)

plot(test.r1fbinde, type="l", col="blue", lwd = 2,
     main='Predicted and actual B. dorsalis. population densities against time', 
     ylab = " Population density of B. dorsalis", 
     xlab = "Time (weeks)", pch=18, cex=0.7, cex.main=0.8, ylim = c(-70,500))
lines(pr.nn_1fbinde, col="red",lty=1, lwd = 2)
legend("topleft",
       c("Actual","Predicted"),
       fill=c("blue","red"))
mtext('Orchard B', outer = TRUE, cex = 1.2)
par(mfrow=c(1,1))

# Residual analysis
library(car)
# Orchard A fccj
residuals.fccj <- subtract(test.r1fccj, pr.nn_1fccj)
residuals.training.fccj <- subtract(train.r1fccj, pr.nn_1_Tfccj)
model.fccj <- as.vector(residuals.fccj)
model.training.fccj <- as.vector(residuals.training.fccj)
durbinWatsonTest(model.fccj)
durbinWatsonTest(model.training.fccj)

# Orchard B fccnde
residuals.fccnde <- subtract(test.r1fccnde, pr.nn_1fccnde)
residuals.training.fccnde <- subtract(train.r1fccnde, pr.nn_1_Tfccnde)
model.fccnde <- as.vector(residuals.fccnde)
model.training.fccnde <- as.vector(residuals.training.fccnde)
durbinWatsonTest(model.fccnde)
durbinWatsonTest(model.training.fccnde)

# Orchard A fbij
residuals.fbij <- subtract(test.r1fbij, pr.nn_1fbij)
residuals.training.fbij <- subtract(train.r1fbij, pr.nn_1_Tfbij)
model.fbij <- as.vector(residuals.fbij)
model.training.fbij <- as.vector(residuals.training.fbij)
durbinWatsonTest(model.fbij)
durbinWatsonTest(model.training.fbij)

# Orchard B fbinde
residuals.fbinde <- subtract(test.r1fbinde, pr.nn_1fbinde)
residuals.training.fbinde <- subtract(train.r1fbinde, pr.nn_1_Tfbinde)
model.fbinde <- as.vector(residuals.fbinde)
model.training.fbinde <- as.vector(residuals.training.fbinde)
durbinWatsonTest(model.fbinde)
durbinWatsonTest(model.training.fbinde)

