
#installing libraries
library(corrplot)
library(caret)
library(class)
library(hydroGOF)
library(purrr)
library(tidyr)
library(ggplot2)
library(gplots)
library(rpart)
library(rpart.plot)
library(FNN)
library(kknn)

setwd("R:/MSIS/Fall 2018/ISDS 574/PUBG/")
df_main <- read.csv("PUBG_dataset.csv", head = TRUE, stringsAsFactor = FALSE)
set.seed(1)
df <- df_main[sample(nrow(df_main), 100000), ]
dim(df_main)
dim(df)
colnames(df)
str(df)
summary(df)

df_bkp <- df
dim(df_bkp)
df <- df_bkp 

colnames(df)

# Removing categorical variable
df <- df[,-c(2,3,16)] 
dim(df)
head(df)
summary(df)


# Scatter Plots
df %>%
  gather(-winPlacePerc, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = winPlacePerc)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# Histogram

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Scatter Plot 

par(mfrow=c(2,2))
for(i in 1:4) {
  plot(df[,i],df$winPlacePerc, xlab = colnames(df)[i], ylab = colnames(df)[25])
}

par(mfrow=c(2,2))
for(i in 5:8) {
  plot(df[,i],df$winPlacePerc, xlab = colnames(df)[i], ylab = colnames(df)[25])
}

par(mfrow=c(2,2))
for(i in 9:12) {
  plot(df[,i],df$winPlacePerc, xlab = colnames(df)[i], ylab = colnames(df)[25])
}

par(mfrow=c(2,2))
for(i in 13:16) {
  plot(df[,i],df$winPlacePerc, xlab = colnames(df)[i], ylab = colnames(df)[25])
}

par(mfrow=c(2,2))
for(i in 17:20) {
  plot(df[,i],df$winPlacePerc, xlab = colnames(df)[i], ylab = colnames(df)[25])
}

par(mfrow=c(2,2))
for(i in 21:24) {
  plot(df[,i],df$winPlacePerc, xlab = colnames(df)[i], ylab = colnames(df)[25])
}

# Box plots

par(mfrow=c(2,2))
for(i in 1:4) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 5:8) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 9:12) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 13:16) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 17:20) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 21:24) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

# Data Cleaning

as <- which(df$assists > 1)
bo <- which(df$boosts >= 6)
Dma <- which(df$damageDealt >= 500)
DBno <- which(df$DBNOs >= 3)
hskill <- which(df$headshotKills >= 2)
hels <- which(df$heals >= 5)
kills_ind <- which(df$kills >= 4)
killstr <- which(df$killStreaks >= 3)
longestKill <- which(df$longestKill >= 53)
#df2 <- df2[-longestKill,]
#dim(df2)
matdur <- which(df$matchDuration <= 641)
maxplace <- which(df$maxPlace >= 80)
numgrp <-  which(df$maxPlace >= 80)
rankpoints <- which(df$rankPoints >= 3500)
revi <- which(df$revives >= 2)
ridedist <- which(df$rideDistance > 1)
roadKills <- which(df$roadKills > 2)
swimdist <- which(df$swimDistance > 1)
tmKill <- which(df$teamKills > 1)
vehdest <- which(df$vehicleDestroys > 1.0)
walkdist <- which(df$walkDistance >= 4735)
wepacc <- which(df$weaponsAcquired >= 10)

x <- unique(c(as,bo,Dma,DBno,hskill,hels,kills_ind,killstr,longestKill,matdur,maxplace,numgrp,rankpoints, revi, ridedist, roadKills, swimdist, tmKill, vehdest, walkdist, wepacc))

df_bkp2 <- df
names(df)

dim(df_bkp)

df3 <- df[-x,]

dim(df3)
names(df3)
length(x)
ids <- df3[,1]
head(ids)

dev.off()# to change the display back to normal

# correlation matrix
cormat<- cor(df3)

corrplot(cormat,method = 'number', title = 'Corealation Matrix', addCoefasPercent = TRUE, number.cex = 0.65)

# checking corelation with y 
cor(df$winPlacePerc, df$roadKills)
df3 <- df3[,-1]
df3 <- df3[,-18]
names(df3)
cormat<- cor(df3)
corrplot(cormat,method = 'number', title = 'Corealation Matrix', addCoefasPercent = TRUE, number.cex = 0.65)
cormat<- cor(df3[1:23])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormat, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         addCoefasPercent = TRUE, number.cex = 0.65,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

heatmap.2(cormat)

# checking corelation of highly corelated variables with y 
cor(df$winPlacePerc, df$numGroups)
cor(df$winPlacePerc, df$maxPlace)
cor(df$winPlacePerc, df$killPoints)
cor(df$winPlacePerc, df$winPoints)
cor(df$winPlacePerc, df$rankPoints)

df3_bkp <- df3
#df3 <- df3_bkp
names(df3_bkp)
names(df3)
dim(df3)
df3 <- df3[,-c(13,15,23)]
names(df3)
dim(df3)

df3_bkp_unscaled <- df3

df3 <- as.data.frame(scale(df3[,c(1:20)]))
df4 <- df3_bkp_unscaled
class(df3)
summary(df3)
names(df3)
names(df3_bkp_unscaled)
dim(df3)

logt<- log(df3_bkp_unscaled$winPlacePerc/(1-df3_bkp_unscaled$winPlacePerc))
lo <- log(o/(1-o))
head(o)
head(logt)
length(logt)
lo <- as.data.frame(lo)
class(lo)
df3 <- cbind(df3,logt)
df3 <- cbind(df3,df3_bkp_unscaled$winPlacePerc)
dim(df3)
names(df3)
colnames(df3)[21] <- "winPlacePerc"
names(df3)
sapply(X = df4, FUN = function(x) sum(is.infinite(x)))
sapply(X = df4, FUN = function(x) sum(is.na(x)))
inf_index <- which(df3$winPlacePerc=="-Inf")
inf_index2 <- which(df4$winPlacePerc=="-Inf")
df4[inf_index2,]
df4 <- df3[-inf_index,]
#df3 <- cbind(df3,df3_bkp_2$winPlacePerc)
dim(df3)
dim(df4)
names(df3)
dim(df4)

# Data Partitioning 
sapply(X = train_df, FUN = function(x) sum(is.na(x)))
trainind <- sample(1:nrow(df3), nrow(df3)*.7)
trainind4 <- sample(1:nrow(df4), nrow(df3)*.7)
length(trainind4)
testind <- setdiff(1:nrow(df3), trainind)
testind4 <- setdiff(1:nrow(df4), trainind)
length(testind4)
train_df <- df3[trainind, ]
test_df <- df3[testind, ]
train_df4 <- df4[trainind, ]
test_df4 <- df4[testind, ]
dim(train_df)
names(train_df)
dim(test_df)
names(train_df)

# Multiple Linear Regression

obj.null <- lm(winPlacePerc ~ 1, dat = train_df)
obj.full = lm(winPlacePerc ~ ., dat = train_df)
obj_fwd = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward')
obj_bkwd = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward')
obj_step = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both')

summary(obj_fwd)
summary(obj_bkwd)
summary(obj_step)

yhat = predict(obj_fwd, newdata=test_df[,])
ytest = test_df$winPlacePerc
rmse(ytest, yhat)
hist(ytest-yhat)

par(mfrow=c(2,2))
plot(obj_fwd)


# Regression Tree 

cart_fit = rpart(winPlacePerc~.,method="anova", data=train_df4)
dev.off()
rpart.plot(cart_fit)
names(cart_fit)
printcp(cart_fit) # display the results 
plotcp(cart_fit) # visualize cross-validation results 
summary(cart_fit) # detailed summary of splits
pfit = prune(cart_fit, cp = cart_fit$cptable[which.min(cart_fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit)

yhat.test = predict(cart_fit, newdata = test_df[,-21])
yhat.test_prune = predict(pfit, newdata = test_df[,-21])
length(yhat.test)
rmse(yhat.test, ytest)
rmse(yhat.test_prune, ytest)
dev.off()

# create additional plots 
par(mfrow=c(1,2))# two plots on one page 
par(mfrow=c(1,1))
rsq.rpart(cart_fit) # visualize cross-validation results    

# plot tree 
#plot(cart_fit, uniform=TRUE, main="Regression Tree for Mileage ")
#text(cart_fit, use.n=TRUE, all=TRUE, cex=.8)

#summary(df3$killPlace)
#summary(df3$walkDistance)
#summary(df3)

# KNN Optimal

knn.reg.bestK = function(Xtrain, Xtest, ytrain, ytest, kmax=NULL) {
  if (is.null(kmax)) kmax = length(ytrain)^.5
  vec.rmse = rep(NA, kmax)
  for (k in 1:kmax) {
    yhat.test = knn.reg(Xtrain, Xtest, ytrain, k)$pred
    vec.rmse[k] = rmse(yhat.test, ytest)
  }
  list(k.opt = which.min(vec.rmse), rmse.min = min(vec.rmse), vec.rmse)
}

knn.reg.bestK(train_df[,1:20], test_df[,1:20], train_df[,21], test_df[,21])

knn_obj <- kknn(winPlacePerc~., train_df, test_df, k=11)
names(knn_obj)
yhat.knn <- predict(knn_obj, newdata = df_test3)
rmse(ytest, yhat.knn)

#dim(train_df2)
#names(train_df2)

dev.off()
#Errors histogram
hist(yhat.knn, main="Predicted_winPlacePerc", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")

#Residual plot
plot(test_df$winPlacePerc, yhat.knn, ylab="Residuals", xlab="WinPlacePerc", main="Residual Plot (test)") 
abline(0, 0, col = "red")

#Q-Q plot
stdres.KNN = (yhat.knn)
qqnorm(stdres.KNN, ylab="Standardized Residuals", xlab="Normal Scores", main="QQ Plot") 
qqline(stdres.KNN)

names(df_bkp)
#df_test <- read.csv("test_V2.csv",head=TRUE, stringsAsFactors = FALSE)