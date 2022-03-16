setwd("c:/b350f")
dir()
data1 <- read.csv("winequality-red.csv")

#Descriptive analysis + normality test
summary(data1)
library(psych)
describe(data1)

#fixed.acidity
par(mfrow=c(1,2))
hist(data1$fixed.acidity, breaks = 20, main="Histogram of fixed.acidity")
qqnorm(data1$fixed.acidity, main = "QQ plot for fixed.acidity")
qqline(data1$fixed.acidity, col = 6, lwd =2)
shapiro.test(data1$fixed.acidity)
#volatile.acidity 
par(mfrow=c(2,2))
hist(data1$volatile.acidity, breaks = 20, main="Histogram of volatile.acidity")
qqnorm(data1$volatile.acidity, main = "QQ plot for volatile.acidity")
qqline(data1$volatile.acidity, col = 6, lwd =2)
shapiro.test(data1$volatile.acidity)
#citric.acid
hist(data1$citric.acid  , breaks = 20, main="Histogram of citric.acid")
qqnorm(data1$citric.acid, main = "QQ plot for citric.acid")
qqline(data1$citric.acid, col = 6, lwd =2)
shapiro.test(data1$citric.acid)
#residual.sugar
hist(data1$residual.sugar, breaks = 20, main="Histogram of residual.sugar")
qqnorm(data1$residual.sugar, main = "QQ plot for residual.sugar")
qqline(data1$residual.sugar, col = 6, lwd =2)
shapiro.test(data1$residual.sugar)
#chlorides
hist(data1$chlorides, breaks = 20, main="Histogram of chlorides")
qqnorm(data1$chlorides, main = "QQ plot for chlorides")
qqline(data1$chlorides, col = 6, lwd =2)
shapiro.test(data1$chlorides)
#free.sulfur.dioxide
hist(data1$free.sulfur.dioxide, breaks = 20, main="Histogram of free.sulfur.dioxide")
qqnorm(data1$free.sulfur.dioxide, main = "QQ plot for free.sulfur.dioxide")
qqline(data1$free.sulfur.dioxide, col = 6, lwd =2)
shapiro.test(data1$free.sulfur.dioxide)
#total.sulfur.dioxide
hist(data1$total.sulfur.dioxide, breaks = 20, main="Histogram of total.sulfur.dioxide")
qqnorm(data1$total.sulfur.dioxide, main = "QQ plot for total.sulfur.dioxide")
qqline(data1$total.sulfur.dioxide, col = 6, lwd =2)
shapiro.test(data1$total.sulfur.dioxide)
#density
hist(data1$density, breaks = 20, main="Histogram of density")
qqnorm(data1$density, main = "QQ plot for density")
qqline(data1$density, col = 6, lwd =2)
shapiro.test(data1$density)
#pH
hist(data1$pH, breaks = 20, main="Histogram of pH")
qqnorm(data1$pH, main = "QQ plot for pH")
qqline(data1$pH, col = 6, lwd =2)
shapiro.test(data1$pH)
#sulphates
hist(data1$sulphates, breaks = 20, main="Histogram of sulphates")
qqnorm(data1$sulphates, main = "QQ plot for sulphates")
qqline(data1$sulphates, col = 6, lwd =2)
shapiro.test(data1$sulphates)
#alcohol
hist(data1$alcohol, breaks = 20, main="Histogram of alcohol")
qqnorm(data1$alcohol, main = "QQ plot for alcohol")
qqline(data1$alcohol, col = 6, lwd =2)
shapiro.test(data1$alcohol)
#quality
hist(data1$quality, breaks = 20, main="Histogram of quality")
qqnorm(data1$quality, main = "QQ plot for quality")
qqline(data1$quality, col = 6, lwd =2)
shapiro.test(data1$quality)

#correlation analysis of 12 variables
library(car)
x <- subset(data1, select = fixed.acidity:quality)
cor(x)
scatterplotMatrix(x = x, diagonal = "historgram")



#checking for missing data
na_count <- colSums(is.na(data1))
na_count
#remove the rows with missing value
screen <- na.omit(data1)

#Outliers Checking
#univariate outliers
#fixed.acidity, volatile.acidity, citric.acid, residual.sugar
par(mfrow=c(2, 2))
Boxplot(data1$fixed.acidity, main = "Boxplot of fixed.acidity",
        ylab = "fixed.acidity",
        id=list(location="avoid"))
Boxplot(data1$volatile.acidity , main = "Boxplot of volatile.acidity ",
        ylab = "volatile.acidity ",
        id=list(location="avoid"))
Boxplot(data1$citric.acid, main = "Boxplot of citric.acid",
        ylab = "citric.acid",
        id=list(location="avoid"))
Boxplot(data1$residual.sugar, main = "Boxplot of residual.sugar",
        ylab = "residual.sugar",
        id=list(location="avoid"))

#chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density
Boxplot(data1$chlorides, main = "Boxplot of chlorides",
        ylab = "chlorides",
        id=list(location="avoid"))
Boxplot(data1$free.sulfur.dioxide , main = "Boxplot of free.sulfur.dioxide ",
        ylab = "free.sulfur.dioxide ",
        id=list(location="avoid"))
Boxplot(data1$total.sulfur.dioxide, main = "Boxplot of total.sulfur.dioxide",
        ylab = "total.sulfur.dioxide",
        id=list(location="avoid"))
Boxplot(data1$density, main = "Boxplot of density", ylab = "density",
        id=list(location="avoid"))

#pH, sulphates, alcohol, quality
Boxplot(data1$pH, main = "Boxplot of pH",
        ylab = "pH",
        id=list(location="avoid"))
Boxplot(data1$sulphates , main = "Boxplot of sulphates ",
        ylab = "sulphates ",
        id=list(location="avoid"))
Boxplot(data1$alcohol, main = "Boxplot of alcohol",
        ylab = "alcohol",
        id=list(location="avoid"))
Boxplot(data1$quality, main = "Boxplot of quality",
        ylab = "quality",
        id=list(location="avoid"))

#bivariate oultiers
#fixed.acidity, volatile.acidity, citric.acid, residual.sugar
par(mfrow=c(2, 2))
dataEllipse(x = data1$fixed.acidity, y = data1$quality, levels=0.95,
            xlab = "fixed.acidity",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$volatile.acidity, y = data1$quality, levels=0.95,
            xlab = "volatile.acidity",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$citric.acid, y = data1$quality, levels=0.95,
            xlab = "citric.acid",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$residual.sugar, y = data1$quality, levels=0.95,
            xlab = "residual.sugar",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))

#chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density
dataEllipse(x = data1$chlorides, y = data1$quality, levels=0.95,
            xlab = "chlorides",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$free.sulfur.dioxide, y = data1$quality, levels=0.95,
            xlab = "free.sulfur.dioxide",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$total.sulfur.dioxide, y = data1$quality, levels=0.95,
            xlab = "total.sulfur.dioxide",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$density, y = data1$quality, levels=0.95,
            xlab = "density",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))

#pH, sulphates, alcohol
dataEllipse(x = data1$pH, y = data1$quality, levels=0.95,
            xlab = "pH",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$sulphates, y = data1$quality, levels=0.95,
            xlab = "sulphates",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data1$alcohol, y = data1$quality, levels=0.95,
            xlab = "alcohol",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))

#multivariate outliers
reg <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
          data = data1)
outlierTest(reg)

#Building the regression model
fit <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
          data = data1)
summary(fit)

#Residual diagnostics
#fit full model
reg1<glm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
          data = data1)
summary(reg)

#regression diagnosis
par(mfrow=c(2,2))
plot(reg)

## plot cook's distance
par(mfrow=c(1,1))
cutoff<-4/(nrow(data1)-length(reg$coefficients)-2)
plot(reg,which=4,cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

## Influence Plot 
influencePlot(reg,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's Distance" )

## plot residual vs predictors
par(mfrow=c(2, 2))
plot(data1$fixed.acidity, resid(reg), 
     main = "residual by regressors for fixed.acidity")

plot(data1$volatile.acidity, resid(reg), 
     main = "residual by regressors for volatile.acidity")

plot(data1$citric.acid, resid(reg), 
     main = "residual by regressors for citric.acid")

plot(data1$residual.sugar, resid(reg), 
     main = "residual by regressors for residual.sugar")

plot(data1$chlorides, resid(reg), 
     main = "residual by regressors for chlorides")

plot(data1$free.sulfur.dioxide, resid(reg), 
     main = "residual by regressors for free.sulfur.dioxide")

plot(data1$total.sulfur.dioxide, resid(reg), 
     main = "residual by regressors for total.sulfur.dioxide")

plot(data1$density, resid(reg), 
     main = "residual by regressors for density")

plot(data1$pH, resid(reg), 
     main = "residual by regressors for pH")

plot(data1$sulphates, resid(reg), 
     main = "residual by regressors for sulphates")

plot(data1$alcohol, resid(reg), 
     main = "residual by regressors for alcohol")

## Durbin-Waston test
library(car)
durbinWatsonTest(reg)

#  Variables Transformation
#quality
log_quality <- log10(data1$quality)
inv_quality<-1/data1$quality
par(mfrow=c(2,2))
hist(log_quality, breaks = 20, main="Histogram of log_quality")
qqnorm(log_quality, main = "QQ plot for log_quality")
qqline(log_quality)
hist(inv_quality, breaks = 20, main="Histogram of inv_quality")
qqnorm(inv_quality, main = "QQ plot for inv_quality")
qqline(inv_quality)
describe(cbind(log_quality,inv_quality))

#fixed.acidity
log_fixed.acidity <- log10(data1$fixed.acidity)
inv_fixed.acidity<-1/data1$fixed.acidity
par(mfrow=c(2,2))
hist(log_fixed.acidity, breaks = 20, main="Histogram of log_fixed.acidity")
qqnorm(log_fixed.acidity, main = "QQ plot for log_fixed.acidity")
qqline(log_fixed.acidity)
hist(inv_quality, breaks = 20, main="Histogram of inv_fixed.acidity")
qqnorm(inv_fixed.acidity, main = "QQ plot for inv_fixed.acidity")
qqline(inv_fixed.acidity)
describe(cbind(log_fixed.acidity,inv_fixed.acidity))

#volatile.acidity
log_volatile.acidity <- log10(data1$volatile.acidity)
inv_volatile.acidity<-1/data1$volatile.acidity
par(mfrow=c(2,2))
hist(log_volatile.acidity, breaks = 20, main="Histogram of log_volatile.acidity")
qqnorm(log_volatile.acidity, main = "QQ plot for log_volatile.acidity")
qqline(log_volatile.acidity)
hist(inv_quality, breaks = 20, main="Histogram of inv_volatile.acidity")
qqnorm(inv_volatile.acidity, main = "QQ plot for inv_volatile.acidity")
qqline(inv_volatile.acidity)
describe(cbind(log_volatile.acidity,inv_volatile.acidity))

#citric.acid
log_citric.acid <- log10(data1$citric.acid)
inv_citric.acid<-1/data1$citric.acid
par(mfrow=c(2,2))
hist(log_citric.acid, breaks = 20, main="Histogram of log_citric.acid")
qqnorm(log_citric.acid, main = "QQ plot for log_citric.acid")
qqline(log_citric.acid)
hist(inv_citric.acid, breaks = 20, main="Histogram of inv_citric.acid")
qqnorm(inv_citric.acid, main = "QQ plot for inv_citric.acid")
qqline(inv_citric.acid)
describe(cbind(log_citric.acid,inv_citric.acid))

#Residual sugar
log_residual.sugar  <- log10(data1$residual.sugar )
inv_residual.sugar <-1/data1$residual.sugar 
par(mfrow=c(2,2))
hist(log_residual.sugar, breaks = 20, main="Histogram of log_residual.sugar")
qqnorm(log_residual.sugar, main = "QQ plot for log_residual.sugar")
qqline(log_residual.sugar)
hist(inv_residual.sugar, breaks = 20, main="Histogram of inv_residual.sugar")
qqnorm(inv_residual.sugar, main = "QQ plot for inv_residual.sugar")
qqline(inv_residual.sugar)
describe(cbind(log_residual.sugar,inv_residual.sugar))

#chlorides
log_chlorides  <- log10(data1$chlorides )
inv_chlorides <-1/data1$chlorides 
par(mfrow=c(2,2))
hist(log_chlorides, breaks = 20, main="Histogram of log_chlorides")
qqnorm(log_chlorides, main = "QQ plot for log_chlorides")
qqline(log_chlorides)
hist(inv_chlorides, breaks = 20, main="Histogram of inv_chlorides")
qqnorm(inv_chlorides, main = "QQ plot for inv_chlorides")
qqline(inv_chlorides)
describe(cbind(log_chlorides,inv_chlorides))

#free.sulfur.dioxide
log_free.sulfur.dioxide  <- log10(data1$free.sulfur.dioxide )
inv_free.sulfur.dioxide <-1/data1$free.sulfur.dioxide 
par(mfrow=c(2,2))
hist(log_free.sulfur.dioxide, breaks = 20, main="Histogram of log_free.sulfur.dioxide")
qqnorm(log_free.sulfur.dioxide, main = "QQ plot for log_free.sulfur.dioxide")
qqline(log_free.sulfur.dioxide)
hist(inv_free.sulfur.dioxide, breaks = 20, main="Histogram of inv_free.sulfur.dioxide")
qqnorm(inv_free.sulfur.dioxide, main = "QQ plot for inv_free.sulfur.dioxide")
qqline(inv_free.sulfur.dioxide)
describe(cbind(log_free.sulfur.dioxide,inv_free.sulfur.dioxide))

#total.sulfur.dioxide
log_total.sulfur.dioxide  <- log10(data1$total.sulfur.dioxide )
inv_total.sulfur.dioxide <-1/data1$total.sulfur.dioxide 
par(mfrow=c(2,2))
hist(log_total.sulfur.dioxide, breaks = 20, main="Histogram of log_total.sulfur.dioxide")
qqnorm(log_total.sulfur.dioxide, main = "QQ plot for log_total.sulfur.dioxide")
qqline(log_total.sulfur.dioxide)
hist(inv_total.sulfur.dioxide, breaks = 20, main="Histogram of inv_total.sulfur.dioxide")
qqnorm(inv_total.sulfur.dioxide, main = "QQ plot for inv_total.sulfur.dioxide")
qqline(inv_total.sulfur.dioxide)
describe(cbind(log_total.sulfur.dioxide,inv_total.sulfur.dioxide))

#density
log_density  <- log10(data1$density )
inv_density <-1/data1$density 
par(mfrow=c(2,2))
hist(log_density, breaks = 20, main="Histogram of log_density")
qqnorm(log_density, main = "QQ plot for log_density")
qqline(log_density)
hist(inv_density, breaks = 20, main="Histogram of inv_density")
qqnorm(inv_density, main = "QQ plot for inv_density")
qqline(inv_density)
describe(cbind(log_density,inv_density))

#pH
log_pH  <- log10(data1$pH )
inv_pH <-1/data1$pH 
par(mfrow=c(2,2))
hist(log_pH, breaks = 20, main="Histogram of log_pH")
qqnorm(log_pH, main = "QQ plot for log_pH")
qqline(log_pH)
hist(inv_pH, breaks = 20, main="Histogram of inv_pH")
qqnorm(inv_pH, main = "QQ plot for inv_pH")
qqline(inv_pH)
describe(cbind(log_pH,inv_pH))

#sulphates
log_sulphates  <- log10(data1$sulphates )
inv_sulphates <-1/data1$sulphates 
par(mfrow=c(2,2))
hist(log_sulphates, breaks = 20, main="Histogram of log_sulphates")
qqnorm(log_sulphates, main = "QQ plot for log_sulphates")
qqline(log_sulphates)
hist(inv_sulphates, breaks = 20, main="Histogram of inv_sulphates")
qqnorm(inv_sulphates, main = "QQ plot for inv_sulphates")
qqline(inv_sulphates)
describe(cbind(log_sulphates,inv_sulphates))

#alcohol
log_alcohol  <- log10(data1$alcohol )
inv_alcohol <-1/data1$alcohol 
par(mfrow=c(2,2))
hist(log_alcohol, breaks = 20, main="Histogram of log_alcohol")
qqnorm(log_alcohol, main = "QQ plot for log_alcohol")
qqline(log_alcohol)
hist(inv_alcohol, breaks = 20, main="Histogram of inv_alcohol")
qqnorm(inv_alcohol, main = "QQ plot for inv_alcohol")
qqline(inv_alcohol)
describe(cbind(log_alcohol,inv_alcohol))

#Transformed model and diagnostics
log_quality<-1/(data1$quality)
log_fixed.acidity <- log10(data1$fixed.acidity)
log_citric.acid <- log10(data1$citric.acid)
log_residual.sugar  <- log10(data1$residual.sugar )
log_chlorides  <- log10(data1$chlorides )
log_free.sulfur.dioxide  <- log10(data1$free.sulfur.dioxide )
log_total.sulfur.dioxide  <- log10(data1$total.sulfur.dioxide )
log_density  <- log10(data1$density )
log_pH  <- log10(data1$pH )
log_sulphates  <- log10(data1$sulphates )
log_alcohol  <- log10(data1$alcohol )

dat_t<-data.frame(cbind(quality=log_quality, fixed.acidity=log_fixed.acidity,
                        volatile.acidity=log_volatile.acidity,
                        citric.acid=data1$citric.acid, 
                        residual.sugar=log_residual.sugar,
                        chlorides=log_chlorides,
                        free.sulfur.dioxide=log_free.sulfur.dioxide,
                        total.sulfur.dioxide = log_total.sulfur.dioxide,
                        density=log_density, pH=log_pH, sulphates=log_sulphates,
                        alcohol=log_alcohol))
  reg_t<-lm(log_quality ~ log_fixed.acidity + log_volatile.acidity + citric.acid + log_residual.sugar + log_chlorides + log_free.sulfur.dioxide + log_total.sulfur.dioxide + log_density + log_pH + log_sulphates + log_alcohol,
                        data = dat_t)
summary(reg_t)

#Examine the outliers in the transformed variables
#multivariate outliers
reg_t<-lm(log_quality ~ log_fixed.acidity + log_volatile.acidity + citric.acid + log_residual.sugar + log_chlorides + log_free.sulfur.dioxide + log_total.sulfur.dioxide + log_density + log_pH + log_sulphates + log_alcohol,
          data = dat_t)
outlierTest(reg_t)

#Residual diagnostic
par(mfrow=c(2,2))
plot(reg_t)
par(mfrow=c(1,1))
cutoff<-4/(nrow(dat)-length(reg_t$coefficients)-2)
plot(reg_t,which=4,cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
influencePlot(reg_t, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's Distance" )
plot(dat_t$volatile.acidity, resid(reg_t), 
     main = "residual by regressors for volatile.acidity")
plot(dat_t$citric.acid , resid(reg_t), 
     main = "residual by regressors for citric.acid ")
plot(dat_t$residual.sugar, resid(reg_t), 
     main = "residual by regressors for residual.sugar")
plot(dat_t$chlorides, resid(reg_t), 
     main = "residual by regressors for chlorides")
plot(dat_t$free.sulfur.dioxide , resid(reg_t), 
     main = "residual by regressors for free.sulfur.dioxide ")
plot(dat_t$total.sulfur.dioxide, resid(reg_t), 
     main = "residual by regressors for total.sulfur.dioxide")
plot(dat_t$density, resid(reg_t), 
     main = "residual by regressors for density")
plot(dat_t$pH , resid(reg_t), 
     main = "residual by regressors for pH ")
plot(dat_t$sulphates, resid(reg_t), 
     main = "residual by regressors for sulphates")
plot(dat_t$alcohol, resid(reg_t), 
     main = "residual by regressors for alcohol")

# Multicollinearity Checking
library(mctest)
omcdiag(mod=reg_t)
# test if individual multicollinearity is present
imcdiag(mod=reg_t)

# stepwise selection
library(MASS)

fit1 <- lm(quality ~ 1, data = dat_t)
fit2 <- lm(quality~ fixed.acidity + volatile.acidity +
             citric.acid + residual.sugar + chlorides +
             free.sulfur.dioxide + total.sulfur.dioxide +
             density + pH + sulphates + alcohol,
           data = dat_t)
step1 <- stepAIC(fit1, direction = "forward", scope= formula(fit2))
step1$anova

# 2. backward selection
step2 <- stepAIC(fit2, direction = "backward")
step2$anova

# 3. stepwise selection
step3 <- stepAIC(fit1, direction = "both", scope= formula(fit2))
step3$anova

#Final regression model
final<-lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
          data = dat_t)
summary(final)
