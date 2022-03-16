# 12 independent variables in the excel, take three only
# Descriptive analysis and normality checking 
# Correlation analysis 
# Cleansing data: missing data and outliers checking 
# Developing basic regression model and performing residual diagnostics 
# Durbin-Waston test
# Improving model by transforming variables (include residual diagnostics)
# Multicollinearity Checking
# Using stepwise regression to develop the final model 


setwd("c:/tes_dat")
dir()
data1 <- read.csv("quality-wineRed.csv")

#Descriptive analysis 
summary(data1)
library(psych)
describe(data1)

# normality test for 12 variables_example: fixed.acidity + volatile.acidity + citric.acid
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

#correlation analysis of 12 variables
library(car)
x <- subset(data1, select = fixed.acidity:quality)
cor(x)
scatterplotMatrix(x = x, diagonal = "historgram")



#checking for missing data
na_count <- colSums(is.na(data001))
na_count
#remove the rows with missing value
screen <- na.omit(data1)

#Outliers Checking
#univariate outliers
#Example: fixed.acidity, volatile.acidity, citric.acid
par(mfrow=c(2, 2))
Boxplot(data001$fixed.acidity, main = "Boxplot of fixed.acidity",
        ylab = "fixed.acidity",
        id=list(location="avoid"))
Boxplot(data001$volatile.acidity , main = "Boxplot of volatile.acidity ",
        ylab = "volatile.acidity ",
        id=list(location="avoid"))
Boxplot(data001$citric.acid, main = "Boxplot of citric.acid",
        ylab = "citric.acid",
        id=list(location="avoid"))



#bivariate oultiers
#Example: fixed.acidity, volatile.acidity, citric.acid
par(mfrow=c(2, 2))
dataEllipse(x = data001$fixed.acidity, y = data1$quality, levels=0.95,
            xlab = "fixed.acidity",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data001$volatile.acidity, y = data1$quality, levels=0.95,
            xlab = "volatile.acidity",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))
dataEllipse(x = data001$citric.acid, y = data1$quality, levels=0.95,
            xlab = "citric.acid",
            ylab = "quality",
            main = "bivariate outliers", id = list(n=5))


#multivariate outliers
#Example: fixed.acidity, volatile.acidity, citric.acid
reg <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid,
          data = data1)
outlierTest(reg)

#Building the regression model
#Example: fixed.acidity, volatile.acidity, citric.acid
fit <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid,
          data = data1)
summary(fit)

#Residual diagnostics
#fit full model
reg1<glm(quality ~ fixed.acidity + volatile.acidity + citric.acid,
          data = data)
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



# Durbin-Waston test
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


#Transformed model and diagnostics
log_quality<-1/(data1$quality)
log_fixed.acidity <- log10(data1$fixed.acidity)
log_citric.acid <- log10(data1$citric.acid)


dat_t<-data.frame(cbind(quality=log_quality, fixed.acidity=log_fixed.acidity,
                        volatile.acidity=log_volatile.acidity,
                        citric.acid=data1$citric.acid))
  reg_t<-lm(log_quality ~ log_fixed.acidity + log_volatile.acidity + citric.acid,
                        data = dat_t)
summary(reg_t)


#Examine the outliers in the transformed variables
#multivariate outliers
reg_t<-lm(log_quality ~ log_fixed.acidity + log_volatile.acidity + citric.acid,
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


# Multicollinearity Checking
library(mctest)
omcdiag(mod=reg_t)

# test if individual multicollinearity is present
imcdiag(mod=reg_t)

# stepwise selection
library(MASS)

fit1 <- lm(quality ~ 1, data = dat_t)
fit2 <- lm(quality~ fixed.acidity + volatile.acidity +
             citric.acid,
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
final<-lm(quality ~ fixed.acidity + volatile.acidity + citric.acid,
          data = dat_t)
summary(final)
