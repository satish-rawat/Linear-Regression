# library loading

library(mice)
library(dplyr)
library(VIM)
library(usdm)
library(car)
library(caret)
library(ggplot2)
library(corrplot)

# loading dataset 

housing<-read.csv("HousingData.csv")
str(housing)
View(housing)
summary(housing)

# tranformation of a column

housing$CHAS <- factor(housing$CHAS)
housing$RAD <- factor(housing$RAD)

# checking pattern for missing value

mice::md.pattern(housing)
mice_plot <- aggr(housing, col=c('orange','green'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(housing), cex.axis=1, gap=5, ylab=c("Missing data","Pattern"))


# imputing missing value

housing[["CHAS"]][is.na(housing[["CHAS"]])] <- 0
imputed_housing <- mice(housing, m=3, maxit = 50, method = 'pmm', seed = 150)
housing_complete <- complete (imputed_housing,3)
summary(housing_complete)

# outlier treatment (detecting outlier using user defined function and method is mean +- 3*sd) 

detect_outliers <- function(feature_name) {
  mean <- mean(feature_name)
  stdev <- sd(feature_name)
  upper_limit <- mean + 3*stdev
  lower_limit <- mean - 3*stdev
  boundary_values <- c(lower_limit,upper_limit)
  quantiles <- raster::quantile(feature_name,seq(0,1,0.01))

  return(list(boundary_values,quantiles))
}

detect_outliers(housing_complete$CRIM) 
housing_complete$CRIM <- ifelse(housing_complete$CRIM >= 29.46,29.46,housing_complete$CRIM)
quantile(housing_complete$CRIM,seq(0,1,0.01))

detect_outliers(housing_complete$ZN) 
housing_complete$ZN <- ifelse(housing_complete$ZN >= 81.45,81.45,housing_complete$ZN)
quantile(housing_complete$ZN,seq(0,1,0.01))

detect_outliers(housing_complete$RM)
housing_complete$RM <- ifelse(housing_complete$RM >= 8.392,8.392,housing_complete$RM)
housing_complete$RM <- ifelse(housing_complete$RM <= 4.17,4.17,housing_complete$RM)
quantile(housing_complete$RM,seq(0,1,0.01))

detect_outliers(housing_complete$DIS) 
housing_complete$DIS <- ifelse(housing_complete$DIS >= 10.11,10.11,housing_complete$DIS)
quantile(housing_complete$DIS,seq(0,1,0.01))

detect_outliers(housing_complete$LSTAT) 
housing_complete$LSTAT <- ifelse(housing_complete$LSTAT >= 33.983,33.983,housing_complete$LSTAT)
quantile(housing_complete$LSTAT,seq(0,1,0.01))

# Generating transformation report for checking distribution of variables

dlookr::transformation_report(housing_complete,target = MEDV, 
                      output_dir = "C:/Users/satish rawat/Desktop/Satish/Boston Housing", 
                      output_format = "html")

# applying transformation to get approximate normal distribution 

cols <- c("CRIM","NOX","DIS")
housing_complete[cols] <- log(housing_complete[cols])
cols1 <- c("LSTAT", "MEDV")
housing_complete[cols1] <- sqrt(housing_complete[cols1])


# Data Visualisation

str(housing_complete)
remove_factor <- housing_complete[c(-4,-9)]
corrplot(cor(remove_factor), method = "pie", type = "upper")  

housing_complete %>%
  dplyr::select(c(CRIM, DIS ,RM, AGE, TAX, LSTAT, MEDV,INDUS,NOX,PTRATIO,ZN)) %>%
  melt(id.vars = "MEDV") %>%
  ggplot(aes(x = value, y = MEDV, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()

housing_complete %>%
  dplyr::select(c(CRIM, DIS ,RM, AGE, TAX, LSTAT, MEDV,INDUS,NOX,PTRATIO,ZN,CHAS)) %>%
  melt(id.vars = "CHAS") %>%
  ggplot(aes(x = value, colour = variable)) +
  geom_histogram() + 
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(x = "Variable Value") +
  theme_minimal()

housing_complete %>%
  dplyr::select(c(CRIM, DIS ,RM, AGE, TAX, LSTAT, MEDV,INDUS,NOX,PTRATIO,ZN,CHAS)) %>%
  melt(id.vars = "CHAS") %>%
  ggplot(aes(x = value, colour = variable)) +
  geom_density() + 
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(x = "Variable Value") +
  theme_minimal()

housing_complete %>%
  dplyr::select(c(CRIM, DIS ,TAX,INDUS,NOX)) %>%
  melt(id.vars = "CRIM") %>%
  ggplot(aes(x = value, y = CRIM, colour = variable)) +
  geom_point(alpha = 0.7) + geom_line() +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "CRIM") +
  theme_minimal()

housing_complete %>%
  dplyr::select(c(NOX, DIS ,TAX,INDUS)) %>%
  melt(id.vars = "INDUS") %>%
  ggplot(aes(x = value, y = INDUS, colour = variable)) +
  geom_point(alpha = 0.7) + geom_line() +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "INDUS") +
  theme_minimal()

ggplot(housing_complete, aes(x = AGE, y = DIS))+ theme_minimal()+geom_point() + geom_smooth() + geom_line()
ggplot(housing_complete, aes(x = LSTAT, y = MEDV))+ theme_minimal()+geom_point() + geom_smooth() + geom_line()
ggplot(housing_complete, aes(x = PTRATIO, y = MEDV))+ theme_minimal()+geom_point() + geom_smooth() + geom_line()
ggplot(housing_complete, aes(x = TAX, y = NOX))+ theme_minimal()+geom_point() + geom_smooth() + geom_line()
ggplot(housing_complete) + geom_bar(aes(x = RAD)) + theme_minimal()
ggplot(housing_complete, aes(x = NOX, y = LSTAT))+ theme_minimal()+geom_point() + geom_smooth() + geom_line()
ggplot(housing_complete, aes(x = cut_number(TAX,5)))+ theme_minimal() + geom_bar() + 
  labs(x = "property-tax rate per $10,000") 
ggplot(housing_complete, aes(x = DIS, y = NOX))+ theme_minimal() + geom_point()
ggplot(housing_complete, aes(x = NOX, y = DIS, color = cut_number(TAX,5)))+ theme_minimal() + geom_point() + 
  labs(x = "Nitric Oxide Concentration", y = "Average Distance From 5 Employment Center",
       color ="property-tax rate per $10,000") 

# Model building and Model Evaluation Using K- Fold Cross Validation

model1 <- lm(MEDV~.,data = housing_complete)
summary(model1)
vif(model1)

step(model1, direction = "both")

model2 <- lm(MEDV ~ CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
               B + LSTAT, data = housing_complete)
summary(model2)
vif(model2)

model3 <- lm(MEDV ~ CHAS + NOX + RM + DIS + TAX + PTRATIO + 
               B + LSTAT, data = housing_complete)
summary(model3)
vif(model3)

model4 <- lm(MEDV ~ CHAS + NOX + RM + DIS + RAD + PTRATIO + 
               B + LSTAT, data = housing_complete)
summary(model4)
vif(model4)

durbinWatsonTest(model1)
durbinWatsonTest(model2)
durbinWatsonTest(model3)
durbinWatsonTest(model4)

# Validate Model using MAE

# define training control Boot And K-Fold Repeated Cross Validation

train_control_boot <- trainControl(method="boot", number=100)
train_control_cv <- trainControl(method="repeatedcv", number=5, repeats=5)

# train the model using boot

model1_boot <- train(MEDV~., data=housing_complete, trControl=train_control_boot, method="lm")
model2_boot <- train(MEDV ~ CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
                       B + LSTAT, data=housing_complete, trControl=train_control_boot, method="lm")
model3_boot <- train(MEDV ~ CHAS + NOX + RM + DIS + TAX + PTRATIO + 
                       B + LSTAT, data=housing_complete, trControl=train_control_boot, method="lm")
model4_boot <- train(MEDV ~ CHAS + NOX + RM + DIS + RAD + PTRATIO + 
                       B + LSTAT, data=housing_complete, trControl=train_control_boot, method="lm")

print(model1_boot)
print(model2_boot)
print(model3_boot)
print(model4_boot)


# train the model using K-fold Repeated CV

model1_cv <- train(MEDV~., data=housing_complete, trControl=train_control_cv, method="lm")
model2_cv <- train(MEDV ~ CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
                     B + LSTAT, data=housing_complete, trControl=train_control_cv, method="lm")
model3_cv <- train(MEDV ~ CHAS + NOX + RM + DIS + TAX + PTRATIO + 
                     B + LSTAT, data=housing_complete, trControl=train_control_cv, method="lm")
model4_cv <- train(MEDV ~ CHAS + NOX + RM + DIS + RAD + PTRATIO + 
                    B + LSTAT, data=housing_complete, trControl=train_control_cv, method="lm")

print(model1_cv)
print(model2_cv)
print(model3_cv)
print(model4_cv)

# Evaluation parameter is MAE
# model3 is best model in Boot strapping method
# model2 is best model in 5 fold repeated cross validation

