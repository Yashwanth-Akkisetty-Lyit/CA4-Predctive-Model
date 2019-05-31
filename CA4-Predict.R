
setwd('C:/Users/yaswa/Documents/CA3-Environment-problems')
dataset1 <- read.csv("data1.csv",header=T,sep=",")
str(dataset1)
vehicle_usage <- data.frame(dataset1)
dataset2 <- read.csv("data2.csv",header=T,sep=",")
air_pollutants <- data.frame(dataset2)

year_quarter <- c('2009Q1', '2009Q2', '2009Q3', '2009Q4', 
                  '2010Q1', '2010Q2', '2010Q3', '2010Q4', 
                  '2011Q1', '2011Q2', '2011Q3', '2011Q4', 
                  '2012Q1', '2012Q2', '2012Q3', '2012Q4', 
                  '2013Q1', '2013Q2', '2013Q3', '2013Q4', 
                  '2014Q1', '2014Q2', '2014Q3', '2014Q4', 
                  '2015Q1', '2015Q2', '2015Q3', '2015Q4')

Public_Transport <- c(249774,	253075,	250744,	246068,	
                      238079,	237501,	241130,	236982,
                      234897,	230233,	232682,	226901,	
                      217131,	218130,	217916,	214984,
                      221368,	223317,	216958,	223716,
                      208912,	207854,	207573,	206872,
                      226347,	226190,	224651,	223490)

Private_Transport <- c(171496,	171659,	182730,	182131,
                       188121,	191361,	189757,	188652,	
                       245133,	244665,	248348,	248475,	
                       339507,	339298,	342363,	340240,	
                       398036,	394371,	396490,	390670,	
                       411261,	409098,	409311,	411054,	
                       445314,	447280,	451535,	445917)

Total_Pollutants <- c(136.77,	130.32,	139.15,	141.55,
                      127.115,	126.21,	129.04,	129.44,
                      119.27,	113.26,	121.73,	119.41,
                      118.26,	119.37,	122.47,	117.73,
                      112.73,	110.51,	108.81,	111.16,
                      112.99,	113.33,	111.4,	117.53,
                      113.305, 110.27,	112.79,	112.09)

All_Means_of_Travel <- c(421270,	424734,	433474,	428199,
                         426200, 428862,	430887,	425634,
                         480030, 474898,	481030,	475376,
                         556638, 557428,	560279,	555224,
                         619404, 617688,	613448,	614386,
                         620173, 616952,	616884,	617926,
                         671661, 673470,	676186,	669407)


final_dataset <- data.frame(year_quarter, Public_Transport,
                            Private_Transport, All_Means_of_Travel, Total_Pollutants)
View(final_dataset)

# Building a linear regression model 
#Using Ireland vehicle and air pollutant dataset
#Which contains details of 
#vehicle usage and air pollutants rate in Ireland.

linear_model <- lm(Total_Pollutants ~  All_Means_of_Travel , data = final_dataset)
linear_model
summary(linear_model)

# The estimates of the beta coefficients
# and the standard errors (SE) defines the accuracy of beta coefficients. 
# For a given beta coefficient, the SE reflects how the coefficient varies under 
# repeated sampling. It can be used to compute the confidence intervals and the t-statistic.
# the t-statistic and the associated p-value, which defines the statistical significance of the beta coefficients.

# Plotting Vehicle usage and Pollutants rate variable to see relationship between the response(Vehicle usage) and
# predictor (pollutants rate) variable

plot(final_dataset$Total_Pollutants, final_dataset$All_Means_of_Travel,
     xlab="Total_Pollutants",
     ylab="All_Means_of_Travel",
     main = "Scatter plot showing regression line
     for Total air polltants from Vehicle Usage")
abline(linear_model)

cor(final_dataset$Total_Pollutants, final_dataset$All_Means_of_Travel)

# Examining the 95% confidence intervals of the model

confint(linear_model)


# Scatter plots helps to visualise any linear relationships between the varaiables
#Here visualing the relationship between vehicle usage rate and pollutants rate


scatter.smooth(final_dataset$Total_Pollutants, final_dataset$All_Means_of_Travel,
               main = "Total_Pollutants ~ All_Means_of_Travel",
               xlab = "Total_Pollutants",
               ylab = "All_Means_of_Travel")

#Box Plot
par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(final_dataset$Total_Pollutants, main = "Total_Pollutants", sub = paste("Outlier rows: ", boxplot.stats(final_dataset$Total_Pollutants)$out)) # box plot for 'Polltants rate'
boxplot(final_dataset$All_Means_of_Travel, main = "All_Means_of_Travel", sub = paste("Outlier rows: ", boxplot.stats(final_dataset$All_Means_of_Travel)$out)) # box plot for 'Vehical usage rate'


# Skewness function to examine normality of data
# install.packages("e1071")
# Density Plot
library(e1071)
# Divide graph area in 2 columns
par(mfrow = c(1, 2))

# Density plot for Total_Pollutants
plot(density(final_dataset$Total_Pollutants), main = "Density Plot :Total_Polluatants",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(final_dataset$Total_Pollutants), 2)))

# Filling the area within the density plot to red
polygon(density(final_dataset$Total_Pollutants), col = "blue")

# Density plot for All_Means_of_Travel
plot(density(final_dataset$All_Means_of_Travel), main = "Density Plot :All_Means_of_Travel",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(final_dataset$All_Means_of_Travel), 2)))

# Filling the area within the density plot to red
polygon(density(final_dataset$All_Means_of_Travel), col = "blue")


# Calculating correlation test between vehicle usage rate and air pollutants rate
cor(final_dataset$Total_Pollutants, final_dataset$All_Means_of_Travel)

# build linear regression model on full data
linearMod <- lm(Total_Pollutants ~ All_Means_of_Travel, data = final_dataset)
linearMod

# model summary
summary(linearMod)

model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients
model_coeffs

# get beta estimate for Total pollutants
beta.estimate <- model_coeffs["All_Means_of_Travel", "Estimate"]

# get std.error for Total pollutants
std_error <- model_coeffs["All_Means_of_Travel", "Std. Error"]

# calculating t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(final_dataset) - ncol(final_dataset)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# sample chooses a random sample
# from 1:all records from final_dataset, 80% of rows
no_of_records <- sample(1:nrow(final_dataset), 0.8 * nrow(final_dataset))
# model training data
training_data <- final_dataset[no_of_records,]
training_data
# test data
testing_data <- final_dataset[-no_of_records,]
testing_data

# Building the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lm_model <- lm(Total_Pollutants ~ All_Means_of_Travel, data = training_data)

# model summary
summary(lm_model)

# predict from testing data
lm_predicted <- predict(lm_model, testing_data)
summary(lm_predicted)

# make actuals_predicteds dataframe.
lm_actuals_preds <- data.frame(cbind(actuals = testing_data$All_Means_of_Travel, 
                                     predicted = lm_predicted))
head(lm_actuals_preds)


AIC(linearMod)

BIC(linearMod)

correlation_accuracy <- cor(lm_actuals_preds)
correlation_accuracy

# Min - max accuracy
lm_min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))
lm_min_max_accuracy

# MAPE
lm_mape <- mean(abs((lm_actuals_preds$predicted - lm_actuals_preds$actuals)) / lm_actuals_preds$actuals)
lm_mape

# Global validation of linear model assumption
#install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)


















































