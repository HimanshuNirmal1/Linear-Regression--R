#KDD Lab 2
#Author: Himanshu Nirmal

## Part 1

auto=read.csv("Auto.csv", header=TRUE, na.strings = "?")
fix(auto)


# a) Produce a scatterplot matrix, which includes all of the variables in the data set.
pairs(auto)

# b) Compute the matrix of correlations between the variables using the function cor() .
# You will need to exclude the name variable, which is qualitative.

str(auto)
# name variable is qualitative in nature

match("name",names(auto))
cor_mat= cor(auto[,-9])
cor_mat

# c) Use the lm() function to perform a multiple linear regression with mpg as the
# response and all other variables except name as the predictors. Use the summary()
# function to print the results. Comment on the output. For instance:

lm1=lm(mpg~.-name,data=auto)
summary(lm1)

# c.i) The r-squared value is 0.8215 which is closer to 1 indicating strong relation between predictors and response. 
# cylinders, horsepower and accelaration have higher p-values as compared to other variables 
# and hence satisfy the null hypothesis on the other hand, displacement, weight, year and origin have 
# negligible p-values thus rejecting the null hypothesis and showing strong relation with the response 
# variable mpg

# c.ii) displacement, weight, year and origin have a statistically significant relationship
# to the response

# c.iii) the coefficient of the year variable suggests that for every 1 year increase, the mpg of the car 
# too increases by 0.750773 thus making it more fuel efficient

# d.) Use the plot() function to produce diagnostic plots of the linear regression fit.
# Comment on any problems you see with the fit. Do the residual plots suggest any
# unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?

par(mfrow=c(2,2))
plot(lm1)

# In the residual vs Fitted plot, it can be observed that the fit is non-linear in nature and the 
# relationship between predictors and response is non-linear. If a more clear pattern is discovered, then the model
# can be fit more perfectly. The datapoints 323,326,327 are outliers which are not unusually large. 
# In the residual vs leverage plot, datapoint 14 can be treated as unusually high leverage since it lies outside the 
# cooks distance and can affect our results if not considered.

# e.) Use the * and : symbols to fit linear regression models with interaction effects.
# Do any interactions appear to be statistically significant?

#removing name and statistically insignificant variables

auto1=auto[,c(-2,-4,-6,-9)]
lm2=lm(mpg~.,data=auto1)
summary(lm2)

#displacement which was initially significant is insignificant & hence should be removed

auto1=auto[,c(-2,-4,-6,-9,-3)]
lm3=lm(mpg~.,data=auto1)
summary(lm3)

lm3=lm(mpg~.+weight:year+weight:origin+year:origin,data=auto1)
summary(lm3)

# the interaction terms weight:year and weight:origin are significant while the term
# year:origin is insignificant

# f.) Try a few different transformations of the variables, such as log(X), X0.5, X2. Comment on your findings.

lm4=lm(mpg~log(weight),data=auto)
summary(lm4)
# RSE= 4.203, R^2= 0.7123, f-statistic= 978.1


lm4=lm(mpg~I(weight^0.5),data=auto)
summary(lm4)
# RSE= 4.255, R^2= 0.7052, f-statistic= 944.8


lm4=lm(mpg~I(weight^2),data=auto)
summary(lm4)
# RSE= 4.639, R^2= 0.6494, f-statistic= 731.7

# As we went from log->x^0.5->x^2 our rse, r^2 and f-statistic decreased with the most drastic decrease being for
# x^2. So we have a better model using log()



## Part 2

# a.) Fit a multiple regression model to predict Sales using Price, Urban, and US.

library(ISLR)
dim(Carseats)
str(Carseats)

lm1=lm(Sales~Price+Urban+US,data=Carseats)
summary(lm1)

# b.) Provide an interpretation of each coefficient in the model. Be careful—some of
# the variables in the model are qualitative!

# b.i) For every 1 value increase in Price, Sales drops by 5.4459 units
# b.ii) UrbanYes has high p-value, not significant to Sales. We can conclude that for every 1 value increase
# in UrbanYes will result in decrease of 2.1916 unit Sales as compared to non-UrbanYes
# b.iii) USYes has low p-value, significant to Sales. We conclude that for every 1 value increase in USYes will
# lead to increase in 120.0573 units Sales as compared to non-USYes


# c.) Write out the model in equation form, being careful to handle the qualitative variables properly.
# Y = 13.0434 - 0.054459 * Price - 0.021916 * UrbanYes + 1.200573 * USYes.

# d.) For which of the predictors can you reject the null hypothesis H0: βj = 0?

lm1=lm(Sales~.,data=Carseats)
summary(lm1)

# We can reject the null hypothesis H0: βj = 0 for: CompPrice, Income, Advertising, Price, 
# ShelveLocGood, ShelveLocMedium, Age

# e.) On the basis of your response to the previous question, fit a smaller model that
# only uses the predictors for which there is evidence of association with the outcome.

lm2=lm(Sales~.-Population-Education-Urban-US,data=Carseats)
summary(lm2)

# f.) How well do the models in (a) and (e) fit the data?
# There is no change in the RSE and a litte decrease in adjusted R-squared for (e) the model fits the data almost
# in the same way. So model (e) is prefered as it has less predictors. 

# g.) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).

confint(lm2)



