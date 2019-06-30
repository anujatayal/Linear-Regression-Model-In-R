# Linear-Regression-Model-In-R
#Read Me
The file   Regression_function contains a function by the name linear_model in which an input is a 2 column table with columns(x,y) and returns an output containing mean average error and equation of form y=mx+c where m is the slope of the line and c is the intercept. 
If the r squared value of the fitted regression line is less than 0.3 then instead of an equation a piecewise linear function is estimated using a 3x2 data frame where 1st column represents the equation and 2nd column represents the  condition 
The function takes care of the outliers as well as null values present in the dataset by filtering them.
The function estimates the linear regression model only if the number of observations is greater than 15.

To run the R file following libraries are required and needed to be downloaded.
library(splines)
library(ModelMetrics)
library(ggplot2)
	
