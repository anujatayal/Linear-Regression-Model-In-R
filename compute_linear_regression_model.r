#####################################################################################################
# compute_linear_regression_model
# Created By: Anuja Tayal
# Creation Date:8 July 2017
# Input : 1) A 2-col matrix where both columns have numeric values
# Output : 1) A 4 column matrix with (model,condition, error, rsquare)
# compute_linear_regression_model fits a linear model to columns of input_data. It fits simple linear 
# model or piecewise linear model. And whose mean average error is minimum, returns the fitted line,
# conditions if necessary, error value, and r square value of fitted model.It first cleans the data 
# by removing na values,duplicate values,removing outliers. It also checks whether the number of 
# observations> CONST_MIN_COUNT. It also checks if there is a constant value of any of the column
# by checking its standard deviation.
# 
# Libraries required
# library(splines)
# library(ModelMetrics)
#####################################################################################################
library(splines)
library(stats)
library(ModelMetrics)
CONST_MIN_COUNT = 10
CONST_NO_OF_KNOTS=2
CONST_P_VALUE=0.001
   
#####################################################################################################
# prepare_data
# Input: input_data(2 column input matrix)
# Output: input_data(2 column input matrix)
# Functionality: The prepare_data basically cleans input data by eliminating rows with NA values, removes
# rows consisting of duplicate records and checks that number of records is greater than min_count as 
# it is very difficult to fit linear model with very few observations.
# Called By: compute_linear_regression_model
# Calls:  remove_outliers
#####################################################################################################

prepare_data <- function(input_data)
{
  if(is.null(input_data))
  {
    print("prepare_data: Incorrect Input Data")
    return(NULL)
  }
  if(ncol(input_data)!=2)
  {
    print("Incorrect Data. There should be two column matrix")
    return(NULL)
  }
  if(class(input_data)!="matrix")
  {
    print("Incorrect Data. The input should be 2 column matrix")
    return(NULL)
  }
  if(!(is.numeric(input_data[,1]) & is.numeric(input_data[,2])))
  {
    print("Incorrect Data. The input should be numeric")
    
    return(NULL)
  }
  input_data<-na.omit(input_data)  #eliminating rows with NA values
  input_data<-unique(input_data) #eliminate duplicate record of input.table
  input_data<-remove_outliers(input_data,P_VALUE)  # call remove_outliers function
  if(nrow(input_data) <CONST_MIN_COUNT)
  {
    print("No of observations is less than CONST_MIN_COUNT")
    return(NULL)
  }
  return(input_data) #return cleaned prepared data
}

#####################################################################################################
# remove_outliers
# Input: input_data(2 column input matrix) and p value
# Output: input_data(2 column input matrix)
# Functionality: The remove_outliers function removes outliers from input data as presence of outliers
# can hamper the correctness of linear model. The function uses mahalanobis function to remove outliers
# as it is good for multi column values by taking cutoff using quantile chi square method with p value as
# P_VALUE
# Called By: prepare_data
# Calls:  -
#####################################################################################################

remove_outliers <- function(input_data,P_VALUE=0.001)
{
  if(is.null(input_data))
  {
    print("remove_outliers: Incorrect Input Data")
    return(NULL)
  }
  if((sd(input_data[,1])==0 | sd(input_data[,2])==0)) ##checking for constant column of input data
  {
    print("Error:-remove_outliers: The value of one column is constant")
    return(NULL)
  }
  mahal<-mahalanobis(input_data,colMeans(input_data,na.rm = TRUE),cov(input_data,use="pairwise.complete.obs"))
  cutoff<-qchisq(1-P_VALUE,ncol(input_data))
  input_data<-input_data[mahal<cutoff,]
  return(input_data)   #return input data without outliers
}


#####################################################################################################
#compute_lm 
#Input: input_data(2 column input matrix)
#Output: slope,intercept,rsquare,error 
#Functionality: The compute_lm function fits a linear model using lm and returns slope and 
# intercept of fitted line ,its rsquare value and mean average error between actual and fitted values
#Called By: compute_linear_regression_model
#Calls:  compute_error
#####################################################################################################

compute_lm <- function(input_data)
{
  if(is.null(input_data))
  {
    print("compute_lm: Incorrect Input Data")
    return(NULL)
  }
  lm_eval=lm(input_data[,2]~input_data[,1])
  #plot(input_data[,1],input_data[,2])
  abline(lm_eval)
  ypred=predict(lm_eval,interval="prediction",se.fit=TRUE)
  ypred1=predict(lm_eval,interval="confidence",se.fit=TRUE)
  #points(input_data[,1],ypred$fit[,2],col="green")
  #points(input_data[,1],ypred$fit[,3],col="green")
  #points(input_data[,1],ypred1$fit[,2],col="yellow")
  #points(input_data[,1],ypred1$fit[,3],col="yellow")
  #abline(confint(lm_eval)[1,1],confint(lm_eval)[2,1],col="red")
  
  h1<-which(lm_eval$residuals>0,arr.ind = TRUE)
  multi=lm(input_data[h1,2]~input_data[h1,1])
  #abline(multi,col="red")
  h1<-which(lm_eval$residuals<0,arr.ind = TRUE)
  multi=lm(input_data[h1,2]~input_data[h1,1])
  #abline(multi,col="red")
  
  error = compute_error(input_data, lm_eval)
  slope=lm_eval$coefficients[[2]]
  intercept=lm_eval$coefficients[[1]]
  rsquare=summary(lm_eval)$r.squared
  return(list(slope=slope,intercept=intercept,rsquare=rsquare, error=error))
}


#####################################################################################################
# compute_piecewise_lm
# Input: input_data(2 column input matrix),NO_OF_KNOTS
# Output: slope,intercept,rsquare,error
# Functionality: The compute_piecewise_lm function divides the input data using NO_OF_KNOTS and fits 
# a linear model to each obtained input and returns slope and intercept of fitted line ,its rsquare 
# value and mean average error between actual and fitted values
# Called By: compute_linear_regression_model
# Calls:  -
#####################################################################################################

compute_piecewise_lm <- function(input_data,NO_OF_KNOTS=1)
{
  if(is.null(input_data))
  {
    print("compute_piecewise_lm: Incorrect Input Data")
    return(NULL)
  }
  slope={}
  intercept={}
  condition={}
  piecewise_lm<-lm(input_data[,2]~bs(input_data[,1],degree=1,df=NO_OF_KNOTS+1)) #applying piecewise linear model 
  points(input_data[,1],fitted(piecewise_lm),col="blue")
  ypred=predict(piecewise_lm,interval="prediction",se.fit=TRUE)
  ypred1=predict(piecewise_lm,interval="confidence",se.fit=TRUE)
  points(input_data[,1],ypred$fit[,2],col="green")
  points(input_data[,1],ypred$fit[,3],col="green")
  points(input_data[,1],ypred1$fit[,2],col="yellow")
  points(input_data[,1],ypred1$fit[,3],col="yellow")
  
  piecewise_attributes <- attr(piecewise_lm$terms, "predvars")
  knots<-piecewise_attributes[[3]]$knots ##value of knots or cutpoints
  boundary_knots<-piecewise_attributes[[3]]$Boundary.knots
  coefficients<-summary(piecewise_lm)$coefficients ##coefficients
  error = compute_error(input_data, piecewise_lm) ## computing error
  for(i in 1:length(knots)) #computing slope,intercept and condition using knots, boundary knots and coefficients
  {
      if(i>1)
      {
        slope[i]=(coefficients[i+1,1]-coefficients[i,1])/(knots[[i]]-knots[[i-1]])
        intercept[i]=coefficients[1,1]+coefficients[i,1]-knots[[i-1]]*slope[i]
        condition[i]=paste("x>",knots[[i-1]],"and x<=",knots[[i]])
      }
    else
    {
      slope[i]=coefficients[i+1,1]/(knots[[i]]-boundary_knots[i])
      intercept[i]=coefficients[1,1]
      condition[i]=paste("x<=",knots[[i]])
    }
    
  }
  slope[i+1]=(coefficients[i+2,1]-coefficients[i+1,1])/(boundary_knots[2]-knots[[i]])
  intercept[i+1]=coefficients[1,1]+coefficients[i+1,1]-knots[[i]]*slope[i+1]
  condition[i+1]=paste("x>",knots[[i]])
  rsquare=summary(piecewise_lm)$r.squared
  return(list(slope=slope,intercept=intercept,condition=condition,rsquare=rsquare, error=error))
}   

#####################################################################################################
#compute_error
#Input: input_data(2 column input matrix) and the model fitted to input data
#Output: error(mean average error)
#Functionality: The compute_error function computes mean average error between the actual
# values and the values fitted by the model.
#Called By: compute_lm,compute_piecewise_lm
#Calls:  -
#####################################################################################################

compute_error <- function(input_data, model)
{
  if(is.null(input_data) | is.null(model))
  {
    print("compute_error: Incorrect Input Data or model")
    return(NULL)
  }
  actual_values<-input_data[,2]
  fitted_values<-fitted(model)
  error<-mae(actual_values,fitted_values)
  return(error)
}


#####################################################################################################
# prepare_results
# Input: fitted model
# Output: A 4 column matrix with (model,condition,rsquare,error)
# Functionality: The function prepare_results basically prepares the output obtained by the model
# in the form of result matrix with (model,condition,rsquare,error)
# Called By: compute_linear_regression_models
# Calls:  
#####################################################################################################

prepare_results <- function(model)
{
  if(is.null(model))
  {
    print("prepare_results: Incorrect Input model")
    return(NULL)
  }
  result_matrix=matrix(nrow=length(model$slope),ncol=4)
  colnames(result_matrix)=c("model","condition","rsquare","error")
  result_matrix[,"model"]=paste("y=",model$slope,"*x+",model$intercept)
  if(!is.null(model$condition))
  {
    result_matrix[,"condition"]=model$condition
  }  
  result_matrix[1,"rsquare"]=model$rsquare
  result_matrix[1,"error"]=model$error
  return(result_matrix)
}

#####################################################################################################
# main function
#compute_linear_regression_model(input_data)
#####################################################################################################

compute_linear_regression_model <- function(input_data)
{
  
  if(is.null(input_data) | (nargs()==0) )
  {
    print("compute_linear_regression_model: Null Input Data")
    return(NULL)
  }
  input_data = prepare_data(input_data)
  input_data<-input_data1
  lm_model_summary = compute_lm(input_data1)
  piecewise_lm_model_summary = compute_piecewise_lm(input_data,NO_OF_KNOTS)
  if(is.null(input_data) | is.null(lm_model_summary) | is.null(piecewise_lm_model_summary))
  {
    print("compute_linear_regression_model: Incorrect Input Data")
    return(NULL)
  }
  lm_model_error<-lm_model_summary$error
  piecewise_model_error<-piecewise_lm_model_summary$error
  if(lm_model_error<piecewise_model_error) { model<-lm_model_summary }
  else {model<-piecewise_lm_model_summary }
  result_matrix = prepare_results(model)
  return(result_matrix)
}
View(input_data)
