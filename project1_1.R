#############################
#Function Declaration
############################
linear_regression <- function(par, data){
  a=par[1]
  b=par[2]
  x=data[,1]
  y=data[,2]
  sum((y-(a+b*x))^2)
}


linear_with_r_regression <- function(par, data){
  a=par[1]
  b=par[2]
  r=par[3]
  x=data[,1]
  y=data[,2]
  if(r==0){
    sum((y-b*x)^2)
  }else{
    sum((y-(a+b*(1-exp(-r*x)))/r)^2)
  }
}

#############################################################################


#############################
#Data Simulation For Sample
############################
set.seed(42)
x=runif(1000, 0, 10)
eps = runif(1000, -1, 1)

a= 30
b= 20
#r= 0.15
r= 0.05

y = a + b*((1-exp(-r*x))/r) + eps

df = data.frame(x, y)
plot(x, y)
#############################################################################

nlminb(start = c(1, 2, 2), objective = linear_with_r_regression, data=df) # R- Linear Regression

nlminb(start = c(1, 2), objective = linear_regression, data=df) # R- Linear Regression


#############################
#Bootstrapping 
############################

bootstrap<-function(data, times=100){
  bootstrapped_data=vector('list',times)
  data_points = length(data[, 1])
  inp_data = data
  for(i in 1:times){
    bootstrapped_data[[i]] = data.frame(data[round(runif(data_points, 1, data_points),digits=0 ), ])
  }
  bootstrapped_data
}


#############################################################################

#############################
#Hypothesis Testing
#H0 = r-model is better
#H1 = Linear Model Better
# So we run rmodel and run linear model on bootstrapped and then we see if rmodel is alwasy better than linear model, but
############################

rmodel_result = nlminb(start = c(1, 2, 2), objective = linear_with_r_regression, data=df) # R- Linear Regression

bootstrap_data = bootstrap(df, times=1000)
linreg_result=c()
linreg_result_residuals=c()
for(i in 1:length(bootstrap_data)){
  linreg_result[[i]] = nlminb(start = c(1, 2), objective = linear_regression, data=bootstrap_data[[i]]) #Linear Regression
  linreg_result_residuals[i]=linreg_result[[i]]$objective
}

sum(rmodel_result$objective<linreg_result_residuals)/length(linreg_result_residuals) # Higher 
#how often your is your bad model better than your good model beacause its lucky

#############################################################################

