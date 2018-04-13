############################
#Function Declaration
############################

########################
u<-function(r, x) {
  
  ((1-exp(-r*x))/r) 
  
}
########################


#####################
bootstrap<-function(data, times=100){
  bootstrapped_data=vector('list',times)
  data_points = length(data[, 1])
  
  
  
  for(i in 1:times){
    #bootstrapped_data[[i]] = data.frame(data[round(runif(data_points, 1, data_points),digits=0 ), ])
    id = round(rnorm(300, 130, sd=50))
    idx = id[id>=1&id<=260][1:260]
    bootstrapped_data[[i]] = data.frame(data[idx, ])
  }
  bootstrapped_data
}
########################
regression_full_model <- function(par, data){
  b0=par[1]
  b1=par[2]
  b2=par[3]
  b3=par[4]
  b4=par[5]
  b5=par[6]
  b6=par[7]
  r1=par[8]
  r2=par[9]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  sum((y-yhat)^2)
  
}
############################

############################
regression_restricted_model_b1 <- function(par, data){
  
  b0=par[1]
  #b1=par[2]
  b2=par[2]
  b3=par[3]
  b4=par[4]
  b5=par[5]
  b6=par[6]
  #r1=par[7]
  r2=par[7]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  #yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  yhat = b0  + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  sum((y-yhat)^2)
}
############################


############################
regression_restricted_model_b2 <- function(par, data){
  
  b0=par[1]
  b1=par[2]
  #b2=par[2]
  b3=par[3]
  b4=par[4]
  b5=par[5]
  b6=par[6]
  r1=par[7]
  #r2=par[7]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  #yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  yhat = b0  + b1* u(r1, x1)  + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  sum((y-yhat)^2)
}
####################################



############################
regression_restricted_model_b3 <- function(par, data){
  
  b0=par[1]
  b1=par[2]
  b2=par[3]
  #b3=par[3]
  b4=par[4]
  b5=par[5]
  b6=par[6]
  r1=par[7]
  r2=par[8]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  #yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  yhat = b0  + b1* u(r1, x1)  + b2*u(r2, x2) + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  sum((y-yhat)^2)
}
####################################



############################
regression_restricted_model_b4 <- function(par, data){
  
  b0=par[1]
  b1=par[2]
  b2=par[3]
  b3=par[4]
  #b4=par[4]
  b5=par[5]
  b6=par[6]
  r1=par[7]
  r2=par[8]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  #yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  yhat = b0  + b1* u(r1, x1)  + b2*u(r2, x2) + b3*x3  + b5*x2*x3 + b6*x1*x3
  sum((y-yhat)^2)
}
####################################


############################
regression_restricted_model_b5 <- function(par, data){
  
  b0=par[1]
  b1=par[2]
  b2=par[3]
  b3=par[4]
  b4=par[5]
  #b5=par[5]
  b6=par[6]
  r1=par[7]
  r2=par[8]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  #yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  yhat = b0  + b1* u(r1, x1)  + b2*u(r2, x2) + b3*x3  + b4*x1*x2 + b6*x1*x3
  sum((y-yhat)^2)
}
####################################



############################
regression_restricted_model_b6 <- function(par, data){
  
  b0=par[1]
  b1=par[2]
  b2=par[3]
  b3=par[4]
  b4=par[5]
  b5=par[6]
  #b6=par[6]
  r1=par[7]
  r2=par[8]
  
  x1=data[,1]
  x2=data[,2]
  x3=data[,3]
  y=data[,4]
  
  #yhat = b0 + b1* u(r1, x1) + b2*u(r2, x2) + b3*x3 + b4*x1*x2 + b5*x2*x3 + b6*x1*x3
  yhat = b0  + b1* u(r1, x1)  + b2*u(r2, x2) + b3*x3  + b4*x1*x2 + b5*x2*x3
  sum((y-yhat)^2)
}
####################################



#Setup
df = read.csv("project1.11.csv")


bcount = 100
#start1= c(3000, 200, 0.5, 0.01, 0.3, 0.1, 0.02, 0.8, 0.9 )
#start2 =c(3000, 200, 0.5, 0.01, 0.3, 0.1, 0.3)
start1 = 1:9
start2 = 1:7
test_model = regression_restricted_model_b1

orig = nlminb(start = start1, objective = regression_full_model, data=df, control = list(iter.max=5000, eval.max=5000))

#Hypothesis
test_m = list()
test_res = c()
bootstrap_data = bootstrap(df, bcount)
for(i in 1:bcount){
  
  test_m[[i]] = nlminb(start = start2, objective = test_model, data=bootstrap_data[[i]], control = list(iter.max=5000, eval.max=5000)) #Linear Regression
  test_res[i]=test_m[[i]]$objective
}

#Pvalue
orig

test_res
orig$objective

sum(orig$objective>test_res)/bcount
plot(density(test_res))
