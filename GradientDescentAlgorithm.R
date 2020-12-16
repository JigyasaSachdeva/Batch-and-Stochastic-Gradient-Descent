#IDS 575- Assignment 2
#Jigyasa Sachdeva
#UIN- 664791188


#Question 1 (e)

library(ISLR)
data(Auto)

library(dplyr)
#Selecting all the numeric variables for simplicity
num_data <- Auto %>% select(-name)

#Linear regression model using lm function
linear_mod <- lm(mpg~., data = num_data)
summary(linear_mod)
#2 most important features are weight and acceleration




#Since data is to be scaled before applying gradient descent:
scale_data = function(a)
{
  b <- (a- mean(a))/sd(a)
  return(b)
}

#Applying scale_data function to all inpendent variables in the dtaaset
scaled_auto <- sapply(num_data, scale_data)
#Converting into a data frame
scaled_auto <- as.data.frame(scaled_auto)


#Linear regression model using lm function
linear_mod <- lm(mpg~., data = scaled_auto)
summary(linear_mod)
coef <- linear_mod$coefficients
coef
#2 most important features are weight and acceleration



#Dividing x and y matrix:
y <- scaled_auto$mpg
y <- as.data.frame(y)
colnames(y) <- "mpg"
x <- scaled_auto %>% select(-mpg)
#Adding a column with all 1: multiply with theta_0
x <- cbind(1, x)
x <- as.matrix(x)




#Batch Gradient descent 
#There are 7 features: there will be 8 thetas (theta 0, theta 1 ... theta 7)
#Initializing all with 0 
theta <- c(0,0,0,0,0,0,0,0) 

#learning rate = 0.05
alpha <- 0.05

#Number of observations
m <- nrow(y)



#Batch Gradient Descent Algorithm:
options(scipen =99)
for(epoch in 1:1500)
{
  for(j in 1:length(theta))
  {
    theta[j] <- theta[j] - alpha*(1/m)*sum((x%*%theta - y)*x[,j])
    #Updating theta for every variable using batch gradient descent algorithm
  }
  print(theta)
}
theta 





#Stochastic Gradient Descent Algorithm: 
theta_s <- c(0,0,0,0,0,0,0,0) 
for(epoch in 1:1500)
{
  for(i in 1: m)
  {
    for(j in 1:length(theta_s))
      {
      theta_s[j] <- theta_s[j] - alpha*(1/m)*sum((x[i,]%*%theta_s - y[i,])*x[i,j])
      #Updating theta for every variable using batch stochastic descent algorithm
    }
  }
  print(theta_s)
}
theta_s

comparison <- cbind(coef, theta, theta_s)
#coef: for linear regression using lm
#theta: batch gradient descent coefficients
#theta_s: stochastic gradient descent algorithm coefficients
comparison











