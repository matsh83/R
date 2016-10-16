



#Predictor variables
X <- cbind(1,c(rnorm(100, 1, 1), rnorm(100,4,1)))


#Response variable
Y <- matrix(data=c(rep(0,100), rep(1,100)))


#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}


#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}



#Intial theta
initial_theta <- rep(0,ncol(X))

# initalize theta vector
theta<- c(0,0)

# Number of the observations
m <- nrow(X)


#Cost at inital theta
cost(initial_theta)

# Set learning parameter
alpha <- 0.001

#Number of iterations
iterations <- seq(0,10, by=0.01)

# updating thetas using gradient update
for(i in iterations)
{
  theta[1] <- theta[1] - alpha * (1/m) * sum(((X%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/m) * sum(((X%*%theta)- y)*X[,2])
}



# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set beta
beta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

### plot ###
color=c(rep("red", 100),rep("blue", 100))
plot(X[,2],Y, pch='|', col=color)

### prediction  ###
xmat=cbind(1, seq(-3, 7, by=0.1))

### the  odds prediction ###
pred_odds<-xmat%*%bet

### The prediction ###
pred=exp(pred_odds)/(1+exp(pred_odds))


### The prediction line ###
lines(xmat[,2], pred, col="darkgreen")


### We know that the red observation chould have a mean at 1 and the blue at 4
### therefore the best split is (4-1)/2+1 =2.5

## the simulated mean is for the red observation is
mean(X[1:100,2])

### and for the blue is
mean(X[101:200,2])

## therefore the simultated split should be (4.037597- 1.08535)/2+1= 2.476123
abline(v=(mean(X[101:200,2])-mean(X[1:100,2]))/2+1, col="brown") 


### and if this is correct should all the observation that is less than 0.5 in the Y-axel classify as the red observation ##
abline(h=0.5, col="brown")

## where we can seen that this seems to be correct classify ##

