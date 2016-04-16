#############################################
##########   LINEAR MODEL  ##################

#############################################
####### create the x and y variable #########
#############################################
set.seed(1)

x.1<-runif(100, 1, 10)  ### X-VARIABLE
e<-rnorm(100, 0, 1)     ### ERROR-TERM
y<-x.1+e                ### Y-VARIABLE


##### plot the x variable against y ######
plot(x.1, y, cex=0.8, col="blue", pch=19)



######### CREATE THE MATRIX  #############
x<-rep(1, length(x.1))      ### the constant
xmat<-cbind(x,x.1)          ### The xmatrix
ymat<-as.matrix(y)          ### the ymatrix

###### ESTIMATE THE PARAMETERS ############
beta<-solve(t(xmat)%*%xmat)%*%t(xmat)%*%ymat  ####  least square estimation 

rownames(beta)<-c('constant', 'cofficient');beta ### name the parameters


############# PREDICTION ##################
pred<-xmat%*%beta               #### predicted value
lines(x.1, pred, col="red")     #### plot the prediction


############# RESIDUALS ###################
resid<-y-pred                 ### The residuals in the model


############## PLot the data  ##################
par(mfrow=c(2,2))

plot(x.1, y, cex=0.8, col="blue", pch=19, main="x.1 against y")         ### x.1 against y    
lines(x.1, pred, col="red")                                             ### prediction line

plot(x.1, resid, cex=0.8, col="blue", pch=19, main='Residuals plot')    ### residuals plot
abline(h=0, col="red")                                                  ### prediction line

hist(resid, 30, main='Histogram', freq=F)                               ### histogram for the residuals 
lines(density(resid), col='red')                                        ### density plot





####################################################################
############ ANALYSIS OF THE RESIDUALS #############################
####################################################################

############# STD.ERROR #################
resid_error<-round((sum((resid)^2)/(length(y)-2))^0.5,4)        ### std.error
### 0.9411

### by the function ###
round(summary(lm(y~x.1))$sigma, 4)
### 0.9411

############# VISULIZE ##################
par(mfrow=c(2,2))
plot(x.1, y, cex=0.8, col="blue", pch=19)                       ### x.1 against vs y
lines(x.1, pred, col="red")                                     ### prediction

lower_bound<-pred-resid_error*1.96                              ### lower bound
lines(x.1, lower_bound, col="green")                            ### predicted lower bound 

higher_bound<-pred+resid_error*1.96                             ### higher bound
lines(x.1, higher_bound, col="green")                           ### predicted lower bound


out_of_bound_y<-y[c(higher_bound<y | lower_bound>y)]  
out_of_bound_x<-x.1[c(higher_bound<y | lower_bound>y)]  


points(out_of_bound_x, out_of_bound_y, col="violet" , pch=19)   ### 
length(out_of_bound_x)

### Rediduals plot ###
plot(x.1, resid, cex=0.8, col="blue", pch=19, main='Residuals plot') ### residuals plot
abline(h=0, col="red")

### Bound 
abline(h=resid_error*1.96, col="green")
abline(h=-resid_error*1.96, col="green")

points(
  x=x.1[c(higher_bound<y | lower_bound>y)],
  y=resid[c(higher_bound<y | lower_bound>y)], 
  col='violet',
  pch=19
)

plot(resid, cex=0.8, col="blue", pch=19) ### residuals plot
abline(h=0, col="red")

abline(h=resid_error*1.96, col="green")
abline(h=-resid_error*1.96, col="green")


points(
  y=resid[c(higher_bound<y | lower_bound>y)],
  x=which(c(higher_bound<y | lower_bound>y)==TRUE),
  col='violet',
  pch=19
)


##############################################################
################### beta std.error ###########################
##############################################################

sigma<-(sum((resid)^2)/(length(y)-2))

#######  std.error.para=((1/(length(x.1)-2)*sum(resid^2))/(sum((x.1-mean(x.1))^2)))^0.5
var_beta<-sigma*(solve(t(xmat)%*%xmat))
std.error<-diag(round(var_beta^0.5,5))

beta
