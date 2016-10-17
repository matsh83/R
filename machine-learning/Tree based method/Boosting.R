### Boosting 
These methods use trees as building blocks to build more complex models. 
Here we will use the Boston Houseing data to explore boosting. 
These data are in the `MASS`packages.
It gives housing values others statistics in each of 506 suburbs of Boston based on a 1970 census.

Boosting is a machine learning ensemble meta-algorithm for primarily reducing bias,
and a family of machine learning algorithms which convert weak learners to strong ones.
 
Lets load the packages and create a training data.

```{r} 
library(ISLR)
library(gbm)
library(MASS)

set.seed(101)

train=sample(1:nrow(Boston),300)
```
Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting tries to 
patch up the deficiencies of the current ensamble.
```{r}
boost.boston=gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=10000,
	shrinkage=0.01, interaction.depth=4)
summary(boost.boston)
plot(boost.boston, i="lstat")
plot(boost.boston, i="rm")
```

Lets make a prediction on the test set. With boosting, the number of trees is a tuning parameter, 
and if we have too many we can overfit. So we should use cross-validation to select the numver if trees.
Instead, we will compute the test error as a function of the numver of trees, and 
make a plot.

```{r}
n.trees=seq(from=100, to=10000, by=100)
predmat=predict(boost.boston, newdata=Boston[-train,], n.trees=n.trees)
## dim(predmat)
berr=with(Boston[-train,], apply((predmat-medv)^2,2,mean))
plot(n.trees, berr, pch=19, ylab="Mean Squared Error", xlab="Trees", 
	main="Boosting Test Error", cex=0.5, type="b", col="green")
```
good=tail(berr)
test=tail(berr)


boost.boston1=gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=10000,
	shrinkage=0.01, interaction.depth=1)

boost.boston2=gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=10000,
	shrinkage=0.01, interaction.depth=2)

boost.boston3=gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=10000,
	shrinkage=0.01, interaction.depth=3)

boost.boston4=gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=10000,
	shrinkage=0.01, interaction.depth=4)

boost.boston5=gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=10000,
	shrinkage=0.01, interaction.depth=5)



plot(n.trees, berr, pch=19, ylab="Mean Squared Error", xlab="Trees", 
	main="Boosting Test Error", cex=0.5, type="b", col="green")










