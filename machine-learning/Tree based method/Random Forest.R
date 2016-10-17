### Random Forest
These methods use trees as building blocks to build more complex models. 
Here we will use the Boston Houseing data to explore random forrest. 
These data are in the `MASS`packages.
It gives housing values others statistics in each of 506 suburbs of Boston based on a 1970 census.

Random Forest build lots of bushy trees, and then average them to reduce the variance.

```{r} 
library(ISLR)
library(randomForest)
library(MASS)

set.seed(101)
train=sample(1:nrow(Boston),300)
```

Lets fit a random forest and see how well its performs. We will use 
the respone `medv`the median housing value (in \$1K dollars)

```{r}
rf.boston=randomForest(medv~., data=Boston, subset=train)
print(rf.boston)
``` 

The MSR and % variance explained are based on OOB o _out-of-bag_
estimates, a very clever device in random forest to get honest error estimates. 
The model reports that `mtry=4`, which is the number of variables randomly chosen at each split. 
Since $p=13$ here, we could try all 13 possible values of `mtry`. We will do so, record the results, and make a plot.

```{r}
oob.err=double(13)
test.err=double(13)

for (mtry in 1:13){
	fit=randomForest(medv~., data=Boston, subset=train, mtry=mtry, ntree=400)
	oob.err[mtry]=fit$mse[400]
	pred=predict(fit, Boston[-train,])
	test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
	cat(mtry, " ")
}

matplot(1:mtry, cbind(test.err, oob.err), pch=19, col=c("red", "blue"), type="b", ylab="Mean squared Error")

legend("topright", legend=c("Test", "OOB"), pch=19, col=c("red", "Blue"))
```

Although the test-error curve drops below the OOB curve, these are estimates based on data, and 
som have their own standard errors (which are typically quite large). Notice that the points at the end 
with `mtry=13`correspond to bagging.




