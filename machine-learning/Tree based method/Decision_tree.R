#### Decision Trees 
We will have a look at the `Carsseats`data using th `tree`package in R. 
We create a binary response variable `High`(for high sales), and we include it i the same dataframe.

```{r} 
library(ISLR)
library(tree)
attach(Carseats)

hist(Sales, 30)
median(Sales)
mean(Sales)
High=ifelse(Sales<=8, "No", "Yes")
Carseats=data.frame(Carseats,High)
```

Now we fit a tree to these data, and summarize and plot it. Notice that we have to _exclude_ `Sales` from
the right-hand side of the formula, because the response is derived from it.


```{r}
tree.carseats=tree(High~. -Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)
```


For detailed summary of the tree, print it:
```{r} 
tree.carseats
```

Lets create a training and test set (250, 150) split of the 400 observations, grow the tree on the training set, and evaluate its performance on the test set.

```{r}
set.seed(1)
train=sample(1:nrow(Carseats), 250)
tree.carseats=tree(High~. - Sales, Carseats, subset=train)

plot(tree.carseats)
text(tree.carseats, pretty=0)


tree.pred=predict(tree.carseats, Carseats[-train,], type="class")

conf=with(Carseats[-train,], table(tree.pred, High))
acc= mean(with(Carseats[-train,], tree.pred==High))

print(conf)
print(acc)
```

This tree was grown to full depth, and might be to variable. We now use CV to prune it.
```{r}
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)

par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$size ,  cv.carseats$k,type="b")



prune.carseats=prune.misclass(tree.carseats, best=8)
plot(prune.carseats);text(prune.carseats,pretty=0)
```

Now lets evaluate this pruned tree on the test data.

```{r}
tree.pred=predict(prune.carseats, Carseats[-train,], type="class")
conf.prune=with(Carseats[-train,], table(tree.pred, High))

acc.prune=mean(with(Carseats[-train,], tree.pred==High))

print(acc.prune)
print(conf.prune)
```

It has done about the same as our original tree. 
So pruning did not hur our missclassification errors, and gave us a simpler tree.

