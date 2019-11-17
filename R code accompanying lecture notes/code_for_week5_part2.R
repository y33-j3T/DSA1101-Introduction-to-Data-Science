install.packages('devtools')

library(devtools)
install_github("genomicsclass/tissuesGeneExpression")

library(tissuesGeneExpression)
data(tissuesGeneExpression)

head(e)
head(tissue)

table(tissue)

ind <- which(tissue != "placenta")
y <- tissue[ind]
X <- t( e[,ind] )

## install package 'class' if you haven't ##
library(class)
pred <- knn(train=X, test=X, cl=y, k=1)
mean(y != pred)

set.seed(1)
n_folds=10
folds_i <- sample(rep(1:n_folds, length.out = 183))
table(folds_i)

#install.packages('rafalib')
library(rafalib)
mypar()
Xsmall <- cmdscale(dist(X))
plot(Xsmall,col=as.fumeric(y))
legend("topleft",levels(factor(y)),fill=seq_along(levels(factor(y))))


## code for the first-fold only when k=1 ##
test_i <- which(folds_i == 1)
pred <- knn(train=Xsmall[ -test_i, ], test=Xsmall[ test_i, ], cl=y[ -test_i ], k=1)
table(true=y[test_i ], pred)
err=mean(y[ test_i ] != pred)
err

## 10-fold CV for k=1 ##
err=numeric(10)
for (j in 1:10) {
	test_i <- which(folds_i == j)
	pred <- knn(train=Xsmall[ -test_i, ], test=Xsmall[ test_i, ], cl=y[ -test_i ], k=1)
	err[j]=mean(y[ test_i ] != pred)
}
err
error=mean(err)
error


## 10-fold CV for k=1,2,...,15 ##
error=numeric(15)

for (k in 1:15) {
	err=numeric(10)
	for (j in 1:10) {
		test_i <- which(folds_i == j)
		pred <- knn(train=Xsmall[ -test_i, ], test=Xsmall[ test_i, ], cl=y[ -test_i ], k=k)
		err[j]=mean(y[ test_i ] != pred)
	}
      error[k]=mean(err)
}

plot(1:15, error, type="o",ylab="misclassification error",xlab="K",cex.axis=1,cex=2)









