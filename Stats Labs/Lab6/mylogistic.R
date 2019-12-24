# clear memory
rm(list=ls(all=TRUE))

# set seed so that every run returns same results
# (for reproducibility)
set.seed(42)

# general purpose thresholding function
# for this to make statistical sense, 
# one should have 0 <= q <= 1.
mythresh <- function(q)
{
   q[which(q>=0.5)] = 1
   q[which(q<0.5)] = 0
   return(q)
}

# make artificial data matrix
n = 1000
p = 10
X = matrix(rnorm(n*p),nrow=n,ncol=p)
ones = matrix(1,nrow=n,ncol=1)
xmat = cbind(ones,X)
X = data.frame(X)

# make random set of coefficients
beta = matrix(rnorm(p+1),nrow=(p+1),ncol=1)

# make response vector
rawy = xmat %*% beta    # the symbol %*% stands for matrix multiplication in R
rawy = as.numeric(rawy)

# add a bit of noise
rawy = rawy + 0.75*rnorm(n)

# logistic transform and thresholding
# idea is to generate vector y with 0's and 1's
y = mythresh(1/(1+exp(-rawy)))

# everything above this line is just for artificial data generation
# actual code folllows here
actual0 = which(y==0)
actual1 = which(y==1)

# q is a vector of 0's and 1's (response variable)
# m is the data matrix

# fit logistic regression
mylogit = glm(y ~ ., data=X, family=binomial(link='logit'))

# note that predict will give you the log odds,
# not the actual probabilities
qfit = predict(mylogit)

# use logistic function to transform log odds to probabilities
pfit = mythresh(1/(1+exp(-qfit)))

# can also do this
pfit2 = mythresh(predict(mylogit, type='response'))

# confusion matrix!
confusion = matrix(nrow=2,ncol=2)

# let us say that 1 = POSITIVE
# let us say that 0 = NEGATIVE

# TRUE POSITIVES (TP)
# actual is 1, and prediction is 1
confusion[1,1] = sum(pfit[actual1]==1)

# FALSE POSITIVES (FP)
# actual is 0, but prediction is 1
confusion[2,1] = sum(pfit[actual0]==1)

# FALSE NEGATIVES (FN)
# actual is 1, but prediction is 0
confusion[1,2] = sum(pfit[actual1]==0)

# TRUE NEGATIVES (TN)
# actual is 0, and prediction is 0
confusion[2,2] = sum(pfit[actual0]==0)
print(confusion)

# classification percentage accuracy = 100*(TP + TN)/n
pctacc = 100*(confusion[1,1]+confusion[2,2])/n
print(pctacc)

