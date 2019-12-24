#Partner: Izcalli Rios-Aguirre

# clear memory
rm(list=ls(all=TRUE))

# simulate data
set.seed(32)
npts = 32
x = seq(from=-1,to=1,length.out=npts)
y = x + rnorm(n=npts,mean=0,sd=0.5)

# split into test and train
trainind = seq(from=1,to=32,by=2)
xtrain = x[trainind]
ytrain = y[trainind]
xtest = x[-trainind]
ytest = y[-trainind]

# plot training set data
#par(mfrow=c(2,1))
#plot(xtrain, ytrain, pch=20)

# Creates test and train MSE vectors with 16 slots, full of zeroes
numpow = c(1:16)
testMSE = numeric(length = 16L)
trainMSE = numeric(length = 16L)

# Calculates the MSE for train set 16 times
for (i in numpow) {
xnam = paste("I(x**",c(i:numpow[i]),")",sep='')
regression = as.formula(paste("y ~ ", paste(xnam, collapse="+")))
train = data.frame(x=xtrain,y=ytrain)
mylm = lm(regression, data=train)
MSE = mean((predict(mylm) - ytrain)^2)
trainMSE[i] = MSE
}

# Calculates the MSE of test set 16 times
for (i in numpow) {
xnam = paste("I(x**",c(i:numpow[i]),")",sep='')
regression = as.formula(paste("y ~ ", paste(xnam, collapse="+")))
test = data.frame(x=xtest,y=ytest)
mylm = lm(regression, data=test)
MSE = mean((predict(mylm) - ytest)^2)
testMSE[i] = MSE
}

#plots trainMSE and testMSE on the same graph
plot(numpow, trainMSE, type = 'l', col = 'green', ylab = "MSE")
lines(numpow, testMSE, type = 'l', col = 'red')

# plot training set predictions
# note that they go right through the data points
#lines(xtrain, predict(mylm), col='red')

# plot test set data
#plot(xtest, ytest, pch=18, ylim=c(-10,10))

# compute test predictions
#test = data.frame(x=xtest,y=ytest)
#testpred = predict(mylm, newdata=test)

# plot test set predictions
#lines(xtest, testpred, col='blue')




