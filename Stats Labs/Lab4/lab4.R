#rm(list=ls(all=TRUE))
#load("C:/Users/iTrut/Desktop/Stats Labs/Lab4/concrete.Rdata")
#colnames(x)     #names of columns of x
#dim(x)     #dimensions of x
#length(y)     #checks length of y: strength of concrete
#class(x)     #x is a data frame
#class(y)     #y is a vector
#pairs(x)     #matrix of all possible scatterplots

#pdf("myplot.pdf",height=10,width=10)
#pairs(x)
#dev.off()

#mylm = lm(y ~ cement,data=x)
#plot(x$cement,y)
#lines(x$cement,predict(mylm))

#predict(mylm)     #in-sample prediction
#plot(y,predict(mylm))

#ypred = predict(mylm,newdata=xtest)     #out of sample prediction
#mean((ytest-ypred)^2)     #out of sample mean squared error

#Task 1
rm(list=ls(all=TRUE))
#load("concrete.Rdata")     #Change to your directory where you have concrete.Rdata.
load("C:/Users/iTrut/Desktop/Stats Labs/Lab4/concrete.Rdata")     #I use windows, and this is the directory of where my concrete.Rdata is located on my desktop.

print(summary(x))

pdf("boxandwhiskersX.pdf",height=20,width=20)
boxplot(x)
dev.off()

pdf("normalizedhistageX.pdf",height=20,width=20)
hist(x$age,probability = TRUE)
dev.off()

pdf("scattercolmnsX.pdf",height=20,width=20)
pairs(x)
dev.off()

#Task 2
#Best 3rd variable with cement and water: Guess: superplasticizer - makes high strength concrete
print("============================================================")

mylm = lm(y ~ cement + water + blast_furnace_slag, data=x)
print("_____blast_furnace_slag_____")
print(mean((y - predict(mylm))^2))                        #in-sample MSE
print(mean((ytest - predict(mylm, newdata=xtest))^2))     #out-of-sample MSE

mylm = lm(y ~ cement + water + fly_ash, data=x)
print("_____fly_ash_____")
print(mean((y - predict(mylm))^2))
print(mean((ytest - predict(mylm, newdata=xtest))^2))

mylm = lm(y ~ cement + water + superplasticizer, data=x)
print("_____superplasticizer_____")
print(mean((y - predict(mylm))^2))
print(mean((ytest - predict(mylm, newdata=xtest))^2))

mylm = lm(y ~ cement + water + coarse_agg, data=x)
print("_____coarse_agg_____")
print(mean((y - predict(mylm))^2))
print(mean((ytest - predict(mylm, newdata=xtest))^2))

mylm = lm(y ~ cement + water + fine_agg, data=x)
print("_____fine_agg_____")
print(mean((y - predict(mylm))^2))
print(mean((ytest - predict(mylm, newdata=xtest))^2))

mylm = lm(y ~ cement + water + age, data=x)
print("_____age_____")
print(mean((y - predict(mylm))^2))
print(mean((ytest - predict(mylm, newdata=xtest))^2))

print("============================================================")
print("Out of all the in-sample MSEs, 'age' has the lowest number. Thus it performs best, and should be the third variable. With the xtest data, the out-sample MSE is still the lowest out of the other out-samples.")

#mylm = lm(y ~ cement + water + blast_furnace_slag, data=x)
#print(mean((y - predict(mylm))^2))
#print(mean((ytest - predict(mylm, newdata=xtest))^2))

#y = ??0 + ??1*cement + ??2*water     #coef(mylm)     set beta1 = coef(mylm)[1], beta2, beta3
#mean((y - predict(mylm))^2) 