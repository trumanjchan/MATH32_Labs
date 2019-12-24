rm(list=ls(all=TRUE))
#load("mlb2015.Rdata")
load("C:/Users/iTrut/Desktop/Stats Labs/Lab10/mlb2015.Rdata")
#To verify
class(mlb2015)
dim(mlb2015)
#how many games each time played that season
mlb2015$G
#each team's winning percentage
mlb2015$W / mlb2015$G
mlb2015$W.Lpct

#lm.test = lm(W.Lpct ~ 0+W,data=mlb2015)
#coefficients(lm.test)
#print(1/162)
#mean(lm.test$residuals^2)




#Coefficients mean %
#Do not use W or L or Team Name as predictor values
mlb = mlb2015[,-c(33,34,1)] 

allmodels = list(NULL) 
n = nrow(mlb)
indiverrors = numeric(length=n) 
for (i in c(1:30)) 
{
  allmodels[[i]] = lm(W.Lpct ~ HR, data=mlb[-i,]) 
  truewinpct = mlb[i,]$W.Lpct 
  predwinpct = predict(allmodels[[i]],newdata=mlb[i,]) 
  indiverrors[i] = (truewinpct-predwinpct)^2 
}
print(mean(indiverrors))     #Need MSE's lower than 0.004084733

#Below is the lm with just HR as a predictor
mytestlm = lm(W.Lpct ~ HR, data = mlb)
#this linear model's overall mean
print("MSE of just HR as predictor: ")
print(mean(mytestlm$residuals^2))

#Using 'HR' mean value of 0.00408 as a cut-off, HR,R.G,PA,R,X2B
#RBI,BB,OBP,SLG,OPS,OPS.,HBP,SF,NumP,PAge,RAperG,ERA,tSho,SV,
#IP,H.1,R.1,ER,HR.1,BB.1 variables have a lower mean(indiverrors)

#thus, use those in a linear model
mylm = lm(W.Lpct ~ HR + R.G + PA + R + X2B + RBI + BB + OBP + SLG + OPS + OPS. + HBP + SF + NumP + PAge + RAperG + ERA + tSho + SV + IP + H.1 + R.1 + ER + HR.1 + BB.1, data = mlb)
print("MSE of all predictors found to have lower MSE than 0.00408: ")
print(mean(mylm$residuals^2))

#Using summary(mylm), keep only variables with P value lower than 0.5
lm = lm(W.Lpct ~ R.G + X2B + RBI + NumP + PAge, data = mlb)
print("MSE of predictors with lower than .5 P-value from above lm: ")
print(mean(lm$residuals^2))

newlm = lm(W.Lpct ~ RBI + NumP, data = mlb)
print("MSE of newlm: ")
print(mean(newlm$residuals^2))

#Using LOOCV, calculate Overall MSE of these two remaining variables
for (i in c(1:30)) 
{
  allmodels[[i]] = lm(W.Lpct ~ RBI + NumP, data=mlb[-i,]) 
  truewinpct = mlb[i,]$W.Lpct 
  predwinpct = predict(allmodels[[i]],newdata=mlb[i,]) 
  indiverrors[i] = (truewinpct-predwinpct)^2 
}
print("Compare this MSE value with our original (0.004084733): ")
print(mean(indiverrors))     #Need MSE's lower than 0.004084733

x <-  c(12, 29)
plot(mlb$W.Lpct, mlb$RBI + mlb$NumP)