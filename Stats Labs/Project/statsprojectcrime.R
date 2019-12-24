rm(list=ls(all=TRUE))
load("crime.Rdata")
#load("C:/Users/iTrut/Desktop/Stats Labs/Project/crime.Rdata")
#Columns 1,2,3,4,5 are identifiers. Remove N/A's, or don't use them.

#Loops through each column. If N/A, replace with that column's mean.
num <- c(1:128)
for (val in num) {
  if ( sum(is.na(x[val])) > 0 ) {
    valuemean = mean(x[,val],na.rm = TRUE)
    narows = which(is.na(x[,val]))
    x[narows,val] = valuemean
  }
}

#Cors for loop to help find best variables. Higher is better.
correlation <- c(6:127)
for (val in correlation) {
  if ( cor(x$ViolentCrimesPerPop, x[val]) > 0.5 ) {
    #Don't overfit! Don't have too many variables in lm's
    print(val)
  }
}
#8,23,34,38,44,46,47,56 have cors w/x[128] > 0.5
#racepctblack, pctWPubAsst, PctPopUnderPov, PctUnemployed,
#MalePctDivorce, FemalePctDiv, TotalPctDiv, PctIlleg
#Found that MSE of Cors variables are close to mean of x[128].

#Implement 10-fold Cross Validation. Lower MSE is good! Finds best variables.
newx = x[,-c(1,2,3,4,5,128)]
# form the subsets
shufflerows = sample(c(1:nrow(newx)))
rowsperfold = floor(nrow(newx)/10)

mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  newxtrain = x[trainrows,]
  newxtest = x[testrows,]
  mymod = lm(ViolentCrimesPerPop ~ racepctblack + pctWPubAsst + PctPopUnderPov + PctUnemployed + MalePctDivorce + FemalePctDiv + TotalPctDiv + PctIlleg, data = newxtrain)
  ypred = predict(mymod, newdata=newxtest)
  mse[i] = mean((ypred - newxtest$ViolentCrimesPerPop)^2)
}
print("Overall MSE from 10-Fold Cross Validation: ")
print("Compare this Overall MSE with training and test sets w/All Variables, and w/Best Variables.")
print(mean(mse))

#Plot pairs(x) 10 variables at a time. This is one way to find correlation between variables, however very slow because there are so many variables, and not all are compared with each other. Thus, make a linear model and check correlation using 'summary(mylm)'
pdf("pairsx10.pdf",height=10,width=10)
pairs(x[,c(6:15)])
dev.off()
pdf("pairsx20.pdf",height=10,width=10)
pairs(x[,c(16:25)])
dev.off()
pdf("pairsx30.pdf",height=10,width=10)
pairs(x[,c(26:35)])
dev.off()
pdf("pairsx40.pdf",height=10,width=10)
pairs(x[,c(36:45)])
dev.off()
pdf("pairsx50.pdf",height=10,width=10)
pairs(x[,c(46:55)])
dev.off()
pdf("pairsx60.pdf",height=10,width=10)
pairs(x[,c(56:65)])
dev.off()
pdf("pairsx70.pdf",height=10,width=10)
pairs(x[,c(66:75)])
dev.off()
pdf("pairsx80.pdf",height=10,width=10)
pairs(x[,c(76:85)])
dev.off()
pdf("pairsx90.pdf",height=10,width=10)
pairs(x[,c(86:95)])
dev.off()
pdf("pairsx100.pdf",height=10,width=10)
pairs(x[,c(96:105)])
dev.off()
pdf("pairsx110.pdf",height=10,width=10)
pairs(x[,c(106:115)])
dev.off()
pdf("pairsx120.pdf",height=10,width=10)
pairs(x[,c(116:125)])
dev.off()
pdf("pairsx128.pdf",height=10,width=10)
pairs(x[,c(126:128)])
dev.off()

#Finds 1-3 star variables relating to ViolentCrimesPerPop. (Use 'summary(mylm)' in console)
mylm = lm(ViolentCrimesPerPop ~ population + householdsize + racepctblack + racePctWhite + racePctAsian + racePctHisp + agePct12t21 + agePct12t29 + agePct16t24 + agePct65up + numbUrban + pctUrban + medIncome + pctWWage + pctWFarmSelf + pctWInvInc + pctWSocSec + pctWPubAsst + pctWRetire + medFamInc + perCapInc + whitePerCap + blackPerCap + indianPerCap + AsianPerCap + OtherPerCap + HispPerCap + NumUnderPov + PctPopUnderPov + PctLess9thGrade + PctNotHSGrad + PctBSorMore + PctUnemployed + PctEmploy + PctEmplManu + PctEmplProfServ + PctOccupManu + PctOccupMgmtProf + MalePctDivorce + MalePctNevMarr + FemalePctDiv + TotalPctDiv + PersPerFam + PctFam2Par + PctKids2Par + PctYoungKids2Par + PctTeen2Par + PctWorkMomYoungKids + PctWorkMom + NumIlleg + PctIlleg + NumImmig + PctImmigRecent + PctImmigRec5 + PctImmigRec8 + PctImmigRec10 + PctRecentImmig + PctRecImmig5 + PctRecImmig8 + PctRecImmig10 + PctSpeakEnglOnly + PctNotSpeakEnglWell + PctLargHouseFam + PctLargHouseOccup + PersPerOccupHous + PersPerOwnOccHous + PersPerRentOccHous + PctPersOwnOccup + PctPersDenseHous + PctHousLess3BR + MedNumBR + HousVacant + PctHousOccup + PctHousOwnOcc + PctVacantBoarded + PctVacMore6Mos + MedYrHousBuilt + PctHousNoPhone + PctWOFullPlumb + OwnOccLowQuart + OwnOccMedVal + OwnOccHiQuart + RentLowQ + RentMedian + RentHighQ + MedRent + MedRentPctHousInc + MedOwnCostPctInc + MedOwnCostPctIncNoMtg + NumInShelters + NumStreet + PctForeignBorn + PctBornSameState + PctSameHouse85 + PctSameCity85 + PctSameState85 + LemasSwornFT + LemasSwFTPerPop + LemasSwFTFieldOps + LemasSwFTFieldPerPop + LemasTotalReq + LemasTotReqPerPop + PolicReqPerOffic + PolicPerPop + RacialMatchCommPol + PctPolicWhite + PctPolicBlack + PctPolicHisp + PctPolicAsian + PctPolicMinor + OfficAssgnDrugUnits + NumKindsDrugsSeiz + PolicAveOTWorked + LandArea + PopDens + PctUsePubTrans + PolicCars + PolicOperBudg + LemasPctPolicOnPatr + LemasGangUnitDeploy + LemasPctOfficDrugUn + PolicBudgPerPop, data=x)
 
#From the above, simplify to a new lm with just star variables.
newlm = lm(ViolentCrimesPerPop ~ racepctblack + pctUrban + pctWWage + pctWFarmSelf + pctWInvInc + pctWRetire + whitePerCap + OtherPerCap + PctPopUnderPov + PctEmploy + PctEmplManu + MalePctNevMarr + PctKids2Par + PctWorkMom + PctIlleg + PctNotSpeakEnglWell + PersPerOccupHous + PersPerRentOccHous + PctPersDenseHous + HousVacant + PctVacantBoarded + PctVacMore6Mos + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet + PolicBudgPerPop, data=x)
#From the above, simplify to a new lm with just star variables.
newlm1 = lm(ViolentCrimesPerPop ~ racepctblack + pctUrban + pctWWage + pctWInvInc + pctWRetire + whitePerCap + OtherPerCap + PctPopUnderPov + PctEmploy + PctEmplManu + PctKids2Par + PctWorkMom + PctIlleg + PctPersDenseHous + HousVacant + PctVacantBoarded + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet, data=x)
#Take out two and one star variables
newlm2 = lm(ViolentCrimesPerPop ~ racepctblack + pctUrban + pctWWage + pctWRetire + PctPopUnderPov + PctKids2Par + PctWorkMom + PctIlleg + PctPersDenseHous + HousVacant + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet, data=x)
#The below linear model only has three star variables. (Use 'summary(newlm3)' in console)
newlm3 = lm(ViolentCrimesPerPop ~ racepctblack + pctUrban + pctWWage + pctWRetire + PctPopUnderPov + PctKids2Par + PctWorkMom + PctPersDenseHous + HousVacant + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet, data=x)

#Plots the linear model with only three star variables against ViolentCrimesPerPop to see correlation.
plot(predict(newlm3), x$ViolentCrimesPerPop, xlab = "newlm3", ylab = "ViolentCrimesPerPop")
abline(a=0, b=1)

#Linear model with all variables found using cors loop
mylm1 = lm(ViolentCrimesPerPop ~ racepctblack + pctWPubAsst + PctPopUnderPov + PctUnemployed + MalePctDivorce + FemalePctDiv + TotalPctDiv + PctIlleg, data = x)
plot(predict(mylm1), x$ViolentCrimesPerPop)
#took out PctPopUnderPov and PctUnemployed for low P-value
mylm2 = lm(ViolentCrimesPerPop ~ racepctblack + MalePctDivorce + FemalePctDiv + TotalPctDiv + PctIlleg, data = x)
plot(predict(mylm2), x$ViolentCrimesPerPop, xlab = "mylm2", ylab = "ViolentCrimesPerPop")
abline(a=0, b=1)

#ViolentCrimesPerPop: total number of violent crimes per 100K popuation {GOAL}
#racepctblack: percentage of population that is african american
#pctUrban: percentage of people living in areas classified as urban
# -pctWWage: percentage of households with wage or salary income in 1989
#pctWRetire: percentage of households with retirement income in 1989 
#PctPopUnderPov: percentage of people under the poverty level
#PctKids2Par: percentage of kids in family housing with two parents
#PctWorkMom: percentage of moms of kids under 18 in labor force
#PctPersDenseHous: percent of persons in dense housing (more than 1 person per room)
#HousVacant: number of vacant households
#RentLowQ: rental housing - lower quartile rent
#MedRent: median gross rent
# -MedOwnCostPctIncNoMtg: median owners cost as a percentage of household income - for owners without a mortgage
#NumStreet: number of homeless people counted in the street

#BASED ON THE ABOVE 3-STAR VARIABLES, WE WILL PLOT THE ONES RELATING TO VIOLENT CRIMES AND SEE THEIR CORRELATION - Took out pctWWage because it's P value is close to value 1 compared to the other variables in summary(newlm3), and took out MedOwnCostPctIncNoMtg because it seems like it has nothing to do with violent crime.
plot(x$racepctblack , x$ViolentCrimesPerPop)
plot(x$pctUrban , x$ViolentCrimesPerPop)
plot(x$pctWRetire , x$ViolentCrimesPerPop)
plot(x$PctPopUnderPov , x$ViolentCrimesPerPop)
plot(x$PctKids2Par , x$ViolentCrimesPerPop)
plot(x$PctWorkMom , x$ViolentCrimesPerPop)
plot(x$PctPersDenseHous , x$ViolentCrimesPerPop)
plot(x$HousVacant , x$ViolentCrimesPerPop)
plot(x$RentLowQ , x$ViolentCrimesPerPop)
plot(x$MedRent , x$ViolentCrimesPerPop)
plot(x$NumStreet , x$ViolentCrimesPerPop)

#Using variables from mylm2
plot(x$racepctblack , x$ViolentCrimesPerPop)
plot(x$MalePctDivorce , x$ViolentCrimesPerPop)
plot(x$FemalePctDiv , x$ViolentCrimesPerPop)
plot(x$TotalPctDiv , x$ViolentCrimesPerPop)
plot(x$PctIlleg , x$ViolentCrimesPerPop)

#Given the plots from above, keep only the variables with less spread: so take out pctUrban.
#Associate similar plots together on the same PDF
pdf("Top.pdf",height=10,width=10)
plot(x$ViolentCrimesPerPop, x$PctKids2Par + x$PctWorkMom, xlab = "ViolentCrimesPerPop", ylab = "PctKids2Par + PctWorkMom")
dev.off()

pdf("Middle.pdf",height=10,width=10)
plot(x$ViolentCrimesPerPop, x$pctWRetire + x$PctWorkMom + x$MedRent, xlab = "ViolentCrimesPerPop", ylab = "pctWRetire + PctWorkMom + MedRent")
dev.off()

pdf("Bottom.pdf",height=10,width=10)
plot(x$ViolentCrimesPerPop, x$racepctblack + x$PctPopUnderPov + x$PctPersDenseHous + x$HousVacant + x$NumStreet, xlab = "ViolentCrimesPerPop", ylab = "racepctblack + PctPopUnderPov + PctPersDenseHous + HousVacant + NumStreet")
dev.off()

# Plot with all best three star variables.
plot(x$ViolentCrimesPerPop, x$racepctblack + x$PctPopUnderPov + x$PctPersDenseHous + x$HousVacant + x$NumStreet + x$pctWRetire + x$PctWorkMom + x$MedRent + x$PctKids2Par + x$PctWorkMom, xlab = "ViolentCrimesPerPop", ylab = "Best 3-Star Variables")

print("The Mean of x$ViolentCrimesPerPop:")
print(mean(x[,128]))
meanCrimePerPop = mean(x[,128])
print("The Mean of Squared Error (Variance) of x$ViolentCrimesPerPop:")     #The closer MSE value to 0, the better!
print(mean( (meanCrimePerPop - x$ViolentCrimesPerPop )^2 ))
#We found the best predictors of violent crime in the above code (Lines 72 & 75), and created three separate plots where similar variables are added together and plotted against what we're trying to predict (x$ViolentCrimePerPop). If you look at the three PDFs exported, Top.PDF, Middle.PDF, and Bottom.PDF, you can see the mean of each of the graphs is near 23.8%, which is the mean of x$ViolentCrimePerPop. Thus, the variables that make those graphs plotted against x$ViolentCrimesPerPop are good combinations that allow us to predict what eventually might lead to violent crimes.

print("__________W/All Variables________________________________________")

# make training and test sets
trainfrac = 0.80
nrows = ceiling(0.8*nrow(x))

# randomly choose "nrows" number of rows for the training set
trainrows = sample(c(1:nrow(x)), nrows)

# form the training set of data set x, and the test set of x (everything not in training set)
xTrain = x[trainrows, ]
xTest  = x[-trainrows, ]

# Linear Model of all variables -c(1:5), with 'xTrain' data set
myModel = lm(ViolentCrimesPerPop ~ population + householdsize + racepctblack + racePctWhite + racePctAsian + racePctHisp + agePct12t21 + agePct12t29 + agePct16t24 + agePct65up + numbUrban + pctUrban + medIncome + pctWWage + pctWFarmSelf + pctWInvInc + pctWSocSec + pctWPubAsst + pctWRetire + medFamInc + perCapInc + whitePerCap + blackPerCap + indianPerCap + AsianPerCap + OtherPerCap + HispPerCap + NumUnderPov + PctPopUnderPov + PctLess9thGrade + PctNotHSGrad + PctBSorMore + PctUnemployed + PctEmploy + PctEmplManu + PctEmplProfServ + PctOccupManu + PctOccupMgmtProf + MalePctDivorce + MalePctNevMarr + FemalePctDiv + TotalPctDiv + PersPerFam + PctFam2Par + PctKids2Par + PctYoungKids2Par + PctTeen2Par + PctWorkMomYoungKids + PctWorkMom + NumIlleg + PctIlleg + NumImmig + PctImmigRecent + PctImmigRec5 + PctImmigRec8 + PctImmigRec10 + PctRecentImmig + PctRecImmig5 + PctRecImmig8 + PctRecImmig10 + PctSpeakEnglOnly + PctNotSpeakEnglWell + PctLargHouseFam + PctLargHouseOccup + PersPerOccupHous + PersPerOwnOccHous + PersPerRentOccHous + PctPersOwnOccup + PctPersDenseHous + PctHousLess3BR + MedNumBR + HousVacant + PctHousOccup + PctHousOwnOcc + PctVacantBoarded + PctVacMore6Mos + MedYrHousBuilt + PctHousNoPhone + PctWOFullPlumb + OwnOccLowQuart + OwnOccMedVal + OwnOccHiQuart + RentLowQ + RentMedian + RentHighQ + MedRent + MedRentPctHousInc + MedOwnCostPctInc + MedOwnCostPctIncNoMtg + NumInShelters + NumStreet + PctForeignBorn + PctBornSameState + PctSameHouse85 + PctSameCity85 + PctSameState85 + LemasSwornFT + LemasSwFTPerPop + LemasSwFTFieldOps + LemasSwFTFieldPerPop + LemasTotalReq + LemasTotReqPerPop + PolicReqPerOffic + PolicPerPop + RacialMatchCommPol + PctPolicWhite + PctPolicBlack + PctPolicHisp + PctPolicAsian + PctPolicMinor + OfficAssgnDrugUnits + NumKindsDrugsSeiz + PolicAveOTWorked + LandArea + PopDens + PctUsePubTrans + PolicCars + PolicOperBudg + LemasPctPolicOnPatr + LemasGangUnitDeploy + LemasPctOfficDrugUn + PolicBudgPerPop, data=xTrain)

# plot predictions vs the truth
plot(predict(myModel), xTrain$ViolentCrimesPerPop, xlab = "myModel", ylab = "ViolentCrimesPerPop")
abline(a=0, b=1)  # if the model were perfect, all points would be on this line

# training MSE
print("Training MSE of myModel: ")
print(mean( (predict(myModel) - xTrain$ViolentCrimesPerPop)^2 ))

# test MSE
print("Test MSE of myModel: ")
print(mean( (predict(myModel, newdata=xTest) - xTest$ViolentCrimesPerPop)^2 ))

print("__________W/Best Variables_______________________________________")

#mynewModel = lm(ViolentCrimesPerPop ~ racepctblack + MalePctDivorce + FemalePctDiv + TotalPctDiv + PctIlleg + pctUrban + pctWWage + pctWRetire + PctKids2Par + PctWorkMom + PctPersDenseHous + HousVacant + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet, data=xTrain)
#Using summary(mynewModel), get rid of pctWWag, pctWRetire
mynewModel = lm(ViolentCrimesPerPop ~ racepctblack + MalePctDivorce + FemalePctDiv + TotalPctDiv + PctIlleg + pctUrban + PctKids2Par + PctWorkMom + PctPersDenseHous + HousVacant + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet, data=xTrain)
print(mean( (predict(mynewModel) - xTrain$ViolentCrimesPerPop)^2 ))
print(mean( (predict(mynewModel, newdata=xTest) - xTest$ViolentCrimesPerPop)^2 ))

# plot predictions vs the truth
plot(predict(mynewModel), xTrain$ViolentCrimesPerPop, xlab = "mynewModel", ylab = "ViolentCrimesPerPop")
abline(a=0, b=1)

# #Shows correlation between ViolentCrimePerPop and our best variables to verify whether they are our best variables (if there really is correlation)
# pdf("pairsBestVariables.pdf",height=20,width=20)
# pairs(x[,c(128,8,17,34,44,46,47,50,54,56,74,77,88,91,94,96)])
# dev.off()

# mynewModel = lm(ViolentCrimesPerPop ~ racepctblack + MalePctDivorce + FemalePctDiv + TotalPctDiv + PctIlleg, data=xTrain)
# print("Training MSE of mynewModel: ")
# print(mean( (predict(mynewModel) - xTrain$ViolentCrimesPerPop)^2 ))
# print("Test MSE of mynewModel: ")
# print(mean( (predict(mynewModel, newdata=xTest) - xTest$ViolentCrimesPerPop)^2 ))
# 
# # plot predictions vs the truth
# plot(predict(mynewModel), xTrain$ViolentCrimesPerPop)
# abline(a=0, b=1)
# 
# pdf("pairsBestVariables.pdf",height=20,width=20)
# pairs(x[,c(8,23,44,46,47,56)])
# dev.off()



