rm(list=ls(all=TRUE))

print("RV 1/3: ")
f.1 = function(x) {
  sample( c(100, 50, 10), 1, prob=c(0.3, 0.5, 0.2) )
}
print("simulating 10000 times: ")
x = sapply(c(1:10000), f.1)     #returns result
#replicate(10000, f.1())
hist(x,breaks=c(0:100))
#plot(x)
cat("mean of simulation is: ",mean(x), "\n")
cat("which is close to the mean: ", c(100, 50, 10) %*% c(0.3, 0.5, 0.2), "\n")



print("RV 2/3: ")
f.2 = function(y) {
  coin = c(0,1)
  die1 = c(1,4)
  die2 = c(1,6)
  toss = sample(coin, 1, replace = TRUE)
  if (toss == 1)
    return(sample(die1, 1, replace = TRUE))
  else
    return(sample(die2, 1, replace = TRUE))
  #die1 = sample(x=4,size=1, replace=TRUE, prob=c(0.20833, 0.20833, 0.20833, 0.20833))
  #die2 = sample(x=2,size=1, replace=TRUE, prob=c(0.08333, 0.08333))
  #sumrolls = die1 + die2
}
print("simulating 10000 times: ")
y = sapply(c(1:10000), f.2)     #returns result
#hist(y, breaks = c(0:100))
cat("mean of simulation is: ",mean(y), "\n")



print("RV 3/3: ")
p = 0.5
q = 0.75
f.3 = function(z) {
  Y = rbinom(n = 100, size = 1, prob = p)
  Z = rbinom(n = 100, size = 1, prob = q)
  X = Y + Z
  plus = mean(sample(X,10000,replace=TRUE))
}
print("simulating 10000 times: ")
z = sapply(c(1:1), f.3)
cat("mean of simulation is: ",mean(z), "\n")



print("Continuous: ")
f.4 = function(t) {
  uniform = runif(n=100,min=-5,max=10)
}
print("simulating 10000 times: ")
t = sapply(c(1:10000), f.4)     #returns result
#replicate(10000, f.5())
cat("mean of simulation is: ",mean(t), "\n")