rm(list=ls(all=TRUE))
# Pareto

dpareto = function(x, shape, scale = 1, log = FALSE) {
  if (shape <= 0) {
    stop("shape should be strictly positive.")
  }
  if (scale <= 0) {
    stop("scale should be strictly positive.")
  }
  d = ifelse(x >= scale, shape/scale*(scale/x)^(shape+1), 0)
  if (log) d = log(d)
  return(d)
}

ppareto = function(x, shape = 3, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  if (shape <= 0) {
    stop("shape should be strictly positive.")
  }
  if (scale <= 0) {
    stop("scale should be strictly positive.")
  }
  p = ifelse(x >= scale, 1-(scale/x)^shape, 0)
  if (!lower.tail) p = 1-p
  if (log.p) p = log(p)
  return(p)
}
pts = seq(from=1,to=5,by=0.01)
truecdf =  ((2*(-1 + pts)*pts*(15 + 2*pts*(-15 - 5*pts + 4*pts^3 + 16*pts^5)))/(1 - 2*pts)^2 - 15*log(-1 + 2*pts))/(16*pts^6)
plot(truecdf)

qpareto = function(p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  if (shape <= 0) {
    stop("shape should be strictly positive.")
  }
  if (scale <= 0) {
    stop("scale should be strictly positive.")
  }
  if (log.p) p = exp(p)
  if (!lower.tail) p = 1-p
  if (all(p >= 0 & p <= 1)) {
    return(scale*(1-p)^(-1/shape))
  } else {
    stop("p should be between 0 and 1.")
  }
}

rpareto = function(n = 10, shape = 3, scale = 1) {
  if (shape <= 0) {
    stop("shape should be strictly positive.")
  }
  if (scale <= 0) {
    stop("scale should be strictly positive.")
  }
  return(qpareto(runif(n), scale=scale, shape=shape))
}
n = sapply(c(1:3), rpareto)
print("Generating 3 samples of X and Y:")
print(n)

print("Show that the empirical CDF matches the exact CDF: See plots!")
Z = 0.5 * sapply(c(1:3), ppareto)
print(Z)
plot(Z, xaxt="n")
axis(1, at = seq(1, 5, by = 1), las=1)

print("E[Z]:")
Z <-- c(1, 1.5, 2)     #Took three average samples to use
p <-- c(0, 0.4375000, 0.4814815)
print(weighted.mean(Z,p))

print("Var[Z]:")
print(var(Z,p))
#TASK 1 ABOVE ^^^

#x = matrix(a,nrow=m,ncol=n) 
n = sapply(c(1:100), rpareto)
x = n[c(1:100)]     #How to put values into the MxN matrix?

