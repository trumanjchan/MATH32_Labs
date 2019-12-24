rm(list=ls(all=TRUE))

# Question 1
#x = seq(from=-5, to=5, by=0.01) 

#y = pnorm(x, mean=0, sd=1)
#plot(x, y, type='l')
#y = pnorm(x, mean=0, sd=2)
#plot(x, y, type='l')
#y = pnorm(x, mean=0, sd=3)
#plot(x, y, type='l')

# In console: pnorm(0, mean=0, sd=1) = 0.5, pnorm(0, mean=0, sd=2) = 0.5, pnorm(0, mean=0, sd=3) = 0.5.
# Answer: No matter the mean and sd, the probability should always be 1. 

# Question 2
x = seq(from=0, to=1, by=0.01)
y = pnorm(x, mean=0.60, sd=sqrt(0.0175))
plot(x, y, type='l')
# b) The probability of a negative grade 'pnorm(-3)' is positive. The probability of a grade bigger than '1' is negative. This is acceptable because the slope from left to right is increasing, and leveling off.
# c) In console: 'A = 0.9', 'pnorm(A)' = 81.59%
# d) z = 0.75. A grade = 75% or higher. Thus, 25% chance a student gets an A.
# e) A = 0.5, B = 1.0, pnorm(A) = 0.69, pnorm(B) = 0.84, B - A = 15%, so 100 - 15 = 85% of a passing grade.

# Question 3
a = seq(from=-5, to=5, by=0.01)
y = pnorm(a, mean=0, sd=0.5)
plot(punif(a, min=-10, max=5))
