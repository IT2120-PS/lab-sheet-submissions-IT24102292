setwd("C:\Users\melfe\OneDrive\Desktop\PS\IT24102292")

#1
#P(10 <= x <= 25)
punif(25,min = 0,max=40, lower.tail= TRUE) - punif(10, min = 0,max = 40, lower.tail = TRUE)

#2
#P(X <= 2)
pexp(2, rate = 1/3, lower.tail = TRUE)

#3
# i P(X > 130)
1 - pnorm(130, mean = 100, sd = 15, lower.tail = TRUE)

# ii 95th percentile
qnorm(0.95,mean = 100, sd = 15, lower.tail = TRUE)
