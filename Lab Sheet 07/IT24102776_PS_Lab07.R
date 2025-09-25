setwd("C:\\Users\\Manith Banula\\Desktop\\lab7")

#Exercise 
#Q1) Uniform distribution
prob_q1 <- (25 - 10) / 40
prob_q1

#Q2 Exponential distribution
lambda <- 1/3
prob_q2 <- pexp(2, rate=lambda)   # P(X ≤ 2)
prob_q2


#Q3 i) Normal distribution - P(X > 130)
prob_q3_i <- 1 - pnorm(130, mean=100, sd=15)
prob_q3_i



#Q3 ii) Normal distribution - 95th percentile
q3_ii <- qnorm(0.95, mean=100, sd=15)
q3_ii
