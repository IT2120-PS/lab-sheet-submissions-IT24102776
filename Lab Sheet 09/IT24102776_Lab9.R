setwd("C:\\Users\\Manith Banula\\Desktop\\Lab9")
getwd()

# Q1
memes <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)
t.test(memes, mu = 3, alternative = "two.sided")

# Q2
mice <- c(17.6, 20.6, 22.2, 15.3, 20.9, 21.0, 18.9, 18.9, 18.9, 18.2)
t.test(mice, mu = 25, alternative = "less")

result <- t.test(mice, mu = 25, alternative = "less")

result$statistic      
result$p.value        
result$conf.int       
mean(mice)     

# Q3
set.seed(123) # reproducible
sugar <- rnorm(30, mean = 9.8, sd = 0.05)

t.test(sugar, mu = 10, alternative = "greater")

# Exercise
set.seed(123)
baking <- rnorm(25, mean = 45, sd = 2)

t.test(baking, mu = 46, alternative = "less")


