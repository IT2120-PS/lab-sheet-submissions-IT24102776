setwd("C:\\Users\\Manith Banula\\Desktop\\IT2401776_Lab8")

# Read data

weights <- data$Weight.kg.
data <- read.table("Exercise - LaptopsWeights.txt", header = TRUE)
fix(data)
attach(data)

head(weights)

#Q1
pop_mean <- mean(weights)
pop_sd <- sd(weights)

cat("Population mean =", pop_mean)
cat("Population standard deviation =", pop_sd)

#Q3
set.seed(123)  # for reproducibility
num_samples <- 25
sample_size <- 6

# Matrix to store samples
samples <- matrix(nrow = sample_size, ncol = num_samples)

# Generate samples
for (i in 1:num_samples) {
  samples[, i] <- sample(weights, size = sample_size, replace = TRUE)
}

# Name samples
colnames(samples) <- paste("Sample", 1:num_samples, sep = "_")

# Calculate sample means and sample standard deviations
sample_means <- apply(samples, 2, mean)
sample_sds <- apply(samples, 2, sd)

# Display first few sample means and SDs
head(sample_means)
head(sample_sds)

#Q3
mean_of_sample_means <- mean(sample_means)
sd_of_sample_means <- sd(sample_means)

cat("Mean of sample means =", mean_of_sample_means)
cat("Standard deviation of sample means =", sd_of_sample_means)