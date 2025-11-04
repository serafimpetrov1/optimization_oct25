# Load required libraries
library(ggplot2)
library(gridExtra)

set.seed(42)
n <- 100

# 1) Gamma and Beta samples with median lines

# Gamma parameters for Layoff
shape_layoff <- 2.0
scale_layoff <- 1.0

# Beta parameters for Bubble
a_bubble <- 2.0
b_bubble <- 5.0

layoff_cont <- rgamma(n, shape=shape_layoff, scale=scale_layoff)
bubble_cont <- rbeta(n, shape1=a_bubble, shape2=b_bubble)

median_layoff <- median(layoff_cont)
median_bubble <- median(bubble_cont)

p1 <- ggplot(data.frame(x=layoff_cont), aes(x)) +
  geom_histogram(binwidth=0.3, fill='skyblue', color='black') +
  geom_vline(xintercept=median_layoff, color='red', linetype='dashed') +
  ggtitle("Layoff: Gamma Distribution Samples") +
  xlab("Value") + ylab("Frequency") +
  annotate("text", x=median_layoff + 0.5, y=10, 
           label=paste("Median =", round(median_layoff,2)), color='red')

p2 <- ggplot(data.frame(x=bubble_cont), aes(x)) +
  geom_histogram(binwidth=0.03, fill='lightgreen', color='black') +
  geom_vline(xintercept=median_bubble, color='red', linetype='dashed') +
  ggtitle("Bubble: Beta Distribution Samples") +
  xlab("Value") + ylab("Frequency") +
  annotate("text", x=median_bubble + 0.05, y=14, 
           label=paste("Median =", round(median_bubble,2)), color='red')

# 2) Zero-inflated Gamma and Beta with threshold=0

p_zero_layoff <- 0.8
p_zero_bubble <- 0.85

layoff_cont_zi <- ifelse(runif(n) < p_zero_layoff, 0, rgamma(n, shape=shape_layoff, scale=scale_layoff/2))
bubble_cont_zi <- ifelse(runif(n) < p_zero_bubble, 0, rbeta(n, shape1=a_bubble, shape2=b_bubble))

p3 <- ggplot(data.frame(x=layoff_cont_zi), aes(x)) +
  geom_histogram(binwidth=0.1, fill='skyblue', color='black') +
  geom_vline(xintercept=0, color='red', linetype='dashed') +
  ggtitle("Layoff: Zero-inflated Gamma Distribution") +
  xlab("Value") + ylab("Frequency") +
  annotate("text", x=0.2, y=85, label="Threshold = 0", color='red')

p4 <- ggplot(data.frame(x=bubble_cont_zi), aes(x)) +
  geom_histogram(binwidth=0.02, fill='lightgreen', color='black') +
  geom_vline(xintercept=0, color='red', linetype='dashed') +
  ggtitle("Bubble: Zero-inflated Beta Distribution") +
  xlab("Value") + ylab("Frequency") +
  annotate("text", x=0.03, y=85, label="Threshold = 0", color='red')

# Arrange plots side-by-side
grid.arrange(p1, p2, ncol=2)
grid.arrange(p3, p4, ncol=2)

# 3) Bayesian simulation of event probabilities

library(VGAM) # for dbeta function (available in base R as well)

alpha_prior <- 2
beta_prior <- 8

# Sample probabilities from Beta prior
p_layoff <- rbeta(1, alpha_prior, beta_prior)
p_bubble <- rbeta(1, alpha_prior, beta_prior)

cat(sprintf("Sampled probability for Layoff event: %.3f\n", p_layoff))
cat(sprintf("Sampled probability for Bubble event: %.3f\n", p_bubble))

# Simulate binary events
layoff_binary <- rbinom(n, size=1, prob=p_layoff)
bubble_binary <- rbinom(n, size=1, prob=p_bubble)

df_bin <- data.frame(Layoff=layoff_binary, Bubble=bubble_binary)

# Contingency table
contingency_table <- table(df_bin$Layoff, df_bin$Bubble)
cat("\nContingency Table (Bayesian simulation):\n")
print(contingency_table)

# Plot prior and sampled probabilities
x <- seq(0, 1, length.out=200)
prior_density <- dbeta(x, alpha_prior, beta_prior)

df_prior <- data.frame(x=x, density=prior_density)

p5 <- ggplot(df_prior, aes(x=x, y=density)) +
  geom_line() +
  geom_vline(xintercept=p_layoff, color='blue', linetype='dashed', size=1) +
  geom_vline(xintercept=p_bubble, color='orange', linetype='dashed', size=1) +
  ggtitle("Prior distribution of event probabilities") +
  xlab("Probability") + ylab("Density") +
  annotate("text", x=p_layoff + 0.05, y=max(prior_density)*0.9, label="Sampled p_layoff", color='blue') +
  annotate("text", x=p_bubble + 0.05, y=max(prior_density)*0.8, label="Sampled p_bubble", color='orange')

print(p5)
