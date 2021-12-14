# Generate random uncorrelated data
set.seed(42)
n = 20
x <- rnorm(n)
y <- rnorm(n)

result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# If we are only interested in positive correlations, could we just perform a one-tailed test?
result <- cor.test(x,y,alternative="greater")
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)


# Perform an alternative test: Spearman's correlation
result <- cor.test(x,y,method="spearman")
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr_Spearman=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# Justify Spearman's correlation because data are not normally distributed?
result <- shapiro.test(x)
hist(x)
text(1,6,sprintf("Normality test: p=%.3f",result$p.value))
result <- shapiro.test(y)
hist(y)
text(1,6,sprintf("Normality test: p=%.3f",result$p.value))

# Unfortunately, the Shapiro-Wilk test does not detect non-normality
# But we can double down: just as we tried an alternative correlation (Spearman),
# we can try an alternative normality test: Q-Q plots.
library(car)
qqPlot(x,id=FALSE)
mtext("Q-Q plot for x",side=3,line=1)

qqPlot(y,id=FALSE)
mtext("Q-Q plot for y",side=3,line=1)

