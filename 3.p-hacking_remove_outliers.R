# Generate random uncorrelated data
set.seed(42)
n = 20
x <- rnorm(n)
y <- rnorm(n)

result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# Is there an outlier?
text(x[13],y[13]+0.2,"Outlier?",col="red")

# Remove outlier
result <- cor.test(x[-13],y[-13])
plot(x[-13],y[-13])
abline(lm(y[-13]~x[-13]))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# Find a different outlier
result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

text(x[16],y[16]+0.2,"Outlier?",col="red")

# Remove this different outlier
result <- cor.test(x[-16],y[-16])
plot(x[-16],y[-16])
abline(lm(y[-16]~x[-16]))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# How do p-values change when removing any single point?
p = vector()
for (i in 1:n) {
  result <- cor.test(x[-i],y[-i])
  p[i] <- result$p.value
}

plot(p,xlab="Removed data point",col=ifelse(p<0.05,"red","black"))
abline(0.05,0,lty=2)


# When is it reasonable to remove outliers?
# Generate correlated data
set.seed(16)
n = 20
x <- rnorm(n)
y <- 2*x+rnorm(n)

# Introduce outlier
x[n] <- 3
y[n] <- -3

result <- cor.test(x,y)

plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

text(x[n]-0.2,y[n]+0.2,"Outlier?",col="red")

# Remove outlier
result <- cor.test(x[-n],y[-n])

plot(x[-n],y[-n])
abline(lm(y[-n]~x[-n]))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)


# Generate random uncorrelated data
set.seed(42)
n = 100

x <- rnorm(n)
y <- rnorm(n)

# Introduce outlier
x[100] <- 5
y[100] <- 5

result <- cor.test(x,y)

plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)
text(x[n]-0.2,y[n]-0.2,"Outlier?",col="red")


# Remove outlier
result <- cor.test(x[-n],y[-n])

plot(x[-n],y[-n])
abline(lm(y[-n]~x[-n]))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# The decision and criteria to remove outliers have to be determined in advance
