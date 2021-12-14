# Generate random uncorrelated data that is close to significant
set.seed(42)

n = 20
x <- rnorm(n)
y <- rnorm(n)
result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# Add one random data point: does correlation become significant?
x_new <- c(x,rnorm(1))
y_new <- c(y,rnorm(1))
result <- cor.test(x_new,y_new)
plot(x_new,y_new,col=c(rep("black",n-1), "red"))
abline(lm(y_new~x_new))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# How often does correlation become significant?
p = vector()
for (i in 1:100) {
  x_new <- c(x,rnorm(1))
  y_new <- c(y,rnorm(1))
  result <- cor.test(x_new,y_new)
  p[i] = result$p.value
}
plot(p,xlab="Attempt #")
mtext(sprintf("%d p-values are <0.05",sum(p<0.05)),side=3)

# Misconception: if there is a true effect, p-values will get smaller and smaller,
# and if there is no effect, p-values will get less and less significant

# Generate data with a true correlation
n <- 1500
x <- rnorm(n)
y <- 0.1*x+rnorm(n)

result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# How does p-value evolve as we include more and more data?
p = vector()
for (i in 20:n) {
  result <- cor.test(x[1:i],y[1:i])
  p[i] <- result$p.value
}

plot(p,xlab="n",pch=20,col=ifelse(p<0.05,"red","black"))
abline(0.05,0,lty=2)
mtext(sprintf("True correlation: evolution of p-value as we add more data"),side=3)


# Generate data with no correlation
x <- rnorm(n)
y <- rnorm(n)

result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("n=%d, corr=%.2f (p=%.3f)",n,result$estimate,result$p.value),side=3,line=1)

# How does p-value evolve as we add more and more data?
p = vector()
for (i in 20:n) {
  result <- cor.test(x[1:i],y[1:i])
  p[i] <- result$p.value
}

plot(p,xlab="n",pch=20,col=ifelse(p<0.05,"red","black"))
abline(0.05,0,lty=2)
mtext(sprintf("No true correlation: evolution of p-value as we add more data"),side=3)

