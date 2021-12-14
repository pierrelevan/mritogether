# Generate random uncorrelated data
set.seed(42)

n = 20
n_experiment = 1
x <- rnorm(n)
y <- rnorm(n)

result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("Experiment %d: corr=%.2f (p=%.3f)",n_experiment,result$estimate,result$p.value),side=3,line=1)

# Repeat the experiment several times
n_experiment <- n_experiment+1

for (i in 1:10) {
  x <- rnorm(n)
  y <- rnorm(n)
  result <- cor.test(x,y)
  plot(x,y)
  abline(lm(y~x))
  mtext(sprintf("Experiment %d: corr=%.2f (p=%.3f)",n_experiment,result$estimate,result$p.value),side=3,line=1)
  n_experiment <- n_experiment+1
}

# How do p-values look like when experiments are repeated?
n_experiments = 1000
p = vector()
for (i in 1:n_experiments) {
  x <- rnorm(n)
  y <- rnorm(n)
  result <- cor.test(x,y)
  p[i] = result$p.value
}
plot(p,pch=20,col=ifelse(p<0.05,"red","black"))
abline(0.05,0,lty=2)

# How many p-values are significant?
hist(p,breaks=20,col=c("red",rep("blue",19)))
mtext(sprintf("%d (%.1f%%) p-values are <0.05",sum(p<0.05),sum(p<0.05)/n_experiments*100),side=3)

# What is there is a true correlation in the data?
x <- rnorm(n)
y <- 0.5*x+rnorm(n)
result <- cor.test(x,y)
plot(x,y)
abline(lm(y~x))
mtext(sprintf("corr=%.2f (p=%.3f)",result$estimate,result$p.value),side=3,line=1)

# How do p-values look like when experiments are repeated?
p = vector()
for (i in 1:n_experiments) {
  x <- rnorm(n)
  y <- 0.5*x+rnorm(n)
  result <- cor.test(x,y)
  p[i] = result$p.value
}
plot(p,pch=20,col=ifelse(p<0.05,"red","black"))
abline(0.05,0,lty=2)

# How many p-values are significant?
hist(p,breaks=20,col=c("red",rep("blue",19)))
mtext(sprintf("%d (%.1f%%) p-values are <0.05",sum(p<0.05),sum(p<0.05)/n_experiments*100),side=3)

# This study is underpowered! (only 50% chance of detecting the effect)