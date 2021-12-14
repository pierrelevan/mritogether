# Generate random data from multiple subjects and multiple brain regions
set.seed(42)
n_subjects <- 20
n_brainregions <- 20

data_baseline <- matrix(rnorm(n_subjects*n_brainregions),n_subjects,n_brainregions)
data_treatment <- matrix(rnorm(n_subjects*n_brainregions),n_subjects,n_brainregions)

plot(colMeans(data_baseline),type="l",col="black",ylab="Mean Signal",xlab="Brain Region",ylim=c(-1,1.5))
lines(colMeans(data_treatment),col="red")

errorbar = apply(data_baseline,2,sd) / sqrt(n_subjects)
arrows(1:n_brainregions, colMeans(data_baseline)-errorbar, 1:n_brainregions, colMeans(data_baseline)+errorbar, length=0.05, angle=90, code=3,col="black")

errorbar = apply(data_treatment,2,sd) / sqrt(n_subjects)
arrows(1:n_brainregions, colMeans(data_treatment)-errorbar, 1:n_brainregions, colMeans(data_treatment)+errorbar, length=0.05, angle=90, code=3,col="red")

legend("topleft",legend=c("Baseline","Treatment"),col=c("black","red"),lty=1)

# One brain region seems to stand out, so statistically test it
result <- t.test(data_baseline[,20],data_treatment[,20])
text(19,1,sprintf("p=%.3f",result$p.value))

