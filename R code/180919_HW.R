#9월19일 과제

n <- 1000; x <- numeric()
for (i in 1:n) {
  repeat {
    u1 <- runif(1); u2 <- runif(1)
    c <- exp(0)/(1-exp(-0.05)) #20.50417
    if (u2 < (1/c)*exp(-u1)/(1-exp(-0.05))) {x[i] <- u1; break}
  }
}
head(x)
mean(x)


