#11월 5일

#8.1절

#Exercise 8.3
indic.fun <- function(x) as.numeric(sum((1:5)*(-log(x))) >= 21.6)
n.sim <- 10000
I.indep <- I.antit <- numeric(n.sim)
for (i in seq(1, n.sim, 2)) {
  u <- runif(5)
  I.indep[i] <- I.antit[i] <- indic.fun(u)
  I.antit[i+1] <- indic.fun(1-u)
  I.indep[i+1] <- indic.fun(runif(5)) 
}
se.indep <- sd(I.indep) / sqrt(n.sim)
c(mean(I.indep), se.indep)
antit <- (I.antit[seq(1, n.sim, 2)] + I.antit[seq(2, n.sim, 2)])/2
se.antit <- sd(antit)/sqrt(n.sim/2)
c(mean(antit), se.antit)
(se.indep^2 - se.antit^2)/(se.indep^2) 
# proportion of variance reduced by antithetic-variable-approach