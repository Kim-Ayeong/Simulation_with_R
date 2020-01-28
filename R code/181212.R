#12월 12일

#10.3절 The Gibbs Sampler

#실습
x <- c(91, 504, 557, 609, 693, 727, 803, 857, 929, 970, 1043, 1089, 1195, 1384, 1713)
n <- length(x)
xbar <- mean(x)
theta0 <- xbar
tau.sq <- var(x)
a <- (n-1)/2
b <- (n-1)*tau.sq/2
post.mean <- function(ss)
  (theta0*ss + xbar*n*tau.sq) / (ss + n*tau.sq)
post.sd <- function(ss) sqrt(ss*tau.sq / (ss + n*tau.sq))
post.a <- 0.5*n + a
post.b <- function(tt) 0.5*sum((x-tt)^2) + b

B <- 1000
theta = ssq = numeric(B)
theta[1] <- xbar
ssq[1] <- tau.sq
for (t in 1:(B-1)) {
  theta[t+1] <- rnorm(1, mean=post.mean(ssq[t]), sd=post.sd(ssq[t]))
  ssq[t+1] <- 1/rgamma(1, shape=post.a, rate=post.b(theta[t+1]))
}
plot(theta)
#95% 신뢰구간 구하기

# Gibbs Sampler: Example 7.3 in Robert and Casella (2009) 
x = c(91, 504, 557, 609, 693, 727, 803, 857, 929, 970, 1043, 1089, 1195, 1384, 1713) 
n = length(x)
xbar = mean(x) 
a = (n-1)/2
b = (n-1)*var(x)/2 
tau.sq = var(x)
theta.zero = mean(x) 
N = 5000
theta = sigma.sq = numeric(N) 
theta[1] = rnorm(1, mean=theta.zero, sd=sqrt(tau.sq)) 
sigma.sq[1] = 1/rgamma(1, a, b) 
cond.mean = function(s2) s2*theta.zero/(s2+n*tau.sq) + n*tau.sq*xbar/(s2+n*tau.sq) 
cond.sd = function(s2) sqrt(s2*tau.sq/(s2 + n*tau.sq)) 
cond.b = function(t) 0.5*sum((x-t)^2) + b
cond.a = 0.5*n + a
for (t in 1:(N-1)) {
  theta[t+1] = rnorm(1, cond.mean(sigma.sq[t]), cond.sd(sigma.sq[t])) 
  sigma.sq[t+1] = 1/rgamma(1, cond.a, cond.b(theta[t+1])) 
}
log(quantile(theta, c(0.05, 0.95))) 
log(sqrt((quantile(sigma.sq, c(0.05, 0.95)))))
log(quantile(theta[(N-999):N], c(0.05, 0.95))) 			# use only the last 1000 values 
log(sqrt((quantile(sigma.sq[(N-999):N], c(0.05, 0.95))))) 
log(t.test(x, conf.level=0.9)$conf.int) 			# Non-Bayesian interval based on t-dist
c(log(sqrt(((n-1)*var(x)/qchisq(0.95, df=n-1)))), log(sqrt(((n-1)*var(x)/qchisq(0.05,df=n-1)))))
								# Non-Bayesian interval based on chi-squared dist

