#9월 5일

#Example 3a: 원주율의 추정
n = 1000000
count = 0
for (i in 1:n) {
u <- runif(2)
if (u[1]^2 + u[2]^2 <= 1) count = count + 1
}
4*count/n

#반복문을 쓰지 않을 경우
u <- matrix(runif(2*n), ncol=2)
I <- (u[,1]^2 + u[,2]^2 <= 1)
4*mean(I)

#실습 7
integrate(function(x) exp(-x^2), -Inf, Inf)
n <- 10000
y <- runif(n)
hy <- exp(-(1/y-1)^2)/y^2
2*mean(hy)

#실습12
n <- 10000
a <- c()
for(i in 1:n){
  S <- runif(1)
  N <- 1
  repeat{
    S <- S + runif(1)
    if (S > 1) {N <- N + 1; break}
    N <- N + 1
  }
  a[i] <- N 
}
#a
mean(a)
table(a)




