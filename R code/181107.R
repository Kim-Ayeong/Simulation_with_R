#11월 7일

#8.2절

#Example 8f
양의 상관관계
만들어놓은 S로 Y 만들기
추정량은 Xbar+c*hat(Ybar-mY)로 사용 -> 분산 줄어듬

#실습
#Exercise 8.12
n <- 1000
x.indep <- numeric(n)
x.contr <- numeric(n/2)
u <- runif(n)
x.indep <- exp(u^2)
u.half <- u[1:(n/2)]
x.antit <- exp(u.half^2) * (1+exp(1-2*u.half))/2
#공정하게 하기위해 2로 나눈 것. control approch로만 한다면 안해도 됨
x <- exp(u.half^2)
y <- u.half^2
cstar <- -cov(x, y)/var(y)
x.contr <- x + cstar*(y - 1/3)
c(mean(x.indep), mean(x.antit), mean(x.contr)) 		#1.473083, 1.466435, 1.463928
c(var(x.indep), var(x.antit), var(x.contr)) 		#0.229154808, 0.029899228, 0.004043112
