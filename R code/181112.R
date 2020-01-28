#11월 12일

#8.3절 조건화의 분산 감소효과

#Example 8k : 교재 156쪽 참고

#Example 8l : Y ~ exp(1), X|y ~ N(y, 2^2)
#theta = P(X>1), E[I(X>1)|Y=y] = 1-Phi((1-y)/2)
I(X>1) : raw estimator

#raw simulation approach
#conditioning technique
#conditioning + antithetic variate
#conditioning + control variate

n <- 100
u <- runif(n)
y <- -log(u)
x <- rnorm(n, mean=y, sd=2)
I <- (x>1)
cond.E <- 1-pnorm((1-y)/2)
anti <- (cond.E + 1 - pnorm((1+log(1-u))/2))/2

#control 만들어보기
X + c(Y-mY)
여기서 X는 cond.E
mean은 비슷하고 var는 앞의 2개보다 어느 정도 작아야 함

c(mean(I), mean(cond.E), mean(anti))
c(var(I), var(cond.E), var(anti))  		#분산이 1/10 줄음
#antithetic 방법은 추가적으로 n개의 값을 생성했으므로 공정한 비교가 아님




