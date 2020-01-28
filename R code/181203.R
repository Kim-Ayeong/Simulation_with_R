#12월 3일

#9.3절

#Ai : i번째 가설이 참인데 기각할 사건, i=1,...,r
r번의 가설검정에서 적어도 한번 제1종 오류를 범할 확률은
P(A1UA2U...UAr) <= P(A1)+P(A2)+...+P(Ar) : Bonferron 부등식
alpha = P(A1)라고 할 때, ~ <= r*alpha

#(1) Wilcoxon Two-Sample Rank-Sum Test
Y 실제상황에서 나온 값들
X 모의상황에서 나온 값들 일 때,
하나의 모집단에서 추출된 확률표본이라 가정(=같은 분포에서 나온 값인지)

recursion formula for small (n, m) > p.233 (9.8)
normal approximation for large (n, m) > p.234 (9.9)

# Exercise 11 (b) : 모의실험에 의한 p값 계산
x = c(65.2, 67.1, 69.4, 78.4, 74, 80.3)
y = c(59.4, 72.1, 68, 66.2, 58.5)
n = length(x); m = length(y)
W = wilcox.test(x, y)$statistic
r = W + n*(n+1)/2					# 또는 xy = c(x, y); r = sum(match(x, sort(xy)))
N = 1000	 					# 모의실험으로 p값을 구하기 위한 반복횟수
R = replicate(N, sum(sample(1:(n+m), n))) 		# 순위합, for 문을 써도 됨
2*min(mean(R <= r), mean(R >= r)) 			# 모의실험에 의한 p값

#Example 9f
x = c(135,104,162,171,129)
y = c(107,94,136,99,114,122,108,130,106,88)
wilcox.test(x, y)
wilcox.test(x, y, exact=FALSE, correct=FALSE) 		#Example 9g
#윌콕슨 검정 없이 순위합 구하기
match(x, sort(c(x, y)))
r = sum(match(x, sort(c(x, y)))) 			#50
#윌콕슨 순위합은 -n(n+1)/2 항 때문에 40

#(2) Two-Sample Kolmogorov-Smirnov Test
# Wilcoxon two-sample rank-sum test vs K-S two-sample test
x = rnorm(200, mean=0, sd=1)
y = rnorm(200, mean=0, sd=2)
wilcox.test(x, y)
ks.test(x, y)
# Wilcoxon test는 중심위치 모수(location parameter)에 대한 검정
# K-S test는 분포함수에 대한 검정

#실습
#Exercise 10번
exact p값
x <- c(65.2, 67.1, 69.4, 78.4, 74.0, 80.3)
y <- c(59.4, 72.1, 68.0, 66.2, 58.5)
wilcox.test(x, y) 					#exact p=value
wilcox.test(x, y, exact=FALSE) 				#normal approximation
n <- length(x); m <- length(y)
r <- wilcox.test(x, y)$statistic + n*(n+1)/2
R <- replicate(2000, sum(sample(1:(n+m), n)))
2*min(mean(R > r), mean(R < r)) 			#p-value by simulation
2*min(mean(R >= r), mean(R <= r)) 			#p-value by simulation
ks.test(x, y)
plot(ecdf(x), main="ecdf of x and y") 			#empirical cdf
par(new=T)
plot(ecdf(y), col="blue", main="", xlab="", ylab="", lty=2)

#Exercise 11번
정규근사 p값, 모의실험 p값

