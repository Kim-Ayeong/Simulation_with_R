#9월 3일

#좋은 난수 발생기의 조건
#1. independent (0, 1) uniform dist.,
#2. 긴 주기 : 난수가 많이 뽑히다보면 일정한 주기가 발견됨,
#3. 빠른 속도

#실습 1. multiplicative congruential generator 방법
#현재값x(n)=초기값x(n-1)*a에서 m으로 나눈 값
m <- 2^31-1; a <- 7^5; x0 <- 1
N <- 100
x <- numeric(N) 		#길이가 100인 숫자형 벡터
x[1] <- (a*x0)%%m
for (i in 2:N)
  x[i] <- (a*x[i-1])%%m
x/m 				#0~1사이의 난수 벡터
y <- x/m
hist(y) 			#히스토그램 그려보기

#함수로 구현해보기
myrunif <- function(N, m=2^31-1, a=7^5, x0=1) {
  x <- numeric(N)
  x[1] <- (a*x0)%%m
  for (i in 2:N)
    x[i] <- (a*x[i-1])%%m
  x/m 
}
y <- myrunif(100)

#Mersenne-Twister 방법
y <- runif(100)

#실습 2
y <- myrunif(1000)
hist(y) 			#U(0,1) 분포 확인
plot(ecdf(y)) 			#U(0,1) 분포 확인 	#ecdf = empirical cdf
plot(y) 			#독립성 확인
plot(y[seq(1, 1000, by=2)], y[seq(2, 1000, by=2)]) 	#홀수, 짝수의 산점도

#실습 3
sum(diff(sort(y))==0) 		#999개의 차이값 발생 	#0이면 중복되는 값이 하나도 없다. 
length(y)==length(unique(y))
