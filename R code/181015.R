#10월 15일

#6.8절

#a<0일 때 행사조건
Pm(=S(N-m)): 만기를 m 앞둔 시점에서의 주가
1. Pm > K
2. 모든 i에 대해 Pm > K+~ (m=0일때는 조건1만 점검)

# Exercise 6.17
n.sim = 1000  							# 모의실험 횟수
N = 20; 							# 총 시점
K = 100; 							# 가격
S.zero = 100; 							# 주가 초기값
mu = -0.05; sg = 0.3; 						# 주가 모형 분포
alp = mu+0.5*sg^2 						# 알파
E = numeric(length=n.sim)					# 기대이익

for (i in 1:n.sim) {
  P = numeric(length=N+1) 						# to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[N+1] = S.zero 							# 단계1
  m = N - 1  								# 단계1
  flag = FALSE 								# 옵션을 행사할 수 있는 조건이 만족되면 TRUE, 아니면 FALSE.
  repeat {
    m.plus = m+1 							# to avoid P[0], we need a new index m.plus
    P[m.plus] = P[m.plus+1] * exp(rnorm(1,mu,sg)) 			# m.plus시점의 주가, 단계2
    if (P[m.plus]>K) flag = TRUE 					# 조건(1) 체크
    if (m>0) { 								# m이 0일 때는 점검하지 않음
      b = ((1:m)*mu-log(K/P[m.plus]))/(sg*sqrt(1:m)) 			# 조건(2) 체크
      op = P[m.plus]*exp((1:m)*alp)*pnorm(sg*sqrt(1:m)+b)-K*pnorm(b)
      flag = all(P[m.plus]>K+op) 					# 1부터 m까지 모두 만족이 되어야 함, any, all 알아두기
    }
    if (flag) break
    else m = m-1 							# 조건(1),(2)가 모두 만족되면 실행 완료
    if (m<0) break 							# 만기가 지났으면 실행 완료
  }
  if (flag) E[i] = P[m.plus]-K
  else E[i] = 0
}
c(mean(E), sd(E)/sqrt(n.sim), mean(E>0)) 				# 기대이익, 표준오차, 옵션행사확률을 추정하여 출력


#과제
#Exercise 6.17에서 만기까지 기다렸다가 행사하는 전략의 기대이익(a>0)

#그래프 그려보기
S0 <- 100; mu <- (-0.05); sigma <- 0.3
N <- 20
x <- rnorm(N, mu, sigma)
S <- S0*exp(cumsum(x)) 							#주가 모형
plot(c(S0, S), type="l", xlab="t", ylab="Stock Price")

#만기까지 기다리기
n.sim = 100  							# 모의실험 횟수
N = 20; 							# 총 시점
K = 100; 							# 가격
S.zero = 100; 							# 주가 초기값
mu = -0.05; sg = 0.3; alp = mu+0.5*sg^2				# 주가 모형 분포
E = numeric(n.sim)						# 기대이익

for (i in 1:n.sim) {
  P = numeric(N+1) 						# to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[1] = S.zero 	
  for (j in 2:N+1) {
    P[j] = P[j-1] * exp(rnorm(1, mu, sg)) 			# j시점의 주가
  }
  if (P[i]>K) E[i] = P[i]-K
  else E[i] = 0
}

E
c(mean(E), sd(E)/sqrt(n.sim)) 					# 기대이익, 표준오차
