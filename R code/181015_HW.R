#10월 15일 과제

#Exercise 6.17에서 만기까지 기다렸다가 행사하는 전략의 기대이익(a>0)

#별도의 모의실험 > 문제 발생!
n.sim = 100  						# 모의실험 횟수
N = 20; 							# 총 시점
K = 100; 							# 가격
S.zero = 100; 						# 주가 초기값
mu = -0.05; sg = 0.3; alp = mu+0.5*sg^2		# 주가 모형 분포
E = numeric(n.sim)					# 기대이익

for (i in 1:n.sim) {
  P = S.zero 	
  for (j in 1:N) {
    P = P*exp(rnorm(1, mu, sg)) 			# 만기시 주가
  }
  if (P>K) E[i] = P-K
  else E[i] = 0
}
#E
c(mean(E), sd(E)/sqrt(n.sim))

----------------------------------------------------

# 한 번의 모의실험 안에서 계산 

n.sim = 1000  						# 모의실험 횟수
N = 20; 							# 총 시점
K = 100; 							# 가격
S.zero = 100; 						# 주가 초기값
mu = -0.05; sg = 0.3; 					# 주가 모형 분포
alp = mu+0.5*sg^2 					# 알파
E1 = numeric(n.sim)					# a<0일 때 기대이익
E2 = numeric(n.sim)					# a>0일 때 기대이익

for (i in 1:n.sim) {
  P = numeric(length=N+1) 						# to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[N+1] = S.zero 							# 단계1
  m = N - 1  								# 단계1
  flag = FALSE 								# 옵션을 행사할 수 있는 조건이 만족되면 TRUE, 아니면 FALSE.

  repeat {
    m.plus = m+1 								# to avoid P[0], we need a new index m.plus
    P[m.plus] = P[m.plus+1] * exp(rnorm(1,mu,sg)) 		# m.plus시점의 주가, 단계2
    if (P[m.plus]>K) flag = TRUE 					# 조건(1) 체크
    if (m>0) { 								# m이 0일 때는 점검하지 않음
      b = ((1:m)*mu-log(K/P[m.plus]))/(sg*sqrt(1:m)) 		# 조건(2) 체크
      op = P[m.plus]*exp((1:m)*alp)*pnorm(sg*sqrt(1:m)+b)-K*pnorm(b)
      flag = all(P[m.plus]>K+op) 					# 1부터 m까지 모두 만족이 되어야 함, any, all 알아두기
    }

    if (flag) break
    else m = m-1 								# 조건(1),(2)가 모두 만족되면 실행 완료
    if (m<0) break 							# 만기가 지났으면 실행 완료
  }
  wait_P = P[m.plus]							# 만기 시 주가
  for (j in N-m.plus:N) {						# break된 후 만기 시 주가 계산
    wait_P = wait_P * exp(rnorm(1,mu,sg))
  }
  if (flag) E1[i] = P[m.plus]-K					# 만기 전 옵션 행사
  else E1[i] = 0
  if (wait_P>K) E2[i] = wait_P-K					# 만기 시 옵션 행사
  else E2[i] = 0
}
#E1
#E2
c(mean(E1), sd(E1)/sqrt(n.sim), mean(E1>0)) 				# 기대이익, 표준오차, 옵션행사확률 추정치
c(mean(E2), sd(E2)/sqrt(n.sim), mean(E2>0)) 				# 기대이익, 표준오차, 옵션행사확률 추정치
