#12월 10일 과제

#Example 9d : Example 9c의 문제에서 모의실험으로 p값 구하기
table(rpois(30, 3.9)) 					#0생성, 5이상은 범주로 묶기

B = 10000 						# 모의실험 횟수. p값이 작으므로 모의실험 횟수를 크게 할 필요가 있음.
N = c(6,2,1,9,7,5) 					# 하루 사고 건수 5건과 8건을 묶어서 한 범주로 만들었음에 주의
n = sum(N) 						# 표본 크기
mhat = (sum(N[1:5] * 0:4) + 4*5 + 1*8)/30 		# 하루 사고 건수 5건이 4일, 8건이 1일
phat = dpois(0:4, mhat)
phat[6] = 1 - ppois(4, mhat)
t = sum((N - n*phat)^2/(n*phat))
1 - pchisq(t, df=4) 					# n이 클 때의 근사적 p값.
# 자유도가 5가 아니라 4임에 주의. 자유도를 제대로 지정했지만 n이 충분히 크지 않으므로 적절하지 않음.

# [Q] 'n이 충분히 크다'는 대략적인 기준은 무엇인가? [A] 관측도수나 기대도수가 5 이상인 셀의 비율이 80% 이상일 때
# 모의실험을 통해 제대로 된 p값을 구해보자.
Nstar = numeric(length = 6) 				# N[1]은 n개 관측값 중에서 0인 관측값의 개수
Tstar = numeric(B)
for (b in 1:B) {
  ystar = rpois(n, mhat)
  for (i in 0:4) Nstar[i + 1] = sum(i == ystar) 	# N_i^* 계산
  Nstar[6] = n - sum(Nstar[1:5])
  mhatstar mean(ystar)
  phatstar dpois(0:4, mhatstar)
  phatstar[6] = 1 - ppois(4, mhatstar)
  Tstar[b] = sum((Nstar - n*phatstar)^2/(n*phatstar))
}
I.vec = (Tstar >= t)
c(mean(I.vec), sd(I.vec)/sqrt(B)) 			# 모의실험에 의한 p값과 표준오차
							#0.0004, 0.00019
