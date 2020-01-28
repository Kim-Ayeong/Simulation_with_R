#9월 19일

#5.3 > 크게 중요하지 않음

#5.4절 중요
#t시간 동안 사건이 몇번 일어났느냐, countion process
#arrival times(도착시각) : 구간(0,t) 사이의 s1, s2, s3
#inter-arrival times(도착간격시간) : 구간(0,t) 사이의 s1-0, s2-s1

#방법1
lambda = 1 			#포아송 과정의 모수(rate)
T = 10 				#구간(0,T)에서 포아송 과정을 생성: 사건의 도착시각과 총 발생 횟수를 기록
t = 0; I = 0L 			#t는 시각을 I는 사건의 횟수를 추적하기 위함. I는 정수0(L로 표시)
S = numeric() 			#도착시각(arrival times)
repeat {
  t = t + rexp(1,lambda) 	#interarrival times는 지수분포이므로
  if (t > T) break
  I = I + 1; S[I] = t
}
cat("Total number of events is", I,
"\nArrival times are \n", signif(S,3), "\n")

#방법2
lambda = 1 			#포아송 과정의 모수(rate)
T = 10 		#구간 (0,T) 에서 포아송 과정을 생성: 사건의 도착시각과 총 발생 횟수를 기록
n = rpois(1,lambda*T) 
S = sort(T*runif(n)) 
cat("total number of events is",n,
"\narrival times are \n",signif(S,3),"\n")

#방법 2는 순환문 repeat를 쓰지 않고 log 계산을 할 필요가 없다는 장점이 있지만 
도착시각을 순서대로 알고 싶은 경우 sort를 해야 함. 

#Exercise 24
Xi는 20~40까지 이산형 확률분포를 같는 확률변수, 1시간 동안 평균=5대
N <- rpois(n=1, lambda=5) 	#the number of buses in 5 hours
sum(sample(20:40, N, rep=T)) 	#the number of fans

#과제 피드백
#방법1
f=function(){기존}
g=function(){dunif(x, 0, 0.05)}
curve 그려보기
for (i in 1:n) {
  repeat {
    u1=runif(1, 0.05); u2=runif(1)
    if(u2 <= f(u1)/(c*g(u1))) x[i]=u1; break
  }
}

#방법2
c=1/(1-exp(-0.05))
f(x)/c*g(x) = e(-x)
for
  repeat
    if (u2<exp(-u1)) x[i]=u1; break

