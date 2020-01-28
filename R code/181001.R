#10월 1일

#5.5절
#5.4에서 t는 고정적인 상수, rate라 불렸지만 5.5에서 t는 시간에 따라 변함
#방법1. thinning algorithm
람다(t) <= 람다(=t에 의존하지 않는 상수)

step1 : t는 흘러가는 시간, I는 사건이 몇번 일어났는지
step3 : t>T 관심있는 시간 범위를 벗어나면 stop
step5 : 확률이 만족되면 I=I+1 사건으로 받아들이고 S(I)=t 사건시점을 기록
step6 : repeat 또는 while로 순환문 작성

#Exercise 25(a)
lambda=7 				#t=0일 때 최대 람다 7
T=10
lt <- function(t) {3+4/(t+1)} #t를 넣으면 람다t가 나오도록 함수 작성
t=0; I=0
S=numeric() 				#도착시각(arrival times)
repeat {
  t=t+(-log(runif(1))/lambda) 		#또는 t=t+rexp(1, lambda)
  if(t>T) break
  if(runif(1)<lt(t)/lambda) {I=I+1; S[I]=t}
}
cat("Total number of events is", I, "\nArrival times are \n", signif(S,3),"\n")
#signif(x, 3): x를 소수점 유효자리 3자리까지 출력

#방법2. Exercise 25(b)
#HPP가 3인 함수 N(t)와 4/(t+1)인 M(t)를 생성 > merge
hpp.fun <- function(lmd, T) {
  t=0; I=0
  S=numeric()
  repeat {
    t=t+rexp(1, lambda)
    if(t>T) break
    I=I+1; S[I]=t
  }
  return(list(n.events=I, arr.time=S))
}
hpp.fun(3, 10)

nhpp.fun <- function(lmd.max, lmd.t, T) {
  t=0; I=0
  S=numeric()
  repeat {
    t=t+rexp(1, lmd.max)
    if(t>T) break
    if(runif(1)<lmd.t(t)/lmd.max) {I=I+1; S[I]=t}
  }
  return(list(n.events=I, arr.time=S))
}
nhpp.fun(3, lmd.t=function(t) 4/(t+1), 10)

lst.1 <- hpp.fun(lmd=3, T=10)
lst.2 <- nhpp.fun(lmd.max=4, lmd.t=function(t) 4/(t+1), T=10)
S <- sort(c(lst.1$arr.times, lst.2$arr.time))
I <- lst.1$n.events + lst.2$n.events 	#또는 length(S)

#방법3. Exercise 26 #step1~10 참고
#t=10일 때 최대 람다(t)=26
#확인 t=0 s=10
실습 t=0~10까지 적분하면 Poisson(m(t+s)=m(t))의 평균과 비슷한지
m(0)=0

#방법4는 무시



