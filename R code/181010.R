#10월 10일

# 6.6절

# Exercise 6.11
n.sim = 100 					   # 모의실험 반복횟수
n0=1; a0=25000; T=365; c=11000		   	   # n:계약자 수, a:자본금, T:총시간, c:시간당 납부하는 보험금
lm=10; nu=0; mu=0 			  	   # lm:보험금 청구율, nu:보험 가입율, mu:해지율
generate.Y = function() rexp(1,rate=1/1000)  	   # 청구금액을 생성하는 함수
I = numeric(length=n.sim)

for (i in 1:n.sim) {
  t=0; a=a0; n=n0 			   	   # Intialize. 이 문제에서 n은 변하지 않는다.
  total.rate = nu + n*mu + n*lm
  tE = rexp(1, rate=total.rate)		   	   # 사건 발생시각
  repeat {
    if (tE > T) {I[i] = 1; break}		   # 청구금액이 자본금보다 작으면 I=1, 흑자 
    if (tE <= T) {
      a = a + n*c*(tE - t)
      t = tE
      J = sample(1:3, 1, prob=c(nu,n*mu,n*lm))
      if (J == 1) n = n + 1			   # 신규고객 가입
      if (J == 2) n = n - 1			   # 기존계약 해지
      if (J == 3) {				   # 고객이 보험금 청구
        Y = generate.Y(); 
        if (Y > a) {I[i] = 0; break} 	   	   # 청구금액이 자본금보다 크면 I=0, 적자
        else a = a - Y
      }
      tE = t + rexp(1,rate=total.rate)
    }
  } 							   # end of repeat
} 							   # of for
I
mean(I) 	 					   # 자본금이 바닥나지 않을 확률(흑자)의 추정값 출력. 추정값은 얼마나 정확한가?

# 과제
# Exercise 6.2
# Exercise 6.12
시각 T 이전에 자본금이 음이 되었다는 사실을 알 때,
자본금이 음이 된 정확한 시각과 부족 금액의 분포를 추정하는 문제

n.sim = 100 					   # 모의실험 반복횟수
n0=1; a0=25000; T=365; c=11000		   	   # n:계약자 수, a:자본금, T:총시간, c:시간당 납부하는 보험금
lm=10; nu=0; mu=0 			  	   # lm:보험금 청구율, nu:보험 가입율, mu:해지율
generate.Y = function() rexp(1,rate=1/1000)  	   # 청구금액을 생성하는 함수
I = numeric(length=n.sim)
time.nega = numeric(length=n.sim)		   # 자본금이 0이 된 시각
money.nega = numeric(length=n.sim)		   # 자본금이 0이 됐을 때, 부족한 금액

for (i in 1:n.sim) {
  t=0; a=a0; n=n0 			   	   # Intialize. 이 문제에서 n은 변하지 않는다.
  total.rate = nu + n*mu + n*lm
  tE = rexp(1, rate=total.rate)		   	   # 사건 발생시각
  repeat {
    if (tE > T) {I[i] = 1; break}		   # 청구금액이 자본금보다 작으면 I=1, 흑자 
    if (tE <= T) {
      a = a + n*c*(tE - t)
      t = tE
      J = sample(1:3, 1, prob=c(nu,n*mu,n*lm))
      if (J == 1) n = n + 1			   # 신규고객 가입
      if (J == 2) n = n - 1			   # 기존계약 해지
      if (J == 3) {				   # 고객이 보험금 청구
        Y = generate.Y(); 
        if (Y > a) {			   	   # 청구금액이 자본금보다 크면 I=0, 적자
	    I[i] = 0;
	    time.nega[i]=tE
	    money.nega[i]=Y-a
	    break
	  } 
        else a = a - Y
      }
      tE = t + rexp(1,rate=total.rate)
    }
  } 							   # end of repeat
} 							   # of for

time.nega
money.nega
I
mean(I)
