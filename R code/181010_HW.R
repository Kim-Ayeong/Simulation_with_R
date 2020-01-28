#10월 10일 과제

#Exercise 6.2

n.sim = 100
rate = 10 						# 도착시간 간격은 지수분포
shape = 3; scale = 1/40 				# 서비스 시간은 감마분포
							# 1인당 평균 처리 시간:3/40, 10명 처리:(3/40)*10=3/4, 시간단 3/40명 처리
T = 9							# 총 처리시간
time.over = time.spend = numeric(n.sim) 	# 고객들의 평균체류시간, 직원(server)의 과외근무시간
time.rest = numeric(n.sim)			# 서버 휴식시간

for (i in (1:n.sim)) {
  t = NofA = NofD = 0				# 현재 시간, 도착한 고객 수, 서비스 완료 고객 수
  n = 0 						# SS(System State) is the number of customers in the system
  tA = rexp(1,rate) 				# 현재 시각 이후 다음 고객이 도착하는 시각
  tD = Inf 						# 현재 서비스 받고 있는 고객의 서비스 완료 시각. 초기값은 무한대
  A = D = numeric() 				# 출력변수: A[i]=i번째 고객의 도착시각, D[i]=i번째 고객의 서비스 완료 시각(departure time)
  repeat {
    if (tA <= tD & tA <= T){  			# case1, 첫번째 사람에 대한 서비스가 완료하기 전에 두번째 사람 도착, 서비스 시간이 끝나기 전에 다음 고객 도착
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma(n=1, shape, scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){			# case2, 새 고객이 도착하기 전에 서비스 완료
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf 			# 대기 중인 고객이 없으면 tD는 무한대
      else tD = t + rgamma(n=1, shape, scale=scale) #대기중인 사람이 있으면 tD 다시 설정
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){		# case3, 마감시간이 지났지만 서비스해야 할 고객이 있음
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0) break 	# case4, 마감시간이 지났고, 서비스해야 할 고객이 없음

  }							# end of repeat
  time.spend[i] = mean(D-A)
  time.over[i] = max(t-T,0)

  if (A[i+1]>D[i]) time.rest[i]=A[i+1]-D[i] # 새로운 고객이 도착한 시간이 지난 서비스를 완료한 시간보다 클 때,
  if (D[NofD]<T) time.rest[i]=T-D[NofD]     # 마지막 서비스를 완료한 시간이 전체 시간 T보다 작을 때,
}							 # end of for (i in (1:n.sim))
cat("총 서버 휴식 시간 : ", sum(time.rest))



# Exercise 6.12

n.sim = 100 					   # 모의실험 반복횟수
n0=1; a0=25000; T=365; c=11000		   # n:계약자 수, a:자본금, T:총시간, c:시간당 납부하는 보험금
lm=10; nu=0; mu=0 			  	   # lm:보험금 청구율, nu:보험 가입율, mu:해지율
generate.Y = function() rexp(1,rate=1/1000)  # 청구금액을 생성하는 함수
I = numeric(length=n.sim)
time.nega = numeric(length=n.sim)		   # 자본금이 0이 된 시각
money.nega = numeric(length=n.sim)		   # 자본금이 0이 됐을 때, 부족한 금액

for (i in 1:n.sim) {
  t=0; a=a0; n=n0 			   	   # Intialize. 이 문제에서 n은 변하지 않는다.
  total.rate = nu + n*mu + n*lm
  tE = rexp(1, rate=total.rate)		   # 사건 발생시각
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


