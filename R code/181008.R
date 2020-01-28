#10월8일

#Ch 6. The Discrete Event Simulation Approach

#6.2절
#single server : 한 사람
#Queueing System : 대기열 시스템, 가장 먼저 들어온 사람이 먼저 나간다
t : 현재 시각
NA : (t까지) 도착한 고객의 수
ND : 서비스 완료된 고객의 수
n : 시각t에 시스템 안에 있는 고객의 수
tA : 시각t 이후에 다음 고객(next customer)이 도착하는 시각
시스템이 비어있지 않은 상태에서 고객이 계속 도착하면 이 값은 계속 커짐
tD : 시각t 현재 서비스 받고 있는 고객의 서비스가 끝나는 시각
현재 서비스 중인 고객이 없으면 ∞, 없으면?
A(i) : i번째 고객의 도착시각
D(i) : i번째 고객의 서비스 완료시각
Tp : 직원의 초과근무 시간(마지막 고객의 서비스 완료시각이 T를 넘을 경우 서비스 완료시각에서 T를 뺀 시간)
구하려는 값 > E[D(i)-A(i), E(Tp)

#경우 4가지
min(tA, tD)<T, n=0
		   n>0
min(tA, tD)>T, tA<tD 		#서비스가 끝나기 전에 다음 고객 도착
		tA>tD 		#고객이 도착하기 전에 서비스가 끝남

#Exercise 6.1
#A Single-Server Queueing System
n.sim = 100
rate = 10 						# 도착시간 간격은 지수분포
shape = 3; scale = 1/40 				# 서비스 시간은 감마분포
							# 1인당 평균 처리 시간:3/40, 10명 처리:(3/40)*10=3/4, 시간단 3/40명 처리
T = 9							# 총 처리시간
time.over = time.spend = numeric(n.sim) 		# 고객들의 평균체류시간, 직원(server)의 과외근무시간
for (i in (1:n.sim)) {
  t = NofA = NofD = 0					# 현재 시간, 도착한 고객 수, 서비스 완료 고객 수
  n = 0 						# SS(System State) is the number of customers in the system
  tA = rexp(1,rate) 					# 현재 시각 이후 다음 고객이 도착하는 시각
  tD = Inf 						# 현재 서비스 받고 있는 고객의 서비스 완료 시각. 초기값은 무한대
  A = D = numeric() 					# 출력변수: A[i]=i번째 고객의 도착시각, D[i]=i번째 고객의 서비스 완료 시각(departure time)
  repeat {
    if (tA <= tD & tA <= T){  				# case1, 첫번째 사람에 대한 서비스가 완료하기 전에 두번째 사람 도착, 서비스 시간이 끝나기 전에 다음 고객 도착
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma(n=1, shape, scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){				# case2, 새 고객이 도착하기 전에 서비스 완료
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf 				# 대기 중인 고객이 없으면 tD는 무한대
      else tD = t + rgamma(n=1, shape, scale=scale) 	#대기중인 사람이 있으면 tD 다시 설정
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){			# case3, 마감시간이 지났지만 서비스해야할 고객이 있음
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0) break  		# case4, 마감시간이 지났고, 서비스해야할 고객이 없음
  }							# end of repeat
  time.spend[i] = mean(D-A)
  time.over[i] = max(t-T,0)
}							# end of for (i in (1:n.sim))
time.spend
time.over

cat("고객 한 명이 시스템에 체류하는 시간의 평균 = ", mean(time.spend),
"\n직원의 초과근무시간의 평균 =", mean(time.over))

#Exercise 6.2
#case 1, 4에서만 발생
(1) 새로운 고객(첫 고객 포함)이 도착했을 때 n=0 이면
서버의 휴식시간(idle time)은 지난번 사건이 일어난 시각부터 새 고객이 도착할 때까지 경과한 시간
(2) 마감 시각 T에서 n=0 이면 서버의 휴식시간은 'T 이전 마지막 고객의 서비스 완료 시점'부터 T 까지의 시간
(1)+(2)=(0, T)까지의 휴식시간

n.sim = 100
rate = 10 						# 도착시간 간격은 지수분포
shape = 3; scale = 1/40 				# 서비스 시간은 감마분포
							# 1인당 평균 처리 시간:3/40, 10명 처리:(3/40)*10=3/4, 시간단 3/40명 처리
T = 9							# 총 처리시간
time.over = time.spend = numeric(n.sim) 		# 고객들의 평균체류시간, 직원(server)의 과외근무시간
time.rest = numeric(n.sim)				# 서버 휴식시간

for (i in (1:n.sim)) {
  t = NofA = NofD = 0					# 현재 시간, 도착한 고객 수, 서비스 완료 고객 수
  n = 0 						# SS(System State) is the number of customers in the system
  tA = rexp(1,rate) 					# 현재 시각 이후 다음 고객이 도착하는 시각
  tD = Inf 						# 현재 서비스 받고 있는 고객의 서비스 완료 시각. 초기값은 무한대
  A = D = numeric() 					# 출력변수: A[i]=i번째 고객의 도착시각, D[i]=i번째 고객의 서비스 완료 시각(departure time)
  repeat {
    if (tA <= tD & tA <= T){  				# case1, 첫번째 사람에 대한 서비스가 완료하기 전에 두번째 사람 도착, 서비스 시간이 끝나기 전에 다음 고객 도착
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma(n=1, shape, scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){				# case2, 새 고객이 도착하기 전에 서비스 완료
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf 				# 대기 중인 고객이 없으면 tD는 무한대
      else tD = t + rgamma(n=1, shape, scale=scale) 	#대기중인 사람이 있으면 tD 다시 설정
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){			# case3, 마감시간이 지났지만 서비스해야 할 고객이 있음
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0) break 			# case4, 마감시간이 지났고, 서비스해야 할 고객이 없음

  }							# end of repeat
  time.spend[i] = mean(D-A)
  time.over[i] = max(t-T,0)

  if (A[i+1]>D[i]) time.rest[i]=A[i+1]-D[i] 		# 새로운 고객이 도착한 시간이 지난 서비스를 완료한 시간보다 클 때,
  if (D[NofD]<T) time.rest[i]=T-D[NofD]     		# 마지막 서비스를 완료한 시간이 전체 시간 T보다 작을 때,
}							# end of for (i in (1:n.sim))

cat("총 서버 휴식 시간 : ", sum(time.rest))




