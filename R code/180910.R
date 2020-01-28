#9월10일

#ch.4

#U가 구간 Pj에 속할 확률 = xj
#이산형 확률난수 생성 > sample 함수 사용
sample(1:4, 1, prob=c(0.2, 0.15, 0.25, 0.4)
#확률의 합이 반드시 1이 되지 않아도 가능, 상대적으로 생각해 자동 변환 
sample(1:4, 100, prob=c(0.2, 0.15, 0.25, 0.4) 		#불가능, size가 1보다 크면 비복원 지정
sample(1:6,10, prob=6:1, rep=TRUE) 			#주사위를 복원추출로 10번 던지기
trunc(10*runif(100))+1 					#trunc():소수점 버리기

#많이 쓰는 이산형 변수
#기하 확률 변수
rgeom(n, prob) 						#R에서는 실패횟수

#베르누이 확률 변수
rbinom(n, size=1, prob)
x <- rbinom(1000,1,0.5) 				#앞뒤 확률이 공정한 동전을 1000번 던지기
y <- rbinom(1000,1,runif(1000))
#베르누이 시행을 1000번, 던질 때마다 앞뒤 확률이 runif(1000)로 다양한 동전
mean(x); mean(y) 					#다르지만 큰 차이가 나지 않음
sd(x); sd(y)
hist(x) 						#0, 1
#둘의 분포 : 0, 1인 Y의 marginal 분포는 평균이 분포를 대신 말해준다고 할 수 있음 > 비슷

#포아송 확률 변수
rpois(n, lambda)
#난수 생성함수의 첫번째 인자는 항상 몇개의 난수를 생성할지

#연습문제 12번
#방법1
lambda <- 5; k <- 10; n <- 20
p.vec <- dpois(0:k, lambda)/ppois(k, lambda)
sample(0:k, n, replace=TRUE, prob=p.vec)
#방법2
y <- vector("integer", n)
for (i in 1:n) {
  repeat {
    z <- rpois(1, lambda)
    if(z<=k) {y[i] <- z; break}
  }
}
y

#연습문제 4번
100장의 서로 다른 카드를 하나씩 오픈할 때, i번째 오픈한 카드가 i인 경우 "hit"
i=1~100까지 일때 hit의 개수
경험적인 평균은 항상 1명
평균, 분산 구해보기




