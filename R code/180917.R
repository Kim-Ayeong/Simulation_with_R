#9월 17일

inverse transform algorithm 적용 불가 (why?) > 적분 모양이 됨
?rgamma 	#rate(=1/beta), scale 구분 주의 
#분포를 모르고, 역함수를 생성할 수 없는 f(x)에 대해 비슷한 분포 g(x)와 비교해서 f(x) 난수를 생성
suppor set : 관측될 수 있는 영역

#Ex 5d)
#beta(2,4) 난수를 rejection method로 생성
n=1000; x=numeric()
for (i in 1:n) {
  repeat {
    u1 = runif(1); u2 = runif(1)
    if (u2 < (256/27)*u1*(1-u1)^3) {x[i] = u1; break}
  }
}
hist(x,breaks=30,prob=T);curve(dbeta(x,2,4),add=T)

#Ex 5e), 5f)는 패스

#Ex 5g)
rgamma(10, 2,1) #버리는 5이하의 값이 너무 많음

#ex 4번
#ex 6번

#과제 피드백 > 사진 참고, c()보다 Numeric()을 이용해 생성하는 것이 더 나음

