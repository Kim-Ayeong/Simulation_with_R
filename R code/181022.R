#10월 22일

#lec7

표본평균 세타햇
1)표준오차가 d 이하일 때 실행 중단
  xbar(n+1) < xbar(n) 갱신
  S^2(n+!) < S^2(n) 갱신
2)신뢰구간

#과제 6.2
T-tD말고 T-t로 하면 해결
총 휴식시간은 2.3~2.5로 나옴
> 점추정량만 구하면 감점, 표준오차나 신뢰구간 구하기

#7.6
n = 100
gx = exp(runif(n)^2)
mean.0 = mean(gx)
var.0 = var(gx)
repeat {
  x = exp(runif(1)^2)
  n = n+1
  mean.1 = mean.0 + (x + mean.0)/(n)
  var.1 = (1-1/(n-1))*var.0 + n*(mean.1-mean.0)^2
  if(sqrt(var.1/n) < d) break
  mean.0 = mean.1
  var.0 = var.1
}
mean.1
sqrt(var.1/n)
n

#7.8
모수의 95% 신뢰구간
# function N = generate N(m)
# N=[];
# for i=1:m
# V=cumsum(rand(1,100));
# N=[N 1+sum(V<1)];
# h N=generateN(1000); mean(N); var(N);

#그래프에서 y범위 안잘리게 범위 미리 설정
