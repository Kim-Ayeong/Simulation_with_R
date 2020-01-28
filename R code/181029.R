#10월 29일

#실습 7.15
#Var(s^2)의 붓스트랩 추정량 구하기
표본오차 : sum((Xi-Xbar)^2)/14
표본평균의 분산 : Xbar ~ N(M, sig) 이므로 Var(Xbar) = sig^2/n 근사~ s^2/n
M의 95% 신뢰구간 = Xbar +- 2*s/sqrt(n) < 중심극한정리
Var(s^2) = Var(sum((Xi-Xbar)^2)/n-1) 구하기

real world에서 X1,,,Xn ~ N(M, sig)일 때는 (n-1)s^2/sig^2 ~ chisq(n-1)

F:실제 분포함수, Fn:경험적 분포함수
모수 theta는 F에 의해 결정되는 추정량, theta(F)

g(Xi,,,Xn)의 표준오차가 아니라 평균제곱오차라고 부르는 이유
 : g(Xi,,,Xn)가 반드시 평균이 아닐 수도 있음, 그러나 추정량일 수는 있음 < 편향 추정량

MSE의 비모수추정량 : EFn[(g(Xi,,,Xn)-theta(Fn))^2]

Fn를 모집단의 참분포 u1,,,uN라고 할 때,
M = u1+,,,+uN/N = ubar
sig^2 = E[(X-M)^2] = sum((ui-M)^2)/N = sum((ui-ubar)^2)/n

모의실험 횟수 B를 충분히 크게 하면
Fn은 F와 가까워지고 theta(Fn)는 근사적으로 theta(F)와 비슷해진다.

#실습 7.15
(표본분산의 기댓값-모분산)의 분산
n = 15
x1,,,x15 = ~
g(x1,,,xn) = sum((xi-xbar)^2)/n-1 = 표본분산 s^2
theta(F) = E(s^2) = sig^2
E((g(x1,,,xn)-sig^2)^2) = Var(s^2) 구하기

#방법1
n <- 15
x <- c(5,4,9,6,21,17,11,20,7,10,21,15,13,16,8)
B <- 200
ssqstar <- numeric(B)
for (i in 1:B) { 
  xstar <- sample(x, 15, rep=T) 		#복원추출로 붓스크랩 표본 뽑기
  ssqstar[i] <- var(xstar) 			#표본분산 추출
}
var(ssqstar) 					#ssqstar = thetastar_hat(1),,,thetastar_hat(B)

#sum_i~B((thetastar_hat(i)-thetastar_hat(i)들의 평균)^2)/B-1 은
근사적으로 EFn((g(x1,,,xx)-theta(Fn))^2) = EF((g(x1,,,xx)-theta(F))^2)

#방법2
xstar.mat <- matrix(sample(x, B*15, rep=T), nrow=15)
ssqstar <- apply(xstar.mat, 2, var)
var(ssqstar) 					#B를 충분히 크게하면 방법1과 방법2의 분산이 비슷해짐

#실습 7.13
n <- 10
x <- c(56,101,78,67,93,87,64,72,80,69)
a <- -5; b <- 5
#확률 p(a < sum(xi/n)-M < b) 추정하기
g(x1,,,xn) = I(a < (xbar-M) < b)
theta(F) = EF[g(x1,,,xn)]
 	 = EF[I(a < xbar-M < b)] 		#xbar:붓스트랩 표본의 표본평균
 	 = p(a < xbar-M < b)
theta(Fn) = P(a < xbarstar-xbar < b)
 	  = EFn(I(a < xbarstar-xbar < b))
xbar = sum(x1+,,,+xn)/n
     = sum_i ~ B(xbarstar(i))/B

#Example 7e는 생략
