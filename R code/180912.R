#9월 12일

#예1
m1=-2; m2=1; s1=1; s2=3;
alp=0.6
curve(dnorm(x, m1, s1), from=-8, to=8, lty=2, col="red", ylab="")
curve(dnorm(x, m2, s2), lty=2, col="blue", add=T)
f.mixture <- function(x) alp*dnorm(x, m1, s1) + (1-alp)*dnorm(x, m2, s2)
curve(f.mixture(x), lwd=2, add=T)

#예2
n = 100
x1=rnorm(n, m1, s1); x2=rnorm(n, m2, s2)
x=ifelse(runif(n) < alp, x1, x2)
x

rmultinom(1, 30, rep(1/6,6)) 	#공정한 주사위를 30번 던지는 실험을 1번 시행
rmultinom(1, 300, rep(1/6,6))
rmultinom(10, 300, rep(1/6,6))
sample(1:6, 30, rep=T)

#과제 피드백
n.cards <- 100
N <- replicate(10000, sum(1:n.cards == sample(1:n.cards, n.cards)))
mean(N); var(N)

#실습1(4판 연습문제 16번)
n <- 100
x1 <- rgeom(n, 1/2)
x2 <- rgeom(n, 1/2)
x <- ifelse(runif(n)<0.5, x1, x2)
x+1
 
#실습2는 삭제
#실습3(4판 연습문제 7번) > 과제


