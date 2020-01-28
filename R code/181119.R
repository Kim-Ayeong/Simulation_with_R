#11월 19일

#8.6절

#Example 8w
h(x)f(x)/g(x)
I(Sn>B) 추정
왜 importance sampling이 필요한가? > SN(straight estimation)이 계속 A보다 작아짐

#실습1
1. X1, X2 ~ iid N(-m, sig^2) 생성 후
2. I*exp(2m*SN/sig^2) 값 추정하기

#straight estimator
mu <- (-0.1); sigma <- 0.3; A <- B <- 5 		#슬라이드에서 sigma 수정
m <- 5000
I.straight <- numeric(m)
for (i in 1:m) {
  SN <- rnorm(1, mean=mu, sd=sigma)
  N <- 1
  repeat {
    if (SN < -A | SN > B) break
    SN <- SN + rnorm(1, mean=mu, sd=sigma)
    N <- N + 1
  }
  I.straight[i] <- as.numeric(SN > B)
}
(est.straight <- mean(I.straight)) 
#반복횟수를 늘려도 계속 0 > importance sampling을 해보아야 함

#importance estimator
mu <- (-0.1); sigma <- 0.3; A <- B <- 5
m <- 5000
onerun.imp <- numeric(m)
for (i in 1:m) {
  SN <- rnorm(1, mean=-mu, sd=sigma) 			#-mu로 바뀜
  N <- 1
  repeat {
    if (SN < -A | SN > B) break
    SN <- SN + rnorm(1, mean=-mu, sd=sigma)
    N <- N + 1
  }
  onerun.imp[i] <- as.numeric(SN > B) * exp(2*mu*SN/sigma^2)
}
(est.imp <- mean(onerun.imp)) 

#결과
for (n in c(10,100,500,1000,2000,5000)) {
  cat(n, "\t", mean(I.straight[1:n]), "\t", mean(onerun.imp[1:n]), "\n")
}


