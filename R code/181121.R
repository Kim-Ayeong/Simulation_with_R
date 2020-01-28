#11월 21일

#8.6절 p.111

#Importance sampling is also quite useful in estimating E[h(X)|X∈A]
x가 f보다 더 잘 일어나는 분포로부터 추출

#Importance sampling can also be used to estimate Pf{X>a}
E[v] = E[V|W=1]P(W=1) + E[V|W=0]P(W=0)
V = I(X>a)f(X)/g(X)
W = I(X>a)
W = 0 or 1인데 0인 경우는 생각X
#밑으로는 생략

# 실습 1
get.SN = function(mu, sigma, A=5, B=5) {
  S = 0
  repeat {
    S = S + rnorm(1, mu, sigma)
    if (S < -A | S > B) break
  }
  return(S)
}

one.run = function(mu=-0.1, sigma=0.3, A=5, B=5) {
  if (get.SN(mu, sigma, A, B) > B) raw.hat = 1 else raw.hat = 0
  S = get.SN(-mu, sigma, A, B)
  if (S > B) imp.hat = exp(2*mu*S/sigma^2) else imp.hat = 0
  return(list(imp.hat=imp.hat, raw.hat=raw.hat))
}

m.values = c(10,100,500,1000,2000,5000)
imp.hat = raw.hat = numeric()
for (i in 1:max(m.values)) {
  lst = one.run(mu=-0.1, A=5, B=5)
  imp.hat[i] = lst$imp.hat
  raw.hat[i] = lst$raw.hat
}

options(digits=4) 			# 줄 맞춰서 출력하기 위해 필요
for (m in m.values) {
  if (m == 10) cat(" m \tMean_Imp_Est\t SE_Imp_Est \tMean_Raw_Est\t SE_Raw_Est\n")
  cat(format(m, width=4), "\t", format(mean(imp.hat[1:m]), width=8), "\t",
  format(sd(imp.hat[1:m])/sqrt(m), width=8), "\t",
  format(mean(raw.hat[1:m]), width=8), "\t",
  format(sd(raw.hat[1:m])/sqrt(m), width=8), "\n")
}

#8.7절

T*=(T1*,,,Tn*)은 T에 상관없이 독립적으로 추출한 r.v

#8.8절

주가모형 : Pn = P0*exp(X1+,,,+Xn), Xi ~ iid N(m, sig^2)
geometric Brownian motion process : log(Pn/P0) = X1+,,,Xn ~ N(n*m, n*sig^2)
European call option : C(K,t,v) = E[(P(t)-K)+]
up-and-in barrier option : R = I(P(s)>b)(P(t)-K)+
P(t) ~ u*exp(w), w ~ N(t*m, t*sig^2)

#실습
#Exotic option의 이익 R을 구해주는 함수
R.exotic <- function(v=100, t=90, K=100, s=30, b=95, mu=0.0001, sigma=0.01) {
  P.s <- v*exp(rnorm(1, mean=s*mu, sd=sqrt(s)*sigma)) 
  if(P.s > b) {
    P.t <- P.s*exp(rnorm(1, mean=(t-s)*mu, sd=sqrt(s)*sigma))
    R <- max(P.t - K, 0)
  } else R <- 0
  return(R)
}
m <- 1000
R.vec <- replicate(m, R.exotic())
c(mean(R.vec), sd(R.vec)/sqrt(m))	# 3.8 ~ 4.6 사이의 값이 나오면 정답

#밑으로는 생략


