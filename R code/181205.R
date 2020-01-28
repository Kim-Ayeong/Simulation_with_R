#12월 5일

#10.1절

irreducible:더이상 축소할 수 없음
aperiodic:주기가 없음
positive recurrent:어떤 상태를 계속 방문할 수 있음
transition probabilities(전이확률) Pij^n : i에서 시작해 n번만에 j에 도달, i에 상관없음
lim(n>inf)P13^n = pi(3)
lim(n>inf)P23^n = pi(3)
lim(n>inf)P33^n = pi(3) 모두 같음, but 비현실적

#10.2절

pi(j) = P(V = vj) = b(j)/B
Markov chain 만들기(알고리즘 이해!)
X시퀀스 만들기 > X시퀀스의 분포=pi > pi 찾기(min(~))
Pij = P(Xn+1=j|Xn=i) = P(X1=j|Xd=i) : 현재 상태 = i, 다음 상태 = j
q(xn, x) 함수는 Y값을 찾기 쉬운 아무 함수로 지정

#실습
g(x, y) = 1(모든 x,y i.e) Y~U(0,1)
alpha(x, y) = min{[y^a-1*(1-y)^b-1] / [x^a-1*(1-x)^b-1], 1}
alpha = 2.7, beta = 6.3

#target f is beta(2.7, 6.3) distribution
a = 2.7; b = 6.3; N = 5000; n.sim = 100
target.f = function(x) x^(a-1)*(1-x)^(b-1)
pval.1 = pval.2 = pval.3 = numeric(n.sim)
for (i in 1:n.sim) { 					#반복실험으로 K-S 검정의 p값의 평균을 구하고자 함
  x = numeric(N)
  x[1] = runif(1)
  for (n in 1:(N-1)) {
    y = runif(1)
    if (runif(1) < target.f(y)/target.f(x[n])) x[n+1] = y 
    else x[n+1] = x[n]
  }
  pval.1[i] = ks.test(jitter(x[1:100]), "pbeta", a, b)$p.value
  pval.2[i] = ks.test(jitter(x[4901:5000]), "pbeta", a, b)$p.value
  pval.3[i] = ks.test(jitter(x[seq(4010, 5000, 10)]), "pbeta", a, b)$p.value
}
c(mean(pval.1), mean(pval.2), mean(pval.3)) 		#0.1014, 0.1082, 0.4866

#Example 9b 질문
#https://rseek.org 참고
x <- rexp(10, rate=0.5)
plot(ecdf(x), main="Fn(x) and F(x)")
curve(pexp(x, rate=0.5), add=T)



