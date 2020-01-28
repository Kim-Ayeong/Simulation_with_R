#12월 10일

#10.3절

#Example 10d
#지수분포의 무기억성
X~e(람다)일 때,
X | X>d =(d)= d+Y, Y~e(람다)

X1 | (X2,...,Xn), S > c
<-> S > c 
<-> X1+...+Xn > c
<-> X1 > c-(X2+...+Xn)
<-> Y+(c-(X2+...+Xn)), Y~e(람다1)

#Example 10h
#p.144 알고리즘
N = 5000
y = w1 = w2 = numeric(N)
y[1] <- runif(1, min=0.02, max=0.1)
w1[1] <- rexp(1, rate=y[1])
w2[1] <- rexp(1, rate=y[1])
for (t in 1:(N-1)) {
  repeat {
    y[t+1] = rgamma(1, shape=3, rate=w1[t]+w2[t])
    if (y[t+1] > 0.02 & y[t+1] < 0.1) break
  }
  w1[t+1] = rgamma(1, 26, rate=y[t+1]+0.5)
  w2[t+1] = rgamma(1, 19, rate=y[t+1]+0.5)
}
c(mean(w1), mean(w2))
c(mean(w1[1001:5000]), mean(w2[1001:5000]))
plot(w1) 				 #빨리 안정된 상태
					 #사진 c~ 네줄 참고



