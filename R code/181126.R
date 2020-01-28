#11월 26일

#9.1절

Dn=max[Fn(x)-F(x)]?
귀무가설이 참일 때 Dn의 분포 파악 > Dn이 충분히 크면 귀무가설 기각

#Asymptotic result
why? > X~F(.)이면 F(X) ~ U(0,1)이다.

#Example 9b and more
#표본이 평균이 100인 지수분포에서 추출된 것이 맞나?
#ecdf() : empirical cdf(경험적 누적분포함수)
draw.FnF = function(x, F, ...) { 		# draw the ecdf Fn and the assumed df F
  plot(ecdf(x), main="Fn(x) and F(x)")
  curve(F(x, ...), add=T)
}
y = c(66,72,81,94,112,116,124,140,145,155)
draw.FnF(y, pexp, rate=0.01)
ks.test(y, "pexp", rate=0.01)
y = rexp(10, rate=0.01) 			# H0(F는 평균 100인 지수분포)가 참일 때 그림과 검정결과
draw.FnF(y, pexp, rate=0.01)
ks.test(y, "pexp", rate=0.01)

#Dn 구하기
y = c(66,72,81,94,112,116,124,140,145,155)
F.y = pexp(y, rate=0.01) 			#F.y = 1-exp(-y/100)도 가능
n = length(y)
d = max((1:n)/n-F.y, F.y-(0:(n-1))/n) 		#max(j/n-F.y, F.y-j/n)를 한번에 구함

#Exercise 9.1
#하양 분홍 빨강 콩의 확률이 1:2:1인가?
#chisq.test() 이용
x = c(141, 291, 132)
chisq.test(x, p=c(1/4, 1/2, 1/4))
chisq.test(x, p=c(1/4, 1/2, 1/4), simulate.p.value=T)

#Exercise 9.4
#표본이 균일분포(50, 200)으로부터 나왔는가?
#ks.test() 이용
x = c(164,142,110,153,103,52,174,88,178,184,58,62,132,128)
draw.FnF = function(x, F, ...) { 		# draw the ecdf Fn and the assumed df F
  plot(ecdf(x), main="Fn(x) and F(x)")
  curve(F(x, ...), add=T)
}
draw.FnF(sort(x), punif, min=50, max=200)
ks.test(x, punif, min=50, max=200) 		#unif 의 cdf
ks.test(x, function(x) (x-50)/150) 		#둘 다 가능




