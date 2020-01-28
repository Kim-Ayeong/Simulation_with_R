#11월 28일

#9.2절 중요

chisq.test()는 적절하지 않음. (why?)
람다를 추정해 확률값을 주고 추정해야하지만 chisq.test()는 알려진 고정 람다값을 이용

#Example 9c 
Y1,...,Y30
Y = # of accident in a day
vi = 0 1 2 3 4 5 8(건) > 5 8을 5이상으로 묶음
Ni = 6 2 1 9 7 4 1(일) > 총 30(일)
H0 : the number of accidents in 30-day period ~ Poisson(람다)
람다hat = ybar = (0*6+1*2+...+5*4+8*1)/30 = (계산)

n = 30
N = c(6,2,1,9,7,5) 					# 하루 사고 건수 5건과 8건을 묶어서 한 범주로 만들었음에 주의
m = (sum(N[1:5] * 0:4) + 4*5 + 1*8)/30 			# 하루 사고 건수 5건이 4일, 8건이 1일
phat = dpois(0:4, m)
phat[6] = 1 - ppois(4, m)
chisq.test(N, p=phat) 					# 자유도에 주의. 잘못된 자유도 때문에 적절하지 않은 p값
# 추정된 모수의 수만큼 조정된 자유도로 다음과 같이 p값을 계산해야 함.
t = sum((N - n*phat)^2/(n*phat))
1 - pchisq(t, 4) 					# n이 클 때의 근사적 p값. (n이 충분히 큰가?) > No

#실습
H0가 참일 때 P값(확률변수) ~ U(0,1)를 따른다
(1)사진 참고
(2) ks.test 함수에서 exact=TRUE로 지정하지 않고 구한 p값은 적절한가? > No
(3) ks.test 함수에서 exact=TRUE로 지정하고 구한 p값은 적절한가? > No

#평균이 theta인 지수분포로부터 난수 n개를 생성
#H0 : 모집단이 지수분포(theta는 미지)
#모의실험으로 정확한(exact) p값을 구해서 ks.test의 점근적(aymptotic) p값과 비교
theta = 1; n = 30; B = 500; n.sim = 200
p.ks = p.sim = numeric(n.sim) 
Dstar = numeric(B)
for (j in a:n.sim) {
  y = rexp(n, rate=theta) 				#n개의 관측값
  theta.hat = 1/mean(y)
  Fofy = pexp(sort(y), theta.hat)
  d = max(1:n/n - Fofy, Fofy - 0:(n-1)/n)
  #위 문장 대신에 d = ks.test(y, "pexp", rate=theta.hat)$statistic를 써도 됨
  #함수 ks.test로 p값 구하기(적절하지 않은 방법)
  p.ks[j] = ks.test(y, "pexp", rate=theta.hat)$p.value
  #모의실험으로 p값 구하기
  for (i in 1:B) {
    y.star = rexp(n, rate=theta.hat)
    theta.star = 1/mean(y.star)
    Fofystar = pexp(sort(y.star), theta.star)
    Dstar[i] = max[1:n/n - Fofystar, Fofystar - 0:(n-1)/n)
  }
  p.sin()
}
boxplot()
ks.test

#The Continuous Data Case(2017 기출 2번), 사진 참고
평균 5인 지수분포로부터 크기 20일 표본 추출
이 표본으로 '모집단 분포가 지수분포이다.'라는 가설을 검정

#방법1 > X
n = 20; mu = 5; m = 1000
pval = numeric(m)
for (i in 1:m) {
  y = rexp(n, rate=1/mu)
  mu.hat = mean(y)
  #ks.test(y, "pexp", rate=1/mu.hat)에서 str()보고 필요한 값 꺼내기
  pval[i] = ks.test(y, "pexp", rate=1/mu.hat)$p.value 		#p값 꺼내기
}
boxplot(pval) 							#uniform 분포X, p를 과대추정함
mean(pval<0.05) 						#0.05가 나와야 uniform 분포, p가 과대추정되어 0.05보다 작게 나옴

#방법2 > p.129 (1)~(3)단계로 추정
n = 20; mu = 5; m = 100; B = 200
pval.over = pval.right = numeric(m)
for (i in 1:m) {
  y = rexp(n, rate=1/mu)
  mu.hat = mean(y)
  d = ks.test(y, "pexp", rate=1/mu.hat)$statistic
  pval.over[i] = ks.test(y, "pexp", rate=1/mu.hat)$p.value
  I = numeric(B)
  for (j in 1:B) {
    yhat = rexp(n, rate=1/mu.hat)
    mu.hat.star = mean(yhat)
    dstar = ks.test(yhat, "pexp", rate=1/mu.hat.star)$statistic
    I[j] = as.numeric(dstar>d)
  }
  pval.right[i] = mean(I)
}
boxplot(pval.right)


