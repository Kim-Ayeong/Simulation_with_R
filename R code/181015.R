#10�� 15��

#6.8��

#a<0�� �� �������
Pm(=S(N-m)): ���⸦ m �յ� ���������� �ְ�
1. Pm > K
2. ��� i�� ���� Pm > K+~ (m=0�϶��� ����1�� ����)

# Exercise 6.17
n.sim = 1000  							# ���ǽ��� Ƚ��
N = 20; 							# �� ����
K = 100; 							# ����
S.zero = 100; 							# �ְ� �ʱⰪ
mu = -0.05; sg = 0.3; 						# �ְ� ���� ����
alp = mu+0.5*sg^2 						# ����
E = numeric(length=n.sim)					# �������

for (i in 1:n.sim) {
  P = numeric(length=N+1) 						# to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[N+1] = S.zero 							# �ܰ�1
  m = N - 1  								# �ܰ�1
  flag = FALSE 								# �ɼ��� ����� �� �ִ� ������ �����Ǹ� TRUE, �ƴϸ� FALSE.
  repeat {
    m.plus = m+1 							# to avoid P[0], we need a new index m.plus
    P[m.plus] = P[m.plus+1] * exp(rnorm(1,mu,sg)) 			# m.plus������ �ְ�, �ܰ�2
    if (P[m.plus]>K) flag = TRUE 					# ����(1) üũ
    if (m>0) { 								# m�� 0�� ���� �������� ����
      b = ((1:m)*mu-log(K/P[m.plus]))/(sg*sqrt(1:m)) 			# ����(2) üũ
      op = P[m.plus]*exp((1:m)*alp)*pnorm(sg*sqrt(1:m)+b)-K*pnorm(b)
      flag = all(P[m.plus]>K+op) 					# 1���� m���� ��� ������ �Ǿ�� ��, any, all �˾Ƶα�
    }
    if (flag) break
    else m = m-1 							# ����(1),(2)�� ��� �����Ǹ� ���� �Ϸ�
    if (m<0) break 							# ���Ⱑ �������� ���� �Ϸ�
  }
  if (flag) E[i] = P[m.plus]-K
  else E[i] = 0
}
c(mean(E), sd(E)/sqrt(n.sim), mean(E>0)) 				# �������, ǥ�ؿ���, �ɼ����Ȯ���� �����Ͽ� ���


#����
#Exercise 6.17���� ������� ��ٷȴٰ� ����ϴ� ������ �������(a>0)

#�׷��� �׷�����
S0 <- 100; mu <- (-0.05); sigma <- 0.3
N <- 20
x <- rnorm(N, mu, sigma)
S <- S0*exp(cumsum(x)) 							#�ְ� ����
plot(c(S0, S), type="l", xlab="t", ylab="Stock Price")

#������� ��ٸ���
n.sim = 100  							# ���ǽ��� Ƚ��
N = 20; 							# �� ����
K = 100; 							# ����
S.zero = 100; 							# �ְ� �ʱⰪ
mu = -0.05; sg = 0.3; alp = mu+0.5*sg^2				# �ְ� ���� ����
E = numeric(n.sim)						# �������

for (i in 1:n.sim) {
  P = numeric(N+1) 						# to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[1] = S.zero 	
  for (j in 2:N+1) {
    P[j] = P[j-1] * exp(rnorm(1, mu, sg)) 			# j������ �ְ�
  }
  if (P[i]>K) E[i] = P[i]-K
  else E[i] = 0
}

E
c(mean(E), sd(E)/sqrt(n.sim)) 					# �������, ǥ�ؿ���