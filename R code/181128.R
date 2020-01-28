#11�� 28��

#9.2�� �߿�

chisq.test()�� �������� ����. (why?)
���ٸ� ������ Ȯ������ �ְ� �����ؾ������� chisq.test()�� �˷��� ���� ���ٰ��� �̿�

#Example 9c 
Y1,...,Y30
Y = # of accident in a day
vi = 0 1 2 3 4 5 8(��) > 5 8�� 5�̻����� ����
Ni = 6 2 1 9 7 4 1(��) > �� 30(��)
H0 : the number of accidents in 30-day period ~ Poisson(����)
����hat = ybar = (0*6+1*2+...+5*4+8*1)/30 = (���)

n = 30
N = c(6,2,1,9,7,5) 					# �Ϸ� ��� �Ǽ� 5�ǰ� 8���� ��� �� ���ַ� ��������� ����
m = (sum(N[1:5] * 0:4) + 4*5 + 1*8)/30 			# �Ϸ� ��� �Ǽ� 5���� 4��, 8���� 1��
phat = dpois(0:4, m)
phat[6] = 1 - ppois(4, m)
chisq.test(N, p=phat) 					# �������� ����. �߸��� ������ ������ �������� ���� p��
# ������ ����� ����ŭ ������ �������� ������ ���� p���� ����ؾ� ��.
t = sum((N - n*phat)^2/(n*phat))
1 - pchisq(t, 4) 					# n�� Ŭ ���� �ٻ��� p��. (n�� ����� ū��?) > No

#�ǽ�
H0�� ���� �� P��(Ȯ������) ~ U(0,1)�� ������
(1)���� ����
(2) ks.test �Լ����� exact=TRUE�� �������� �ʰ� ���� p���� �����Ѱ�? > No
(3) ks.test �Լ����� exact=TRUE�� �����ϰ� ���� p���� �����Ѱ�? > No

#����� theta�� ���������κ��� ���� n���� ����
#H0 : �������� ��������(theta�� ����)
#���ǽ������� ��Ȯ��(exact) p���� ���ؼ� ks.test�� ������(aymptotic) p���� ��
theta = 1; n = 30; B = 500; n.sim = 200
p.ks = p.sim = numeric(n.sim) 
Dstar = numeric(B)
for (j in a:n.sim) {
  y = rexp(n, rate=theta) 				#n���� ������
  theta.hat = 1/mean(y)
  Fofy = pexp(sort(y), theta.hat)
  d = max(1:n/n - Fofy, Fofy - 0:(n-1)/n)
  #�� ���� ��ſ� d = ks.test(y, "pexp", rate=theta.hat)$statistic�� �ᵵ ��
  #�Լ� ks.test�� p�� ���ϱ�(�������� ���� ���)
  p.ks[j] = ks.test(y, "pexp", rate=theta.hat)$p.value
  #���ǽ������� p�� ���ϱ�
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

#The Continuous Data Case(2017 ���� 2��), ���� ����
��� 5�� ���������κ��� ũ�� 20�� ǥ�� ����
�� ǥ������ '������ ������ ���������̴�.'��� ������ ����

#���1 > X
n = 20; mu = 5; m = 1000
pval = numeric(m)
for (i in 1:m) {
  y = rexp(n, rate=1/mu)
  mu.hat = mean(y)
  #ks.test(y, "pexp", rate=1/mu.hat)���� str()���� �ʿ��� �� ������
  pval[i] = ks.test(y, "pexp", rate=1/mu.hat)$p.value 		#p�� ������
}
boxplot(pval) 							#uniform ����X, p�� ����������
mean(pval<0.05) 						#0.05�� ���;� uniform ����, p�� ���������Ǿ� 0.05���� �۰� ����

#���2 > p.129 (1)~(3)�ܰ�� ����
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

