#10�� 15�� ����

#Exercise 6.17���� ������� ��ٷȴٰ� ����ϴ� ������ �������(a>0)

#������ ���ǽ��� > ���� �߻�!
n.sim = 100  						# ���ǽ��� Ƚ��
N = 20; 							# �� ����
K = 100; 							# ����
S.zero = 100; 						# �ְ� �ʱⰪ
mu = -0.05; sg = 0.3; alp = mu+0.5*sg^2		# �ְ� ���� ����
E = numeric(n.sim)					# �������

for (i in 1:n.sim) {
  P = S.zero 	
  for (j in 1:N) {
    P = P*exp(rnorm(1, mu, sg)) 			# ����� �ְ�
  }
  if (P>K) E[i] = P-K
  else E[i] = 0
}
#E
c(mean(E), sd(E)/sqrt(n.sim))

----------------------------------------------------

# �� ���� ���ǽ��� �ȿ��� ��� 

n.sim = 1000  						# ���ǽ��� Ƚ��
N = 20; 							# �� ����
K = 100; 							# ����
S.zero = 100; 						# �ְ� �ʱⰪ
mu = -0.05; sg = 0.3; 					# �ְ� ���� ����
alp = mu+0.5*sg^2 					# ����
E1 = numeric(n.sim)					# a<0�� �� �������
E2 = numeric(n.sim)					# a>0�� �� �������

for (i in 1:n.sim) {
  P = numeric(length=N+1) 						# to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[N+1] = S.zero 							# �ܰ�1
  m = N - 1  								# �ܰ�1
  flag = FALSE 								# �ɼ��� ����� �� �ִ� ������ �����Ǹ� TRUE, �ƴϸ� FALSE.

  repeat {
    m.plus = m+1 								# to avoid P[0], we need a new index m.plus
    P[m.plus] = P[m.plus+1] * exp(rnorm(1,mu,sg)) 		# m.plus������ �ְ�, �ܰ�2
    if (P[m.plus]>K) flag = TRUE 					# ����(1) üũ
    if (m>0) { 								# m�� 0�� ���� �������� ����
      b = ((1:m)*mu-log(K/P[m.plus]))/(sg*sqrt(1:m)) 		# ����(2) üũ
      op = P[m.plus]*exp((1:m)*alp)*pnorm(sg*sqrt(1:m)+b)-K*pnorm(b)
      flag = all(P[m.plus]>K+op) 					# 1���� m���� ��� ������ �Ǿ�� ��, any, all �˾Ƶα�
    }

    if (flag) break
    else m = m-1 								# ����(1),(2)�� ��� �����Ǹ� ���� �Ϸ�
    if (m<0) break 							# ���Ⱑ �������� ���� �Ϸ�
  }
  wait_P = P[m.plus]							# ���� �� �ְ�
  for (j in N-m.plus:N) {						# break�� �� ���� �� �ְ� ���
    wait_P = wait_P * exp(rnorm(1,mu,sg))
  }
  if (flag) E1[i] = P[m.plus]-K					# ���� �� �ɼ� ���
  else E1[i] = 0
  if (wait_P>K) E2[i] = wait_P-K					# ���� �� �ɼ� ���
  else E2[i] = 0
}
#E1
#E2
c(mean(E1), sd(E1)/sqrt(n.sim), mean(E1>0)) 				# �������, ǥ�ؿ���, �ɼ����Ȯ�� ����ġ
c(mean(E2), sd(E2)/sqrt(n.sim), mean(E2>0)) 				# �������, ǥ�ؿ���, �ɼ����Ȯ�� ����ġ