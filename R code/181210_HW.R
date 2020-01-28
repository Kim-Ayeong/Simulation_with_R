#12�� 10�� ����

#Example 9d : Example 9c�� �������� ���ǽ������� p�� ���ϱ�
table(rpois(30, 3.9)) 					#0����, 5�̻��� ���ַ� ����

B = 10000 						# ���ǽ��� Ƚ��. p���� �����Ƿ� ���ǽ��� Ƚ���� ũ�� �� �ʿ䰡 ����.
N = c(6,2,1,9,7,5) 					# �Ϸ� ��� �Ǽ� 5�ǰ� 8���� ��� �� ���ַ� ��������� ����
n = sum(N) 						# ǥ�� ũ��
mhat = (sum(N[1:5] * 0:4) + 4*5 + 1*8)/30 		# �Ϸ� ��� �Ǽ� 5���� 4��, 8���� 1��
phat = dpois(0:4, mhat)
phat[6] = 1 - ppois(4, mhat)
t = sum((N - n*phat)^2/(n*phat))
1 - pchisq(t, df=4) 					# n�� Ŭ ���� �ٻ��� p��.
# �������� 5�� �ƴ϶� 4�ӿ� ����. �������� ����� ���������� n�� ����� ũ�� �����Ƿ� �������� ����.

# [Q] 'n�� ����� ũ��'�� �뷫���� ������ �����ΰ�? [A] ���������� ��뵵���� 5 �̻��� ���� ������ 80% �̻��� ��
# ���ǽ����� ���� ����� �� p���� ���غ���.
Nstar = numeric(length = 6) 				# N[1]�� n�� ������ �߿��� 0�� �������� ����
Tstar = numeric(B)
for (b in 1:B) {
  ystar = rpois(n, mhat)
  for (i in 0:4) Nstar[i + 1] = sum(i == ystar) 	# N_i^* ���
  Nstar[6] = n - sum(Nstar[1:5])
  mhatstar mean(ystar)
  phatstar dpois(0:4, mhatstar)
  phatstar[6] = 1 - ppois(4, mhatstar)
  Tstar[b] = sum((Nstar - n*phatstar)^2/(n*phatstar))
}
I.vec = (Tstar >= t)
c(mean(I.vec), sd(I.vec)/sqrt(B)) 			# ���ǽ��迡 ���� p���� ǥ�ؿ���
							#0.0004, 0.00019