#10��8��

#Ch 6. The Discrete Event Simulation Approach

#6.2��
#single server : �� ���
#Queueing System : ��⿭ �ý���, ���� ���� ���� ����� ���� ������
t : ���� �ð�
NA : (t����) ������ ������ ��
ND : ���� �Ϸ�� ������ ��
n : �ð�t�� �ý��� �ȿ� �ִ� ������ ��
tA : �ð�t ���Ŀ� ���� ����(next customer)�� �����ϴ� �ð�
�ý����� ������� ���� ���¿��� ������ ��� �����ϸ� �� ���� ��� Ŀ��
tD : �ð�t ���� ���� �ް� �ִ� ������ ���񽺰� ������ �ð�
���� ���� ���� ������ ������ ��, ������?
A(i) : i��° ������ �����ð�
D(i) : i��° ������ ���� �Ϸ�ð�
Tp : ������ �ʰ��ٹ� �ð�(������ ������ ���� �Ϸ�ð��� T�� ���� ��� ���� �Ϸ�ð����� T�� �� �ð�)
���Ϸ��� �� > E[D(i)-A(i), E(Tp)

#��� 4����
min(tA, tD)<T, n=0
		   n>0
min(tA, tD)>T, tA<tD 		#���񽺰� ������ ���� ���� ���� ����
		tA>tD 		#������ �����ϱ� ���� ���񽺰� ����

#Exercise 6.1
#A Single-Server Queueing System
n.sim = 100
rate = 10 						# �����ð� ������ ��������
shape = 3; scale = 1/40 				# ���� �ð��� ��������
							# 1�δ� ��� ó�� �ð�:3/40, 10�� ó��:(3/40)*10=3/4, �ð��� 3/40�� ó��
T = 9							# �� ó���ð�
time.over = time.spend = numeric(n.sim) 		# �������� ���ü���ð�, ����(server)�� ���ܱٹ��ð�
for (i in (1:n.sim)) {
  t = NofA = NofD = 0					# ���� �ð�, ������ ���� ��, ���� �Ϸ� ���� ��
  n = 0 						# SS(System State) is the number of customers in the system
  tA = rexp(1,rate) 					# ���� �ð� ���� ���� ������ �����ϴ� �ð�
  tD = Inf 						# ���� ���� �ް� �ִ� ������ ���� �Ϸ� �ð�. �ʱⰪ�� ���Ѵ�
  A = D = numeric() 					# ��º���: A[i]=i��° ������ �����ð�, D[i]=i��° ������ ���� �Ϸ� �ð�(departure time)
  repeat {
    if (tA <= tD & tA <= T){  				# case1, ù��° ����� ���� ���񽺰� �Ϸ��ϱ� ���� �ι�° ��� ����, ���� �ð��� ������ ���� ���� ���� ����
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma(n=1, shape, scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){				# case2, �� ������ �����ϱ� ���� ���� �Ϸ�
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf 				# ��� ���� ������ ������ tD�� ���Ѵ�
      else tD = t + rgamma(n=1, shape, scale=scale) 	#������� ����� ������ tD �ٽ� ����
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){			# case3, �����ð��� �������� �����ؾ��� ������ ����
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0) break  		# case4, �����ð��� ������, �����ؾ��� ������ ����
  }							# end of repeat
  time.spend[i] = mean(D-A)
  time.over[i] = max(t-T,0)
}							# end of for (i in (1:n.sim))
time.spend
time.over

cat("���� �� ���� �ý��ۿ� ü���ϴ� �ð��� ��� = ", mean(time.spend),
"\n������ �ʰ��ٹ��ð��� ��� =", mean(time.over))

#Exercise 6.2
#case 1, 4������ �߻�
(1) ���ο� ����(ù ���� ����)�� �������� �� n=0 �̸�
������ �޽Ľð�(idle time)�� ������ ����� �Ͼ �ð����� �� ������ ������ ������ ����� �ð�
(2) ���� �ð� T���� n=0 �̸� ������ �޽Ľð��� 'T ���� ������ ������ ���� �Ϸ� ����'���� T ������ �ð�
(1)+(2)=(0, T)������ �޽Ľð�

n.sim = 100
rate = 10 						# �����ð� ������ ��������
shape = 3; scale = 1/40 				# ���� �ð��� ��������
							# 1�δ� ��� ó�� �ð�:3/40, 10�� ó��:(3/40)*10=3/4, �ð��� 3/40�� ó��
T = 9							# �� ó���ð�
time.over = time.spend = numeric(n.sim) 		# �������� ���ü���ð�, ����(server)�� ���ܱٹ��ð�
time.rest = numeric(n.sim)				# ���� �޽Ľð�

for (i in (1:n.sim)) {
  t = NofA = NofD = 0					# ���� �ð�, ������ ���� ��, ���� �Ϸ� ���� ��
  n = 0 						# SS(System State) is the number of customers in the system
  tA = rexp(1,rate) 					# ���� �ð� ���� ���� ������ �����ϴ� �ð�
  tD = Inf 						# ���� ���� �ް� �ִ� ������ ���� �Ϸ� �ð�. �ʱⰪ�� ���Ѵ�
  A = D = numeric() 					# ��º���: A[i]=i��° ������ �����ð�, D[i]=i��° ������ ���� �Ϸ� �ð�(departure time)
  repeat {
    if (tA <= tD & tA <= T){  				# case1, ù��° ����� ���� ���񽺰� �Ϸ��ϱ� ���� �ι�° ��� ����, ���� �ð��� ������ ���� ���� ���� ����
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma(n=1, shape, scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){				# case2, �� ������ �����ϱ� ���� ���� �Ϸ�
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf 				# ��� ���� ������ ������ tD�� ���Ѵ�
      else tD = t + rgamma(n=1, shape, scale=scale) 	#������� ����� ������ tD �ٽ� ����
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){			# case3, �����ð��� �������� �����ؾ� �� ������ ����
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0) break 			# case4, �����ð��� ������, �����ؾ� �� ������ ����

  }							# end of repeat
  time.spend[i] = mean(D-A)
  time.over[i] = max(t-T,0)

  if (A[i+1]>D[i]) time.rest[i]=A[i+1]-D[i] 		# ���ο� ������ ������ �ð��� ���� ���񽺸� �Ϸ��� �ð����� Ŭ ��,
  if (D[NofD]<T) time.rest[i]=T-D[NofD]     		# ������ ���񽺸� �Ϸ��� �ð��� ��ü �ð� T���� ���� ��,
}							# end of for (i in (1:n.sim))

cat("�� ���� �޽� �ð� : ", sum(time.rest))



