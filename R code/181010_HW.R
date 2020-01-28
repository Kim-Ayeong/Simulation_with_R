#10�� 10�� ����

#Exercise 6.2

n.sim = 100
rate = 10 						# �����ð� ������ ��������
shape = 3; scale = 1/40 				# ���� �ð��� ��������
							# 1�δ� ��� ó�� �ð�:3/40, 10�� ó��:(3/40)*10=3/4, �ð��� 3/40�� ó��
T = 9							# �� ó���ð�
time.over = time.spend = numeric(n.sim) 	# �������� ���ü���ð�, ����(server)�� ���ܱٹ��ð�
time.rest = numeric(n.sim)			# ���� �޽Ľð�

for (i in (1:n.sim)) {
  t = NofA = NofD = 0				# ���� �ð�, ������ ���� ��, ���� �Ϸ� ���� ��
  n = 0 						# SS(System State) is the number of customers in the system
  tA = rexp(1,rate) 				# ���� �ð� ���� ���� ������ �����ϴ� �ð�
  tD = Inf 						# ���� ���� �ް� �ִ� ������ ���� �Ϸ� �ð�. �ʱⰪ�� ���Ѵ�
  A = D = numeric() 				# ��º���: A[i]=i��° ������ �����ð�, D[i]=i��° ������ ���� �Ϸ� �ð�(departure time)
  repeat {
    if (tA <= tD & tA <= T){  			# case1, ù��° ����� ���� ���񽺰� �Ϸ��ϱ� ���� �ι�° ��� ����, ���� �ð��� ������ ���� ���� ���� ����
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma(n=1, shape, scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){			# case2, �� ������ �����ϱ� ���� ���� �Ϸ�
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf 			# ��� ���� ������ ������ tD�� ���Ѵ�
      else tD = t + rgamma(n=1, shape, scale=scale) #������� ����� ������ tD �ٽ� ����
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){		# case3, �����ð��� �������� �����ؾ� �� ������ ����
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0) break 	# case4, �����ð��� ������, �����ؾ� �� ������ ����

  }							# end of repeat
  time.spend[i] = mean(D-A)
  time.over[i] = max(t-T,0)

  if (A[i+1]>D[i]) time.rest[i]=A[i+1]-D[i] # ���ο� ������ ������ �ð��� ���� ���񽺸� �Ϸ��� �ð����� Ŭ ��,
  if (D[NofD]<T) time.rest[i]=T-D[NofD]     # ������ ���񽺸� �Ϸ��� �ð��� ��ü �ð� T���� ���� ��,
}							 # end of for (i in (1:n.sim))
cat("�� ���� �޽� �ð� : ", sum(time.rest))



# Exercise 6.12

n.sim = 100 					   # ���ǽ��� �ݺ�Ƚ��
n0=1; a0=25000; T=365; c=11000		   # n:����� ��, a:�ں���, T:�ѽð�, c:�ð��� �����ϴ� �����
lm=10; nu=0; mu=0 			  	   # lm:����� û����, nu:���� ������, mu:������
generate.Y = function() rexp(1,rate=1/1000)  # û���ݾ��� �����ϴ� �Լ�
I = numeric(length=n.sim)
time.nega = numeric(length=n.sim)		   # �ں����� 0�� �� �ð�
money.nega = numeric(length=n.sim)		   # �ں����� 0�� ���� ��, ������ �ݾ�

for (i in 1:n.sim) {
  t=0; a=a0; n=n0 			   	   # Intialize. �� �������� n�� ������ �ʴ´�.
  total.rate = nu + n*mu + n*lm
  tE = rexp(1, rate=total.rate)		   # ��� �߻��ð�
  repeat {
    if (tE > T) {I[i] = 1; break}		   # û���ݾ��� �ں��ݺ��� ������ I=1, ���� 
    if (tE <= T) {
      a = a + n*c*(tE - t)
      t = tE
      J = sample(1:3, 1, prob=c(nu,n*mu,n*lm))
      if (J == 1) n = n + 1			   # �ű԰��� ����
      if (J == 2) n = n - 1			   # ������� ����
      if (J == 3) {				   # ������ ����� û��
        Y = generate.Y(); 
        if (Y > a) {			   	   # û���ݾ��� �ں��ݺ��� ũ�� I=0, ����
	    I[i] = 0;
	    time.nega[i]=tE
	    money.nega[i]=Y-a
	    break
	  } 
        else a = a - Y
      }
      tE = t + rexp(1,rate=total.rate)
    }
  } 							   # end of repeat
} 							   # of for
time.nega
money.nega

