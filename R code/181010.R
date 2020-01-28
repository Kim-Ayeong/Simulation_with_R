#10�� 10��

# 6.6��

# Exercise 6.11
n.sim = 100 					   # ���ǽ��� �ݺ�Ƚ��
n0=1; a0=25000; T=365; c=11000		   	   # n:����� ��, a:�ں���, T:�ѽð�, c:�ð��� �����ϴ� �����
lm=10; nu=0; mu=0 			  	   # lm:����� û����, nu:���� ������, mu:������
generate.Y = function() rexp(1,rate=1/1000)  	   # û���ݾ��� �����ϴ� �Լ�
I = numeric(length=n.sim)

for (i in 1:n.sim) {
  t=0; a=a0; n=n0 			   	   # Intialize. �� �������� n�� ������ �ʴ´�.
  total.rate = nu + n*mu + n*lm
  tE = rexp(1, rate=total.rate)		   	   # ��� �߻��ð�
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
        if (Y > a) {I[i] = 0; break} 	   	   # û���ݾ��� �ں��ݺ��� ũ�� I=0, ����
        else a = a - Y
      }
      tE = t + rexp(1,rate=total.rate)
    }
  } 							   # end of repeat
} 							   # of for
I
mean(I) 	 					   # �ں����� �ٴڳ��� ���� Ȯ��(����)�� ������ ���. �������� �󸶳� ��Ȯ�Ѱ�?

# ����
# Exercise 6.2
# Exercise 6.12
�ð� T ������ �ں����� ���� �Ǿ��ٴ� ����� �� ��,
�ں����� ���� �� ��Ȯ�� �ð��� ���� �ݾ��� ������ �����ϴ� ����

n.sim = 100 					   # ���ǽ��� �ݺ�Ƚ��
n0=1; a0=25000; T=365; c=11000		   	   # n:����� ��, a:�ں���, T:�ѽð�, c:�ð��� �����ϴ� �����
lm=10; nu=0; mu=0 			  	   # lm:����� û����, nu:���� ������, mu:������
generate.Y = function() rexp(1,rate=1/1000)  	   # û���ݾ��� �����ϴ� �Լ�
I = numeric(length=n.sim)
time.nega = numeric(length=n.sim)		   # �ں����� 0�� �� �ð�
money.nega = numeric(length=n.sim)		   # �ں����� 0�� ���� ��, ������ �ݾ�

for (i in 1:n.sim) {
  t=0; a=a0; n=n0 			   	   # Intialize. �� �������� n�� ������ �ʴ´�.
  total.rate = nu + n*mu + n*lm
  tE = rexp(1, rate=total.rate)		   	   # ��� �߻��ð�
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
I
mean(I)