#9�� 19��

#5.3 > ũ�� �߿����� ����

#5.4�� �߿�
#t�ð� ���� ����� ��� �Ͼ����, countion process
#arrival times(�����ð�) : ����(0,t) ������ s1, s2, s3
#inter-arrival times(�������ݽð�) : ����(0,t) ������ s1-0, s2-s1

#���1
lambda = 1 			#���Ƽ� ������ ���(rate)
T = 10 				#����(0,T)���� ���Ƽ� ������ ����: ����� �����ð��� �� �߻� Ƚ���� ���
t = 0; I = 0L 			#t�� �ð��� I�� ����� Ƚ���� �����ϱ� ����. I�� ����0(L�� ǥ��)
S = numeric() 			#�����ð�(arrival times)
repeat {
  t = t + rexp(1,lambda) 	#interarrival times�� ���������̹Ƿ�
  if (t > T) break
  I = I + 1; S[I] = t
}
cat("Total number of events is", I,
"\nArrival times are \n", signif(S,3), "\n")

#���2
lambda = 1 			#���Ƽ� ������ ���(rate)
T = 10 		#���� (0,T) ���� ���Ƽ� ������ ����: ����� �����ð��� �� �߻� Ƚ���� ���
n = rpois(1,lambda*T) 
S = sort(T*runif(n)) 
cat("total number of events is",n,
"\narrival times are \n",signif(S,3),"\n")

#��� 2�� ��ȯ�� repeat�� ���� �ʰ� log ����� �� �ʿ䰡 ���ٴ� ������ ������ 
�����ð��� ������� �˰� ���� ��� sort�� �ؾ� ��. 

#Exercise 24
Xi�� 20~40���� �̻��� Ȯ�������� ���� Ȯ������, 1�ð� ���� ���=5��
N <- rpois(n=1, lambda=5) 	#the number of buses in 5 hours
sum(sample(20:40, N, rep=T)) 	#the number of fans

#���� �ǵ��
#���1
f=function(){����}
g=function(){dunif(x, 0, 0.05)}
curve �׷�����
for (i in 1:n) {
  repeat {
    u1=runif(1, 0.05); u2=runif(1)
    if(u2 <= f(u1)/(c*g(u1))) x[i]=u1; break
  }
}

#���2
c=1/(1-exp(-0.05))
f(x)/c*g(x) = e(-x)
for
  repeat
    if (u2<exp(-u1)) x[i]=u1; break
