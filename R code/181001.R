#10�� 1��

#5.5��
#5.4���� t�� �������� ���, rate�� �ҷ����� 5.5���� t�� �ð��� ���� ����
#���1. thinning algorithm
����(t) <= ����(=t�� �������� �ʴ� ���)

step1 : t�� �귯���� �ð�, I�� ����� ��� �Ͼ����
step3 : t>T �����ִ� �ð� ������ ����� stop
step5 : Ȯ���� �����Ǹ� I=I+1 ������� �޾Ƶ��̰� S(I)=t ��ǽ����� ���
step6 : repeat �Ǵ� while�� ��ȯ�� �ۼ�

#Exercise 25(a)
lambda=7 				#t=0�� �� �ִ� ���� 7
T=10
lt <- function(t) {3+4/(t+1)} #t�� ������ ����t�� �������� �Լ� �ۼ�
t=0; I=0
S=numeric() 				#�����ð�(arrival times)
repeat {
  t=t+(-log(runif(1))/lambda) 		#�Ǵ� t=t+rexp(1, lambda)
  if(t>T) break
  if(runif(1)<lt(t)/lambda) {I=I+1; S[I]=t}
}
cat("Total number of events is", I, "\nArrival times are \n", signif(S,3),"\n")
#signif(x, 3): x�� �Ҽ��� ��ȿ�ڸ� 3�ڸ����� ���

#���2. Exercise 25(b)
#HPP�� 3�� �Լ� N(t)�� 4/(t+1)�� M(t)�� ���� > merge
hpp.fun <- function(lmd, T) {
  t=0; I=0
  S=numeric()
  repeat {
    t=t+rexp(1, lambda)
    if(t>T) break
    I=I+1; S[I]=t
  }
  return(list(n.events=I, arr.time=S))
}
hpp.fun(3, 10)

nhpp.fun <- function(lmd.max, lmd.t, T) {
  t=0; I=0
  S=numeric()
  repeat {
    t=t+rexp(1, lmd.max)
    if(t>T) break
    if(runif(1)<lmd.t(t)/lmd.max) {I=I+1; S[I]=t}
  }
  return(list(n.events=I, arr.time=S))
}
nhpp.fun(3, lmd.t=function(t) 4/(t+1), 10)

lst.1 <- hpp.fun(lmd=3, T=10)
lst.2 <- nhpp.fun(lmd.max=4, lmd.t=function(t) 4/(t+1), T=10)
S <- sort(c(lst.1$arr.times, lst.2$arr.time))
I <- lst.1$n.events + lst.2$n.events 	#�Ǵ� length(S)

#���3. Exercise 26 #step1~10 ����
#t=10�� �� �ִ� ����(t)=26
#Ȯ�� t=0 s=10
�ǽ� t=0~10���� �����ϸ� Poisson(m(t+s)=m(t))�� ��հ� �������
m(0)=0

#���4�� ����


