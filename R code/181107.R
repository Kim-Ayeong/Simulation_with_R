#11�� 7��

#8.2��

#Example 8f
���� �������
�������� S�� Y �����
�������� Xbar+c*hat(Ybar-mY)�� ��� -> �л� �پ��

#�ǽ�
#Exercise 8.12
n <- 1000
x.indep <- numeric(n)
x.contr <- numeric(n/2)
u <- runif(n)
x.indep <- exp(u^2)
u.half <- u[1:(n/2)]
x.antit <- exp(u.half^2) * (1+exp(1-2*u.half))/2
#�����ϰ� �ϱ����� 2�� ���� ��. control approch�θ� �Ѵٸ� ���ص� ��
x <- exp(u.half^2)
y <- u.half^2
cstar <- -cov(x, y)/var(y)
x.contr <- x + cstar*(y - 1/3)
c(mean(x.indep), mean(x.antit), mean(x.contr)) 		#1.473083, 1.466435, 1.463928
c(var(x.indep), var(x.antit), var(x.contr)) 		#0.229154808, 0.029899228, 0.004043112