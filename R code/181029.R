#10�� 29��

#�ǽ� 7.15
#Var(s^2)�� �׽�Ʈ�� ������ ���ϱ�
ǥ������ : sum((Xi-Xbar)^2)/14
ǥ������� �л� : Xbar ~ N(M, sig) �̹Ƿ� Var(Xbar) = sig^2/n �ٻ�~ s^2/n
M�� 95% �ŷڱ��� = Xbar +- 2*s/sqrt(n) < �߽ɱ�������
Var(s^2) = Var(sum((Xi-Xbar)^2)/n-1) ���ϱ�

real world���� X1,,,Xn ~ N(M, sig)�� ���� (n-1)s^2/sig^2 ~ chisq(n-1)

F:���� �����Լ�, Fn:������ �����Լ�
��� theta�� F�� ���� �����Ǵ� ������, theta(F)

g(Xi,,,Xn)�� ǥ�ؿ����� �ƴ϶� �������������� �θ��� ����
 : g(Xi,,,Xn)�� �ݵ�� ����� �ƴ� ���� ����, �׷��� �������� ���� ���� < ���� ������

MSE�� ���������� : EFn[(g(Xi,,,Xn)-theta(Fn))^2]

Fn�� �������� ������ u1,,,uN��� �� ��,
M = u1+,,,+uN/N = ubar
sig^2 = E[(X-M)^2] = sum((ui-M)^2)/N = sum((ui-ubar)^2)/n

���ǽ��� Ƚ�� B�� ����� ũ�� �ϸ�
Fn�� F�� ��������� theta(Fn)�� �ٻ������� theta(F)�� ���������.

#�ǽ� 7.15
(ǥ���л��� ���-��л�)�� �л�
n = 15
x1,,,x15 = ~
g(x1,,,xn) = sum((xi-xbar)^2)/n-1 = ǥ���л� s^2
theta(F) = E(s^2) = sig^2
E((g(x1,,,xn)-sig^2)^2) = Var(s^2) ���ϱ�

#���1
n <- 15
x <- c(5,4,9,6,21,17,11,20,7,10,21,15,13,16,8)
B <- 200
ssqstar <- numeric(B)
for (i in 1:B) { 
  xstar <- sample(x, 15, rep=T) 		#��������� �׽�ũ�� ǥ�� �̱�
  ssqstar[i] <- var(xstar) 			#ǥ���л� ����
}
var(ssqstar) 					#ssqstar = thetastar_hat(1),,,thetastar_hat(B)

#sum_i~B((thetastar_hat(i)-thetastar_hat(i)���� ���)^2)/B-1 ��
�ٻ������� EFn((g(x1,,,xx)-theta(Fn))^2) = EF((g(x1,,,xx)-theta(F))^2)

#���2
xstar.mat <- matrix(sample(x, B*15, rep=T), nrow=15)
ssqstar <- apply(xstar.mat, 2, var)
var(ssqstar) 					#B�� ����� ũ���ϸ� ���1�� ���2�� �л��� �������

#�ǽ� 7.13
n <- 10
x <- c(56,101,78,67,93,87,64,72,80,69)
a <- -5; b <- 5
#Ȯ�� p(a < sum(xi/n)-M < b) �����ϱ�
g(x1,,,xn) = I(a < (xbar-M) < b)
theta(F) = EF[g(x1,,,xn)]
 	 = EF[I(a < xbar-M < b)] 		#xbar:�׽�Ʈ�� ǥ���� ǥ�����
 	 = p(a < xbar-M < b)
theta(Fn) = P(a < xbarstar-xbar < b)
 	  = EFn(I(a < xbarstar-xbar < b))
xbar = sum(x1+,,,+xn)/n
     = sum_i ~ B(xbarstar(i))/B

#Example 7e�� ����