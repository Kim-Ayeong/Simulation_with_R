#10�� 31�� ����

#�ǽ� 7.13
#Ȯ�� p = p(a < sum(xi)/n-M < b) �� �׽�Ʈ�� ������ ���ϱ�
n = 10; B = 200; a = -5; b = 5
x = c(56,101,78,67,93,87,64,72,80,69)
pstar <- numeric(B)
xstar.mat <- matrix(sample(x, B*10, rep=T), nrow=10)		#��������� �׽�Ʈ�� ǥ�� �̱�
xbarstar <- apply(xstar.mat, 2, mean)
(xbar <- mean(xbarstar))
tempstar <- apply(xstar.mat, 2, var)
(tempv <- var(tempstar))  					#var(s^2)
head(xbarstar-xbar)   						#Ȯ��
(pstar <- ifelse(a<xbarstar-xbar & xbarstar-xbar<b, 1, 0))
mean(pstar)

#���� �ǵ��
n; B; a; B=10000; x;
m = mean(x)
xstar.mat <- matrix(x, B*n, rep+T), nrow=n)
sm <- apply(xstar.mat, 2, mean)
mean(a<(sm-m) & (sm-m)<b)