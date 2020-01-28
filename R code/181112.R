#11�� 12��

#8.3�� ����ȭ�� �л� ����ȿ��

#Example 8k : ���� 156�� ����

#Example 8l : Y ~ exp(1), X|y ~ N(y, 2^2)
#theta = P(X>1), E[I(X>1)|Y=y] = 1-Phi((1-y)/2)
I(X>1) : raw estimator

#raw simulation approach
#conditioning technique
#conditioning + antithetic variate
#conditioning + control variate

n <- 100
u <- runif(n)
y <- -log(u)
x <- rnorm(n, mean=y, sd=2)
I <- (x>1)
cond.E <- 1-pnorm((1-y)/2)
anti <- (cond.E + 1 - pnorm((1+log(1-u))/2))/2

#control ������
X + c(Y-mY)
���⼭ X�� cond.E
mean�� ����ϰ� var�� ���� 2������ ��� ���� �۾ƾ� ��

c(mean(I), mean(cond.E), mean(anti))
c(var(I), var(cond.E), var(anti))  		#�л��� 1/10 ����
#antithetic ����� �߰������� n���� ���� ���������Ƿ� ������ �񱳰� �ƴ�



