#10�� 22��

#lec7

ǥ����� ��Ÿ��
1)ǥ�ؿ����� d ������ �� ���� �ߴ�
  xbar(n+1) < xbar(n) ����
  S^2(n+!) < S^2(n) ����
2)�ŷڱ���

#���� 6.2
T-tD���� T-t�� �ϸ� �ذ�
�� �޽Ľð��� 2.3~2.5�� ����
> ���������� ���ϸ� ����, ǥ�ؿ����� �ŷڱ��� ���ϱ�

#7.6
n = 100
gx = exp(runif(n)^2)
mean.0 = mean(gx)
var.0 = var(gx)
repeat {
  x = exp(runif(1)^2)
  n = n+1
  mean.1 = mean.0 + (x + mean.0)/(n)
  var.1 = (1-1/(n-1))*var.0 + n*(mean.1-mean.0)^2
  if(sqrt(var.1/n) < d) break
  mean.0 = mean.1
  var.0 = var.1
}
mean.1
sqrt(var.1/n)
n

#7.8
����� 95% �ŷڱ���
# function N = generate N(m)
# N=[];
# for i=1:m
# V=cumsum(rand(1,100));
# N=[N 1+sum(V<1)];
# h N=generateN(1000); mean(N); var(N);

#�׷������� y���� ���߸��� ���� �̸� ����