#9�� 17��

inverse transform algorithm ���� �Ұ� (why?) > ���� ����� ��
?rgamma 	#rate(=1/beta), scale ���� ���� 
#������ �𸣰�, ���Լ��� ������ �� ���� f(x)�� ���� ����� ���� g(x)�� ���ؼ� f(x) ������ ����
suppor set : ������ �� �ִ� ����

#Ex 5d)
#beta(2,4) ������ rejection method�� ����
n=1000; x=numeric()
for (i in 1:n) {
  repeat {
    u1 = runif(1); u2 = runif(1)
    if (u2 < (256/27)*u1*(1-u1)^3) {x[i] = u1; break}
  }
}
hist(x,breaks=30,prob=T);curve(dbeta(x,2,4),add=T)

#Ex 5e), 5f)�� �н�

#Ex 5g)
rgamma(10, 2,1) #������ 5������ ���� �ʹ� ����

#ex 4��
#ex 6��

#���� �ǵ�� > ���� ����, c()���� Numeric()�� �̿��� �����ϴ� ���� �� ����
