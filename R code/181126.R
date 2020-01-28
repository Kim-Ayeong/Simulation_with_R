#11�� 26��

#9.1��

Dn=max[Fn(x)-F(x)]?
�͹������� ���� �� Dn�� ���� �ľ� > Dn�� ����� ũ�� �͹����� �Ⱒ

#Asymptotic result
why? > X~F(.)�̸� F(X) ~ U(0,1)�̴�.

#Example 9b and more
#ǥ���� ����� 100�� ������������ ����� ���� �³�?
#ecdf() : empirical cdf(������ ���������Լ�)
draw.FnF = function(x, F, ...) { 		# draw the ecdf Fn and the assumed df F
  plot(ecdf(x), main="Fn(x) and F(x)")
  curve(F(x, ...), add=T)
}
y = c(66,72,81,94,112,116,124,140,145,155)
draw.FnF(y, pexp, rate=0.01)
ks.test(y, "pexp", rate=0.01)
y = rexp(10, rate=0.01) 			# H0(F�� ��� 100�� ��������)�� ���� �� �׸��� �������
draw.FnF(y, pexp, rate=0.01)
ks.test(y, "pexp", rate=0.01)

#Dn ���ϱ�
y = c(66,72,81,94,112,116,124,140,145,155)
F.y = pexp(y, rate=0.01) 			#F.y = 1-exp(-y/100)�� ����
n = length(y)
d = max((1:n)/n-F.y, F.y-(0:(n-1))/n) 		#max(j/n-F.y, F.y-j/n)�� �ѹ��� ����

#Exercise 9.1
#�Ͼ� ��ȫ ���� ���� Ȯ���� 1:2:1�ΰ�?
#chisq.test() �̿�
x = c(141, 291, 132)
chisq.test(x, p=c(1/4, 1/2, 1/4))
chisq.test(x, p=c(1/4, 1/2, 1/4), simulate.p.value=T)

#Exercise 9.4
#ǥ���� ���Ϻ���(50, 200)���κ��� ���Դ°�?
#ks.test() �̿�
x = c(164,142,110,153,103,52,174,88,178,184,58,62,132,128)
draw.FnF = function(x, F, ...) { 		# draw the ecdf Fn and the assumed df F
  plot(ecdf(x), main="Fn(x) and F(x)")
  curve(F(x, ...), add=T)
}
draw.FnF(sort(x), punif, min=50, max=200)
ks.test(x, punif, min=50, max=200) 		#unif �� cdf
ks.test(x, function(x) (x-50)/150) 		#�� �� ����



