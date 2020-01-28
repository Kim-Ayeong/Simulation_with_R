#12�� 5��

#10.1��

irreducible:���̻� ����� �� ����
aperiodic:�ֱⰡ ����
positive recurrent:� ���¸� ��� �湮�� �� ����
transition probabilities(����Ȯ��) Pij^n : i���� ������ n������ j�� ����, i�� �������
lim(n>inf)P13^n = pi(3)
lim(n>inf)P23^n = pi(3)
lim(n>inf)P33^n = pi(3) ��� ����, but ��������

#10.2��

pi(j) = P(V = vj) = b(j)/B
Markov chain �����(�˰����� ����!)
X������ ����� > X�������� ����=pi > pi ã��(min(~))
Pij = P(Xn+1=j|Xn=i) = P(X1=j|Xd=i) : ���� ���� = i, ���� ���� = j
q(xn, x) �Լ��� Y���� ã�� ���� �ƹ� �Լ��� ����

#�ǽ�
g(x, y) = 1(��� x,y i.e) Y~U(0,1)
alpha(x, y) = min{[y^a-1*(1-y)^b-1] / [x^a-1*(1-x)^b-1], 1}
alpha = 2.7, beta = 6.3

#target f is beta(2.7, 6.3) distribution
a = 2.7; b = 6.3; N = 5000; n.sim = 100
target.f = function(x) x^(a-1)*(1-x)^(b-1)
pval.1 = pval.2 = pval.3 = numeric(n.sim)
for (i in 1:n.sim) { 					#�ݺ��������� K-S ������ p���� ����� ���ϰ��� ��
  x = numeric(N)
  x[1] = runif(1)
  for (n in 1:(N-1)) {
    y = runif(1)
    if (runif(1) < target.f(y)/target.f(x[n])) x[n+1] = y 
    else x[n+1] = x[n]
  }
  pval.1[i] = ks.test(jitter(x[1:100]), "pbeta", a, b)$p.value
  pval.2[i] = ks.test(jitter(x[4901:5000]), "pbeta", a, b)$p.value
  pval.3[i] = ks.test(jitter(x[seq(4010, 5000, 10)]), "pbeta", a, b)$p.value
}
c(mean(pval.1), mean(pval.2), mean(pval.3)) 		#0.1014, 0.1082, 0.4866

#Example 9b ����
#https://rseek.org ����
x <- rexp(10, rate=0.5)
plot(ecdf(x), main="Fn(x) and F(x)")
curve(pexp(x, rate=0.5), add=T)


