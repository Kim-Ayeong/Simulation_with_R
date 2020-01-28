#12�� 3��

#9.3��

#Ai : i��° ������ ���ε� �Ⱒ�� ���, i=1,...,r
r���� ������������ ��� �ѹ� ��1�� ������ ���� Ȯ����
P(A1UA2U...UAr) <= P(A1)+P(A2)+...+P(Ar) : Bonferron �ε��
alpha = P(A1)��� �� ��, ~ <= r*alpha

#(1) Wilcoxon Two-Sample Rank-Sum Test
Y ������Ȳ���� ���� ����
X ���ǻ�Ȳ���� ���� ���� �� ��,
�ϳ��� �����ܿ��� ����� Ȯ��ǥ���̶� ����(=���� �������� ���� ������)

recursion formula for small (n, m) > p.233 (9.8)
normal approximation for large (n, m) > p.234 (9.9)

# Exercise 11 (b) : ���ǽ��迡 ���� p�� ���
x = c(65.2, 67.1, 69.4, 78.4, 74, 80.3)
y = c(59.4, 72.1, 68, 66.2, 58.5)
n = length(x); m = length(y)
W = wilcox.test(x, y)$statistic
r = W + n*(n+1)/2					# �Ǵ� xy = c(x, y); r = sum(match(x, sort(xy)))
N = 1000	 					# ���ǽ������� p���� ���ϱ� ���� �ݺ�Ƚ��
R = replicate(N, sum(sample(1:(n+m), n))) 		# ������, for ���� �ᵵ ��
2*min(mean(R <= r), mean(R >= r)) 			# ���ǽ��迡 ���� p��

#Example 9f
x = c(135,104,162,171,129)
y = c(107,94,136,99,114,122,108,130,106,88)
wilcox.test(x, y)
wilcox.test(x, y, exact=FALSE, correct=FALSE) 		#Example 9g
#���۽� ���� ���� ������ ���ϱ�
match(x, sort(c(x, y)))
r = sum(match(x, sort(c(x, y)))) 			#50
#���۽� �������� -n(n+1)/2 �� ������ 40

#(2) Two-Sample Kolmogorov-Smirnov Test
# Wilcoxon two-sample rank-sum test vs K-S two-sample test
x = rnorm(200, mean=0, sd=1)
y = rnorm(200, mean=0, sd=2)
wilcox.test(x, y)
ks.test(x, y)
# Wilcoxon test�� �߽���ġ ���(location parameter)�� ���� ����
# K-S test�� �����Լ��� ���� ����

#�ǽ�
#Exercise 10��
exact p��
x <- c(65.2, 67.1, 69.4, 78.4, 74.0, 80.3)
y <- c(59.4, 72.1, 68.0, 66.2, 58.5)
wilcox.test(x, y) 					#exact p=value
wilcox.test(x, y, exact=FALSE) 				#normal approximation
n <- length(x); m <- length(y)
r <- wilcox.test(x, y)$statistic + n*(n+1)/2
R <- replicate(2000, sum(sample(1:(n+m), n)))
2*min(mean(R > r), mean(R < r)) 			#p-value by simulation
2*min(mean(R >= r), mean(R <= r)) 			#p-value by simulation
ks.test(x, y)
plot(ecdf(x), main="ecdf of x and y") 			#empirical cdf
par(new=T)
plot(ecdf(y), col="blue", main="", xlab="", ylab="", lty=2)

#Exercise 11��
���Աٻ� p��, ���ǽ��� p��
