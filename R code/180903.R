#9�� 3��

#���� ���� �߻����� ����
#1. independent (0, 1) uniform dist.,
#2. �� �ֱ� : ������ ���� �����ٺ��� ������ �ֱⰡ �߰ߵ�,
#3. ���� �ӵ�

#�ǽ� 1. multiplicative congruential generator ���
#���簪x(n)=�ʱⰪx(n-1)*a���� m���� ���� ��
m <- 2^31-1; a <- 7^5; x0 <- 1
N <- 100
x <- numeric(N) 		#���̰� 100�� ������ ����
x[1] <- (a*x0)%%m
for (i in 2:N)
  x[i] <- (a*x[i-1])%%m
x/m 				#0~1������ ���� ����
y <- x/m
hist(y) 			#������׷� �׷�����

#�Լ��� �����غ���
myrunif <- function(N, m=2^31-1, a=7^5, x0=1) {
  x <- numeric(N)
  x[1] <- (a*x0)%%m
  for (i in 2:N)
    x[i] <- (a*x[i-1])%%m
  x/m 
}
y <- myrunif(100)

#Mersenne-Twister ���
y <- runif(100)

#�ǽ� 2
y <- myrunif(1000)
hist(y) 			#U(0,1) ���� Ȯ��
plot(ecdf(y)) 			#U(0,1) ���� Ȯ�� 	#ecdf = empirical cdf
plot(y) 			#������ Ȯ��
plot(y[seq(1, 1000, by=2)], y[seq(2, 1000, by=2)]) 	#Ȧ��, ¦���� ������

#�ǽ� 3
sum(diff(sort(y))==0) 		#999���� ���̰� �߻� 	#0�̸� �ߺ��Ǵ� ���� �ϳ��� ����. 
length(y)==length(unique(y))