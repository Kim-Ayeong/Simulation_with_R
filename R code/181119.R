#11�� 19��

#8.6��

#Example 8w
h(x)f(x)/g(x)
I(Sn>B) ����
�� importance sampling�� �ʿ��Ѱ�? > SN(straight estimation)�� ��� A���� �۾���

#�ǽ�1
1. X1, X2 ~ iid N(-m, sig^2) ���� ��
2. I*exp(2m*SN/sig^2) �� �����ϱ�

#straight estimator
mu <- (-0.1); sigma <- 0.3; A <- B <- 5 		#�����̵忡�� sigma ����
m <- 5000
I.straight <- numeric(m)
for (i in 1:m) {
  SN <- rnorm(1, mean=mu, sd=sigma)
  N <- 1
  repeat {
    if (SN < -A | SN > B) break
    SN <- SN + rnorm(1, mean=mu, sd=sigma)
    N <- N + 1
  }
  I.straight[i] <- as.numeric(SN > B)
}
(est.straight <- mean(I.straight)) 
#�ݺ�Ƚ���� �÷��� ��� 0 > importance sampling�� �غ��ƾ� ��

#importance estimator
mu <- (-0.1); sigma <- 0.3; A <- B <- 5
m <- 5000
onerun.imp <- numeric(m)
for (i in 1:m) {
  SN <- rnorm(1, mean=-mu, sd=sigma) 			#-mu�� �ٲ�
  N <- 1
  repeat {
    if (SN < -A | SN > B) break
    SN <- SN + rnorm(1, mean=-mu, sd=sigma)
    N <- N + 1
  }
  onerun.imp[i] <- as.numeric(SN > B) * exp(2*mu*SN/sigma^2)
}
(est.imp <- mean(onerun.imp)) 

#���
for (n in c(10,100,500,1000,2000,5000)) {
  cat(n, "\t", mean(I.straight[1:n]), "\t", mean(onerun.imp[1:n]), "\n")
}

