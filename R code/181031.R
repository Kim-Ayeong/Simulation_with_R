#10�� 31��

#ch.8

#Example 8a
E[N]�� �������� ǥ�ؿ����� ŭ�� Ȯ���� �� �ִ�.

alp = 0.9; B = 0.8; n.sim = 100
N = numeric(n.sim)
for (i in 1:n.sim) {
  S0 = 0
  N[i] = 1
  repeat {
    S1 = alp*S0 + (1-alp)*rnorm(1)
    if (abs(S1) > B) break
    N[i] = N[i] + 1
    S0 = S1
  }
}
c(mean(N), sd(N)/sqrt(n.sim)

# Estimating the reliability function of the bridge system
# �ý��� �ŷڵ��� �������� �ݺ��ؼ� ���� ����� �������� ǥ�ؿ����� ����
# nI�� �ݺ��ؼ� �ϳ��� �������� ��µ�, �� �۾��� nJ�� �ݺ��ؼ� nJ���� �������� ����.
# nJ���� �������� �̿��� �������� ǥ�ؿ����� ����.
p = rep(0.5, 5) 					# component reliability
phi = function(s) max(s[1]*s[3]*s[5], s[2]*s[3]*s[4], s[1]*s[4], s[2]*s[5])
nJ = 100; nI = 200
# Use of antithetic variable vs. No use of antithetic variable
theta.antithetic = theta.noantithetic = numeric(nJ)
for (j in 1:nJ) {
  x = w = numeric(nI)
  for (i in seq(1, nI, 2)) {
    u = runif(5)
    s = as.numeric(u < p)
    x[i] = phi(s)
    s = as.numeric(1 - u < p)
    x[i + 1] = phi(s)
    w[i] = x[i]
    w[i + 1] = phi(as.numeric(runif(5) < p))
  }
  theta.antithetic[j] = mean(x)
  theta.noantithetic[j] = mean(w)
}
sd(theta.antithetic) / sd(theta.noantithetic)

# Estimating the reliability function of the bridge system
# nI�� �ݺ��ؼ� �������� �� ���� ���ϰ� �������� ǥ�ؿ����� ���� ������ �̿��� ǥ�ؿ����� ����
p = rep(0.5, 5) 					# component reliability
phi = function(s) max(s[1]*s[3]*s[5], s[2]*s[3]*s[4], s[1]*s[4], s[2]*s[5])
nI = 200
# Use of antithetic variable vs. No use of antithetic variable
x = w = numeric(nI); y = numeric(0.5*nI)
for (i in seq(1, nI,2)) {
  u = runif(5)
  s = as.numeric(u < p)
  x[i] = phi(s)
  s = as.numeric(1 - u < p)
  x[i + 1] = phi(s)
  w[i] = x[i]
  w[i + 1] = phi(as.numeric(runif(5) < p))
}
for (i in 1:(0.5*nI)) y[i]=(x[2*i - 1] + x[2*i])/2
c(mean(x), sd(y)/sqrt(0.5*nI)) 				# ���������� ���� ���� �������� ǥ�ؿ���
c(mean(w), sd(w)/sqrt(nI)) 				# ���������� ���� �ʾ��� ���� �������� ǥ�ؿ���
(sd(y)/sqrt(0.5*nI)) / (sd(w)/sqrt(nI)) 		# �� �������� ǥ�ؿ����� ��

#~p.79