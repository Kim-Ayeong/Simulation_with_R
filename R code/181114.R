#11�� 14��

#8.4��

#�ǽ�
# ����: �ٸ� �������� ���ǽ��� Ƚ���� ������ų �� 
������ ������ ���� �ʰ� �̹� �ǽ��� ���� ����� �̿��� �� ������ 
��ȭ����� �̿��� �������� �������� ���� ���ǽ��� Ƚ���� �����ϹǷ� �Ұ���.

n.values = c(5,10,100,500,1000,5000)
for (n in n.values) {
  u = runif(n)
  tmp = sqrt(1 - ((u + 0:(n-1))/n)^2)
  str.only = 4*mean(tmp)
  str.anti = 2*mean(tmp + sqrt(1 - ((1:n - u)/n)^2))
  v1 = 2*u - 1
  v2 = 2*runif(n) - 1
  cond = 4*mean(sqrt(1-v1^2))
  raw = 4*mean(v1^2 + v2^2 <= 1)
  if (n == 5) cat("n\t str + anti\t stratified \t conditioning\t raw\n")
  cat(n, "\t", format(str.anti, nsmall=6), "\t",
    format(str.only, nsmall=6), "\t",
    format(cond, nsmall=6), "\t", format(raw, nsmall=6), "\n")
}