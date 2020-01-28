#11월 14일

#8.4절

#실습
# 참고: 다른 추정량은 모의실험 횟수를 증가시킬 때 
새로이 실험을 하지 않고 이미 실시한 실험 결과를 이용할 수 있으나 
층화기법을 이용한 추정량은 추정량의 식이 모의실험 횟수에 의존하므로 불가능.

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