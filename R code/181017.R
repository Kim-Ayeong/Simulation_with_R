#10월 17일

#6.9절

프로그램의 오류를 수정하기 (또는 피하기) 위한 기법들
1. 모듈 : 기능별로 별개의 함수(모듈) 만들기
예)
2. 특별한 경우 확인 : 알고 있는 이론과 비교
예 1), 2)
3. 수작업
4. 변수의 값을 추적 : browser() 라는 명령어를 이용해 변수가 갖는 값의 변화를 추적할 수 있다

# browser()를 이용한 debugging
a = 0
for (i in 1:3) {
  for(j in 1:3) {
    a = i + j
    browser()
  }
}
#i=1, j=1, a=2에서 제어권이 나에게 넘어옴
i, j, a를 확인
n - 바로 다음 문장 실행 #변수 n 출력 = get('n')
c - 다음 browser()를 만날 때까지 계속 실행
Q - debugging 끝내기

#실습
Exercise 6.17에서 m, Pm(그리고 그 밖에 필요한 값들)이 주어졌을 때,
옵션을 행사하기 위한 두 조건의 만족 여부를 알아보는 부분을 별개의 함수로 만들어 보자. 
(만족이면 1, 아니면 0을 함수의 출력값이 되도록 함.)

mytest <- function(m, mu, sg) {
  N = 20; K = 100; S.zero = 100;
  alp = mu + 0.5 * sg^2
  result = FALSE
  P = numeric(m)
  P[1] = S.zero
  for (i in 2:m) {
    P[i] = P[i-1]*exp(rnorm(1, mu, sg))
  }
  if (P[m]>K) result=TRUE
  if (m>0) {
    b = ((1:m)*mu - log(K/P[m]))/(sg*sqrt(1:m))
    op = P[m]*exp((1:m)*alp)*pnorm(sg*sqrt(1:m)+b)-K*pnorm(b)
    if (all(P)>K+op) result=TRUE
  }
  result
}
mytest(10, -0.05, 0.3)

#중간고사 기출 2번은 생략
[3-1] 원래 Ta 공식이 맞음