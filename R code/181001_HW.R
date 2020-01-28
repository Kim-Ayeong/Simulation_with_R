#10월1일 과제

step 1: t=0, J=1, I=0
step 2: Generate a random number U and set X=(?1/λJ)logU
step 3: If t+X>tJ, go to Step 8
step 4: t=t+X
step 5: Generate a random number U
step 6: If U<=λ(t)/λJ, set I=I+1, S(I)=t
# step 7: Go to Step 2
# step 8: If J=k+1, stop
# step 9: X=(X?tJ+t)λJ /λJ+1, t=tJ, J=J+1
# step 10: Go to Step 3

lambda=26  						#t=10일 때
lt <- function(t) {
  if(0<t & t<5) t/5
  else if(5<t & t<10) 1+5*(t-5)
  else o
}
t=0; T=10; I=0						#step1
S=numeric()
repeat {
  X=(-1/lambda)*log(runif(1)) 				#step2
  t=t+X 
  if(t+X>10) break 					#step4
  if(runif(1)<=lt(t)/lambda) {I=I+1; S[I]=t} 	#step5,6
}
cat("Total number of events is", I,
"\nArrival times are \n", signif(S,3),"\n")


