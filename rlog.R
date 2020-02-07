rlog=function(n,theta){
  u=runif(n)
  N=30
  k=1:N
  a=-1/log(1-theta)
  fk=exp(log(a)+k*log(theta)-log(k))
  Fk=cumsum(fk)
  x=integer(n)
  
  for(i in 1:n){
    x[i]=as.integer(sum(u[i]>Fk))
      while(x[i]==N){
        logf = log(a)+(N+1)*log(theta)-log(N+1)
        fk=c(fk,exp(logf))
        Fk=c(Fk,Fk[N]+fk[N+1])
        N=N+1
        x[i]=as.integer(sum(u[i]>Fk))
      }
  }
  x+1
}
x = rlog(1000,0.5)
hist(x,probability = TRUE)