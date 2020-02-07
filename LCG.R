#Gerador congruencial linear
LCG = function(numeros,m,a,c,seed){
  
  x = c(((a*seed+c)%%m));
  
  for (i in 2:numeros){
    x[i] = (a*x[i-1]+c)%%m ; 
  }
  return(x/m)
}

#MTI(Método da Transformação inversa)
#f.d.p -> cos(x)/2, pi/2 < x < pi/2
#por MTI -> arcsin(2u-1); 0 < u< 1
n=100
u = LCG(n,777,40692,127,0)
#u = runif(300,seed=0)
x = asin(2*u-1)

hist(x,probability = TRUE,
     col="darkblue",border = 'black',main="Histogram Cos(x)/2")
a = seq(-pi/2,pi/2,by=0.2)
lines(a,cos(a)/2,col='red',lwd=2)

mean(x)
var(x)


