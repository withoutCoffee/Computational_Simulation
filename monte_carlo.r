#integração e monte carlo

#integração da distribuição normal
s = 0

n = 3000000

for (i in 1:n){
  x = rnorm(n = 1)
  
  if(x>0 && x<1){
    s = s +1
  }
}

s / n