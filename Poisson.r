# Simulação da distribuição de poisson utilizando o métdo da transformação
# Aproxmação pela exponecial

# Gera numero aleatório poisson
p = function(l){
  s = 0
  i = 1
  
  u = runif(n = 1)
  s = s-log(u)
  
  while(s <= l){
    
    i =  i + 1
    u[i] = runif(n = 1)
    s = s-log(u[i])
    
  }
  return (i-1)
}

# Gera N numeros aleatórios poisson
poisson = function(n,lambda){
  
  x = 0
  for (i in 1:n){
    x[i] = p(l=lambda)
  }
  return(x)
}
set.seed(0)


x = poisson(n=11111,lambda = 20/0.155)


hist(x,probability = TRUE,
     col="darkblue",border = 'black',
     main="Distribuicao de Poisson")



