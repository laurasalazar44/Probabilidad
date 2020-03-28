
#Parte 2 - Tarea 1
p <- 0.447
n <- 41


bernoulli <- function(p){
  x <- runif(1) # Crea numeros aleatorios entre 0 y 1
  if(x <= p){
    return (TRUE)
  }
  else{
    return(FALSE)
  }
}


binomial <- function(n, p){
  r = 0
  for (i in 1:n){
    if (bernoulli(p) == TRUE){
      r = r+1
    }
  }
  return (r)
}

geometrica <- function(p){
  r = 0
  t = 0
  while (r != 1){
    if (bernoulli(p) == TRUE){
      t = t+1
    }
  }
  return (t)
}

poisson <- function(lambda, k){
  f <- exp(-lambda)*((lambda^k)/factorial(k))
  return (f)
}

prueba <- binomial(41,0.447)
message(prueba)
barplot(table(prueba))

