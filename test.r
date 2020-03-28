
p <- 0.447

n <- 41

Estudiantes <- 0:41
#A.)

# X se distribuye respecto al número de estudiantes que utilicen Wikipedia. 
# Es decir, entre más estudiantes utilicen Wikipedia, la variable aleatoria crecerá más.

#FUNCION DE MASA DE PROBABILIDAD.

binomial <- function(n, k, p){
  f <- (factorial(n)/(factorial(k)*factorial(n-k)))*(p^k)*(1-p)^(n-k)
  return (f)
}

Wikipedia <- vector("numeric", 42)
for(i in Estudiantes){
  Wikipedia[i] = binomial(n, i, p)
}

plot(Estudiantes,Wikipedia)


#ENCUENTRE LA PROBABILIDAD DE Estudiantes SEA IGUAL A 17
message("Sea X la variable aleatoria que cuenta el número de estudiantes que utilizan Wikipedia")
 message("Probabilidad de que X sea igual a 17: ",binomial(n, 17, p))
 
#ENCUENTRE LA PROBABILIDAD DE Estudiantes SEA COMO MAEstudiantesIMO 13
 vec2 <- vector("numeric", 13) 
 for(i in Estudiantes){
   if (i == 14){
     break
   }
   vec2[i] = binomial(n, i, p)
 }

 message("Probabilidad de que X sea como máximo 13: ",sum(vec2)) #La suma de todas la probabilidades 
 
 #ENCUENTRE LA PROBABILIDAD DE QUE Estudiantes SEA MAYOR QUE 11
 vec3 <- vector("numeric", 30)
 for(i in Estudiantes){
   if (i >11){
     vec3[i] = binomial(n, i, p)
     
   }
 }
 message("Probabilidad de que X sea mayor que 11: ",sum(vec3))
 
 #ENCUENTRE LA PROBABILIDAD DE QUE Estudiantes ESTÉ ENTRE 16 Y 19(INDLUÍDOS)
  vec4 <- vector("numeric", 4)
 
  for(i in Estudiantes){
   if (i>=16 && i<=19){
     #message(i)
     vec4[i-16] = binomial(n, i, p)
   }
 }
 
message("Probabilidad de que X esté entre 16 y 19:  ",sum(vec4))

