#*******************************************
#*#  MODELOS AUTORREGRESIVOS DE LA 
#*   ECONOMETR�A CLASICA
#*******************************************
#*
# Cargar el archivo de Excel Table_17.2_con_lags
# Es una regresi�n de un modelo autorregresivo (ver 
# Ejemplo 17.7 en Gujarati pag. 627)
library(AER)   # Para el comando ivreg
library(lmtest)  # Para el test de autocorrelaci�n BG

# Carga de los datos como data frame
datos=data.frame(Table_17_2_con_lags)

# Se realiza la regresi�n del modelo indicado por MCO
r_ols=lm(PPCE~PPCE_L1+PPDI, data=datos)
summary(r_ols)

# Se verifica la existencia o no de autocorrelaci�n en el error
bgtest(r_ols, order=1)

# Dado que hay autocorrelaci�n se obtienen los errores
# est�ndar robustos a la autocorrelaci�n 
coeftest(r_ols, vcov=vcovHAC)

# C�lculo del efecto de largo plazo del ingreso sobre el consumo
LP=(r_ols$coefficients[3])/(1-r_ols$coefficients[2])  ; LP
# Verificar con Gujarati

# De todas maneras, dado que hay autocorrelaci�n hay endogeneidad
# Corresponde realizar la regresi�n con variables instrumentales
# El intrumento para la variable PPCE(t-1) es el rezago PPCE(t-2)
r_vi=ivreg(PPCE~PPCE_L1+PPDI|PPCE_L2+PPDI, data=datos)
# en es sumario se piden errores robusto y tests
summary(r_vi,vcov = sandwich, diagnostics=T)

# El test de Hausman en la salida anterior valida el uso de variables
# instrumentales y rechaza que sea un instrumento debil

# C�lculo del efecto de largo plazo del ingreso sobre el consumo
LP=(r_vi$coefficients[3])/(1-r_vi$coefficients[2])  ; LP
# Este es el coeficiente correcto porque la regresi�n OLS 
# tenia problemas de endogeneidad



#*******************************************
# METODO GENERALIZADO DE MOMENTOS
#*******************************************
#*
#*# Genero una muestra de 20 observaciones de una
#*  distribuci�n de Poisson con lambda = 3. Le sumo
#*  un "ruido" normal con media 0 y varianza 0.25
N=20
set.seed(123)
x=rpois(N,3) + rnorm(N,0,0.25)

# C�lculo de los lambda a partir de los momentos
# Primer momento: lambda=promedio(x)
lambda1=mean(x) ;  lambda1

# Segundo momento, tengo una ecuaci�n cuadr�tica
# lambda^2 + lambda - sum(x^2)/N = 0
# es una cuadr�tica del tipo a x^2 + b x + c = 0
# Cuya soluci�n es r = - b/2a +/- sqrt(b^2 - 4 a c)
lambda2=-0.5+sqrt(0.25+sum(x^2)/N)  ; lambda2

# Calculo los momentos no centrados anteriores
# para utilizarlos en los c�lculos
S1=mean(x)
S2=sum(x^2)/N

# Programo una funci�n f que calcula QN
# Los argumentos que utiliza la funci�n ser�n
# lam = el par�metro lambda
# S1 = El primer momento
# S2 = El segundo momento
# W = La matriz de ponderaciones a utilizar
f=function(lam,S1,S2,W) {
  # Genero el vector de momentos muestrales que 
  # deber�a anularse
  S=as.matrix(c(S1-lam,S2-lam^2-lam), nrow=2)
  # Calculo QN
  QN=t(S) %*% solve(W) %*% S
  return(QN)
}

# M�todo two steps
# Primer paso:
# Comienzo la iteraci�n con uno de los valores
# proporcionados por los momentos (elijo lambda1)
# y uso W=I 
ld=lambda1
W=diag(2)

# Con la funci�n nlm, que es una funci�n de minimizaci�n
# no lineal disponible en R, minimizo f (o sea QN) eligiendo
# ld (o sea el lambda). Ingreso un valor incial ld y los 
# otros argumentos exigidos por f
# Sale el valor m�nimo de la funci�n objetivo 
# y el correspondiente valor del par�metro lambda
ld=nlm(f,ld,S1,S2,W)
QN=ld$minimum     ;  QN
ld=ld$estimate    ;  ld

# Paso 2 
# a) Con la estimaci�n del 1er paso calculo F
  # Matriz de varianzas y covarianzas estimada
  # Inicializo la matriz en cero
for(i in 1:1)  {
  # Matriz de varianzas y covarianzas estimada
  # Inicializo la matriz en cero
  F=matrix(c(0,0,0,0),nrow=2)
  # Voy sumando los respectivos resultados para cada Xi
  for(j in 1:length(x)) {
    Fi=as.matrix(c(x[j]-ld,x[j]^2-ld^2-ld),nrow=2)
    Fi=Fi %*% t(Fi)
    F=F+Fi
  }
  # Normalizo dividiendo por N
  F=F/N
  # b)  Minimizo la funci�n f con W = inv(F)
  W=solve(F)
  ld=nlm(f,ld,S1,S2,W)
  QN=ld$minimum
  ld=ld$estimate
  
  print(ld)
  print(QN)
}


# Para realizar Hansen iterado cambio los
# l�mites del for()

# Test J de Sargent y Hansen
J=N*QN
pvalue=pchisq(1,J)
print(pvalue)


#*******************************************
#     REGRESIONES ESPURIAS
#*******************************************
# Genero dos series de ruido puramente aleatorias
# independientes entre si
T=200
y=rnorm(T)
x=rnorm(T)

# Realizo una regresi�n entre ambas que obviamente 
# es no significativa
r1=lm(y~x)
summary(r1)

# Genero una tendencia creciente y genero
# se la sumo a las series
trend=seq(200)
y1=y+0.02*trend
x1=x+0.01*trend

# Ahora la regresi�n da fuertemente significativa
# mostrando que se trata de una REGRESION ESPURIA
r2=lm(y1~x1)
summary(r2)
