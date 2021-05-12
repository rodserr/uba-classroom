
# SIMULACIÓN CLASE 2 4/05/2021

#Ejercicio 1
#En un proceso de manufactura el porcentaje de defectuosos es 
#del 10%. Se fabrican 30 unidades por hora.
#a) Simular la cantidad de defectuosos por hora en un 
#periodo de 24 horas.
#b) Simular la cantidad de defectuosos por hora 
#en un periodo de 48 horas.
#c) Verificar si el número de defectuosos excede los 5 en 
#algún momento del proceso ¿En cuántas ocasiones
#ocurre esto?
#d) Repetir la simulación si el porcentaje de defectuoso es del 8%

#a) Simulamos la cantidad de defectuosos
#La variable responde a un modelo binomial de parámetros
#n de la binomial=30 y p=0,10
#n=24 en rbinom(n,etc) indica que generemos n=24 valores random
set.seed(1245)
defectuosos<-rbinom(24,30,0.10)
defectuosos
#b)
set.seed(1245)
defectuosos<-rbinom(48,30,0.10)
defectuosos
#c)
#Utilizamos la función ANY
any(defectuosos>5)
sum (defectuosos>5)

#d) 
set.seed(1245)
defectuosos<-rbinom(24,30,0.08)
defectuosos
###############################################################
#Ejercicio 2
#Se lanza 10 veces una moneda. Calcular la probabilidad de 
#obtener:
#a) tres caras
#b) a lo sumo seis caras

#a) #dbinom nos informa en referencia a la función de 
#probabilidad teórica de una variable aleatoria discreta
#X=cantidad de caras obtenidas en los 10 tiros
#p(X=3)
dbinom (x=3,size=10, prob=0.5)
#a) 
muestras_n_3 <- 0
for (j in 1:10000){
  n_caras <- sum(rbinom(10, 1, prob=0.5))
  if (n_caras == 3)
    muestras_n_3 <- muestras_n_3 +1
}
muestras_n_3 / 10000
#Comparemos con la teórica
#p(X menor o igual a 6)=F.de distribucion F(6)
#b) 
muestras_n_6 <- 0
for (j in 1:10000){
  n_caras <- sum(rbinom(10, 1, prob=0.5))
  if (n_caras <= 6)
    muestras_n_6 <- muestras_n_6 +1
}
muestras_n_6 / 10000

#pbinom informa en referencia a la función de distribución 
#de la teórica
pbinom(q=6, size=10,prob=0.5)

###########################################################
#Ejercicio 3
#Simular 12000 valores de una variable aleatoria con 
#distribución Binomial de parámetros n = 30 y p =0,4.
#Utilizar los valores simulados para estimar:
#a) P(X< 8)=p(X menor o igual que 7)=F.distrib en 7 F(7)
#X es una v.a. discreta
#b) P(X = 5)
#c) E(X)

#Simulamos los valores bajo este modelo
set.seed(1245)
J<-rbinom(12000,30,0.4)
#a)
sum(J<8)/length(J)
#Probabilidad teórica
pbinom(q=7,size=30,prob=0.4)
#b)
sum(J == 5)/length(J)
#probabilidad teórica
dbinom(5,30,0.4)
#c)
mean(J)

##########################################################
#Ejercicio 4
#El tiempo de atención en una sucursal bancaria puede 
#ser modelado por una variable aleatoria con distribución
#exponencial de razón 5 clientes por minuto.
#a) Calcular la probabilidad de que un cliente sea atendido 
#en menos de 2 minutos.
#b) Simular 1500 valores de la variable anterior 
#y comparar su histograma con la función de densidad
#teórica de la distribución (utilizar semilla 433)
#d) Var(X)

#a) Calculamos probabilidad teórica
#p(t<2)=p(t menor o igual a 2)=F(2) porque t es una v.a. continua
pexp(2,rate=5)

#b) Simulamos 1500 valores de la variable anterior
set.seed(433)
H<-rexp(1500, rate=5)
#Se muestran los 40 primeros valores de la simulación
H[1:40]
#b)
par(mfrow=c(1,2))
set.seed(433)
x=rexp(1500,5)
hist(x,freq=F, main="Densidad simulada",xlim=c(0,1))
curve(dexp(x,5),main="Densidad teorica", add=F, col="red")
mean(x)
var(x)
sd(x)
############################################################
#Ejercicio 5
#En un día cualquiera, un comercio puede vender 0; 1; 2, 5 o 10 
#artículos de determinado producto. Un análisis de datos 
#históricos ha revelado que la distribución 
#del número de artículos vendidos es la siguiente:

#P(0)=0.35   P(1)=0,2   P(2)=0,1  P(5)=0,25  P(10)=0,10
#a) Determinar, mediante simulación, la distribución 
#del número total de artículos vendidos en un año
#(considerar 365 días)
#b) Se estima que para cubrir los costos mensuales,
#las ventas deben superar los 80 artículos. ¿Cuál es la
#probabilidad de que el comercio no llegue a cubrir los costos?
#(considerar un mes de 30 días)

#a)
#Confeccionamos la siguiente función
set.seed(2021)
ventas<-function(h){
  J <- sample(
    c(0,1,2,5,10),
    size=h,
    replace=T,
    prob=c(0.35,0.2,0.1,0.25,0.10))
  sum (J) }

ventas(365)
#Cantidad de simulaciones
N<-10000

Y<-replicate(N, ventas(365))
#El vector Y contiene la simulacion de ventas totales
#para 365 días
Y[1:40]
tabla<-table(Y)/N
par(mfrow=c(1,1))
hist(Y)
#b)
ventas_mes <-replicate(N, ventas(30))
sum(ventas_mes <= 80)/10000
