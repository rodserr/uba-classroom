###############################################
# Generaci�n de una poblaci�n simulada de N individuos
# Distribuci�n U(0,100)
# Verificaci�n de par�metros
N=100000 # Nro de individuos en la poblaci�n
X=c(runif(N,0, 100))
media=mean(X)  # te�rica mean = (0 + 100)/2=50
mediana=median(X)  # te�rica median = (0 + 100)/2=50
Varianza=var(X)     # te�rica = (100 - 0)^2/12 = 833,33...
install.packages("moments")
library("moments")

Simetr�a_std=skewness(X) # Te�rica S= 0
Curtosis_std=kurtosis(X)# Te�rica K = 1.8
windows()
hist(X)
dev.off()

#############################################

# Insesgadez del promedio
n=10  # tama�o de la muestra
# Tomo muestras n=10 de la distribuci�n uniforme
# y calculo promedios. Lo hago 1000 veces
# Generacion de la distribuci�n muestral
d_muestral=c()
for(i in 1:1000)   {
  muestra=sample(X,size=n,replace=T)
  prom_m=mean(muestra)
  d_muestral=c(d_muestral,prom_m)
}
# View(d_muestral)
# Insesgadez
media_pob=mean(X)
media_pm=mean(d_muestral) # te�rica = media_pob
# Varianza
var_pob=var(X)
var_pm=var(d_muestral)  # te�rica = var_pob/10
windows()
hist(d_muestral)
dev.off()

###############################################

# Teorema central del limite
# Genero muestras de n creciente y verifico
# la tendencia de la distrib. del promedio
# a la normalidad
pvalues=matrix(c(0,0,0,0,0),ncol=5)
for(j in 1:4)   {
  n=10^j
  d_muestral=c()
  for(i in 1:1000)   {
    muestra=sample(X,size=n,replace=T)
    prom_m=mean(muestra)
    d_muestral=c(d_muestral,prom_m)
}
  pvalue=shapiro.test(d_muestral)
  pvalue1=pvalue[2]
  pvalue=ks.test(d_muestral,pnorm,mean(d_muestral), sd(d_muestral))
  pvalue2=pvalue[2]
  pvalue=jarque.test(d_muestral)
  pvalue3=pvalue[2]
  curtosis=kurtosis(d_muestral)
  res=matrix(c(n,pvalue1,pvalue2,pvalue3,curtosis),ncol=5)
  print(res)
  pvalues=rbind(pvalues,res)
}
pvalues=matrix(unlist(pvalues),ncol=5,byrow=F)  
pvalues=pvalues[-1,]
colnames(pvalues)<-c("n","Shapiro","KS","JB","Curtosis")
View(pvalues)

######################################################

# Ley de los grandes n�meros
# A medida que la muestra es m�s grande
# m�s se acerca el promedio a la media
resultado=matrix(0,0,ncol=2)
for(n in 1:10)   {
  prom=mean(sample(X,100*n,replace=T))
  resultadoi=matrix(c(100*n,prom), ncol=2)
  resultado=rbind(resultado,resultadoi)
}
View(resultado)
plot(resultado[ ,1],resultado[ ,2], type="l")



