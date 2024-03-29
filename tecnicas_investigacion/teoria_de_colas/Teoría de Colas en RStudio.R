
#######################################################################################################

### E72: Especializaci�n en M�todos Cuantitativos para la Gesti�n y An�lisis de Datos en Organizaciones

### T�CNICAS DE INVESTIGACI�N OPERATIVA PARA LA GESTI�N

### Resoluci�n de los ejemplos de la clase te�rica mediante la utilizaci�n de la librer�a �queueing�

### Mar�a Jos� Bianco / Natalia Salaberry

#######################################################################################################

### Paquete y librer�a

install.packages("queueing")
library(queueing)

###Ejemplo (slide 24) M/M/1

resultados<-summary(QueueingModel(NewInput.MM1(lambda=45/60, mu=60/60, n=0)))

res<-data.frame(Reduce(rbind, resultados))

print(t(res))


###Ejemplo (slide 27) M/M/1

resultados<-summary(QueueingModel(NewInput.MM1(lambda=(45/60)/2, mu=(60/60)/2,  n=0)))

res<-data.frame(Reduce(rbind, resultados))

print(t(res))

res$L*2


#Ejemplo (slide 34) M/M/s

resultados <- summary(QueueingModel(NewInput.MMC(lambda=45/60, mu=60/60, c=2, n=0)))

res<-data.frame(Reduce(rbind, resultados))

print(t(res))


#Ejemplo (slide 45) M/M/1/k

resultados <- summary(QueueingModel(NewInput.MM1K(lambda=18, mu=6, k=4)))

res<-data.frame(Reduce(rbind, resultados))

print(t(res))


#Ejemplo (slide 47) M/M/1/k

resultados <- summary(QueueingModel(NewInput.MM1K(lambda=6, mu=6, k=4)))

res<-data.frame(Reduce(rbind, resultados))

print(t(res))


#Ejemplo (slide 57) Costos

resultados1<-summary(QueueingModel(NewInput.MM1(lambda=45/60, mu=60/60, n=0)))
resultados1<- data.frame(Reduce(rbind, resultados1))

CT1<- 10*(resultados1$L) + 7*(resultados1$c)


resultados2 <- summary(QueueingModel(NewInput.MMC(lambda=45/60, mu=60/60, c=2, n=0)))
resultados2<- data.frame(Reduce(rbind, resultados2))

CT2<- 10*resultados2$L +7*resultados2$c


Comparativo<-rbind("CT1"=CT1, "CT2"=CT2)
Comparativo




