#######################################################################################################

### Especialización y Maestría en Métodos Cuantitativos para la Gestión y Análisis de Datos en Organizaciones

### TÉCNICAS DE INVESTIGACIÓN OPERATIVA PARA LA GESTIÓN

### Resolución de Actividad de Teoría de Colas

### María José Bianco / Natalia Salaberry

#######################################################################################################

### Paquete y librería

install.packages("queueing")
library(queueing)



## Resolución de la Actividad

# Situación actual
resultados1<-summary(QueueingModel(NewInput.MM1(lambda=15/60, mu=20/60, n=0)))
resultados1<- data.frame(Reduce(rbind, resultados1))

resultados1

CT1<- 25*(resultados1$L) + 150*(resultados1$c)
CT1

# Alternativa A
resultados2 <- summary(QueueingModel(NewInput.MM1K(lambda=15/60, mu=20/60, k=4)))
resultados2<- data.frame(Reduce(rbind, resultados2))

resultados2

CT2<- 25*(resultados2$L) + 150*(resultados2$c)
CT2

# Alternativa B
resultados3 <- summary(QueueingModel(NewInput.MMC(lambda=15/60, mu=20/60, c=2, n=0)))
resultados3<- data.frame(Reduce(rbind, resultados3))
resultados3

CT3<- 25*(resultados3$L) + 200*1 + 150*1
CT3

ComparativoCostos<-rbind("CT1"=CT1, "CT2"=CT2, "CT3"=CT3)
ComparativoCostos

ComparativoDesempeño<-rbind("Resultados1"=resultados1, "Resultados2"=resultados2, "Resultados3"=resultados3)
ComparativoDesempeño

