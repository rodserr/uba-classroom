############################################################################
UBA-FCE-POSGRADO
MODELOS DE REGRESION GENERALIZADOS
PRACTICA 3: MODELOS PARA DATOS DE CONTEO
Act Miguel Cordoba
############################################################################

--------------------
EJEMPLO 1 - LLOYDS
--------------------

library(MASS)
require(MASS)
s<-ships
ship2<-subset(s,service>0)
ship2$year<-as.factor(ship2$year)
ship2$period<-as.factor(ship2$period)
summary(ship2)


m1<-glm(incidents~type+year+period,family=poisson(link="log"),data=ship2,offset=log(service))
summary(m1)


library(ggplot2)
ggplot(ship2, aes(x = year, y = log(incidents), group = period, color = period)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(~type) +
  theme_bw()


ship4<-subset(s,service>0)
p4<-glm(formula = incidents ~ type + year + period + type:year, family = poisson, 
    data = ship4, offset = log(service))
summary(p4)

m3<-glm(incidents ~ type*(year+ period), family = poisson(link="log"), offset = log(service), data=ship4)
summary(m3)
step(p4,test="Chisq")

step(m1,test="Chisq")

export_summs(p4,m3)

library(mfx)
m3<-poissonmfx(incidents ~ type*(year+ period), data=ship4)
summary(m3)
m3

mg<-margins(p4)
summary(mg)

-----------------------------------
EJEMPLO 2 - SINGAPORE CAR 
-----------------------------------
##Carga de datos##
Singapore = read.csv(choose.files(),  quote = "",header=TRUE)
names(Singapore)
dim(Singapore)
Singapore[110:118,]
fix(Singapore)
attach(Singapore)
Singapore


#Descriptiva - efecto caracteristica vehiculo#
table(VehicleType)
Auto = 1*(VehicleType=="A")
table(Auto)
table(Auto, Clm_Count)
table(VAgeCat)
table(VAgecat1)
table(VAgecat1, Clm_Count)
table(Auto, VAgeCat,Clm_Count)
table(VAgeCat)



#Descriptiva - efecto caracteristicas personales#
SingaporeA = subset(Singapore, (Auto==1))
detach(Singapore);attach(SingaporeA)
dim(SingaporeA)
names(SingaporeA)
Count0 = 1*(Clm_Count==0)
table(Count0,SexInsured)
table(Count0,AgeCat)
table(Count0,NCD)
detach(SingaporeA);attach(Singapore)

##Regresion de Poisson##
Female = 1*(SexInsured == "F" )
NCD1F = relevel(factor(NCD), ref="50")
AgeCatF = relevel(factor(AgeCat), ref="7")
VAgecat1F = relevel(factor(VAgecat1), ref="6")
poiss<- glm(Clm_Count~Female+Auto +Auto:NCD1F + VAgecat1F, offset=LNWEIGHT,poisson(link=log))
summary(poiss)


CountPoisson3 = glm(Clm_Count~Female+Auto
   +Auto:AgeCatF + Auto:NCD1F + VAgecat1F, 
   offset=LNWEIGHT,poisson(link=log))
summary(CountPoisson3)
coeftest(CountPoisson3)
logLik(CountPoisson3)


##overdispersion##

grafico##
plot(log(fitted(poiss)),log((Singapore$Clm_Count-fitted(poiss))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)

test sobredispersion##
library(AER)
dispersiontest(poiss)


##Quase likelihood##
CountPoisson4 = glm(Clm_Count~Female+Auto
   +Auto:AgeCatF + Auto:NCD1F + VAgecat1F, 
   offset=LNWEIGHT,quasipoisson(link = "log"))
summary(CountPoisson4)
coeftest(CountPoisson4)
logLik(CountPoisson4)

qpoiss<- glm(Clm_Count~Female+Auto + Auto:NCD1F + VAgecat1F, offset=LNWEIGHT,quasipoisson(link=log))
summary(qpoiss)


##comparacion poisson com qpoiss##

library(jtools)
library(margins)
library(huxtable)

export_summs(poiss,qpoiss)



##Regresion BN##
library(MASS)
neg<-glm.nb(Clm_Count~Female+Auto+ Auto:NCD1F+VAgecat1F+offset(LNWEIGHT),link=log)
summary(neg)



##comparacion de los tres ajustes#


library(jtools)
library(margins)
library(huxtable)

export_summs(poiss,qpoiss,neg)



##Comparacion de valores ajustados##

Table1 = function(y){
   options(digits=8)
   table1p = cbind(sum(dpois(0,y)),
                   sum(dpois(1,y)),
                   sum(dpois(2,y)),
                   sum(dpois(3,y)),
                   sum(dpois(4,y)));
   actual = data.frame(table(Clm_Count))[,2];actual[5] = 0
   diff = actual-table1p
   PearsonG = sum(diff*diff/table1p);
   cbind(table1p,PearsonG)}

Table1(poiss$fitted.values)

Table1.nb = function(y){
   options(digits=8)
   table1p = cbind(sum(dnbinom(0,size=2.33,mu=y)),
                   sum(dnbinom(1,size=2.33,mu=y)),
                   sum(dnbinom(2,size=2.33,mu=y)),
                   sum(dnbinom(3,size=2.33,mu=y)),
                   sum(dnbinom(4,size=2.33,mu=y))                
                   );
   actual = data.frame(table(Clm_Count))[,2];actual[5] = 0
   diff = actual-table1p
   PearsonG = sum(diff*diff/table1p);
   cbind(table1p,PearsonG)}

Table1.nb(neg$fitted.values)


Table1.q = function(y){
   options(digits=8)
   table1p = cbind(sum(dpois(0,y)),
                   sum(dpois(1,y)),
                   sum(dpois(2,y)),
                   sum(dpois(3,y)),
                   sum(dpois(4,y)));
   actual = data.frame(table(Clm_Count))[,2];actual[5] = 0
   diff = actual-table1p
   PearsonG = sum(diff*diff/table1p);
   cbind(table1p,PearsonG)}

Table1.q(qpoiss$fitted.values)

v<-c(0,1,2,3,4,"","chi-cuadrado")
obs<-c(6996,455,28,4,"")
poisson<-Table1(poiss$fitted.values)
quasiverosimil<-Table1.q(qpoiss$fitted.values)
binomial_neg<-Table1.nb(neg$fitted.values)

comp<-data.frame(poisson,quasiverosimil,binomial_neg)



bind(v,Table1.q(qpoiss$fitted.values))


Table1.z = function(y){
   options(digits=8)
   table1p = cbind(sum(dpois(0,y)),
                   sum(dpois(1,y)),
                   sum(dpois(2,y)),
                   sum(dpois(3,y)),
                   sum(dpois(4,y)));
   actual = data.frame(table(Clm_Count))[,2];actual[5] = 0
   diff = actual-table1p
   PearsonG = sum(diff*diff/table1p);
   cbind(table1p,PearsonG)}

Table1.z(zip$fitted.values)


##ZIP MODEL  (se debe cargar el paquete pscl)
library(pscl)
zip<-zeroinfl(Clm_Count~Female+Auto+NCD1F + VAgecat1F+offset(LNWEIGHT)|Female+NCD1F+VAgecat1F,data=Singapore)
summary(zip)

library(Countr)

compareToGLM(poiss,neg)

library<-(countreg)

root<-rootogram(CountNB1,main="BN")



-----------------------------------
EJEMPLO 3 - RECREACION
-----------------------------------
library(AER)
data("RecreationDemand")
RecreationDemand

pois<-glm(trips~quality+ski+income+userfee+costC+costS+costH,data=RecreationDemand,poisson(link=log))
summary(pois)

rbind(obs = table(RecreationDemand$trips)[1:10], exp = round(
+ sapply(0:9, function(x) sum(dpois(x, fitted(pois))))))


plot(log(fitted(pois)),log((RecreationDemand$trips-fitted(pois))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)

dispersiontest(pois)

qpois<-glm(trips~quality+ski+income+userfee+costC+costS+costH,data=RecreationDemand,quasipoisson(link=log))
summary(qpois)

library(countreg)

root<-rootogram(pois,main="Poisson")
root<-rootogram(qpois,main="QuasiPoisson")


root<-rootogram(bn,main="BN")
root<-rootogram(rd_zinb,main="ZINB")
root<-rootogram(hurdle,main="Hurdle")
mfrow=c(3,1)




rbind(obs = table(RecreationDemand$trips)[1:10], exp = round(
+ sapply(0:9, function(x) sum(dpois(x, fitted(qpois))))))

library(MASS)
bn<-glm.nb(trips~quality+ski+income+userfee+costC+costS+costH,data=RecreationDemand,link=log)
summary(bn)

round(colSums(predict(bn, type = "prob")[,1:10]))

??predict

rbind(obs = table(RecreationDemand$trips)[1:10], exp = round(
+ sapply(0:9, function(x) sum(dnbinom(x,size=0.7293,fitted(bn))))))

library(pscl)
rd_zinb <- zeroinfl(trips ~ . | quality + income,data = RecreationDemand, dist = "negbin")
summary(rd_zinb)

hurdle<- hurdle(trips ~ . | quality + income,data = RecreationDemand, dist = "negbin")
summary(hurdle)


round(colSums(predict(rd_zinb, type = "prob")[,1:10]))

library(nonnest2)
vuongtest(bn,rd_zinb)

vuongtest(hurdle,rd_zinb)

vuongtest(bn,hurdle)



root<-rootogram(pois,main="BN")

root<-rootogram(bn,main="BN")

root<-rootogram(rd_zinb,main="ZINB")











