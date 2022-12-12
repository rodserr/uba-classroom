#********************************************
#*          Test de raiz unitaria 
#*       Augmented Dickey Fuller Test
#********************************************
#* Versión 26/07/2022    Encoding: UTF-8
#*
# Este programa permite seleccionar el modelo para
# realizar el test de Dickey Fuller Aumentado y
# selecciona la cantidad de lags a incluir en la
# regresión del test ADF (para absorber la posible
# autocorrelación)
# Finalmente, ejecuta el test con el modelo y los 
# lags seleccionados

# Argumentos
# y: Serie a testear
# mod: Modelo adoptado
#     mod=1: Sin intercepto ni tendencia
#     mod=2  Con intercepto pero sin tendencia
#     mod=3: Con intercepto y tendencia
# sel.lags: Método de selección de la cantidad de lags 
#     "AIC" Modelo con menor criterio de Akaike (Default)
#     "BIC" Modelo con menor criterio de Schwarz
#     "SW"  Modelo con criterio Step Wise
# alfa: Nivel de significatividad de "corte" elegido 
#  (sólo para la selección por Step Wise) (0.10 es el default)

# install.packages("dynlm")
  library(dynlm)
# install.packages("urca")  
  library(urca)

  Test.ADF=function(y,mod,sel.lags="AIC",alfa=0.10) {
  
  # Convierto la serie en serie de tiempo
    x=ts(y,start=1)
  # Determino la máxima cantidad de lags
  maxlag=trunc(min(length(x)/3,12)*(length(x)/100)^0.25)
  print(paste("Cantidad máxima de lags: ", maxlag))

  # Preparo las fórmulas (no valen para lag=0)
  if(mod==1){form=d(x) ~ L(x)+L(d(x),1:i) -1}
  if(mod==2){form=d(x) ~ L(x)+L(d(x),1:i)}
  if(mod==3){form=d(x) ~ L(x)+trend(x)+L(d(x),1:i)}
#**********************************************************
  if(sel.lags=="SW"){
    tabla=c()  # No habrá tabla
    # Inicializo con lag=0
    lag=0
      for(i in maxlag:1) {
      reg=dynlm(form)
      A=summary(reg)
      if(A$coefficients[(i+mod),4]< alfa){
        lag=i ; break }
      }
    print(paste("Metodo SW , lag = ",lag))
  }
#**********************************************************  
  if(sel.lags=="BIC" | sel.lags=="AIC"){
    tabla=matrix(rep(0,((maxlag+1)*3)),ncol=3)
    colnames(tabla)=c("Lag","AIC","BIC")
    for(i in 1:maxlag) {
    reg=dynlm(form,start=maxlag+2)
      tabla[i+1,1]=i
      tabla[i+1,2]=AIC(reg)
      tabla[i+1,3]=BIC(reg)
    }
      if(mod==1){form0=d(x) ~ L(x) -1}
      if(mod==2){form0=d(x) ~ L(x)}
      if(mod==3){form0=d(x) ~ L(x)+trend(x)}
      reg0=dynlm(form0, start=maxlag+2)
      tabla[1,1]=0
      tabla[1,2]=AIC(reg0)
      tabla[1,3]=BIC(reg0)
  }
#**********************************************************  
    if(sel.lags=="AIC"){
    tabla.ord=tabla[order(tabla[,2],decreasing=F),]
    lag=tabla.ord[1,1]
    print(paste("Método AIC , lag = ",lag))
    }
#**********************************************************
  if(sel.lags=="BIC"){
    tabla.ord=tabla[order(tabla[,3],decreasing=F),]
    lag=tabla.ord[1,1]
    print(paste("Método BIC , lag = ",lag))
  }
#**********************************************************   
if(mod==1){salida<-ur.df(x, lags = lag, selectlags = "Fixed", type="none")}
if(mod==2){salida<-ur.df(x, lags = lag, selectlags = "Fixed", type="drift")}
if(mod==3){salida<-ur.df(x, lags = lag, selectlags = "Fixed", type="trend")}
print(summary(salida))
Test.ADF=list(mod,sel.lags,lag,tabla)
names(Test.ADF)=c("Modelo","Selector de Lags","Lags sel","Tabla")
return(Test.ADF)
}
# Ejemplo
# Test=Test.ADF(lc,2,"BIC")


