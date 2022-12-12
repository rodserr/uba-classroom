#********************************************
#*       SELECCIÓN DE LAGS METODO STEP WISE
#********************************************
# Selecciona la cantidad de lags a incluir en la
# regresión del test ADF para absorber la posible
# autocorrelación, mediante el método step wise
# Argumentos
# y: Serie a testear
# m: Modelo adoptado
#     m=0: Sin intercepto ni tendencia
#     m=1  Con intercepto pero sin tendencia
#     m=2: Con intercepto y tendencia
# alfa: Nivel de significatividad de "corte" elegido

# install.packages("dynlm")
library(dynlm)

ADF_SW=function(y,m,alfa){

  maxlag=trunc(min(length(y)/3,12)*(length(y)/100)^0.25)
  lag=0
  if(m==0){form=d(x) ~ L(x)+L(d(x),1:i) -1}
  if(m==1){form=d(x) ~ L(x)+L(d(x),1:i)}
  if(m==2){form=d(x) ~ L(x)+trend(x)+L(d(x),1:i)}
  
      for(i in maxlag:1) {
#        x=y[1:(length(y)-maxlag+i)]
#        x=ts(x,start=1900, frequency=1)
      x=y
      reg=dynlm(form)
      A=summary(reg)
      if(A$coefficients[(1+i+m),4]< alfa){lag=i ; break }
      }
      if(lag==0){
             if(m==0){form=d(x) ~ L(x) -1}
             if(m==1){form=d(x) ~ L(x)}
             if(m==2){form=d(x) ~ L(x)+trend(x)}
             reg=dynlm(form)
      }
      else{
        if(m==0){form=d(x) ~ L(x)+L(d(x),1:lag) -1}
        if(m==1){form=d(x) ~ L(x)+L(d(x),1:lag)}
        if(m==2){form=d(x) ~ L(x)+trend(x)+L(d(x),1:lag)}
        reg=dynlm(form)
      }
  print("longitud de lags seleccionada:  ")
  print(lag)
  return(reg)
}

# Ejemplo
# salida=ADF_SW(lc,2,0.05)
# summary(salida)

