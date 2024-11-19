
# Funci?n de c?lculo de la tasa incremental para el modelo log?stico con los datos de presi?n arterial (short)
# objeto es el ajuste (e.g. heart.glm2) y x1,x2 los valores de bp para los que se obtiene la predicci?n

tasa.incremental<-function(objeto,x1,x2,decimales=2){
incre<-abs(x1-x2)
P1<-data.frame(a=x1);names(P1)<-names(objeto$coeff[2])
P2<-data.frame(a=x2);names(P2)<-names(objeto$coeff[2])

a26<-predict(objeto,P1,type="response") 
a27<-predict(objeto,P2,type="response")

# Tasa media de crecimiento en la proporcion de enf. coronarias cuando x=x1 . Tasa incremental de cambio
tasa<-objeto$coef[2]*a26*(1-a26)*incre

# Calculo de los odds ratio para x1 y para x2
oddsx1<-a26/(1-a26)
oddsx2<-a27/(1-a27)
 
tasam<-oddsx2/oddsx1 ### tasa media de crecimiento en los odds de x1 a x2
exponencial<-exp(objeto$coef[2]) #### exp de beta1. Incremento por unidad a?adida
print(c("x1=",x1,"x2=",x2,"incremento=",incre))
print("pi.x1.1=probabilidad de éxito en x1")
print("pi.x2.1=probabilidad de éxito en x2")
print("odds.x1.1=odds en x1")
print("odds.x2.1=odds en x2")
print("tasa.inc.p=tasa incremental de cambio de probabilidad x1 a x2")
print("tasa.inc.odds=tasa incremental de cambio de odds(x1) a odds(x2)")



return(salida=c("pi.x1"=round(a26,decimales), "tasa.inc.p"=round(tasa,decimales),pi.x2=round(a27,decimales),odds.x1=round(oddsx1,decimales),
               tasa.inc.odds=round(tasam,decimales), odds.x2=round(oddsx2,decimales)))}

# Ejemplos de uso - datos short
#tasa.incremental2(heart.glm2,112,113)
#tasa.incremental2(heart.glm2,130,140)

# Ejemplos de uso - datos short
#tasa.incremental2(cangrejos.glm2,25,26)
#tasa.incremental2(cangrejos.glm2,21,29)

# Ejemplos de uso - datos insectos
#tasa.incremental2(Insectos.glm2,1.77,1.78)
