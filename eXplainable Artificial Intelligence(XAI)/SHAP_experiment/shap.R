## Distribution uniforme
set.seed(2021)
 U<- runif(1000,min=-50,max=50) 
## Distribution gaussienne ou normale
G<-rnorm(1000)
### Distribution de Cauchy
C<-rcauchy(1000)
########### Fonction lineaire
Lineaire <- function(N){
a=-10 ; b=28   
   Y1<-a*U+b
  
  return (Y1)   
}

Y_1<-Lineaire(1000)
### Fonction polynomiale deg 2
Polynome<- function(N){
  
     Y2<-2*U^2-5*U+10 
  
  return (Y2/10)   
}

Y_2<-Polynome(1000)

### Fonction polynomiale deg 3
Polynome<- function(N){
  
     Y3<--5*U^3-7*U^2+ 13*U-7
  
  return (Y3/1000)   
}

Y_3<-Polynome(1000)

### Fonction polynomiale deg 4
 Polynome<- function(N){
  
     Y4<--0.05*U^4+130*U^2-36
  
  return (Y4/100)   
}

Y_4<-Polynome(1000)
### Fonction polynomiale deg 5

Polynome<- function(N){

   #Y5<-0.05*(U-5)^3 *(-U-0.6)^2
   
     Y5<--0.0754*U^5+170*U^3+150*U^2-220*U-11
  
  return (Y5/10000)   
}

Y_5<-Polynome(1000)

### Fonction multilineaire
Multilineaire<- function(N){
     M<-17*U+4*G-5*C+3
  
  return (M/10)   
}

Multi<-Multilineaire(1000)
### Fonction logarithme
Logarithme<- function(N){

     L<-log(U^2)
  
  return (L*100)   
}

Log<-Logarithme(1000)

#### Fonction Exponentielle
Exponentielle<- function(N){

     E<-exp(U)
  
  return (E/10e+18)   
}

Exp<-Exponentielle(1000)

## Fonction sinusoidale 
Sinusoidale<- function(N){

     S<-sin(U)
  
  return (S*500)   
}

Sinus<-Sinusoidale(1000)

####
Base<-cbind(Y_1,Y_2,Y_3,Y_4,Y_5,Multi,Log,Exp,Sinus)
## sauvegarde en format csv
 write.table(Base,file="Base.csv",row.names=FALSE,col.names=FALSE)

########## Visualisation

png("Ecdf.png", width = 1200, height = 800)
par(mfrow=c(3,3)) 

plot(U,Y_1,main="Linear",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Y_1),col="red",lwd=2) 
grid(lwd=3)
plot(U,Y_2,main="Quadratic",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Y_2),col="red",lwd=2) 
grid(lwd=3)
plot(U,Y_3,main="Cubic",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Y_3),col="red",lwd=2) 
grid(lwd=3)
plot(U,Y_4,main="Quartic",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Y_4),col="red",lwd=2) 
grid(lwd=3)
plot(U,Y_5,main="Quintic",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Y_5),col="red",lwd=2) 
grid(lwd=3)
plot(U,Multi,main="Multilineaire",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Multi),col="red",lwd=2) 
grid(lwd=3)
plot(U,Log,main="Logarithme",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Log),col="red",lwd=2) 
grid(lwd=3)
plot(U,Exp,main="Exponentielle",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Exp),col="red",lwd=2) 
grid(lwd=3)
plot(U,Sinus,main="Sinus",font.main=6,col.main="blue",cex.main=2)
lines(smooth.spline(U, Sinus),col="red",lwd=2) 
grid(lwd=3)
dev.off()

########################################### Fakes Dat

## Distribution gaussienne ou normale
G<-rnorm(1000)
### Distribution de Cauchy
C<-rcauchy(1000)
X1<- runif(1000)
X2<- runif(1000,min=0,max=100)
X3<- runif(1000,min=-100,max=0)
X4<- C+X1
X5<- C+X1+G
X6<- X1*G*C
Fakes<-cbind(U,C,G,X1,X2,X3,X4,X5,X6)
## sauvegarde en format csv
 write.table(Fakes,file="Fakes_data.csv",row.names=FALSE,col.names=FALSE)
