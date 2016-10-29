### TP R CNAM 4###

# le modulo
16%%3

i=1
while(i<=12){
if (i%%2==0){
  print(paste(i,"est pair"))}
else {
  if(i%%3==0){print(paste(i ,"est multiple de 3"))}
      
      
else { print(paste(i,"raté"))}
}# la boucle du premier else se ferme là 
i<-i+1 # a ne pas oublier 

}



x<-seq(0,2*pi,0.1)
y<-sin(x)

pdf(file="D:/Users/dupon_ax/Documents/test.pdf")
plot(y~x,type="l",main="Graphique de sinusoide",ylab="ordonnees",xlab="x")
#voir l'aide de paste pour le titre
lines(x,y^2,lwd=2,col="green")
dev.off()

a<-rpois(1000,3) # poisson parametre 3

table(a)

pdf(file="D:/Users/dupon_ax/Documents/test2.pdf")
plot(table(a)/1000,type="h")
lines(dpois(0:10,3),type="h",col="red")
dev.off()

ggplot(diamonds, aes(carat)) +geom_histogram()


# calcul densité du mélange 1/2N (-2,0.6)+1/2 N(2,1)
a<-0.5
x<-seq(-6,6,0.1)

plot(x,a*dnorm(x,-0.5,0.6)+(1-a)*dnorm(x,0.5,1),type="l")



toto<-function(a,b){
c<-a+b
d<-a-b
e<-a*matrix(1:4,2)
return(list(plus=c,moin=d,matri=e)) # revelation utiliser une liste
}


res<-toto(2,3)
res


#tracer surface z=x2+y2 sur[-1;1][-1;1]

x<-seq(-1,1,length=40)
y<-x


f<-function(x,y){x^2+y^2}
z<-outer(x,y,f)
persp(x,y,z,expand=0.5,theta=45,phi=15)


# Loi Gausienne N( (0,0),s) gausienne a 2 dimensions

s<-matrix (c(1,1,1,4),2)
h<-solve(s)
x<-seq(-6,6,length=40)
y<-x

toto<-function(x,y){exp(-0.5* (h[1,1]*x^2+h[2,2]*y^2+2*h[1,2]*x*y))}



z<-outer(x,y,toto)
persp(x,y,z,theta=45,phi=15)

windows()
contour(x,y,z) # les masses s'articulent autour d'uen certaine direction (la correlation)
