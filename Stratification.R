library(MASS)
library(lpSolve)
library(sampling)
library(readxl)

projet_sondage_Responses <- read_excel("C:/Users/amirk/Downloads/projet-sondage-Responses.xlsx")
View(projet_sondage_Responses)
base=projet_sondage_Responses
sexe=base$Sexe 
sexe
Nh=table(sexe)
Nh
N=sum(Nh)
ST=list(NULL)
for(i in 1:20) {
  st=strata(base,stratanames=c("Sexe"),size=c(20,20),method=c("srswor"))
  st
  ST[[i]]=st
}
ST
PIK=list(NULL)
for(i in 1:20) {
  PIK[[i]]=ST[[i]]$Prob
}
PIK
Ech=list(NULL) 
for(i in 1:20) {
  Ech[[i]]=getdata(base,ST[[i]])
}
Ech
Y=matrix(nrow=20,ncol=40) 
for(i in 1:20) {
  Y[i,]=Ech[[i]]$Y
}
Y

y_estim=list(NULL) 
for(i in 1:20) {
  y_estim[[i]]=(1/N)*sum(Y[i,]/PIK[[i]])
}
y_estim
nh=c(20,20)
nh
for (i in 1:20){
  ybar_estim=c((1/nh[1])*sum(Y[i,1:20]),(1/nh[2])*sum(Y[i,21:40]))
}
ybar_estim
yh_bar_estim=ybar_estim
ybar_estim_H=list(NULL) 
for(i in 1:20) 
{
  ybar_estim_H[[i]]=yh_bar_estim[1]
}
ybar_estim_H
ybar_estim_F=list(NULL) 
for(i in 1:20) 
{
  ybar_estim_F[[i]]=yh_bar_estim[2]
}
h5=ybar_estim_H
mean(as.numeric(h5))

s2y=list(NULL)
for(i in 1:20) 
{
  s2y[[i]]=c((1/(nh[1] ))*sum((Y[i,1:nh[1]-1]-
                                  ybar_estim_H[[i]])^2),(1/(nh[2]-1))*sum((Y[i,1:nh[2]]-
                                                                             ybar_estim_F[[i]])^2))
}
s2y
varestimybar=list(NULL)
for(i in 1:20) 
{
  varestimybar[[i]]=(1/N^2)*sum((Nh*(Nh-nh)/nh)*s2y[[i]])
}
varestimybar
nh=(Nh*s2y[[i]])/((1/n)*sum((Nh*s2y[[i]])))
nh

n=sum(nh[1],nh[2])
n
varestimybaropt=list(NULL)
for(i in 1:20) {
  varestimybaropt[[i]]=(1/N^2)*((1/n)*(sum((Nh*sqrt(s2y[[i]])))^2)-(sum((Nh*s2y[[i]]))))
}
varestimybaropt


icmin=list(NULL)
for(i in 1:20) {
  icmin[[i]]=-1.96*sqrt(varestimybar[[i]])+y_estim[[i]]
}
icmin
icmax=list(NULL)
for(i in 1:20) {
  icmax[[i]]=1.96*sqrt(varestimybar[[i]])+y_estim[[i]]
}
icmax
s=1:20
h=y_estim
h1=icmin
h2=icmax
h3=varestimybar
h4=varestimybaropt
h4
library(lattice)
xyplot(h1+h2+h ~ 
         s,col=c("blue","red","green"),main="Sondage 
stratifi?",ylab="D?pence",type='o')
xyplot(h3+h4 ~ 
         s,col=c("blue","red"),main="defference entre les variances proportionnelles et optimales ",ylab="variance",type='o')
mean(as.numeric(varestimybar)) 
mean(as.numeric(varestimybaropt))
mean(as.numeric(h)) 
h
mean(as.numeric(h4))
as.numeric(h2)

