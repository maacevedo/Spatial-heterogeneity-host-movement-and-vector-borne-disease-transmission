library(Rmisc)

load("con.rdata")

##############################
#Data prep
##############################

#Merge data
load('results1-20.rdata')
R0s.merge=R0s.lB
X.prop.merge=X.prop.lB
R0is.merge=R0is.lB

load('results21-40.rdata')
R0s.merge=c(R0s.merge,R0s.lB)
X.prop.merge=c(X.prop.merge,X.prop.lB)
R0is.merge=c(R0is.merge,R0is.lB)

load('results41-60.rdata')
R0s.merge=c(R0s.merge,R0s.lB)
X.prop.merge=c(X.prop.merge,X.prop.lB)
R0is.merge=c(R0is.merge,R0is.lB)


load('results61-80.rdata')
R0s.merge=c(R0s.merge,R0s.lB)
X.prop.merge=c(X.prop.merge,X.prop.lB)
R0is.merge=c(R0is.merge,R0is.lB)

load('results81-100.rdata')
R0s.merge=c(R0s.merge,R0s.lB)
X.prop.merge=c(X.prop.merge,X.prop.lB)
R0is.merge=c(R0is.merge,R0is.lB)



#Extract data
R0s.all=list()
X.prop.all=list()
R0is.all=list()


for (i in 2:5){
    R0s.all.vec=numeric(0)
    X.prop.all.vec=numeric(0)
    R0is.all.vec=numeric(0)
  for (j in 1:100){
    R0s.all.vec=rbind(R0s.all.vec,R0s.merge[[j]][[i]])
    X.prop.all.vec=rbind(X.prop.all.vec,X.prop.merge[[j]][[i]])
    R0is.all.vec=rbind(R0is.all.vec,R0is.merge[[j]][[i]])
  }  
    R0s.all[[i]]=R0s.all.vec
    X.prop.all[[i]]=X.prop.all.vec
    R0is.all[[i]]=R0is.all.vec
}

#Calculate mean and 95% CI
R0s.m.l=list()
R0s.u.l=list()
R0s.l.l=list()

X.prop.m.l=list()
X.prop.u.l=list()
X.prop.l.l=list()


for (f in 2:5){
  R0s.m=numeric(221)
  R0s.u=numeric(221)
  R0s.l=numeric(221)
  
  X.prop.m=numeric(221)
  X.prop.u=numeric(221)
  X.prop.l=numeric(221)
  for (e in 1:221){
  
    R0s.m[e]=as.numeric(CI(R0s.all[[f]][,e])[2])
    R0s.u[e]=as.numeric(CI(R0s.all[[f]][,e])[1])
    R0s.l[e]=as.numeric(CI(R0s.all[[f]][,e])[3])
    
    X.prop.m[e]=as.numeric(CI(X.prop.all[[f]][,e])[2])
    X.prop.u[e]=as.numeric(CI(X.prop.all[[f]][,e])[1])
    X.prop.l[e]=as.numeric(CI(X.prop.all[[f]][,e])[3])
  
  }
  
  R0s.m.l[[f-1]]=R0s.m
  R0s.u.l[[f-1]]=R0s.u
  R0s.l.l[[f-1]]=R0s.l
  
  X.prop.m.l[[f-1]]=X.prop.m
  X.prop.u.l[[f-1]]=X.prop.u
  X.prop.l.l[[f-1]]=X.prop.l  
}  

########################################
#Plots
########################################
x11()
par(mfrow=c(1,3))
par(ps = 14, cex = 1, cex.main = 1)

con=con/100
con=con[1:177]
R0s.m.l[[1]]=R0s.m.l[[1]][1:177]
R0s.m.l[[2]]=R0s.m.l[[2]][1:177]
R0s.m.l[[3]]=R0s.m.l[[3]][1:177]
R0s.m.l[[4]]=R0s.m.l[[4]][1:177]

R0s.u.l[[1]]=R0s.u.l[[1]][1:177]
R0s.u.l[[2]]=R0s.u.l[[2]][1:177]
R0s.u.l[[3]]=R0s.u.l[[3]][1:177]
R0s.u.l[[4]]=R0s.u.l[[4]][1:177]

R0s.l.l[[1]]=R0s.l.l[[1]][1:177]
R0s.l.l[[2]]=R0s.l.l[[2]][1:177]
R0s.l.l[[3]]=R0s.l.l[[3]][1:177]
R0s.l.l[[4]]=R0s.l.l[[4]][1:177]


#R0
plot(con,R0s.m.l[[1]],type="l",col=rgb(0.8,0.78,0.45),ylim=c(2.0,4.5),xlab=expression(bold(paste("Rate of movement, k (",days^-1,")"))),ylab=expression(bold(R[0])))
polygon(c(con,rev(con)),c(R0s.u.l[[1]],rev(R0s.l.l[[1]])),col = rgb(0.8,0.78,0.45,alpha=0.1), border = FALSE)
lines(con, R0s.m.l[[1]], lwd = 2,col=rgb(0.8,0.78,0.45),lty=1)

polygon(c(con,rev(con)),c(R0s.u.l[[2]],rev(R0s.l.l[[2]])),col = rgb(0,1,0,alpha=0.1), border = FALSE)
lines(con, R0s.m.l[[2]], lwd = 2,col="green",lty=1) 

polygon(c(con,rev(con)),c(R0s.u.l[[3]],rev(R0s.l.l[[3]])),col = rgb(0,0,1,alpha=0.1), border = FALSE)
lines(con, R0s.m.l[[3]], lwd = 2,col="blue",lty=1) 

polygon(c(con,rev(con)),c(R0s.u.l[[4]],rev(R0s.l.l[[4]])),col = rgb(1,0,0,alpha=0.1), border = FALSE)
lines(con, R0s.m.l[[4]], lwd = 2,col=rgb(1,0,0),lty=1) 

lines(con,rep(2.16,length(con)),lwd=2,col="black")

legend("topright",c(expression(CV==0.00),expression(CV==0.17),expression(CV==0.33),expression(CV==0.50),expression(CV==0.67)),
       lty=1,lwd=3,col=c("black",rgb(0.8,0.78,0.45),rgb(0,1,0),rgb(0,0,1),rgb(1,0,0)),bty="n",cex=0.8)

#Prevalence
X.prop.m.l[[1]]=X.prop.m.l[[1]][1:177]
X.prop.m.l[[2]]=X.prop.m.l[[2]][1:177]
X.prop.m.l[[3]]=X.prop.m.l[[3]][1:177]
X.prop.m.l[[4]]=X.prop.m.l[[4]][1:177]

X.prop.u.l[[1]]=X.prop.u.l[[1]][1:177]
X.prop.u.l[[2]]=X.prop.u.l[[2]][1:177]
X.prop.u.l[[3]]=X.prop.u.l[[3]][1:177]
X.prop.u.l[[4]]=X.prop.u.l[[4]][1:177]

X.prop.l.l[[1]]=X.prop.l.l[[1]][1:177]
X.prop.l.l[[2]]=X.prop.l.l[[2]][1:177]
X.prop.l.l[[3]]=X.prop.l.l[[3]][1:177]
X.prop.l.l[[4]]=X.prop.l.l[[4]][1:177]

plot(con,X.prop.m.l[[1]],type="l",col=rgb(0.8,0.78,0.45),ylim=c(0,0.5),xlab=expression(bold(paste("Rate of movement, k (",days^-1,")"))),ylab=expression(bold("Prevalence")))
polygon(c(con,rev(con)),c(X.prop.u.l[[1]],rev(X.prop.l.l[[1]])),col = rgb(0.8,0.78,0.45,alpha=0.1), border = FALSE)
lines(con, X.prop.m.l[[1]], lwd = 2,col=rgb(0.8,0.78,0.45,alpha=0.1),lty=1)

polygon(c(con,rev(con)),c(X.prop.u.l[[2]],rev(X.prop.l.l[[2]])),col = rgb(0,1,0,alpha=0.1), border = FALSE)
lines(con, X.prop.m.l[[2]], lwd = 2,col="green",lty=1) 

polygon(c(con,rev(con)),c(X.prop.u.l[[3]],rev(X.prop.l.l[[3]])),col = rgb(0,0,1,alpha=0.1), border = FALSE)
lines(con, X.prop.m.l[[3]], lwd = 2,col="blue",lty=1) 

polygon(c(con,rev(con)),c(X.prop.u.l[[4]],rev(X.prop.l.l[[4]])),col = rgb(1,0,0,alpha=0.1), border = FALSE)
lines(con, X.prop.m.l[[4]], lwd = 2,col=rgb(1,0,0),lty=1) 

lines(con,rep(0.05,length(con)),lwd=2,col="black")


boxplot(rep(2.16,1000),as.numeric(R0is.all[[2]]),as.numeric(R0is.all[[3]]),as.numeric(R0is.all[[4]]),as.numeric(R0is.all[[5]]),
        col=c("black","khaki3","green","blue","red"),ylab=expression(bold(R[0][i])),xlab=expression(bold("Heterogeneity (CV)")),axes=FALSE)
axis(1,at=1:5,label=c(0,0.17,0.33,0.5,0.67))
axis(2)
box()



