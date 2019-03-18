# install.packages("forecast")
# install.packages("xlsx")
# install.packages("chron")
# install.packages("stats")
# install.packages("xts")
# install.packages("ggplot2")
# install.packages("strucchange")
# install.packages("astsa")
# install.packages("metrics")
# install.packages("stats4")
# install.packages("tseries")
rm(list=ls())

library(forecast)
library(xlsx)
library(chron)
library(stats)
library(xts)
library(ggplot2)
library(splines)
library(strucchange)
library(astsa)
library(Metrics)
library(stats4)
library(tseries)

# Directorio PC personal
# setwd("C:/Users/USUARIO/Dropbox/Uniandes/Tesis Maestría Industrial/Datos")
# Directorio PC Oficina
setwd("C:/Users/pa.uriza274/Dropbox/Uniandes/Tesis Maestría Industrial/Datos")

fest<-as.Date(as.matrix(read.table("festivos.dat")))
IPCMes<-as.matrix(read.table("IPCMes.dat"))
ONI<-ts(as.vector(t(as.matrix(read.table("ONI.dat")))),start = c(1950, 6), frequency = 12)

anios<-1995:2016
Pr<-matrix()
for(y in anios){
P<-t(read.xlsx(paste0("Precio_Bolsa_Nacional_($kwh)_",y,".xlsx"),1,startRow = 3,colIndex = 2:25))
dim(P)<-c(dim(P)[1]*dim(P)[2],1)

Pr<-rbind(Pr,P)
}
Pr<-Pr[-1]

t <- seq(from = as.POSIXct("1995-07-20 00:00",tz = "GMT"),to = as.POSIXct("2016-08-14 23:00",tz = "GMT"), by = "hour")

#Precio<-xts(Pr,order.by = t)
Precio.msts<- msts(Pr, seasonal.periods = c(24,8766)) #Multi seasonal Time series (Hours,Years)

ff<-matrix(nrow = length(Pr),ncol =length(fest) )
for(f in seq_along(fest)){
ff[,f]=(as.Date(t)==fest[f])
}
ff=as.logical(rowSums(ff))

Año<-factor(years(t))
#Mes<-factor(months(t),levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
Mes<-factor(months(t),levels = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
Día<-factor(weekdays(t),levels = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))
Hora<-factor(rep(0:23,7697))
FDSFNom<-factor(as.character((Día=="sábado"|Día=="domingo")*1+ff*2))
levels(FDSFNom)<-c("Weekday","Weekend","Holiday","Holiday")
FDSF<-factor(as.logical(ff+(Día=="sábado"|Día=="domingo")))

y=as.numeric(Año)
tim=1:length(Precio.msts)
nHorasMA=as.vector(t(tapply(Año,list(Año,Mes),FUN = length))) #n Horas al mes-año
nHorasMA=nHorasMA[!is.na(nHorasMA)]
DiscFac=rep(rev(c(1,cumprod(rev(1/(1+IPCMes))))),nHorasMA)
Prices=(Precio.msts/DiscFac)

#Corrección precio para poder sacar ln
Prices[1]=Prices[2];Prices[Prices==0]=NA;Prices=na.approx(Prices)
Prices=msts(Prices, seasonal.periods = c(24,8766))
Prices.ts=ts(Prices,frequency = 24)

# Data<-data.frame(Año,Mes,Día,Hora,FDSF,Prices)

# cosP=cos((201/365+tim)*2*pi/8760)

#Gráficas justificación modelos

#Estacionalidad Hora
PrHora<-tapply(Prices,list(Hora,FDSFNom),mean,na.rm=TRUE)
PrHora.Std<-tapply(Prices,list(Hora,FDSFNom),sd,na.rm=TRUE)
NHora<-tapply(!is.na(Prices),list(Hora,FDSFNom),sum,na.rm=TRUE)
tIC95Dia<-qt(0.975,NHora-1)
HWHora<-tIC95Dia*PrHora.Std/sqrt(NHora)
dim(PrHora)<-c(dim(PrHora)[1]*dim(PrHora)[2],1)
dim(HWHora)<-c(dim(PrHora)[1]*dim(PrHora)[2],1)
TipoDia=factor(rep(c("Weekday","Weekend","Holiday"),c(24,24,24)),levels=c("Weekday","Weekend","Holiday"))
HoraDia=rep(0:23,3)
DataHora<-data.frame(HoraDia,TipoDia,PrHora,HWHora)

PerfilDia <- ggplot(DataHora, aes(x=HoraDia, y=PrHora)) + geom_point(shape=19,color="black") + xlab("Day Hour") + ylab("Price (COP/kWh)")
PerfilDia + geom_ribbon(aes(ymin = PrHora - HWHora, ymax = PrHora + HWHora), fill = "grey70",alpha=0.5) + geom_line() + xlim(0, 23)+ facet_grid(. ~ TipoDia,scale="free") + theme_bw()

#Estacionalidad Mes
PrMes<-tapply(Prices,Mes,mean,na.rm=TRUE)
PrMes.Std<-tapply(Prices,Mes,sd,na.rm=TRUE)
NMes<-tapply(!is.na(Prices),Mes,sum,na.rm=TRUE)
tIC95Mes<-qt(0.975,NMes-1)
HWMes<-tIC95Mes*PrMes.Std/sqrt(NMes)
Meses<-1:12#c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dic")
DataMes<-data.frame(NMes,PrMes,HWMes)

perfilAño <- ggplot(DataMes, aes(x=Meses, y=PrMes)) + geom_point(shape=19,color="black") + xlab("Month") + ylab("Price (COP/kWh)")
perfilAño + geom_ribbon(aes(ymin = PrMes - HWMes, ymax = PrMes + HWMes), fill = "grey70",alpha=0.5) + geom_line() + theme_bw() + scale_x_continuous(breaks = 1:12)

smoothMes <-predict(interpSpline(Meses, PrMes))
plot(Meses, PrMes)
lines(smoothMes)

#Tendencia a través de los años
PrAño<-tapply(Prices,Año,mean,na.rm=TRUE)
PrAño.Std<-tapply(Prices,Año,sd,na.rm=TRUE)
Naño<-as.vector(t(tapply(Año,Año,FUN = length)))
tIC95Año<-qt(0.975,Naño-1)
HWAño<-tIC95Año*PrAño.Std/sqrt(Naño)
Años<-1995:2016
DataAño<-data.frame(Naño,PrAño,HWAño)

tendencia <- ggplot(DataAño, aes(x=Años, y=PrAño)) + geom_point(shape=19,color="black") + xlab("Year") + ylab("Price (COP/kWh)")
tendencia + geom_smooth() + theme_bw() + scale_x_continuous(breaks = seq(1995,2016,5))

smoothAño <-predict(interpSpline(Años, PrAño))
plot(Años, PrAño)
lines(smoothAño)

#FChow
PrAño=ts(PrAño,start = 1995)
tA=time(PrAño)
fs <- Fstats(PrAño~tA)
plot(fs)
sctest(fs)
plot(Años,PrAño)
lines(breakpoints(fs))
lfs <- Fstats(log(PrAño)~tA)
plot(lfs)
sctest(lfs)
plot(Años,log(PrAño))
lines(breakpoints(lfs))

#ENSO
PrMA<-as.vector(as.matrix(tapply(Prices,list(Mes,Año),mean,na.rm=TRUE)))
PrMA=PrMA[!is.na(PrMA)]
PrMA=ts(PrMA,start = c(1995,7),frequency = 12)
ONIw=window(ONI,start=c(1995,7))
ONIwf=ONIw[length(ONIw)]
ONIw=c(ONIw,rep(ONIwf,3))
tONI=time(ONIw)
dataONI=data.frame(tONI,PrMA,ONIw)
ggplot(aes(x = tONI, y = PrMA, color = ONIw), data = dataONI) + geom_point(size = 2) +
    scale_color_gradient2("ONI",low = "blue", mid = "black", high = "red") + theme_bw() +
    xlab("Date") + ylab("Price (COP/kWh)") + scale_x_continuous(breaks = 1995:2016) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(aes(x = tONI, y = PrMA, color = ONIw), data = dataONI) + geom_point(size = 2) +
    scale_color_gradient2("ONI",low = "grey99", mid = "grey80", high = "black") +geom_point(shape = 1,size = 2,colour = "black")+ theme_bw() +
    xlab("Date") + ylab("Price (COP/kWh)") + scale_x_continuous(breaks = 1995:2016) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# nn=Naño[2:(length(Naño)-1)]
# time=seq(from=8760-3960,to = 8760-1,by = 1)/8760
# for(n in seq_along(nn)){
#     time=c(time,(seq(from=0,to =nn[n]-1,by = 1))/nn[n] + n)
# }
# time=c(time,(seq(from=0,to =5448-1,by = 1))/8784 + 21)

lPrices=log(Prices)
lPrices.ts=ts(Prices,frequency = 24)
d=as.numeric(y>16)
ONIh=rep(ONIw,nHorasMA)
ENSO=ONIh>0.5

Datos=data.frame(t,Prices,lPrices,Mes,FDSF,y,d)
Datos$Prices=msts(Datos$Prices,seasonal.periods = c(24,8766))
Datos$lPrices=msts(Datos$lPrices,seasonal.periods = c(24,8766))
Datos=cbind(Datos,fourier(Datos$Prices,K = c(7,5)),fourier(Datos$lPrices,K = c(7,5)),as.numeric(Mes=="febrero"),as.numeric(Mes=="marzo"),as.numeric(Mes=="abril"),as.numeric(Mes=="mayo"),as.numeric(Mes=="junio"),as.numeric(Mes=="julio"),as.numeric(Mes=="agosto"),as.numeric(Mes=="septiembre"),as.numeric(Mes=="octubre"),as.numeric(Mes=="noviembre"),as.numeric(Mes=="diciembre"))
names(Datos)[8:dim(Datos)[2]]=c("S1_24","C1_24","S2_24","C2_24","S3_24","C3_24","S4_24","C4_24","S5_24","C5_24","S6_24","C6_24","S7_24","C7_24","S1_8766","C1_8766","S2_8766","C2_8766","S3_8766","C3_8766","S4_8766","C4_8766","S5_8766","C5_8766",
                      "lS1_24","lC1_24","lS2_24","lC2_24","lS3_24","lC3_24","lS4_24","lC4_24","lS5_24","lC5_24","lS6_24","lC6_24","lS7_24","lC7_24","lS1_8766","lC1_8766","lS2_8766","lC2_8766","lS3_8766","lC3_8766","lS4_8766","lC4_8766","lS5_8766","lC5_8766",
                     "febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")

DatosO=Datos[(t>=as.POSIXct("1997-01-01 00:00",tz = "GMT"))&(t<as.POSIXct("2014-01-01 00:00",tz = "GMT")),]
Datos.lag=Datos[(t>=as.POSIXct("1996-12-31 23:00",tz = "GMT"))&(t<as.POSIXct("2013-12-31 23:00",tz = "GMT")),]
names(Datos.lag)=c("t.lag","Prices.lag","lPrices.lag","Mes.lag","FDSF.lag","y.lag","d.lag",
                   "S1_24.lag","C1_24.lag","S2_24.lag","C2_24.lag","S3_24.lag","C3_24.lag","S4_24.lag","C4_24.lag","S5_24.lag","C5_24.lag","S6_24.lag","C6_24.lag","S7_24.lag","C7_24.lag","S1_8766.lag","C1_8766.lag","S2_8766.lag","C2_8766.lag","S3_8766.lag","C3_8766.lag","S4_8766.lag","C4_8766.lag","S5_8766.lag","C5_8766.lag",
                   "lS1_24.lag","lC1_24.lag","lS2_24.lag","lC2_24.lag","lS3_24.lag","lC3_24.lag","lS4_24.lag","lC4_24.lag","lS5_24.lag","lC5_24.lag","lS6_24.lag","lC6_24.lag","lS7_24.lag","lC7_24.lag","lS1_8766.lag","lC1_8766.lag","lS2_8766.lag","lC2_8766.lag","lS3_8766.lag","lC3_8766.lag","lS4_8766.lag","lC4_8766.lag","lS5_8766.lag","lC5_8766.lag",
                   "febrero.lag","marzo.lag","abril.lag","mayo.lag","junio.lag","julio.lag","agosto.lag","septiembre.lag","octubre.lag","noviembre.lag","diciembre.lag")

DATOS=cbind(DatosO,Datos.lag)

testO=Datos[(t>=as.POSIXct("2014-01-01 00:00",tz = "GMT"))&(t<as.POSIXct("2016-08-15 00:00",tz = "GMT")),]
test.lag=Datos[(t>=as.POSIXct("2013-12-31 00:00",tz = "GMT"))&(t<as.POSIXct("2016-08-14 00:00",tz = "GMT")),]
names(test.lag)=c("t.lag","Prices.lag","lPrices.lag","Mes.lag","FDSF.lag","y.lag","d.lag",
                  "S1_24.lag","C1_24.lag","S2_24.lag","C2_24.lag","S3_24.lag","C3_24.lag","S4_24.lag","C4_24.lag","S5_24.lag","C5_24.lag","S6_24.lag","C6_24.lag","S7_24.lag","C7_24.lag","S1_8766.lag","C1_8766.lag","S2_8766.lag","C2_8766.lag","S3_8766.lag","C3_8766.lag","S4_8766.lag","C4_8766.lag","S5_8766.lag","C5_8766.lag",
                  "lS1_24.lag","lC1_24.lag","lS2_24.lag","lC2_24.lag","lS3_24.lag","lC3_24.lag","lS4_24.lag","lC4_24.lag","lS5_24.lag","lC5_24.lag","lS6_24.lag","lC6_24.lag","lS7_24.lag","lC7_24.lag","lS1_8766.lag","lC1_8766.lag","lS2_8766.lag","lC2_8766.lag","lS3_8766.lag","lC3_8766.lag","lS4_8766.lag","lC4_8766.lag","lS5_8766.lag","lC5_8766.lag",
                  "febrero.lag","marzo.lag","abril.lag","mayo.lag","junio.lag","julio.lag","agosto.lag","septiembre.lag","octubre.lag","noviembre.lag","diciembre.lag")

TEST=cbind(testO,test.lag)

#Modelo 1
fit1<-lm(Prices~y+`S1_24`+`C1_24`+`S2_24`+`C2_24`+`S3_24`+`C3_24`+`S4_24`+`C4_24`+`S5_24`+`C5_24`+`S6_24`+`C6_24`+`S7_24`+`C7_24`+
             `febrero`+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre+FDSF, data = DATOS);summary(fit1)
plot(DATOS$t,DATOS$Prices,type="l")
lines(DATOS$t,fitted(fit1),col="red")
betas1=coef(fit1)

res1=resid(fit1)
resfit1=lm(I(res1[-length(res1)]-res1[-1])~res1[-1])
kappa1.lm=-log(1+coef(resfit1)[1])

ffit1<-nls(formula = Prices~phi*Prices.lag+b0+
               b1*y+gs1*`S1_24`+gc1*`C1_24`+gs2*`S2_24`+gc2*`C2_24`+gs3*`S3_24`+gc3*`C3_24`+gs4*`S4_24`+gc4*`C4_24`+gs5*`S5_24`+gc5*`C5_24`+gs6*`S6_24`+gc6*`C6_24`+gs7*`S7_24`+gc7*`C7_24`+
               m2*`febrero`+m3*`marzo`+m4*`abril`+m5*`mayo`+m6*`junio`+m7*`julio`+m8*`agosto`+m9*`septiembre`+m10*`octubre`+m11*`noviembre`+m12*`diciembre`+
               d1*as.numeric(FDSF)-phi*(b0+b1*y.lag+gs1*`S1_24.lag`+gc1*`C1_24.lag`+gs2*`S2_24.lag`+gc2*`C2_24.lag`+gs3*`S3_24.lag`+gc3*`C3_24.lag`+gs4*`S4_24.lag`+gc4*`C4_24.lag`+gs5*`S5_24.lag`+gc5*`C5_24.lag`+gs6*`S6_24.lag`+gc6*`C6_24.lag`+gs7*`S7_24.lag`+gc7*`C7_24.lag`+
                                            m2*`febrero.lag`+m3*`marzo.lag`+m4*`abril.lag`+m5*`mayo.lag`+m6*`junio.lag`+m7*`julio.lag`+m8*`agosto.lag`+m9*`septiembre.lag`+m10*`octubre.lag`+m11*`noviembre.lag`+m12*`diciembre.lag`+
                                            d1*as.numeric(FDSF.lag)),data = DATOS,
           # start = list(phi=0.9,b0=betas3[1],b1=betas3[2],gs1=betas3[3],gc1=betas3[4],gs2=betas3[5],gc2=betas3[6],gs3=betas3[7],gc3=betas3[8],gs4=betas3[9],gc4=betas3[10],gs5=betas3[11],gc5=betas3[12],gs6=betas3[13],gc6=betas3[14],gs7=betas3[15],gc7=betas3[16],
           #              m2=betas3[17],m3=betas3[18],m4=betas3[19],m5=betas3[20],m6=betas3[21],m7=betas3[22],m8=betas3[23],m9=betas3[24],m10=betas3[25],m11=betas3[26],m12=betas3[27],d1=betas4[28]))
           start = list(phi=0.9,b0=betas1[1],b1=betas1[2],gs1=betas1[3],gc1=betas1[4],gs2=betas1[5],gc2=betas1[6],gs3=betas1[7],gc3=betas1[8],gs4=betas1[9],gc4=betas1[10],gs5=betas1[11],gc5=betas1[12],gs6=betas1[13],gc6=betas1[14],gs7=betas1[15],gc7=betas1[16],
                        m2=0,m3=0,m4=0,m5=0,m6=0,m7=0,m8=0,m9=0,m10=0,m11=0,m12=0,d1=0.5))

e=resid(ffit1)

plot(Datos$t,Datos$Prices,type="l")
lines(DATOS$t,fitted(ffit1),col="red")
lines(TEST$t,predict(ffit1,newdata = TEST),col="blue")
summary(ffit1)

sigmae1=sqrt(deviance(ffit1)/df.residual(ffit1))

bbetas1=coef(ffit1)
phi=bbetas1[1];b0=bbetas1[2];b1=bbetas1[3];gs1=bbetas1[4];gc1=bbetas1[5];gs2=bbetas1[6];gc2=bbetas1[7];gs3=bbetas1[8];gc3=bbetas1[9];gs4=bbetas1[10];gc4=bbetas1[11];gs5=bbetas1[12];gc5=bbetas1[13];gs6=bbetas1[14];gc6=bbetas1[15];gs7=bbetas1[16];gc7=bbetas1[17];
m2=bbetas1[18];m3=bbetas1[19];m4=bbetas1[20];m5=bbetas1[21];m6=bbetas1[22];m7=bbetas1[23];m8=bbetas1[24];m9=bbetas1[25];m10=bbetas1[26];m11=bbetas1[27];m12=bbetas1[28];d1=bbetas1[29]


f1=b0+b1*TEST$y+gs1*TEST$`S1_24`+gc1*TEST$`C1_24`+gs2*TEST$`S2_24`+gc2*TEST$`C2_24`+gs3*TEST$`S3_24`+gc3*TEST$`C3_24`+gs4*TEST$`S4_24`+gc4*TEST$`C4_24`+gs5*TEST$`S5_24`+gc5*TEST$`C5_24`+gs6*TEST$`S6_24`+gc6*TEST$`C6_24`+gs7*TEST$`S7_24`+gc7*TEST$`C7_24`+
    m2*TEST$febrero+m3*TEST$marzo+m4*TEST$abril+m5*TEST$mayo+m6*TEST$junio+m7*TEST$julio+m8*TEST$agosto+m9*TEST$septiembre+m10*TEST$octubre+m11*TEST$noviembre+m12*TEST$diciembre+
    d1*as.numeric(TEST$FDSF)
x.lag1=DATOS$Prices[length(DATOS$t)]-fitted(ffit1)[length(DATOS$t)]
x1=phi*x.lag1+0*rnorm(1,sd = sigmae1)#exp(-(1-phi))*x.lag4+*rnorm(1,sd = sigmae4)

for(i in seq(2,length(TEST$t))){
    x1[i]=phi*x1[i-1]+0*rnorm(1,sd = sigmae1)#exp(-(1-phi))*x4[i-1]+rnorm(1,sd = sigmae4)
}
Pred1=f1+x1

plot(Datos$t,Datos$Prices,type="l",xlim = c(as.POSIXct("2014-01-01 00:00",tz = "GMT"),as.POSIXct("2016-01-01 23:00",tz = "GMT")),ylim = c(0,500))
lines(DATOS$t,fitted(ffit1),col="red")
lines(TEST$t,Pred1,col="blue")
#lines(TEST$t,exp(predict(ffit1,newdata = TEST)),col="green")
plot(x1)

mse1=mse(actual = TEST$Prices,predicted = Pred1)


#Modelo 2
fit2<-lm(Prices~y+`S1_24`+`C1_24`+`S2_24`+`C2_24`+`S3_24`+`C3_24`+`S4_24`+`C4_24`+`S5_24`+`C5_24`+`S6_24`+`C6_24`+`S7_24`+`C7_24`+
             `S1_8766`+`C1_8766`+`S2_8766`+`C2_8766`+`S3_8766`+`C3_8766`+`S4_8766`+`C4_8766`+`S5_8766`+`C5_8766`+
             +FDSF, data = DATOS);summary(fit2)
plot(DATOS$t,DATOS$Prices,type="l")
lines(DATOS$t,fitted(fit2),col="red")
betas2=coef(fit2)

res2=resid(fit2)
resfit2=lm(I(res2[-length(res2)]-res2[-1])~res2[-1])
kappa2.lm=-log(1+coef(resfit2)[1])

ffit2<-nls(formula = Prices~phi*Prices.lag+b0+
               b1*y+gs1*`S1_24`+gc1*`C1_24`+gs2*`S2_24`+gc2*`C2_24`+gs3*`S3_24`+gc3*`C3_24`+gs4*`S4_24`+gc4*`C4_24`+gs5*`S5_24`+gc5*`C5_24`+gs6*`S6_24`+gc6*`C6_24`+gs7*`S7_24`+gc7*`C7_24`+
               fs1*`S1_8766`+fc1*`C1_8766`+fs2*`S2_8766`+fc2*`C2_8766`+fs3*`S3_8766`+fc3*`C3_8766`+fs4*`S4_8766`+fc4*`C4_8766`+fs5*`S5_8766`+fc5*`C5_8766`+
               d1*as.numeric(FDSF)-phi*(b0+b1*y.lag+gs1*`S1_24.lag`+gc1*`C1_24.lag`+gs2*`S2_24.lag`+gc2*`C2_24.lag`+gs3*`S3_24.lag`+gc3*`C3_24.lag`+gs4*`S4_24.lag`+gc4*`C4_24.lag`+gs5*`S5_24.lag`+gc5*`C5_24.lag`+gs6*`S6_24.lag`+gc6*`C6_24.lag`+gs7*`S7_24.lag`+gc7*`C7_24.lag`+
                                            fs1*`S1_8766.lag`+fc1*`C1_8766.lag`+fs2*`S2_8766.lag`+fc2*`C2_8766.lag`+fs3*`S3_8766.lag`+fc3*`C3_8766.lag`+fs4*`S4_8766.lag`+fc4*`C4_8766.lag`+fs5*`S5_8766.lag`+fc5*`C5_8766.lag`+
                                            d1*as.numeric(FDSF.lag)),data = DATOS,
           start = list(phi=0.5,b0=betas2[1],b1=betas2[2],gs1=betas2[3],gc1=betas2[4],gs2=betas2[5],gc2=betas2[6],gs3=betas2[7],gc3=betas2[8],gs4=betas2[9],gc4=betas2[10],gs5=betas2[11],gc5=betas2[12],gs6=betas2[13],gc6=betas2[14],gs7=betas2[15],gc7=betas2[16],
                        fs1=betas2[17],fc1=betas2[18],fs2=betas2[19],fc2=betas2[20],fs3=betas2[21],fc3=betas2[22],fs4=betas2[23],fc4=betas2[24],fs5=betas2[25],fc5=betas2[26],d1=betas2[27]))
           # start = list(phi=0.5,b0=betas1[1],b1=betas1[2],gs1=betas1[3],gc1=betas1[4],gs2=betas1[5],gc2=betas1[6],gs3=betas1[7],gc3=betas1[8],gs4=betas1[9],gc4=betas1[10],gs5=betas1[11],gc5=betas1[12],gs6=betas1[13],gc6=betas1[14],gs7=betas1[15],gc7=betas1[16],
           #              fs1=betas1[17],fc1=betas1[18],fs2=betas1[19],fc2=betas1[20],fs3=betas1[21],fc3=betas1[22],fs4=betas1[23],fc4=betas1[24],fs5=betas1[25],fc5=betas1[26],d1=betas1[27]))

e=resid(ffit2)

plot(Datos$t,Datos$Prices,type="l")
lines(DATOS$t,fitted(ffit2),col="red")
lines(TEST$t,predict(ffit2,newdata = TEST),col="blue")
summary(ffit2)

sigmae2=sqrt(deviance(ffit2)/df.residual(ffit2))

bbetas2=coef(ffit2)
phi=bbetas2[1];b0=bbetas2[2];b1=bbetas2[3];gs1=bbetas2[4];gc1=bbetas2[5];gs2=bbetas2[6];gc2=bbetas2[7];gs3=bbetas2[8];gc3=bbetas2[9];gs4=bbetas2[10];gc4=bbetas2[11];gs5=bbetas2[12];gc5=bbetas2[13];gs6=bbetas2[14];gc6=bbetas2[15];gs7=bbetas2[16];gc7=bbetas2[17];
fs1=bbetas2[18];fc1=bbetas2[19];fs2=bbetas2[20];fc2=bbetas2[21];fs3=bbetas2[22];fc3=bbetas2[23];fs4=bbetas2[24];fc4=bbetas2[25];fs5=bbetas2[26];fc5=bbetas2[27];d1=bbetas2[28]


f2=b0+b1*TEST$y+gs1*TEST$`S1_24`+gc1*TEST$`C1_24`+gs2*TEST$`S2_24`+gc2*TEST$`C2_24`+gs3*TEST$`S3_24`+gc3*TEST$`C3_24`+gs4*TEST$`S4_24`+gc4*TEST$`C4_24`+gs5*TEST$`S5_24`+gc5*TEST$`C5_24`+gs6*TEST$`S6_24`+gc6*TEST$`C6_24`+gs7*TEST$`S7_24`+gc7*TEST$`C7_24`+
    fs1*TEST$`S1_8766`+fc1*TEST$`C1_8766`+fs2*TEST$`S2_8766`+fc2*TEST$`C2_8766`+fs3*TEST$`S3_8766`+fc3*TEST$`C3_8766`+fs4*TEST$`S4_8766`+fc4*TEST$`C4_8766`+fs5*TEST$`S5_8766`+fc5*TEST$`C5_8766`+
    d1*as.numeric(TEST$FDSF)
x.lag2=DATOS$Prices[length(DATOS$t)]-fitted(ffit2)[length(DATOS$t)]
x2=exp(-(1-phi))*x.lag2+rnorm(1,sd = sigmae2)

for(i in seq(2,length(TEST$t))){
    x2[i]=phi*x2[i-1]+0*rnorm(1,sd = sigmae2)#exp(-(1-phi))*x4[i-1]+rnorm(1,sd = sigmae4)
}
Pred2=f2+x2

plot(Datos$t,Datos$Prices,type="l",xlim = c(as.POSIXct("2014-01-01 00:00",tz = "GMT"),as.POSIXct("2016-01-01 23:00",tz = "GMT")),ylim = c(0,500))
lines(DATOS$t,fitted(ffit2),col="red")
lines(TEST$t,Pred2,col="blue")
#lines(TEST$t,exp(predict(ffit4,newdata = TEST)),col="green")
plot(x2)

mse2=mse(actual = TEST$Prices,predicted = Pred2)


#Modelo 3
fit3<-lm(lPrices~y+`lS1_24`+`lC1_24`+`lS2_24`+`lC2_24`+`lS3_24`+`lC3_24`+`lS4_24`+`lC4_24`+`lS5_24`+`lC5_24`+`lS6_24`+`lC6_24`+`lS7_24`+`lC7_24`+
             `febrero`+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre+FDSF, data = DATOS);summary(fit3)
plot(DATOS$t,DATOS$lPrices,type="l")
lines(DATOS$t,fitted(fit3),col="red")
betas3=coef(fit3)

res3=resid(fit3)
resfit3=lm(I(res3[-length(res3)]-res3[-1])~res3[-1])
kappa3.lm=-log(1+coef(resfit3)[1])

ffit3<-nls(formula = lPrices~phi*lPrices.lag+b0+
               b1*y+gs1*`lS1_24`+gc1*`lC1_24`+gs2*`lS2_24`+gc2*`lC2_24`+gs3*`lS3_24`+gc3*`lC3_24`+gs4*`lS4_24`+gc4*`lC4_24`+gs5*`lS5_24`+gc5*`lC5_24`+gs6*`lS6_24`+gc6*`lC6_24`+gs7*`lS7_24`+gc7*`lC7_24`+
               m2*`febrero`+m3*`marzo`+m4*`abril`+m5*`mayo`+m6*`junio`+m7*`julio`+m8*`agosto`+m9*`septiembre`+m10*`octubre`+m11*`noviembre`+m12*`diciembre`+
               d1*as.numeric(FDSF)-phi*(b0+b1*y.lag+gs1*`lS1_24.lag`+gc1*`lC1_24.lag`+gs2*`lS2_24.lag`+gc2*`lC2_24.lag`+gs3*`lS3_24.lag`+gc3*`lC3_24.lag`+gs4*`lS4_24.lag`+gc4*`lC4_24.lag`+gs5*`lS5_24.lag`+gc5*`lC5_24.lag`+gs6*`lS6_24.lag`+gc6*`lC6_24.lag`+gs7*`lS7_24.lag`+gc7*`lC7_24.lag`+
                                            m2*`febrero.lag`+m3*`marzo.lag`+m4*`abril.lag`+m5*`mayo.lag`+m6*`junio.lag`+m7*`julio.lag`+m8*`agosto.lag`+m9*`septiembre.lag`+m10*`octubre.lag`+m11*`noviembre.lag`+m12*`diciembre.lag`+
                                            d1*as.numeric(FDSF.lag)),data = DATOS,
           # start = list(phi=0.9,b0=betas3[1],b1=betas3[2],gs1=betas3[3],gc1=betas3[4],gs2=betas3[5],gc2=betas3[6],gs3=betas3[7],gc3=betas3[8],gs4=betas3[9],gc4=betas3[10],gs5=betas3[11],gc5=betas3[12],gs6=betas3[13],gc6=betas3[14],gs7=betas3[15],gc7=betas3[16],
           #              m2=betas3[17],m3=betas3[18],m4=betas3[19],m5=betas3[20],m6=betas3[21],m7=betas3[22],m8=betas3[23],m9=betas3[24],m10=betas3[25],m11=betas3[26],m12=betas3[27],d1=betas4[28]))
           start = list(phi=0.9,b0=betas3[1],b1=betas3[2],gs1=betas3[3],gc1=betas3[4],gs2=betas3[5],gc2=betas3[6],gs3=betas3[7],gc3=betas3[8],gs4=betas3[9],gc4=betas3[10],gs5=betas3[11],gc5=betas3[12],gs6=betas3[13],gc6=betas3[14],gs7=betas3[15],gc7=betas3[16],
                        m2=0,m3=0,m4=0,m5=0,m6=0,m7=0,m8=0,m9=0,m10=0,m11=0,m12=0,d1=0.5))

e=resid(ffit3)

plot(Datos$t,Datos$lPrices,type="l")
lines(DATOS$t,fitted(ffit3),col="red")
lines(TEST$t,predict(ffit3,newdata = TEST),col="blue")
summary(ffit3)

sigmae3=sqrt(deviance(ffit3)/df.residual(ffit3))

bbetas3=coef(ffit3)
phi=bbetas3[1];b0=bbetas3[2];b1=bbetas3[3];gs1=bbetas3[4];gc1=bbetas3[5];gs2=bbetas3[6];gc2=bbetas3[7];gs3=bbetas3[8];gc3=bbetas3[9];gs4=bbetas3[10];gc4=bbetas3[11];gs5=bbetas3[12];gc5=bbetas3[13];gs6=bbetas3[14];gc6=bbetas3[15];gs7=bbetas3[16];gc7=bbetas3[17];
m2=bbetas3[18];m3=bbetas3[19];m4=bbetas3[20];m5=bbetas3[21];m6=bbetas3[22];m7=bbetas3[23];m8=bbetas3[24];m9=bbetas3[25];m10=bbetas3[26];m11=bbetas3[27];m12=bbetas3[28];d1=bbetas3[29]


f3=b0+b1*TEST$y+gs1*TEST$`lS1_24`+gc1*TEST$`lC1_24`+gs2*TEST$`lS2_24`+gc2*TEST$`lC2_24`+gs3*TEST$`lS3_24`+gc3*TEST$`lC3_24`+gs4*TEST$`lS4_24`+gc4*TEST$`lC4_24`+gs5*TEST$`lS5_24`+gc5*TEST$`lC5_24`+gs6*TEST$`lS6_24`+gc6*TEST$`lC6_24`+gs7*TEST$`lS7_24`+gc7*TEST$`lC7_24`+
    m2*TEST$febrero+m3*TEST$marzo+m4*TEST$abril+m5*TEST$mayo+m6*TEST$junio+m7*TEST$julio+m8*TEST$agosto+m9*TEST$septiembre+m10*TEST$octubre+m11*TEST$noviembre+m12*TEST$diciembre+
    d1*as.numeric(TEST$FDSF)
x.lag3=DATOS$lPrices[length(DATOS$t)]-fitted(ffit3)[length(DATOS$t)]
x3=phi*x.lag3+0*rnorm(1,sd = sigmae3)#exp(-(1-phi))*x.lag4+*rnorm(1,sd = sigmae4)

for(i in seq(2,length(TEST$t))){
    x3[i]=phi*x3[i-1]+0*rnorm(1,sd = sigmae3)#exp(-(1-phi))*x4[i-1]+rnorm(1,sd = sigmae4)
}
lPred3=f3+x3

plot(Datos$t,exp(Datos$lPrices),type="l",xlim = c(as.POSIXct("2014-01-01 00:00",tz = "GMT"),as.POSIXct("2016-01-01 23:00",tz = "GMT")),ylim = c(0,500))
lines(DATOS$t,exp(fitted(ffit3)),col="red")
lines(TEST$t,exp(lPred3),col="blue")
#lines(TEST$t,exp(predict(ffit3,newdata = TEST)),col="green")
plot(x3)

mse3=mse(actual = TEST$Prices,predicted = exp(lPred3))

#Modelo 4
fit4<-lm(lPrices~y+`lS1_24`+`lC1_24`+`lS2_24`+`lC2_24`+`lS3_24`+`lC3_24`+`lS4_24`+`lC4_24`+`lS5_24`+`lC5_24`+`lS6_24`+`lC6_24`+`lS7_24`+`lC7_24`+
             `lS1_8766`+`lC1_8766`+`lS2_8766`+`lC2_8766`+`lS3_8766`+`lC3_8766`+`lS4_8766`+`lC4_8766`+`lS5_8766`+`lC5_8766`+
             +FDSF, data = DATOS);summary(fit4)
plot(DATOS$t,DATOS$lPrices,type="l")
lines(DATOS$t,fitted(fit4),col="red")
betas4=coef(fit4)

res4=resid(fit4)
resfit4=lm(I(res4[-length(res4)]-res4[-1])~res4[-1])
kappa4.lm=-log(1+coef(resfit4)[1])

write.table(x = res4,file = "res4.dat")

ffit4<-nls(formula = lPrices~phi*lPrices.lag+b0+
        b1*y+gs1*`lS1_24`+gc1*`lC1_24`+gs2*`lS2_24`+gc2*`lC2_24`+gs3*`lS3_24`+gc3*`lC3_24`+gs4*`lS4_24`+gc4*`lC4_24`+gs5*`lS5_24`+gc5*`lC5_24`+gs6*`lS6_24`+gc6*`lC6_24`+gs7*`lS7_24`+gc7*`lC7_24`+
        fs1*`lS1_8766`+fc1*`lC1_8766`+fs2*`lS2_8766`+fc2*`lC2_8766`+fs3*`lS3_8766`+fc3*`lC3_8766`+fs4*`lS4_8766`+fc4*`lC4_8766`+fs5*`lS5_8766`+fc5*`lC5_8766`+
        d1*as.numeric(FDSF)-phi*(b0+b1*y.lag+gs1*`lS1_24.lag`+gc1*`lC1_24.lag`+gs2*`lS2_24.lag`+gc2*`lC2_24.lag`+gs3*`lS3_24.lag`+gc3*`lC3_24.lag`+gs4*`lS4_24.lag`+gc4*`lC4_24.lag`+gs5*`lS5_24.lag`+gc5*`lC5_24.lag`+gs6*`lS6_24.lag`+gc6*`lC6_24.lag`+gs7*`lS7_24.lag`+gc7*`lC7_24.lag`+
                               fs1*`lS1_8766.lag`+fc1*`lC1_8766.lag`+fs2*`lS2_8766.lag`+fc2*`lC2_8766.lag`+fs3*`lS3_8766.lag`+fc3*`lC3_8766.lag`+fs4*`lS4_8766.lag`+fc4*`lC4_8766.lag`+fs5*`lS5_8766.lag`+fc5*`lC5_8766.lag`+
                               d1*as.numeric(FDSF.lag)),data = DATOS,
    start = list(phi=0.5,b0=betas4[1],b1=betas4[2],gs1=betas4[3],gc1=betas4[4],gs2=betas4[5],gc2=betas4[6],gs3=betas4[7],gc3=betas4[8],gs4=betas4[9],gc4=betas4[10],gs5=betas4[11],gc5=betas4[12],gs6=betas4[13],gc6=betas4[14],gs7=betas4[15],gc7=betas4[16],
                 fs1=betas4[17],fc1=betas4[18],fs2=betas4[19],fc2=betas4[20],fs3=betas4[21],fc3=betas4[22],fs4=betas4[23],fc4=betas4[24],fs5=betas4[25],fc5=betas4[26],d1=betas4[27]))

e=resid(ffit4)

plot(Datos$t,Datos$lPrices,type="l")
lines(DATOS$t,fitted(ffit4),col="red")
lines(TEST$t,predict(ffit4,newdata = TEST),col="blue")
summary(ffit4)

sigmae4=sqrt(deviance(ffit4)/df.residual(ffit4))

bbetas4=coef(ffit4)
phi=bbetas4[1];b0=bbetas4[2];b1=bbetas4[3];gs1=bbetas4[4];gc1=bbetas4[5];gs2=bbetas4[6];gc2=bbetas4[7];gs3=bbetas4[8];gc3=bbetas4[9];gs4=bbetas4[10];gc4=bbetas4[11];gs5=bbetas4[12];gc5=bbetas4[13];gs6=bbetas4[14];gc6=bbetas4[15];gs7=bbetas4[16];gc7=bbetas4[17];
fs1=bbetas4[18];fc1=bbetas4[19];fs2=bbetas4[20];fc2=bbetas4[21];fs3=bbetas4[22];fc3=bbetas4[23];fs4=bbetas4[24];fc4=bbetas4[25];fs5=bbetas4[26];fc5=bbetas4[27];d1=bbetas4[28]


f4=b0+b1*TEST$y+gs1*TEST$`lS1_24`+gc1*TEST$`lC1_24`+gs2*TEST$`lS2_24`+gc2*TEST$`lC2_24`+gs3*TEST$`lS3_24`+gc3*TEST$`lC3_24`+gs4*TEST$`lS4_24`+gc4*TEST$`lC4_24`+gs5*TEST$`lS5_24`+gc5*TEST$`lC5_24`+gs6*TEST$`lS6_24`+gc6*TEST$`lC6_24`+gs7*TEST$`lS7_24`+gc7*TEST$`lC7_24`+
    fs1*TEST$`lS1_8766`+fc1*TEST$`lC1_8766`+fs2*TEST$`lS2_8766`+fc2*TEST$`lC2_8766`+fs3*TEST$`lS3_8766`+fc3*TEST$`lC3_8766`+fs4*TEST$`lS4_8766`+fc4*TEST$`lC4_8766`+fs5*TEST$`lS5_8766`+fc5*TEST$`lC5_8766`+
    d1*as.numeric(TEST$FDSF)
x.lag4=DATOS$lPrices[length(DATOS$t)]-fitted(ffit4)[length(DATOS$t)]
x4=exp(-(1-phi))*x.lag4+rnorm(1,sd = sigmae4)

for(i in seq(2,length(TEST$t))){
    x4[i]=phi*x4[i-1]+0*rnorm(1,sd = sigmae4)#exp(-(1-phi))*x4[i-1]+rnorm(1,sd = sigmae4)
}
lPred4=f4+x4

plot(Datos$t,exp(Datos$lPrices),type="l",xlim = c(as.POSIXct("2014-01-01 00:00",tz = "GMT"),as.POSIXct("2016-01-01 23:00",tz = "GMT")),ylim = c(0,500))
lines(DATOS$t,exp(fitted(ffit4)),col="red")
lines(TEST$t,exp(lPred4),col="blue")
#lines(TEST$t,exp(predict(ffit4,newdata = TEST)),col="green")
plot(x4)

mse4=mse(actual = TEST$Prices,predicted = exp(lPred4))

# ##MLE
# 
# #res4 sería x para la estimación
# 
# Pt=res4[2:length(res4)]
# Pt_1=res4[1:(length(res4)-1)]
# F4=function(phi, mu_J,sigmaSq, sigmaSq_J, lambda){
#     R=lambda*exp((-(Pt-phi*Pt_1-mu_J)^2)/(2*(sigmaSq+sigmaSq_J)))*(1/sqrt(2*pi*(sigmaSq+sigmaSq_J)))+(1-lambda)*exp((-(Pt-phi*Pt_1)^2)/(2*sigmaSq))*(1/sqrt(2*pi*sigmaSq))
#     -sum(log(R))
#     }
# # 
# mleffit4<-mle(minuslogl = F4,method = "L-BFGS-B",start = list(phi=0.9, mu_J=0.2, sigmaSq=5, sigmaSq_J=5,lambda=0.5),lower = c(0,-30,1,1,0),upper = c(1,30,50,50,1))
# 
# 
# Pt=DATOS$lPrices
# Pt_1=DATOS$lPrices.lag
# betas4=as.numeric(betas4)
# st=list(phi=0.5,b0=betas4[1],b1=betas4[2],gs1=betas4[3],gc1=betas4[4],gs2=betas4[5],gc2=betas4[6],gs3=betas4[7],gc3=betas4[8],gs4=betas4[9],gc4=betas4[10],gs5=betas4[11],gc5=betas4[12],gs6=betas4[13],gc6=betas4[14],gs7=betas4[15],gc7=betas4[16],
#         fs1=betas4[17],fc1=betas4[18],fs2=betas4[19],fc2=betas4[20],fs3=betas4[21],fc3=betas4[22],fs4=betas4[23],fc4=betas4[24],fs5=betas4[25],fc5=betas4[26],d1=betas4[27],
#         mu_J=0.2,sigmaSq=var(Pt),sigmaSq_J=var(Pt),lambda=0.5)
# 
# F4=function(phi,b0,b1,gs1,gc1,gs2,gc2,gs3,gc3,gs4,gc4,gs5,gc5,gs6,gc6,gs7,gc7,fs1,fc1,fs2,fc2,fs3,fc3,fs4,fc4,fs5,fc5,d1,mu_J,sigmaSq, sigmaSq_J, lambda){
#     f=b0+b1*DATOS$y+gs1*DATOS$`lS1_24`+gc1*DATOS$`lC1_24`+gs2*DATOS$`lS2_24`+gc2*DATOS$`lC2_24`+gs3*DATOS$`lS3_24`+gc3*DATOS$`lC3_24`+gs4*DATOS$`lS4_24`+gc4*DATOS$`lC4_24`+gs5*DATOS$`lS5_24`+gc5*DATOS$`lC5_24`+gs6*DATOS$`lS6_24`+gc6*DATOS$`lC6_24`+gs7*DATOS$`lS7_24`+gc7*DATOS$`lC7_24`+
#         fs1*DATOS$`lS1_8766`+fc1*DATOS$`lC1_8766`+fs2*DATOS$`lS2_8766`+fc2*DATOS$`lC2_8766`+fs3*DATOS$`lS3_8766`+fc3*DATOS$`lC3_8766`+fs4*DATOS$`lS4_8766`+fc4*DATOS$`lC4_8766`+fs5*DATOS$`lS5_8766`+fc5*DATOS$`lC5_8766`+
#         d1*as.numeric(DATOS$FDSF)
#     R=lambda*exp((-(Pt-f-phi*Pt_1-mu_J)^2)/(2*(sigmaSq+sigmaSq_J)))*(1/sqrt(2*pi*(sigmaSq+sigmaSq_J)))+(1-lambda)*exp((-(Pt-f-phi*Pt_1)^2)/(2*sigmaSq))*(1/sqrt(2*pi*sigmaSq))
#     -sum(log(R))
# }
# # 
# mleffit4<-mle(minuslogl = F4,method = "L-BFGS-B",start = st,lower = c(0,-30,1,1,0),upper = c(1,30,50,50,1))
# optim(par = st,fn = F4,method = "L-BFGS-B",hessian = FALSE)
