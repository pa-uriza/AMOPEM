# install.packages("forecast")
# install.packages("xlsx")
# install.packages("chron")
# install.packages("stats")
# install.packages("xts")
# install.packages("ggplot2")
# install.packages("strucchange")
# install.packages("astsa")
# installed.packages("metrics")
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

# Directorio PC personal
setwd("C:/Users/USUARIO/Dropbox/Uniandes/Tesis Maestría Industrial/Datos")
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
# Mes<-factor(months(t),levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
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

#Corrección precio
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
Datos=cbind(Datos,fourier(Datos$Prices,K = c(5,5)),fourier(Datos$lPrices,K = c(7,5)))
names(Datos)[8:51]=c("S1_24","C1_24","S2_24","C2_24","S3_24","C3_24","S4_24","C4_24","S5_24","C5_24","S1_8766","C1_8766","S2_8766","C2_8766","S3_8766","C3_8766","S4_8766","C4_8766","S5_8766","C5_8766",
                      "lS1_24","lC1_24","lS2_24","lC2_24","lS3_24","lC3_24","lS4_24","lC4_24","lS5_24","lC5_24","lS6_24","lC6_24","lS7_24","lC7_24","lS1_8766","lC1_8766","lS2_8766","lC2_8766","lS3_8766","lC3_8766","lS4_8766","lC4_8766","lS5_8766","lC5_8766")

DatosO=Datos[(t>=as.POSIXct("2000-01-01 00:00",tz = "GMT"))&(t<as.POSIXct("2014-01-01 00:00",tz = "GMT")),]
Datos.lag=Datos[(t>=as.POSIXct("1999-12-31 23:00",tz = "GMT"))&(t<as.POSIXct("2013-12-31 23:00",tz = "GMT")),]
names(Datos.lag)=c("t.lag","Prices.lag","lPrices.lag","Mes.lag","FDSF.lag","y.lag","d.lag",
                   "S1_24.lag","C1_24.lag","S2_24.lag","C2_24.lag","S3_24.lag","C3_24.lag","S4_24.lag","C4_24.lag","S5_24.lag","C5_24.lag","S1_8766.lag","C1_8766.lag","S2_8766.lag","C2_8766.lag","S3_8766.lag","C3_8766.lag","S4_8766.lag","C4_8766.lag","S5_8766.lag","C5_8766.lag",
                   "lS1_24.lag","lC1_24.lag","lS2_24.lag","lC2_24.lag","lS3_24.lag","lC3_24.lag","lS4_24.lag","lC4_24.lag","lS5_24.lag","lC5_24.lag","lS6_24.lag","lC6_24.lag","lS7_24.lag","lC7_24.lag","lS1_8766.lag","lC1_8766.lag","lS2_8766.lag","lC2_8766.lag","lS3_8766.lag","lC3_8766.lag","lS4_8766.lag","lC4_8766.lag","lS5_8766.lag","lC5_8766.lag")

DATOS=cbind(DatosO,Datos.lag)

testO=Datos[(t>=as.POSIXct("2014-01-01 00:00",tz = "GMT"))&(t<as.POSIXct("2016-08-15 00:00",tz = "GMT")),]
test.lag=Datos[(t>=as.POSIXct("2013-12-31 23:00",tz = "GMT"))&(t<as.POSIXct("2016-08-14 23:00",tz = "GMT")),]
names(test.lag)=c("t.lag","Prices.lag","lPrices.lag","Mes.lag","FDSF.lag","y.lag","d.lag",
                   "S1_24.lag","C1_24.lag","S2_24.lag","C2_24.lag","S3_24.lag","C3_24.lag","S4_24.lag","C4_24.lag","S5_24.lag","C5_24.lag","S1_8766.lag","C1_8766.lag","S2_8766.lag","C2_8766.lag","S3_8766.lag","C3_8766.lag","S4_8766.lag","C4_8766.lag","S5_8766.lag","C5_8766.lag",
                   "lS1_24.lag","lC1_24.lag","lS2_24.lag","lC2_24.lag","lS3_24.lag","lC3_24.lag","lS4_24.lag","lC4_24.lag","lS5_24.lag","lC5_24.lag","lS6_24.lag","lC6_24.lag","lS7_24.lag","lC7_24.lag","lS1_8766.lag","lC1_8766.lag","lS2_8766.lag","lC2_8766.lag","lS3_8766.lag","lC3_8766.lag","lS4_8766.lag","lC4_8766.lag","lS5_8766.lag","lC5_8766.lag")

TEST=cbind(testO,test.lag)

# Datos$Prices=msts(Datos$Prices,seasonal.periods = c(24,8766))
# Datos$lPrices=msts(Datos$lPrices,seasonal.periods = c(24,8766))
# Datos$Prices.ts=ts(Datos$Prices.ts,frequency = 24)
# Datos$lPrices.ts=ts(Datos$lPrices.ts,frequency = 24)
# Datos=cbind(Datos,fourier(Datos$Prices,K = c(5,5)),fourier(Datos$lPrices,K = c(7,5)))
# names(Datos)[12:51]=c("S1_24","C1_24","S2_24","C2_24","S3_24","C3_24","S4_24","C4_24","S5_24","C5_24","S1_8766","C1_8766","S2_8766","C2_8766","S3_8766","C3_8766","S4_8766","C4_8766","S5_8766","C5_8766",
                      # "lS1_24","lC1_24","lS2_24","lC2_24","lS3_24","lC3_24","lS4_24","lC4_24","lS5_24","lC5_24","lS6_24","lC6_24","lS7_24","lC7_24","lS1_8766","lC1_8766","lS2_8766","lC2_8766","lS3_8766","lC3_8766","lS4_8766","lC4_8766","lS5_8766","lC5_8766")

# Datos.lag=data.frame(t,Prices,lPrices,Prices.ts,lPrices.ts,Mes,FDSF,y,d,ENSO,ONIh)
# Datos.lag=Datos.lag[(t>=as.POSIXct("1999-12-31 23:00",tz = "GMT"))&(t<as.POSIXct("2013-12-31 23:00",tz = "GMT")),]
# Datos.lag$Prices=msts(Datos.lag$Prices,seasonal.periods = c(24,8766))
# Datos.lag$lPrices=msts(Datos.lag$lPrices,seasonal.periods = c(24,8766))
# Datos.lag$Prices.ts=ts(Datos.lag$Prices.ts,frequency = 24)
# Datos.lag$lPrices.ts=ts(Datos.lag$lPrices.ts,frequency = 24)
# Datos.lag=cbind(Datos.lag,fourier(Datos.lag$Prices,K = c(5,5)),fourier(Datos.lag$lPrices,K = c(7,5)))
# names(Datos.lag)=c("t.lag","Prices.lag","lPrices.lag","Prices.ts.lag","lPrices.ts.lag","Mes.lag","FDSF.lag","y.lag","d.lag","ENSO.lag","ONIh.lag",
#                    "S1_24.lag","C1_24.lag","S2_24.lag","C2_24.lag","S3_24.lag","C3_24.lag","S4_24.lag","C4_24.lag","S5_24.lag","C5_24.lag","S1_8766.lag","C1_8766.lag","S2_8766.lag","C2_8766.lag","S3_8766.lag","C3_8766.lag","S4_8766.lag","C4_8766.lag","S5_8766.lag","C5_8766.lag",
#                    "lS1_24.lag","lC1_24.lag","lS2_24.lag","lC2_24.lag","lS3_24.lag","lC3_24.lag","lS4_24.lag","lC4_24.lag","lS5_24.lag","lC5_24.lag","lS6_24.lag","lC6_24.lag","lS7_24.lag","lC7_24.lag","lS1_8766.lag","lC1_8766.lag","lS2_8766.lag","lC2_8766.lag","lS3_8766.lag","lC3_8766.lag","lS4_8766.lag","lC4_8766.lag","lS5_8766.lag","lC5_8766.lag")

# DATOS=cbind(Datos,Datos.lag)

#Modelo 1
fit1<-lm(Prices~y+Mes+fourier(Prices.ts,K = 2)+FDSF,data = Datos);summary(fit1)
plot(Datos$t,Datos$Prices,type="l")
lines(Datos$t,fitted(fit1),col="red")
shapiro.test(resid(fit1))

#Modelo 2
fit2<-lm(Prices~y+fourier(Prices,K = c(5,5))+FDSF+ONIh,data = Datos);summary(fit2)
plot(Datos$t,Datos$Prices,type="l")
lines(Datos$t,fitted(fit2),col="red")

#Modelo 3
fit3<-lm(lPrices~y+Mes+fourier(lPrices.ts,K = 1)+FDSF,data = Datos);summary(fit3)
plot(t,Prices,type="l")
lines(t,fitted(fit3),col="red")
shapiro.test(resid(fit3))

#Modelo 4
fit4<-lm(lPrices~y+`lS1_24`+`lC1_24`+`lS2_24`+`lC2_24`+`lS3_24`+`lC3_24`+`lS4_24`+`lC4_24`+`lS5_24`+`lC5_24`+`lS6_24`+`lC6_24`+`lS7_24`+`lC7_24`+
             `lS1_8766`+`lC1_8766`+`lS2_8766`+`lC2_8766`+`lS3_8766`+`lC3_8766`+`lS4_8766`+`lC4_8766`+`lS5_8766`+`lC5_8766`+
             +FDSF, data = DATOS);summary(fit4)

betas4=coef(fit4)

res4=resid(fit4)
resfit4=lm(I(res4[-length(res4)]-res4[-1])~res4[-1])
kappa4.lm=-log(1+coef(resfit4)[1])

ffit4<-nls(formula = lPrices~phi*lPrices.lag+b0+
        b1*y+gs1*`lS1_24`+gc1*`lC1_24`+gs2*`lS2_24`+gc2*`lC2_24`+gs3*`lS3_24`+gc3*`lC3_24`+gs4*`lS4_24`+gc4*`lC4_24`+gs5*`lS5_24`+gc5*`lC5_24`+gs6*`lS6_24`+gc6*`lC6_24`+gs7*`lS7_24`+gc7*`lC7_24`+
        fs1*`lS1_8766`+fc1*`lC1_8766`+fs2*`lS2_8766`+fc2*`lC2_8766`+fs3*`lS3_8766`+fc3*`lC3_8766`+fs4*`lS4_8766`+fc4*`lC4_8766`+fs5*`lS5_8766`+fc5*`lC5_8766`+
        d1*as.numeric(FDSF)-phi*(b0+b1*y.lag+gs1*`lS1_24.lag`+gc1*`lC1_24.lag`+gs2*`lS2_24.lag`+gc2*`lC2_24.lag`+gs3*`lS3_24.lag`+gc3*`lC3_24.lag`+gs4*`lS4_24.lag`+gc4*`lC4_24.lag`+gs5*`lS5_24.lag`+gc5*`lC5_24.lag`+gs6*`lS6_24.lag`+gc6*`lC6_24.lag`+gs7*`lS7_24.lag`+gc7*`lC7_24.lag`+
                               fs1*`lS1_8766.lag`+fc1*`lC1_8766.lag`+fs2*`lS2_8766.lag`+fc2*`lC2_8766.lag`+fs3*`lS3_8766.lag`+fc3*`lC3_8766.lag`+fs4*`lS4_8766.lag`+fc4*`lC4_8766.lag`+fs5*`lS5_8766.lag`+fc5*`lC5_8766.lag`+
                               d1*as.numeric(FDSF.lag)),data = DATOS,
    start = list(phi=0.5,b0=betas4[1],b1=betas4[2],gs1=betas4[3],gc1=betas4[4],gs2=betas4[5],gc2=betas4[6],gs3=betas4[7],gc3=betas4[8],gs4=betas4[9],gc4=betas4[10],gs5=betas4[11],gc5=betas4[12],gs6=betas4[13],gc6=betas4[14],gs7=betas4[15],gc7=betas4[16],
                 fs1=betas4[17],fc1=betas4[18],fs2=betas4[19],fc2=betas4[20],fs3=betas4[21],fc3=betas4[22],fs4=betas4[23],fc4=betas4[24],fs5=betas4[25],fc5=betas4[26],d1=betas4[27]))

e4=resid(ffit4)

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
  x4[i]=exp(-(1-phi))*x4[i-1]+rnorm(1,sd = sigmae4)
}
lPred4=f4+x4

plot(Datos$t,Datos$lPrices,type="l")
lines(DATOS$t,fitted(ffit4),col="red")
lines(TEST$t,lPred4,col="blue")
plot(x4)

