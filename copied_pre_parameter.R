#File 1 Parameters and distributions 

library (truncdist)
library(triangle)

iterations <- 50000
set.seed(100)

#Total hand area (cm^2)
T.handarea<-runif(iterations, min=445, max=535) 

#Surface area of laundry 
Surface.area.laundry<-runif(iterations, min=4.9*10^3, max=1.8*10^4)
Item.laundry<-runif(iterations, min=14, max=18)

#Fraction of hand surface(hand-surface, hand-face) (unitless)
Frac.HS <-runif(iterations,min=0.13, max=0.25)
Frac.HF <-runif(iterations, min=0.008, max=0.012) 

#Duration (min) / Contact time 
Dur.wash <- 50
Dur.dry <- 60
Contact.time.laundry<-runif(iterations, min=1/60, max=10/60)
Contact.time.face.w<-runif(iterations, min=1/60, max=50)
Contact.time.face.d<-runif(iterations, min=1/60, max=60)
Contact.time.face.f<-runif(iterations, min=1/60, max=60)

#Initial concentration on hand/face
Conc.i.hand<-0
Conc.i.face<-0



if(organism == "SARS-CoV-2") {  
  #Transfer efficiency
  TE.dry <-rtrunc(iterations,"norm", mean=0.0003, sd=0.0002, a=0, b=1)
  TE.wet <-rtriangle(n=iterations, a=0.00001, b=0.0001, c=0.00005)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  #Reduction, inactivation
  Reduc.wash <-rtrunc(iterations,"norm", mean=3.0, sd=0.02, a=0, b=5.0)
  Reduc.dry <-rtrunc(iterations, "norm", mean=3.0, sd=0.02, a=0, b=5.0)
  Reduc.hwash<-runif(iterations, min=2.03, max=5.0)
  Inact.h <-rtriangle(iterations, a=0.0056, b=0.0125, c=0.0084)
  Inact.s <-rtriangle(iterations, a=0.000075, b=0.000125, c=0.0001)
  
  #Initial concentration on laundry on Sunday
  Viralload <-runif(iterations,min=3.3*10^6, max=2.35*10^9)
  Freq.cough.day<-rtriangle(n=iterations, a=240, b=1872, c=864)
  Volume.cough<-5.964*10^(-3)
  Conversion.ratio<-runif(iterations, min=100, max=1000)
  Conc.onecloth<-(Viralload*Volume.cough*Freq.cough.day)/(Conversion.ratio*Surface.area.laundry)
  Conc.i.laundry<-(Conc.onecloth*exp(-Inact.s*6*24*60)+Conc.onecloth*exp(-Inact.s*5*24*60)+ 
                     Conc.onecloth*exp(-Inact.s*4*24*60)+Conc.onecloth*exp(-Inact.s*3*24*60)+
                     Conc.onecloth*exp(-Inact.s*2*24*60)+Conc.onecloth*exp(-Inact.s*1*24*60)+
                     Conc.onecloth)/Item.laundry
  
} else if (organism == "Rotavirus") 
{
  #Transfer efficiency
  TE.dry <-rtrunc(iterations,"norm", mean=0.0003, sd=0.0002, a=0, b=1)
  TE.wet <-rtriangle(n=iterations, a=0.00001, b=0.0001, c=0.00005)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  #Reduction, inactivation
  Reduc.wash <-rtrunc(iterations,"norm", mean=5.0, sd=0.25, a=0, b=7.0)
  Reduc.dry <-rtrunc(iterations, "norm", mean=6.9, sd=0.06, a=0, b=9.0)
  Reduc.hwash<-runif(iterations, min=0.14, max=4.32)
  Inact.h <-rtrunc(iterations, "norm", mean=0.0045, sd=0.000258, a=0, b=1)
  Inact.s <-runif(iterations, min=0.0025, max=0.0042)
  
  #Initial concentration on laundry on Sunday
  Conc.feces <-runif(iterations,min=10^10, max=10^12)
  Mass.feces <-runif(iterations, min=0.01, max=1)
  
  Conc.onecloth<-Conc.feces*Mass.feces/(Surface.area.laundry)
  Conc.i.laundry<-(Conc.onecloth*exp(-Inact.s*6*24*60)+Conc.onecloth*exp(-Inact.s*5*24*60)+ 
                     Conc.onecloth*exp(-Inact.s*4*24*60)+Conc.onecloth*exp(-Inact.s*3*24*60)+
                     Conc.onecloth*exp(-Inact.s*2*24*60)+Conc.onecloth*exp(-Inact.s*1*24*60)+
                     Conc.onecloth)/Item.laundry
  
  #Dose response value 
  alpha<-2.53E-01
  N50<-6.17E+00
  beta<-N50/(2^(1/alpha)-1)
  
} else if(organism=="Norovirus"){
  #Transfer efficiency
  TE.dry <-rtrunc(iterations,"norm", mean=0.0003, sd=0.0002, a=0, b=1)
  TE.wet <-rtriangle(n=iterations, a=0.00001, b=0.0001, c=0.00005)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  #Reduction, inactivation (cold, cotton)
  Reduc.wash <-rtrunc(iterations,"norm", mean=2.4, sd=0.62, a=0, b=4.0)
  Reduc.dry <-rtrunc(iterations, "norm", mean=6.1, sd=0.10, a=0, b=8.0)
  Reduc.hwash<-runif(iterations, min=0.91, max=3.78)
  Inact.h <-runif(iterations, min=0.0002, max=0.0118)
  Inact.s <-runif(iterations, min=0.0008, max=0.0016) 
  
  #Initial concentration on laundry on Sunday
  Viralload <-runif(iterations,min=10^7.71, max=10^10.94)
  Mass.feces <-runif(iterations, min=0.01, max=1)
  Conversion.ratio<-runif(iterations, min=100, max=1000)
  Conc.onecloth<-Viralload*Mass.feces/(Surface.area.laundry*Conversion.ratio)
  Conc.i.laundry<-(Conc.onecloth*exp(-Inact.s*6*24*60)+Conc.onecloth*exp(-Inact.s*5*24*60)+ 
                     Conc.onecloth*exp(-Inact.s*4*24*60)+Conc.onecloth*exp(-Inact.s*3*24*60)+
                     Conc.onecloth*exp(-Inact.s*2*24*60)+Conc.onecloth*exp(-Inact.s*1*24*60)+
                     Conc.onecloth)/Item.laundry
  
  
} else if(organism=="Salmonella"){
  #Transfer efficiency
  TE.dry <-rtrunc(iterations,"norm", mean=0.068, sd=0.070, a=0, b=1)
  TE.wet <-rtriangle(n=iterations, a=0.00001, b=0.0001, c=0.00003)
  TE.face<-rtrunc(iterations,"norm", mean=0.3397, sd=0.1604, a=0, b=1)
  
  #Reduction, inactivation
  Reduc.wash <-rtrunc(iterations,"norm", mean=3.4, sd=0.39, a=0, b=5.0)
  Reduc.dry <-rtrunc(iterations, "norm", mean=7.3, sd=0.42, a=0, b=9.0)
  Reduc.hwash<-runif(iterations, min=0.6, max=5.8)
  Inact.h <-runif(iterations, min=0.0275, max=0.0533)
  Inact.s <-runif(iterations, min=0.0013, max=0.0015)
  
  #Initial concentration on laundry on Sunday
  Conc.feces <-runif(iterations,min=10^6, max=10^10)
  Mass.feces <-runif(iterations, min=0.01, max=1)
  Conc.onecloth<-Conc.feces*Mass.feces/Surface.area.laundry
  Conc.i.laundry<-(Conc.onecloth*exp(-Inact.s*6*24*60)+Conc.onecloth*exp(-Inact.s*5*24*60)+ 
                     Conc.onecloth*exp(-Inact.s*4*24*60)+Conc.onecloth*exp(-Inact.s*3*24*60)+
                     Conc.onecloth*exp(-Inact.s*2*24*60)+Conc.onecloth*exp(-Inact.s*1*24*60)+
                     Conc.onecloth)/Item.laundry
  #Dose response value 
  alpha<-2.1E-01
  N50<-4.98E+01
  beta<-N50/(2^(1/alpha)-1)
  
  
} else { #E.coli 
  
  #Transfer efficiency
  TE.dry <-rtrunc(iterations,"norm", mean=0.068, sd=0.070, a=0, b=1)
  TE.wet <-rtriangle(n=iterations, a=0.00001, b=0.0001, c=0.00003)
  TE.face<-rtrunc(iterations,"norm", mean=0.3397, sd=0.1604, a=0, b=1)
  
  #Reduction, inactivation (cold, cotton)
  Reduc.wash <-rtrunc(iterations,"norm", mean=3.5, sd=0.96, a=0, b=6.0)
  Reduc.dry <-rtrunc(iterations, "norm", mean=8.0, sd=0.93, a=0, b=10.0)
  Reduc.hwash<-runif(iterations, min=0.6, max=5.8)
  Inact.h <-runif(iterations, min=0.05, max=0.0753)
  Inact.s <-runif(iterations, min=0.0015, max=0.023)
  
  #Reduction (various options) 
  Reduc.wash.terry <-rtrunc(iterations, "norm", mean=2.1, sd=0.21, a=0, b=4)
  Reduc.wash.terry.w<- rtrunc(iterations, "norm", mean=2.1, sd=0.11, a=0, b=4.0)
  Reduc.dry.line<-rtrunc(iterations, "norm", mean=5.2, sd=0.55, a=0, b=7.0)
  
  #Initial concentration on laundry on Sunday
  Conc.feces <-runif(iterations,min=10^7, max=10^9)
  Mass.feces <-runif(iterations, min=0.01, max=1)
  Conc.onecloth<-Conc.feces*Mass.feces/Surface.area.laundry
  Conc.i.laundry<-(Conc.onecloth*exp(-Inact.s*6*24*60)+Conc.onecloth*exp(-Inact.s*5*24*60)+ 
                     Conc.onecloth*exp(-Inact.s*4*24*60)+Conc.onecloth*exp(-Inact.s*3*24*60)+
                     Conc.onecloth*exp(-Inact.s*2*24*60)+Conc.onecloth*exp(-Inact.s*1*24*60)+
                     Conc.onecloth)/Item.laundry
  #Dose response value 
  alpha<-1.55E-01
  N50<-2.11E+06
  beta<-N50/(2^(1/alpha)-1)
  
}





