#File 1 Parameters and distributions 

library (truncdist)
library(triangle)

iterations <- 50000
set.seed(25)

#Total hand area (cm^2)
A.hand<-runif(iterations, min=445, max=535) 

#Surface area of laundry 
Surface.area.laundry<-runif(iterations, min=4.9*10^3, max=1.8*10^4)
Item.laundry<-runif(iterations, min=14, max=18)
A.laundry<- Surface.area.laundry*Item.laundry

#Fraction of hand surface(hand-surface, hand-face) (unitless)
Frac.HS <-runif(iterations,min=0.13, max=0.25)
Frac.HF <-runif(iterations, min=0.008, max=0.012) 

#Duration (min) / Contact time 
T.load_dry <- rtriangle(iterations, a=14.6/60, b=113.2/60, c=63.9/60)
T.load_wet <- rtriangle(iterations, a=9.8/60, b=75.4/60, c=43.6/60)
T.after<-15 

##Contact.time.laundry<-runif(iterations, min=1/60, max=10/60)
##Contact.time.face.w<-runif(iterations, min=1/60, max=50)
##Contact.time.face.d<-runif(iterations, min=1/60, max=60)
##Contact.time.face.f<-runif(iterations, min=1/60, max=60)

#Contact frequency (times/min) / Contact number (times)
HL.dry<- 12.1
HL.wet<- 11.4

HM.dry<-0.53
HE.dry<-0.20
HN.dry<-0.57

HM.wet<-0.89
HE.wet<-0.22
HN.wet<-0

# contact number
n.HL.dry<- 13
n.HL.wet<- 9

n.HM.dry<-8
n.HE.dry<-3
n.HN.dry<-9

n.HM.wet<-14
n.HE.wet<-4
n.HN.wet<-0


#Initial concentration on hand/face
Conc.i.hand<-0
Conc.i.face<-0



if(organism == "Influenza A") {  
  #Transfer efficiency
  TE.dry <-rtriangle(n=iterations, a=4.74E-05, b=4.94E-02, c=9.09E-04)
  TE.wet <-rtriangle(n=iterations, a=6.77E-04, b=3.77E-02, c=7.32E-03)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  #Reduction
  Reduc.wash <-4
  Reduc.wash.c <-2.34
  ##Reduc.dry <-rtrunc(iterations, "norm", mean=3.0, sd=0.02, a=0, b=5.0)
  ##Reduc.hwash<-runif(iterations, min=2.03, max=5.0)
  
  #Number of contacts
  n.HF.wet<-n.HM.wet+n.HN.wet+n.HE.wet
  n.HF.dry<-n.HM.dry+n.HN.dry+n.HE.dry
  
  #Initial concentration on laundry on Sunday

  Viralload <-10^(rnorm(iterations,mean=6.94,sd=2.5))
  Freq.cough.day<-rtriangle(n=iterations, a=240, b=1872, c=864)
  Volume.cough<-5.964*10^(-3)
  Conversion.ratio<-runif(iterations, min=10, max=509)
  Conc.i.laundry<-(Viralload*Volume.cough*Freq.cough.day)/(Conversion.ratio*A.laundry)
  
  #Dose response value 
  
  alpha<-4.29E-01
  N50<-6.66E+05
  beta<-N50/(2^(1/alpha)-1)
  
  cal_risk<-function(dose) {
    1-(1+(dose/beta))^(-alpha)  
  }
  
  
} else if (organism == "E.faecium") 
{
  #Transfer efficiency
  TE.dry <-rtriangle(n=iterations, a=6.98E-05, b=3.29E-01, c=1.07E-03)
  TE.wet <-rtriangle(n=iterations, a=4.00E-04, b=6.58E-02, c=5.32E-03)
  TE.face<-0.4099
  
  #Reduction, inactivation
  Reduc.wash <-5.35
  Reduc.wash.c <-0.82
  ##Reduc.dry <-rtrunc(iterations, "norm", mean=6.9, sd=0.06, a=0, b=9.0)
  ##Reduc.hwash<-runif(iterations, min=0.14, max=4.32)

  #Number of contacts
  n.HF.wet<-n.HM.wet+n.HE.wet
  n.HF.dry<-n.HM.dry+n.HE.dry
  
  #Initial concentration on laundry on Sunday
  Conc.feces <-runif(iterations,min=10^4, max=10^6)
  Mass.feces <-1
  
  Conc.i.laundry<-Conc.feces*Mass.feces/A.laundry
  
  #Dose response value 
  k=2.19E-11
  
  cal_risk<-function(dose) {
    1-exp(-dose/k)  }
  
  
} else { #E.coli 
  
  #Transfer efficiency
  TE.dry <-rtriangle(n=iterations, a=1.42E-05, b=3.29E-01, c=7.52E-04)
  TE.wet <-rtriangle(n=iterations, a=4.35E-05, b=6.58E-02, c=1.91E-03)
  TE.face<-rtrunc(iterations,"norm", mean=0.3397, sd=0.1604, a=0, b=1)
  
  #Reduction, inactivation (cold, cotton)
  Reduc.wash <-4.32
  Reduc.wash.c <-0.26
  ##Reduc.dry <-rtrunc(iterations, "norm", mean=8.0, sd=0.93, a=0, b=10.0)
  ##Reduc.hwash<-runif(iterations, min=0.6, max=5.8)
  
  #Number of contacts
  n.HF.wet<-n.HM.wet
  n.HF.dry<-n.HM.dry
  
  #Initial concentration on laundry on Sunday
  Conc.feces <-runif(iterations,min=10^7, max=10^9)
  Mass.feces <-1
  
  Conc.i.laundry<-Conc.feces*Mass.feces/A.laundry
  
  #Dose response value 
  alpha<-1.55E-01
  N50<-2.11E+06
  beta<-N50/(2^(1/alpha)-1)
  
  cal_risk<-function(dose) {
    1-(1+(dose/beta))^(-alpha)  
  }
  
}





