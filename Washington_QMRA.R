organism<-"E.coli"
source("Washington_parameter.R")

#Scenario 1. touching wet laundry to put it into dryer (baseline, using china standard)-------------------
numevents<-2
eventsname<-c("Washer to dryer","Hand to face touch")

Conc.h.b<-matrix(nrow=numevents,ncol=iterations)
rownames(Conc.h.b)<-eventsname

Conc.l.b<-matrix(nrow=numevents, ncol=iterations)
rownames(Conc.l.b)<-eventsname

Dose.b<-matrix(nrow=numevents, ncol=iterations)
rownames(Dose.b)<-eventsname

Risk.b<-matrix(nrow=numevents, ncol=iterations)
rownames(Risk.b)<-eventsname

##Event 1. Loading washed clothes to dryer
Conc.h.current<-0
Conc.l.current<-Conc.i.laundry/10^(Reduc.wash.c)

for (i in 1:n.HL.wet) {
  Conc.h.current<-Conc.h.current-{TE.wet*Frac.HS*(Conc.h.current-Conc.l.current)}
  Conc.l.current<-Conc.l.current-{TE.wet*Frac.HS*A.hand/A.laundry*(Conc.l.current-Conc.h.current)}
}


Conc.h.b[1,]<- Conc.h.current
Conc.l.b[1,]<- Conc.l.current
Dose.b[1,]<-0
Risk.b[1,]<-0

##Event 2. Hand to face contact 
n.HF.wet<-n.HM.wet

Conc.h.current<-Conc.h.b[1,]
Dose<-Dose.b[1,]

for (i in 1:n.HF.wet){
  Conc.h.current <-(1-TE.face*Frac.HF)*(Conc.h.current)
  Dose<-Dose+(Conc.h.current*TE.face*Frac.HF*A.hand)
}

Conc.h.b[2,]<- Conc.h.current
Conc.l.b[2,]<-Conc.l.b[1,]
Dose.b[2,]<-Dose
Risk.b[2,]<-1-(1+(Dose.b[2,]/beta))^(-alpha)

#Scenario 2. touching wet laundry to put it into dryer (Using White)-------------------
numevents<-2
eventsname<-c("Washer to dryer","Hand to face touch")

Conc.h.i<-matrix(nrow=numevents,ncol=iterations)
rownames(Conc.h.i)<-eventsname

Conc.l.i<-matrix(nrow=numevents, ncol=iterations)
rownames(Conc.l.i)<-eventsname

Dose.i<-matrix(nrow=numevents, ncol=iterations)
rownames(Dose.i)<-eventsname

Risk.i<-matrix(nrow=numevents, ncol=iterations)
rownames(Risk.i)<-eventsname

##Event 1. Loading washed clothes to dryer
Conc.h.current<-0
Conc.l.current<-Conc.i.laundry/10^(Reduc.wash)

for (i in 1:n.HL.wet) {
  Conc.h.current<-Conc.h.current-{TE.wet*Frac.HS*(Conc.h.current-Conc.l.current)}
  Conc.l.current<-Conc.l.current-{TE.wet*Frac.HS*A.hand/A.laundry*(Conc.l.current-Conc.h.current)}
}


Conc.h.i[1,]<- Conc.h.current
Conc.l.i[1,]<- Conc.l.current
Dose.i[1,]<-0
Risk.i[1,]<-0

##Event 2. Hand to face contact 
n.HF.wet<-n.HM.wet

Conc.h.current<-Conc.h.i[1,]
Dose<-Dose.i[1,]

for (i in 1:n.HF.wet){
  Conc.h.current <-(1-TE.face*Frac.HF)*(Conc.h.current)
  Dose<-Dose+(Conc.h.current*TE.face*Frac.HF*A.hand)
}

Conc.h.i[2,]<- Conc.h.current
Conc.l.i[2,]<-Conc.l.i[1,]
Dose.i[2,]<-Dose
Risk.i[2,]<-1-(1+(Dose.i[2,]/beta))^(-alpha)


#plotting--------need to update
library(ggplot2)
library(ggpubr)

Conc.h.dataframe<-as.data.frame(t(Conc.h))
Conc.l.dataframe<-as.data.frame(t(Conc.l))
Dose.dataframe<-as.data.frame(t(Dose))
Risk.dataframe<-as.data.frame(t(Risk))

event<-rep(c(rep(1,iterations),rep(2,iterations),rep(3,iterations),rep(4,iterations),rep(5,iterations),rep(6,iterations),rep(7,iterations),rep(8,iterations)),4)
type<-c(rep("Hand",8*iterations),rep("Laundry",8*iterations),rep("Dose",8*iterations),rep("Risk",8*iterations))
value<-c(Conc.h.dataframe$`Hamper to washer`, Conc.h.dataframe$`Hand to face touch #1`,
         Conc.h.dataframe$`between face touch and end of washing cycle`,Conc.h.dataframe $`Washer to dryer`, 
         Conc.h.dataframe $`Hand to face touch #2`, Conc.h.dataframe $`Dry cycle`,
         Conc.h.dataframe $`Dryer to folding area`,Conc.h.dataframe $`Hand to face touch #3`,
         
         Conc.l.dataframe$`Hamper to washer`, Conc.l.dataframe$`Hand to face touch #1`,
         Conc.l.dataframe$`between face touch and end of washing cycle`,Conc.l.dataframe $`Washer to dryer`, 
         Conc.l.dataframe $`Hand to face touch #2`, 
         Conc.l.dataframe $`Dry cycle`,Conc.l.dataframe $`Dryer to folding area`,
         Conc.l.dataframe $`Hand to face touch #3`,
         
         Dose.dataframe$`Hamper to washer`, Dose.dataframe$`Hand to face touch #1`,
         Dose.dataframe$`between face touch and end of washing cycle`,Dose.dataframe $`Washer to dryer`,
         Dose.dataframe $`Hand to face touch #2`, Dose.dataframe $`Dry cycle`,Dose.dataframe $`Dryer to folding area`,
         Dose.dataframe $`Hand to face touch #3`,
         
         Risk.dataframe$`Hamper to washer`, Risk.dataframe$`Hand to face touch #1`,
         Risk.dataframe$`between face touch and end of washing cycle`,Risk.dataframe $`Washer to dryer`,
         Risk.dataframe $`Hand to face touch #2`, Risk.dataframe $`Dry cycle`,Risk.dataframe $`Dryer to folding area`,
         Risk.dataframe $`Hand to face touch #3`)

data<-data.frame(event,type,value)

windows()
ggplot(data)+geom_violin(aes(x=event,y=value,fill=type, group=event),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  facet_wrap(~type,scales="free") +
  scale_y_continuous(trans="log10")

#Result pulling -------------------------------------

#Conc.h
matrix.Conc.h<-matrix(nrow=numevents*2, ncol=5)
colnames(matrix.Conc.h)<-c('mean', 'sd', 'min', 'max','median')
rownames(matrix.Conc.h)<-c('washer to dryer.b','hand to face touch.b','washer to dryer.i','hand to face touch.i')


for (f in 1:2){
  matrix.Conc.h[f,1]<-mean(Conc.h.b[f,])
  matrix.Conc.h[f,2]<-sd(Conc.h.b[f,])
  matrix.Conc.h[f,3]<-min(Conc.h.b[f,])
  matrix.Conc.h[f,4]<-max(Conc.h.b[f,])
  matrix.Conc.h[f,5]<-median(Conc.h.b[f,])
}

for (f in 1:2){
  matrix.Conc.h[f+2,1]<-mean(Conc.h.i[f,])
  matrix.Conc.h[f+2,2]<-sd(Conc.h.i[f,])
  matrix.Conc.h[f+2,3]<-min(Conc.h.i[f,])
  matrix.Conc.h[f+2,4]<-max(Conc.h.i[f,])
  matrix.Conc.h[f+2,5]<-median(Conc.h.i[f,])
}

#Conc.l

matrix.Conc.l<-matrix(nrow=numevents*2, ncol=5)
colnames(matrix.Conc.l)<-c('mean', 'sd', 'min', 'max','median')
rownames(matrix.Conc.l)<-c('washer to dryer.b','hand to face touch.b','washer to dryer.i','hand to face touch.i')


for (f in 1:2){
  matrix.Conc.l[f,1]<-mean(Conc.l.b[f,])
  matrix.Conc.l[f,2]<-sd(Conc.l.b[f,])
  matrix.Conc.l[f,3]<-min(Conc.l.b[f,])
  matrix.Conc.l[f,4]<-max(Conc.l.b[f,])
  matrix.Conc.l[f,5]<-median(Conc.l.b[f,])
}

for (f in 1:2){
  matrix.Conc.l[f+2,1]<-mean(Conc.l.i[f,])
  matrix.Conc.l[f+2,2]<-sd(Conc.l.i[f,])
  matrix.Conc.l[f+2,3]<-min(Conc.l.i[f,])
  matrix.Conc.l[f+2,4]<-max(Conc.l.i[f,])
  matrix.Conc.l[f+2,5]<-median(Conc.l.i[f,])
}

#Dose
matrix.Dose<-matrix(nrow=numevents*2, ncol=5)
colnames(matrix.Dose)<-c('mean', 'sd', 'min', 'max', 'median')
rownames(matrix.Dose)<-c('washer to dryer.b','hand to face touch.b','washer to dryer.i','hand to face touch.i')

for (f in 1:2){
  matrix.Dose[f,1]<-mean(Dose.b[f,])
  matrix.Dose[f,2]<-sd(Dose.b[f,])
  matrix.Dose[f,3]<-min(Dose.b[f,])
  matrix.Dose[f,4]<-max(Dose.b[f,])
  matrix.Dose[f,5]<-median(Dose.b[f,])
}

for (f in 1:2){
  matrix.Dose[f+2,1]<-mean(Dose.i[f,])
  matrix.Dose[f+2,2]<-sd(Dose.i[f,])
  matrix.Dose[f+2,3]<-min(Dose.i[f,])
  matrix.Dose[f+2,4]<-max(Dose.i[f,])
  matrix.Dose[f+2,5]<-median(Dose.i[f,])
}


#Risk 
matrix.Risk<-matrix(nrow=numevents*2, ncol=5)
colnames(matrix.Risk)<-c('mean', 'sd', 'min', 'max', 'median')
rownames(matrix.Risk)<-c('washer to dryer.b','hand to face touch.b','washer to dryer.i','hand to face touch.i')

for (f in 1:2){
  matrix.Risk[f,1]<-mean(Risk.b[f,])
  matrix.Risk[f,2]<-sd(Risk.b[f,])
  matrix.Risk[f,3]<-min(Risk.b[f,])
  matrix.Risk[f,4]<-max(Risk.b[f,])
  matrix.Risk[f,5]<-median(Risk.b[f,])
}

for (f in 1:2){
  matrix.Risk[f+2,1]<-mean(Risk.i[f,])
  matrix.Risk[f+2,2]<-sd(Risk.i[f,])
  matrix.Risk[f+2,3]<-min(Risk.i[f,])
  matrix.Risk[f+2,4]<-max(Risk.i[f,])
  matrix.Risk[f+2,5]<-median(Risk.i[f,])
}

# Risk result by handwashing scenarios (copied from previous laundry code. not used for right now)
#Risk, handwashing scenario 1 
matrix.Risk.1<-matrix(nrow=8, ncol=4)
colnames(matrix.Risk.1)<-c('mean', 'sd', 'min', 'max')
rownames(matrix.Risk.1)<-c(1:8)

for (f in 1:8){
  matrix.Risk.1[f,1]<-mean(Risk.1[f,])
  matrix.Risk.1[f,2]<-sd(Risk.1[f,])
  matrix.Risk.1[f,3]<-min(Risk.1[f,])
  matrix.Risk.1[f,4]<-max(Risk.1[f,])
}

#Pull out the data 
library(openxlsx)
write.csv(matrix.Conc.h, file="Conc.h.csv")
write.csv(matrix.Conc.l, file="Conc.l.csv")
write.csv(matrix.Dose, file="Dose.csv")
write.csv(matrix.Risk, file="Risk.csv")
##write.csv(matrix.Risk.1, file="Risk.1.csv")
##write.csv(matrix.Risk.2,  file="Risk.2.csv")
##write.csv(matrix.Risk.3, file="Risk.3.csv")




######===================================================================#######============
##Hand washing scenario 1----------------------------------------------------

#Matrix 
numevents<-8
eventsname<-c("Hamper to washer","Hand to face touch #1", "between face touch and end of washing cycle", 
              "Washer to dryer","Hand to face touch #2","Dry cycle","Dryer to folding area",
              "Hand to face touch #3")

Conc.h.1<-matrix(nrow=numevents,ncol=iterations)
rownames(Conc.h.1)<-eventsname

Conc.l.1<-matrix(nrow=numevents, ncol=iterations)
rownames(Conc.l.1)<-eventsname

Dose.1<-matrix(nrow=numevents, ncol=iterations)
rownames(Dose.1)<-eventsname

Risk.1<-matrix(nrow=numevents, ncol=iterations)
rownames(Risk.1)<-eventsname


#Event 1. Loading dirty clothes from hamper to washer

Conc.h.1[1,]<-Conc.i.hand*exp(-Inact.h*Contact.time.laundry)-{TE.dry*Frac.HS*(Conc.i.hand*exp(-Inact.h*Contact.time.laundry)-Conc.onecloth*exp(-Inact.s*Contact.time.laundry))}
Conc.l.1[1,]<-Conc.i.laundry*exp(-Inact.s*Contact.time.laundry)-{TE.dry*Frac.HS*A.hand/Surface.area.laundry*(Conc.i.laundry*exp(-Inact.s*Contact.time.laundry)-Conc.i.hand*exp(-Inact.h*Contact.time.laundry))}
Dose.1[1,]<-0
Risk.1[1,]<-0

#Event 2. Hand washing + Hand to face contact #1

Conc.h.1[2,]<-(1-TE.face*Frac.HF)*{(Conc.h.1[1,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.w)}
Conc.l.1[2,]<-Conc.l.1[1,]/10^(Reduc.wash*Contact.time.face.w/Dur.wash)
Dose.1[2,]<-(Conc.h.1[1,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.w)*TE.face*Frac.HF*A.hand
Risk.1[2,]<-1-(1+(Dose.1[2,]/beta))^(-alpha)


#Event 3. Washing Laundry 
Conc.h.1[3,]<-Conc.h.1[2,]*exp(-Inact.h*(Dur.wash-Contact.time.face.w))
Conc.l.1[3,]<-Conc.l.1[2,]/10^(Reduc.wash*(Dur.wash-Contact.time.face.w)/Dur.wash)
Dose.1[3,]<-Dose.1[2,]
Risk.1[3,]<-Risk.1[2,]

#Event 4. Loading washed clothes to dryer
Conc.h.1[4,]<-Conc.h.1[3,]*exp(-Inact.h*Contact.time.laundry)-{TE.wet*Frac.HS*
    (Conc.h.1[3,]*exp(-Inact.h*Contact.time.laundry)-Conc.l.1[3,]*exp(-Inact.s*Contact.time.laundry))}
Conc.l.1[4,]<-Conc.l.1[3,]*exp(-Inact.s*Contact.time.laundry)-{TE.wet*Frac.HS*A.hand/Surface.area.laundry*
    (Conc.l.1[3,]*exp(-Inact.s*Contact.time.laundry)-Conc.h.1[3,]*exp(-Inact.h*Contact.time.laundry))}
Dose.1[4,]<-Dose.1[3,]
Risk.1[4,]<-Risk.1[3,]

#Event 5. Hand to face contact #2
Conc.h.1[5,]<-(1-TE.face*Frac.HF)*(Conc.h.1[4,]*exp(-Inact.h*Contact.time.face.d))
Conc.l.1[5,]<-Conc.l.1[4,]/10^(Reduc.dry*Contact.time.face.d/Dur.dry)
Dose.1[5,]<-Dose.1[4,]+(Conc.h.1[4,]*exp(-Inact.h*Contact.time.face.d)*TE.face*Frac.HF*A.hand)
Risk.1[5,]<- 1-(1+(Dose.1[5,]/beta))^(-alpha)

#Event 6. Drying Laundry 
Conc.h.1[6,]<-Conc.h.1[5,]*exp(-Inact.h*(Dur.dry-Contact.time.face.d))
Conc.l.1[6,]<-Conc.l.1[5,]/10^(Reduc.dry*(Dur.dry-Contact.time.face.d)/Dur.dry)
Dose.1[6,]<-Dose.1[5,]
Risk.1[6,]<-Risk.1[5,]

#Event 7. Dryer to folding area 
Conc.h.1[7,]<-Conc.h.1[6,]*exp(-Inact.h*Contact.time.laundry)-{TE.dry*Frac.HS*(Conc.h.1[6,]*exp(-Inact.h*Contact.time.laundry)-Conc.l.1[6,]*exp(-Inact.s*Contact.time.laundry))}
Conc.l.1[7,]<-Conc.l.1[6,]*exp(-Inact.s*Contact.time.laundry)-{TE.dry*Frac.HS*A.hand/Surface.area.laundry*(Conc.l.1[6,]*exp(-Inact.s*Contact.time.laundry)-Conc.h.1[6,]*exp(-Inact.h*Contact.time.laundry))}
Dose.1[7,]<-Dose.1[6,]
Risk.1[7,]<-Risk.1[6,]

#Event 8. Hand to face Contact #3
Conc.h.1[8,]<-(1-TE.face*Frac.HF)*(Conc.h.1[7,]*exp(-Inact.h*Contact.time.face.f))
Conc.l.1[8,]<-Conc.l.1[7,]*exp(-Inact.s*Contact.time.face.f)
Dose.1[8,]<-Dose.1[7,]+(Conc.h.1[7,]*exp(-Inact.h*Contact.time.face.f)*TE.face*Frac.HF*A.hand)
Risk.1[8,]<- 1-(1+(Dose.1[8,]/beta))^(-alpha)

#plotting
library(ggplot2)
library(ggpubr)

Conc.h.1.dataframe<-as.data.frame(t(Conc.h.1))
Conc.l.1.dataframe<-as.data.frame(t(Conc.l.1))
Dose.1.dataframe<-as.data.frame(t(Dose.1))
Risk.1.dataframe<-as.data.frame(t(Risk.1))


event<-rep(c(rep(1,iterations),rep(2,iterations),rep(3,iterations),rep(4,iterations),rep(5,iterations),rep(6,iterations),rep(7,iterations),rep(8,iterations)),4)
type<-c(rep("Hand",8*iterations),rep("Laundry",8*iterations),rep("Dose",8*iterations),rep("Risk",8*iterations))
value<-c(Conc.h.1.dataframe$`Hamper to washer`, Conc.h.1.dataframe$`Hand to face touch #1`,
         Conc.h.1.dataframe$`between face touch and end of washing cycle`,Conc.h.1.dataframe $`Washer to dryer`, 
         Conc.h.1.dataframe $`Hand to face touch #2`, Conc.h.1.dataframe $`Dry cycle`,
         Conc.h.1.dataframe $`Dryer to folding area`,Conc.h.1.dataframe $`Hand to face touch #3`,
         
         Conc.l.1.dataframe$`Hamper to washer`, Conc.l.1.dataframe$`Hand to face touch #1`,
         Conc.l.1.dataframe$`between face touch and end of washing cycle`,Conc.l.1.dataframe $`Washer to dryer`, 
         Conc.l.1.dataframe $`Hand to face touch #2`, 
         Conc.l.1.dataframe $`Dry cycle`,Conc.l.1.dataframe $`Dryer to folding area`,
         Conc.l.1.dataframe $`Hand to face touch #3`,
         
         Dose.1.dataframe$`Hamper to washer`, Dose.1.dataframe$`Hand to face touch #1`,
         Dose.1.dataframe$`between face touch and end of washing cycle`,Dose.1.dataframe $`Washer to dryer`,
         Dose.1.dataframe $`Hand to face touch #2`, Dose.1.dataframe $`Dry cycle`,Dose.1.dataframe $`Dryer to folding area`,
         Dose.1.dataframe $`Hand to face touch #3`,
         
         Risk.1.dataframe$`Hamper to washer`, Risk.1.dataframe$`Hand to face touch #1`,
         Risk.1.dataframe$`between face touch and end of washing cycle`,Risk.1.dataframe $`Washer to dryer`,
         Risk.1.dataframe $`Hand to face touch #2`, Risk.1.dataframe $`Dry cycle`,Risk.1.dataframe $`Dryer to folding area`,
         Risk.1.dataframe $`Hand to face touch #3`)

data<-data.frame(event,type,value)

windows()
ggplot(data)+geom_violin(aes(x=event,y=value,fill=type, group=event),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  facet_wrap(~type,scales="free") +
  scale_y_continuous(trans="log10")



##Hand washing scenario 2----------------------------------------------------

#Matrix 
numevents<-8
eventsname<-c("Hamper to washer","Hand to face touch #1", "between face touch and end of washing cycle", 
              "Washer to dryer","Hand to face touch #2","Dry cycle","Dryer to folding area",
              "Hand to face touch #3")

Conc.h.2<-matrix(nrow=numevents,ncol=iterations)
rownames(Conc.h.2)<-eventsname

Conc.l.2<-matrix(nrow=numevents, ncol=iterations)
rownames(Conc.l.2)<-eventsname

Dose.2<-matrix(nrow=numevents, ncol=iterations)
rownames(Dose.2)<-eventsname

Risk.2<-matrix(nrow=numevents, ncol=iterations)
rownames(Risk.2)<-eventsname


#Event 1. Loading dirty clothes from hamper to washer

Conc.h.2[1,]<-Conc.i.hand*exp(-Inact.h*Contact.time.laundry)-{TE.dry*Frac.HS*(Conc.i.hand*exp(-Inact.h*Contact.time.laundry)-Conc.onecloth*exp(-Inact.s*Contact.time.laundry))}
Conc.l.2[1,]<-Conc.i.laundry*exp(-Inact.s*Contact.time.laundry)-{TE.dry*Frac.HS*A.hand/Surface.area.laundry*(Conc.i.laundry*exp(-Inact.s*Contact.time.laundry)-Conc.i.hand*exp(-Inact.h*Contact.time.laundry))}
Dose.2[1,]<-0
Risk.2[1,]<-0

#Event 2. Hand to face contact #1

Conc.h.2[2,]<-(1-TE.face*Frac.HF)*(Conc.h.2[1,]*exp(-Inact.h*Contact.time.face.w))
Conc.l.2[2,]<-Conc.l.2[1,]/10^(Reduc.wash*Contact.time.face.w/Dur.wash)
Dose.2[2,]<-Conc.h.2[1,]*exp(-Inact.h*Contact.time.face.w)*TE.face*Frac.HF*A.hand
Risk.2[2,]<-1-(1+(Dose.2[2,]/beta))^(-alpha)


#Event 3. Washing Laundry 
Conc.h.2[3,]<-Conc.h.2[2,]*exp(-Inact.h*(Dur.wash-Contact.time.face.w))
Conc.l.2[3,]<-Conc.l.2[2,]/10^(Reduc.wash*(Dur.wash-Contact.time.face.w)/Dur.wash)
Dose.2[3,]<-Dose.2[2,]
Risk.2[3,]<-Risk.2[2,]

#Event 4. Loading washed clothes to dryer
Conc.h.2[4,]<-Conc.h.2[3,]*exp(-Inact.h*Contact.time.laundry)-{TE.wet*Frac.HS*
    (Conc.h.2[3,]*exp(-Inact.h*Contact.time.laundry)-Conc.l.2[3,]*exp(-Inact.s*Contact.time.laundry))}
Conc.l.2[4,]<-Conc.l.2[3,]*exp(-Inact.s*Contact.time.laundry)-{TE.wet*Frac.HS*A.hand/Surface.area.laundry*
    (Conc.l.2[3,]*exp(-Inact.s*Contact.time.laundry)-Conc.h.2[3,]*exp(-Inact.h*Contact.time.laundry))}
Dose.2[4,]<-Dose.2[3,]
Risk.2[4,]<-Risk.2[3,]

#Event 5. Hand Washing + Hand to face contact #2

Conc.h.2[5,]<-(1-TE.face*Frac.HF)*{(Conc.h.2[4,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.d)}
Conc.l.2[5,]<-Conc.l.2[4,]/10^(Reduc.dry*Contact.time.face.d/Dur.dry)
Dose.2[5,]<-Dose.2[4,]+((Conc.h.2[4,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.d)*TE.face*Frac.HF*A.hand)
Risk.2[5,]<- 1-(1+(Dose.2[5,]/beta))^(-alpha)

#Event 6. Drying Laundry 
Conc.h.2[6,]<-Conc.h.2[5,]*exp(-Inact.h*(Dur.dry-Contact.time.face.d))
Conc.l.2[6,]<-Conc.l.2[5,]/10^(Reduc.dry*(Dur.dry-Contact.time.face.d)/Dur.dry)
Dose.2[6,]<-Dose.2[5,]
Risk.2[6,]<-Risk.2[5,]

#Event 7. Dryer to folding area 
Conc.h.2[7,]<-Conc.h.2[6,]*exp(-Inact.h*Contact.time.laundry)-{TE.dry*Frac.HS*(Conc.h.2[6,]*exp(-Inact.h*Contact.time.laundry)-Conc.l.2[6,]*exp(-Inact.s*Contact.time.laundry))}
Conc.l.2[7,]<-Conc.l.2[6,]*exp(-Inact.s*Contact.time.laundry)-{TE.dry*Frac.HS*A.hand/Surface.area.laundry*(Conc.l.2[6,]*exp(-Inact.s*Contact.time.laundry)-Conc.h.2[6,]*exp(-Inact.h*Contact.time.laundry))}
Dose.2[7,]<-Dose.2[6,]
Risk.2[7,]<-Risk.2[6,]

#Event 8. Hand to face Contact #3
Conc.h.2[8,]<-(1-TE.face*Frac.HF)*(Conc.h.2[7,]*exp(-Inact.h*Contact.time.face.f))
Conc.l.2[8,]<-Conc.l.2[7,]*exp(-Inact.s*Contact.time.face.f)
Dose.2[8,]<-Dose.2[7,]+(Conc.h.2[7,]*exp(-Inact.h*Contact.time.face.f)*TE.face*Frac.HF*A.hand)
Risk.2[8,]<- 1-(1+(Dose.2[8,]/beta))^(-alpha)

#plotting
library(ggplot2)
library(ggpubr)

Conc.h.2.dataframe<-as.data.frame(t(Conc.h.2))
Conc.l.2.dataframe<-as.data.frame(t(Conc.l.2))
Dose.2.dataframe<-as.data.frame(t(Dose.2))
Risk.2.dataframe<-as.data.frame(t(Risk.2))


event<-rep(c(rep(1,iterations),rep(2,iterations),rep(3,iterations),rep(4,iterations),rep(5,iterations),rep(6,iterations),rep(7,iterations),rep(8,iterations)),4)
type<-c(rep("Hand",8*iterations),rep("Laundry",8*iterations),rep("Dose",8*iterations),rep("Risk",8*iterations))
value<-c(Conc.h.2.dataframe$`Hamper to washer`, Conc.h.2.dataframe$`Hand to face touch #1`,
         Conc.h.2.dataframe$`between face touch and end of washing cycle`,Conc.h.2.dataframe $`Washer to dryer`, 
         Conc.h.2.dataframe $`Hand to face touch #2`, Conc.h.2.dataframe $`Dry cycle`,
         Conc.h.2.dataframe $`Dryer to folding area`,Conc.h.2.dataframe $`Hand to face touch #3`,
         
         Conc.l.2.dataframe$`Hamper to washer`, Conc.l.2.dataframe$`Hand to face touch #1`,
         Conc.l.2.dataframe$`between face touch and end of washing cycle`,Conc.l.2.dataframe $`Washer to dryer`, 
         Conc.l.2.dataframe $`Hand to face touch #2`, 
         Conc.l.2.dataframe $`Dry cycle`,Conc.l.2.dataframe $`Dryer to folding area`,
         Conc.l.2.dataframe $`Hand to face touch #3`,
         
         Dose.2.dataframe$`Hamper to washer`, Dose.2.dataframe$`Hand to face touch #1`,
         Dose.2.dataframe$`between face touch and end of washing cycle`,Dose.2.dataframe $`Washer to dryer`,
         Dose.2.dataframe $`Hand to face touch #2`, Dose.2.dataframe $`Dry cycle`,Dose.2.dataframe $`Dryer to folding area`,
         Dose.2.dataframe $`Hand to face touch #3`,
         
         Risk.2.dataframe$`Hamper to washer`, Risk.2.dataframe$`Hand to face touch #1`,
         Risk.2.dataframe$`between face touch and end of washing cycle`,Risk.2.dataframe $`Washer to dryer`,
         Risk.2.dataframe $`Hand to face touch #2`, Risk.2.dataframe $`Dry cycle`,Risk.2.dataframe $`Dryer to folding area`,
         Risk.2.dataframe $`Hand to face touch #3`)

data<-data.frame(event,type,value)

windows()
ggplot(data)+geom_violin(aes(x=event,y=value,fill=type, group=event),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  facet_wrap(~type,scales="free") +
  scale_y_continuous(trans="log10")


##Hand washing scenario 3----------------------------------------------------

#Matrix 
numevents<-8
eventsname<-c("Hamper to washer","Hand to face touch #1", "between face touch and end of washing cycle", 
              "Washer to dryer","Hand to face touch #2","Dry cycle","Dryer to folding area",
              "Hand to face touch #3")

Conc.h.3<-matrix(nrow=numevents,ncol=iterations)
rownames(Conc.h.3)<-eventsname

Conc.l.3<-matrix(nrow=numevents, ncol=iterations)
rownames(Conc.l.3)<-eventsname

Dose.3<-matrix(nrow=numevents, ncol=iterations)
rownames(Dose.3)<-eventsname

Risk.3<-matrix(nrow=numevents, ncol=iterations)
rownames(Risk.3)<-eventsname


#Event 1. Loading dirty clothes from hamper to washer

Conc.h.3[1,]<-Conc.i.hand*exp(-Inact.h*Contact.time.laundry)-{TE.dry*Frac.HS*(Conc.i.hand*exp(-Inact.h*Contact.time.laundry)-Conc.onecloth*exp(-Inact.s*Contact.time.laundry))}
Conc.l.3[1,]<-Conc.i.laundry*exp(-Inact.s*Contact.time.laundry)-{TE.dry*Frac.HS*A.hand/Surface.area.laundry*(Conc.i.laundry*exp(-Inact.s*Contact.time.laundry)-Conc.i.hand*exp(-Inact.h*Contact.time.laundry))}
Dose.3[1,]<-0
Risk.3[1,]<-0

#Event 2. Hand washing + Hand to face contact #1

Conc.h.3[2,]<-(1-TE.face*Frac.HF)*{(Conc.h.3[1,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.w)}
Conc.l.3[2,]<-Conc.l.3[1,]/10^(Reduc.wash*Contact.time.face.w/Dur.wash)
Dose.3[2,]<-(Conc.h.3[1,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.w)*TE.face*Frac.HF*A.hand
Risk.3[2,]<-1-(1+(Dose.3[2,]/beta))^(-alpha)



#Event 3. Washing Laundry 
Conc.h.3[3,]<-Conc.h.3[2,]*exp(-Inact.h*(Dur.wash-Contact.time.face.w))
Conc.l.3[3,]<-Conc.l.3[2,]/10^(Reduc.wash*(Dur.wash-Contact.time.face.w)/Dur.wash)
Dose.3[3,]<-Dose.3[2,]
Risk.3[3,]<-Risk.3[2,]

#Event 4. Loading washed clothes to dryer
Conc.h.3[4,]<-Conc.h.3[3,]*exp(-Inact.h*Contact.time.laundry)-{TE.wet*Frac.HS*
    (Conc.h.3[3,]*exp(-Inact.h*Contact.time.laundry)-Conc.l.3[3,]*exp(-Inact.s*Contact.time.laundry))}
Conc.l.3[4,]<-Conc.l.3[3,]*exp(-Inact.s*Contact.time.laundry)-{TE.wet*Frac.HS*A.hand/Surface.area.laundry*
    (Conc.l.3[3,]*exp(-Inact.s*Contact.time.laundry)-Conc.h.3[3,]*exp(-Inact.h*Contact.time.laundry))}
Dose.3[4,]<-Dose.3[3,]
Risk.3[4,]<-Risk.3[3,]

#Event 5. Hand Washing + Hand to face contact #2

Conc.h.3[5,]<-(1-TE.face*Frac.HF)*{(Conc.h.3[4,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.d)}
Conc.l.3[5,]<-Conc.l.3[4,]/10^(Reduc.dry*Contact.time.face.d/Dur.dry)
Dose.3[5,]<-Dose.3[4,]+((Conc.h.3[4,]/10^Reduc.hwash)*exp(-Inact.h*Contact.time.face.d)*TE.face*Frac.HF*A.hand)
Risk.3[5,]<- 1-(1+(Dose.3[5,]/beta))^(-alpha)

#Event 6. Drying Laundry 
Conc.h.3[6,]<-Conc.h.3[5,]*exp(-Inact.h*(Dur.dry-Contact.time.face.d))
Conc.l.3[6,]<-Conc.l.3[5,]/10^(Reduc.dry*(Dur.dry-Contact.time.face.d)/Dur.dry)
Dose.3[6,]<-Dose.3[5,]
Risk.3[6,]<-Risk.3[5,]

#Event 7. Dryer to folding area 
Conc.h.3[7,]<-Conc.h.3[6,]*exp(-Inact.h*Contact.time.laundry)-{TE.dry*Frac.HS*(Conc.h.3[6,]*exp(-Inact.h*Contact.time.laundry)-Conc.l.3[6,]*exp(-Inact.s*Contact.time.laundry))}
Conc.l.3[7,]<-Conc.l.3[6,]*exp(-Inact.s*Contact.time.laundry)-{TE.dry*Frac.HS*A.hand/Surface.area.laundry*(Conc.l.3[6,]*exp(-Inact.s*Contact.time.laundry)-Conc.h.3[6,]*exp(-Inact.h*Contact.time.laundry))}
Dose.3[7,]<-Dose.3[6,]
Risk.3[7,]<-Risk.3[6,]

#Event 8. Hand to face Contact #3
Conc.h.3[8,]<-(1-TE.face*Frac.HF)*(Conc.h.3[7,]*exp(-Inact.h*Contact.time.face.f))
Conc.l.3[8,]<-Conc.l.3[7,]*exp(-Inact.s*Contact.time.face.f)
Dose.3[8,]<-Dose.3[7,]+(Conc.h.3[7,]*exp(-Inact.h*Contact.time.face.f)*TE.face*Frac.HF*A.hand)
Risk.3[8,]<- 1-(1+(Dose.3[8,]/beta))^(-alpha)

#plotting
library(ggplot2)
library(ggpubr)

Conc.h.3.dataframe<-as.data.frame(t(Conc.h.3))
Conc.l.3.dataframe<-as.data.frame(t(Conc.l.3))
Dose.3.dataframe<-as.data.frame(t(Dose.3))
Risk.3.dataframe<-as.data.frame(t(Risk.3))


event<-rep(c(rep(1,iterations),rep(2,iterations),rep(3,iterations),rep(4,iterations),rep(5,iterations),rep(6,iterations),rep(7,iterations),rep(8,iterations)),4)
type<-c(rep("Hand",8*iterations),rep("Laundry",8*iterations),rep("Dose",8*iterations),rep("Risk",8*iterations))
value<-c(Conc.h.3.dataframe$`Hamper to washer`, Conc.h.3.dataframe$`Hand to face touch #1`,
         Conc.h.3.dataframe$`between face touch and end of washing cycle`,Conc.h.3.dataframe $`Washer to dryer`, 
         Conc.h.3.dataframe $`Hand to face touch #2`, Conc.h.3.dataframe $`Dry cycle`,
         Conc.h.3.dataframe $`Dryer to folding area`,Conc.h.3.dataframe $`Hand to face touch #3`,
         
         Conc.l.3.dataframe$`Hamper to washer`, Conc.l.3.dataframe$`Hand to face touch #1`,
         Conc.l.3.dataframe$`between face touch and end of washing cycle`,Conc.l.3.dataframe $`Washer to dryer`, 
         Conc.l.3.dataframe $`Hand to face touch #2`, 
         Conc.l.3.dataframe $`Dry cycle`,Conc.l.3.dataframe $`Dryer to folding area`,
         Conc.l.3.dataframe $`Hand to face touch #3`,
         
         Dose.3.dataframe$`Hamper to washer`, Dose.3.dataframe$`Hand to face touch #1`,
         Dose.3.dataframe$`between face touch and end of washing cycle`,Dose.3.dataframe $`Washer to dryer`,
         Dose.3.dataframe $`Hand to face touch #2`, Dose.3.dataframe $`Dry cycle`,Dose.3.dataframe $`Dryer to folding area`,
         Dose.3.dataframe $`Hand to face touch #3`,
         
         Risk.3.dataframe$`Hamper to washer`, Risk.3.dataframe$`Hand to face touch #1`,
         Risk.3.dataframe$`between face touch and end of washing cycle`,Risk.3.dataframe $`Washer to dryer`,
         Risk.3.dataframe $`Hand to face touch #2`, Risk.3.dataframe $`Dry cycle`,Risk.3.dataframe $`Dryer to folding area`,
         Risk.3.dataframe $`Hand to face touch #3`)

data<-data.frame(event,type,value)

windows()
ggplot(data)+geom_violin(aes(x=event,y=value,fill=type, group=event),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  facet_wrap(~type,scales="free") +
  scale_y_continuous(trans="log10")


#Sensitivity Analysis---------------------------------------------------

spear.Ecol<-data.frame(A.hand, Surface.area.laundry, Frac.HS, Frac.HF, Item.laundry,
                       Contact.time.laundry, Contact.time.face.w, Contact.time.face.d, Contact.time.face.f,
                       Reduc.wash, Reduc.dry, Reduc.hwash, TE.dry, TE.wet, TE.face,
                       Conc.feces, Mass.feces, Conc.onecloth, Inact.h, Inact.s, Risk.3[8,])  

spear.anal<-cor(spear.Ecol,method="spearman")

View(spear.anal)

library(openxlsx)
write.xlsx(spear.anal, sheetName="Ecol", file="Sensitivity.ecol.xlsx")

