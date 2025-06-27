# List of pathogens
organisms <- c("E.coli", "E.faecium", "Influenza A")

Conc_h_all <- data.frame()
Conc_l_all <- data.frame()
Dose_all   <- data.frame()
Risk_all   <- data.frame()

#For loop for changing organisms 

for (org in organisms) {
  organism <- org
  source("Washington_parameter.R")
  
  ##Run simulation
  
  ##Scenario 1. touching wet laundry to put it into dryer (baseline, using china standard)-------------------
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
  
  ###Event 1. Loading washed clothes to dryer
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
  
  ###Event 2. Hand to face contact 
  
  Conc.h.current<-Conc.h.b[1,]
  Dose<-Dose.b[1,]
  
  for (i in 1:n.HF.wet){
    Conc.h.current <-(1-TE.face*Frac.HF)*(Conc.h.current)
    Dose<-Dose+(Conc.h.current*TE.face*Frac.HF*A.hand)
  }
  
  Conc.h.b[2,]<- Conc.h.current
  Conc.l.b[2,]<-Conc.l.b[1,]
  Dose.b[2,]<-Dose
  Risk.b[2,]<-cal_risk(Dose.b[2,])

  ##Scenario 2. touching wet laundry to put it into dryer (Using White)-------------------
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
  
  ###Event 1. Loading washed clothes to dryer
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
  
  ###Event 2. Hand to face contact 
  
  Conc.h.current<-Conc.h.i[1,]
  Dose<-Dose.i[1,]
  
  for (i in 1:n.HF.wet){
    Conc.h.current <-(1-TE.face*Frac.HF)*(Conc.h.current)
    Dose<-Dose+(Conc.h.current*TE.face*Frac.HF*A.hand)
  }
  
  Conc.h.i[2,]<- Conc.h.current
  Conc.l.i[2,]<-Conc.l.i[1,]
  Dose.i[2,]<-Dose
  Risk.i[2,]<-cal_risk(Dose.i[2,])
  
  
  ##Data pulling and wrapping as data.frame
  
  df_ch_b <- data.frame(
    value = as.vector(t(Conc.h.b)),
    event = rep(eventsname, each = iterations),
    scenario = "Baseline",
    microorganism = organism
  )
  df_ch_i <- data.frame(
    value = as.vector(t(Conc.h.i)),
    event = rep(eventsname, each = iterations),
    scenario = "Intervention",
    microorganism = organism
  )
  Conc_h_all <- rbind(Conc_h_all, df_ch_b, df_ch_i)
  
  df_cl_b <- data.frame(
    value = as.vector(t(Conc.l.b)),
    event = rep(eventsname, each = iterations),
    scenario = "Baseline",
    microorganism = organism
  )
  df_cl_i <- data.frame(
    value = as.vector(t(Conc.l.i)),
    event = rep(eventsname, each = iterations),
    scenario = "Intervention",
    microorganism = organism
  )
  Conc_l_all <- rbind(Conc_l_all, df_cl_b, df_cl_i)
  
  df_dose_b <- data.frame(
    value = as.vector(t(Dose.b)),
    event = rep(eventsname, each = iterations),
    scenario = "Baseline",
    microorganism = organism
  )
  df_dose_i <- data.frame(
    value = as.vector(t(Dose.i)),
    event = rep(eventsname, each = iterations),
    scenario = "Intervention",
    microorganism = organism
  )
  Dose_all <- rbind(Dose_all, df_dose_b, df_dose_i)
  
  df_risk_b <- data.frame(
    value = as.vector(t(Risk.b)),
    event = rep(eventsname, each = iterations),
    scenario = "Baseline",
    microorganism = organism
  )
  df_risk_i <- data.frame(
    value = as.vector(t(Risk.i)),
    event = rep(eventsname, each = iterations),
    scenario = "Intervention",
    microorganism = organism
  )
  Risk_all <- rbind(Risk_all, df_risk_b, df_risk_i)

}

#Summary Statistic extraction 
library(dplyr)

sum_by_micro_and_event <- function (df, value_name){
  df %>%
    group_by(microorganism, event, scenario) %>%
    summarize (
      mean = mean(value, na.rm=TRUE),
      sd = sd(value, na.rm=TRUE),
      min = min(value, na.rm=TRUE),
      max = max(value, na.rm=TRUE),
      median = median(value, na.rm=TRUE),
      .groups= "drop"
    ) %>%
    mutate(variable=value_name) %>%
    select(microorganism, scenario, event, variable, everything())
}

summary_Conc_h<-sum_by_micro_and_event(Conc.h)


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


