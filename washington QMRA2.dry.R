# List of pathogens
organisms <- c("E.coli", "E.faecium", "Influenza A")

Conc_h_all.d <- data.frame()
Conc_l_all.d <- data.frame()
Dose_all.d   <- data.frame()
Risk_all.d   <- data.frame()

#For loop for changing organisms 

for (org in organisms) {
  organism <- org
  source("Washington_parameter.R")
  
  ##Run simulation
  
  ##Scenario 1. touching dry laundry to transfer to washer -------------------
  numevents<-2
  eventsname<-c("hamper to waser","Hand to face touch")
  
  Conc.h.b<-matrix(nrow=numevents,ncol=iterations)
  rownames(Conc.h.b)<-eventsname
  
  Conc.l.b<-matrix(nrow=numevents, ncol=iterations)
  rownames(Conc.l.b)<-eventsname
  
  Dose.b<-matrix(nrow=numevents, ncol=iterations)
  rownames(Dose.b)<-eventsname
  
  Risk.b<-matrix(nrow=numevents, ncol=iterations)
  rownames(Risk.b)<-eventsname
  
  ###Event 1. Loading dry clothes to washer
  Conc.h.current<-0
  Conc.l.current<-Conc.i.laundry
  
  for (i in 1:n.HL.dry) {
    Conc.h.current<-Conc.h.current-{TE.dry*Frac.HS*(Conc.h.current-Conc.l.current)}
    Conc.l.current<-Conc.l.current-{TE.dry*Frac.HS*A.hand/A.laundry*(Conc.l.current-Conc.h.current)}
  }
  
  
  Conc.h.b[1,]<- Conc.h.current
  Conc.l.b[1,]<- Conc.l.current
  Dose.b[1,]<-0
  Risk.b[1,]<-0
  
  ###Event 2. Hand to face contact 
  
  Conc.h.current<-Conc.h.b[1,]
  Dose<-Dose.b[1,]
  
  for (i in 1:n.HF.dry){
    Conc.h.current <-(1-TE.face*Frac.HF)*(Conc.h.current)
    Dose<-Dose+(Conc.h.current*TE.face*Frac.HF*A.hand)
  }
  
  Conc.h.b[2,]<- Conc.h.current
  Conc.l.b[2,]<-Conc.l.b[1,]
  Dose.b[2,]<-Dose
  Risk.b[2,]<-cal_risk(Dose.b[2,])
  

  
  ##Data pulling and wrapping as data.frame
  
  df_ch_b <- data.frame(
    value = as.vector(t(Conc.h.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  Conc_h_all.d <- rbind(Conc_h_all.d, df_ch_b)
  
  df_cl_b <- data.frame(
    value = as.vector(t(Conc.l.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  Conc_l_all.d <- rbind(Conc_l_all.d, df_cl_b)
  
  df_dose_b <- data.frame(
    value = as.vector(t(Dose.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  Dose_all.d <- rbind(Dose_all.d, df_dose_b)
  
  df_risk_b <- data.frame(
    value = as.vector(t(Risk.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  Risk_all.d <- rbind(Risk_all.d, df_risk_b)
  
}

#Summary Statistic extraction & save result CSV. files 
library(dplyr)

sum_by_micro_and_event <- function (df, value_name){
  df %>%
    group_by(microorganism, event) %>%
    summarize (
      mean = mean(value, na.rm=TRUE),
      sd = sd(value, na.rm=TRUE),
      min = min(value, na.rm=TRUE),
      max = max(value, na.rm=TRUE),
      median = median(value, na.rm=TRUE),
      .groups= "drop"
    ) %>%
    mutate(variable=value_name) %>%
    select(microorganism, event, variable, everything())
}


summary_Conc_h.d<-sum_by_micro_and_event(Conc_h_all.d, "Conc.h")
summary_Conc_l.d<-sum_by_micro_and_event(Conc_l_all.d, "Conc.l")
summary_Dose.d<-sum_by_micro_and_event(Dose_all.d, "Dose")
summary_Risk.d<-sum_by_micro_and_event(Risk_all.d, "Risk")

write.csv(summary_Conc_h.d, "summary_Conc_h_d.csv", row.names = FALSE)
write.csv(summary_Conc_l.d, "summary_Conc_l_d.csv", row.names = FALSE)
write.csv(summary_Dose.d,   "summary_Dose_d.csv", row.names = FALSE)
write.csv(summary_Risk.d,   "summary_Risk_d.csv", row.names = FALSE)


#plotting
library(ggplot2)
library(ggpubr)
library(scales)


risk_result<-Risk_all.d %>% filter(event=="Hand to face touch")

windows()
ggplot(risk_result, aes(x=microorganism, y=value, fill=microorganism))+
  geom_violin (trim=FALSE,alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  geom_boxplot(width =0.1, outlier.shape=NA, position=position_dodge(0.9))+
  scale_y_log10(breaks=c( 1e+1, 1e-2, 1e-5, 1e-8, 1e-11, 1e-14, 1e-17), labels = label_log(base=10, signed=NULL)) +
  #scale_y_continuous(trans="log10" , breaks=c( 1e+1, 1e-2, 1e-5, 1e-8, 1e-11, 1e-14, 1e-17),label=c(1, -2, -5, -8, -11, -14, -17)) +
  labs(title="Infection risk during the transfer of dry laundry to the washing machine", x= "Pathogen", y="Infection Risk") #+
#facet_wrap(~microorganism,scales="free")


ggsave("Risk_dry.v2.tiff", dpi=600, dev='tiff', height=6, width=6, units="in")

