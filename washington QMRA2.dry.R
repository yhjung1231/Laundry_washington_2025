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
  
  ##Scenario 1. touching dry laundry to put it into washer -------------------
  numevents<-2
  eventsname<-c("hamper to washer","Hand to face touch")
  
  Conc.h.b<-matrix(nrow=numevents,ncol=iterations)
  rownames(Conc.h.b)<-eventsname
  
  Conc.l.b<-matrix(nrow=numevents, ncol=iterations)
  rownames(Conc.l.b)<-eventsname
  
  Dose.b<-matrix(nrow=numevents, ncol=iterations)
  rownames(Dose.b)<-eventsname
  
  Risk.b<-matrix(nrow=numevents, ncol=iterations)
  rownames(Risk.b)<-eventsname
  
  ###Event 1. transfer dry clothes to washer
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
  
  Conc_h_all <- data.frame(
    value = as.vector(t(Conc.h.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  

  Conc_l_all <- data.frame(
    value = as.vector(t(Conc.l.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  
  Dose_all <- data.frame(
    value = as.vector(t(Dose.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  Risk_all  <- data.frame(
    value = as.vector(t(Risk.b)),
    event = rep(eventsname, each = iterations),
    microorganism = organism
  )
  
  
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


summary_Conc_h<-sum_by_micro_and_event(Conc_h_all, "Conc.h")
summary_Conc_l<-sum_by_micro_and_event(Conc_l_all, "Conc.l")
summary_Dose<-sum_by_micro_and_event(Dose_all, "Dose")
summary_Risk<-sum_by_micro_and_event(Risk_all, "Risk")

write.csv(summary_Conc_h, "summary_Conc_h.dry.csv", row.names = FALSE)
write.csv(summary_Conc_l, "summary_Conc_l.dry.csv", row.names = FALSE)
write.csv(summary_Dose,   "summary_Dose.dry.csv", row.names = FALSE)
write.csv(summary_Risk,   "summary_Risk.dry.csv", row.names = FALSE)


#plotting
library(ggplot2)
library(ggpubr)

risk_result<-Risk_all %>% filter(event=="Hand to face touch")

windows()
ggplot(risk_result, aes(x=microorganism, y=value, fill=microorganism))+
  geom_violin (trim=FALSE,alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  geom_boxplot(width =0.1, outlier.shape=NA, position=position_dodge(0.9))+
  scale_y_continuous(trans="log10" ) +
  labs(title="Infection risk during the transfer of dry laundry to the drying machine", x= "Pathogen", y="Infection Risk (log10)") #+
#facet_wrap(~microorganism,scales="free")

ggsave("Risk_dry.tiff", dpi=600, dev='tiff', height=6, width=8, units="in")

#, breaks=c( 1e+1, 1e-2, 1e-5, 1e-8, 1e-11, 1e-14, 1e-17),
#label=c(1, -2, -5, -8, -11, -14, -17)