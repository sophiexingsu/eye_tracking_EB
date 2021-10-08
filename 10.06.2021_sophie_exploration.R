#explore eye-tracking from the binned data 
#loading the enviornment 
load("~/Box/DCL_ARCHIVE/Documents/Events/exp108_PTSD_prediction_mechanisms/Pilot1/Data/2014-11-10_fixreportBinnedAll.RData") 
library(tidyverse)
library(ggplot2)
#combine segmentation data
seg=read.table("Data/2013-12-02_SegDataRaw.txt",header=TRUE) 
#combined and binned
string=paste(fixbins$SubjNum,fixbins$Movie,fixbins$Time)
string=paste(fixbins$SubjNum,fixbins$Movie,fixbins$Time,"Coarse")
string_1=paste(fixbins$SubjNum,fixbins$Movie,fixbins$Time,"Fine")
string=c(string,string_1)
seg_bp=data.frame(eid=string)
seg_bp=seg_bp%>%
  separate(.,eid,c("SubjNum","Movie","Time","Grain")," ")
seg_bp$Fine=0
seg_bp$Corase=0 
seg$binned_times=ceiling(seg$MS/1000)
seg$binned_times=ceiling(seg$MS/1000)
for (s in unique(seg_bp$SubjNum)){
  for (a in unique(seg_bp$Movie))
  seg_bp$Corase[seg_bp$SubjNum==s&seg_bp$Time%in%seg$binned_times[seg$SubjNum==s & seg$Movie==a &seg$Grain=="Coarse"]]=1
}
for (s in unique(seg_bp$SubjNum)){
  for (a in unique(seg_bp$Movie))
    seg_bp$Fine[seg_bp$SubjNum==s&seg_bp$Time%in%seg$binned_times[seg$SubjNum==s & seg$Movie==a &seg$Grain=="Fine"]]=1
} 
fixbins$eid=paste(fixbins$SubjNum,fixbins$Movie,fixbins$Time)
seg_bp$eid=paste(seg_bp$SubjNum,seg_bp$Movie,seg_bp$Time)
combined=left_join(fixbins,seg_bp,by="eid")
combined$corase=NA
combined$corase[combined$Corase==0]="Not Boundary"
combined$corase[combined$Corase==1]="Boundary"
## explore the relationship between boundaries and 
par(mfrow=c(1,2)) 
for(i in c("FixDuration.Passive","AvgVelocity.Passive")){
ggplot(combined, aes(x=i, fill=corase, color=corase)) +geom_histogram(position="identity")+theme(legend.position="top")
}
#ggplot(combined, des(x=SaccadeDistance.Passive,))
###exploration of the current situations
logit<-glm(Corase~FixDuration.Passive+SaccadeDistance.Passive+AvgVelocity.Passive+BlinkFreq.Passive+Time.x-1,data=combined,family=binomial)


