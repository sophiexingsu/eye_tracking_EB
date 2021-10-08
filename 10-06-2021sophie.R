# explore the relationship betwen eye-features and event boundaries 
# inspried by Jeff Zacks's binning code (bin.fixreport.s160203.r)
master=read.table("~/Box/DCL_ARCHIVE/Documents/Events/exp108_PTSD_prediction_mechanisms/Pilot1/Data/2014-02-25_All_pe108_FixationReport.txt",header=TRUE,sep = '\t')
seg=read.table("Data/2013-12-02_SegDataRaw.txt",header=TRUE)
#rawfix<- master[master$VIDEO_NAME_END != "." & (master$CURRENT_FIX_MSG_TEXT_1 %in% c("Passive Passive", "Task Large", "Task Small")),] 
#for this project, I am just looking at passive looking data 
rawfix<- master[master$VIDEO_NAME_END != "." & (master$CURRENT_FIX_MSG_TEXT_1=="Passive Passive"),] 
X_current<-NA
X_eye<-NA
X_between<-NA
X_Next<-NA
X_eye_Next<-NA
theta<-NA
x_offset <- 713.195
#Distance in pixels from (x0,y0) to bottom of screen:
y_offset <- 768
#coversion from pixel to mm in horizontal screen direction:
pixel.h <-.402
#conversion from pixel to mm in vertical screen direction:
pixel.v <- .337  
#Distance in mm from eye to screen right in front of eye
straight.eye.distance  <- 577
for (i in 1:(length(rawfix$CURRENT_FIX_X)-1)) {
  #Grab data from current fixation
  current_fix_x <- as.numeric(as.character(rawfix$CURRENT_FIX_X[i]))
  current_fix_y <- as.numeric(as.character(rawfix$CURRENT_FIX_Y[i]))
  
  current_fix_x_new <- current_fix_x-x_offset
  current_fix_y_new <- current_fix_y-y_offset
  
  #Convert to mm from pixels
  current_fix_x_new <- current_fix_x_new*pixel.h
  current_fix_y_new <- current_fix_y_new*pixel.v
  
  
  #Grab data from next fixation
  next_fix_x <- as.numeric(as.character(rawfix$CURRENT_FIX_X[i+1]))
  next_fix_y <- as.numeric(as.character(rawfix$CURRENT_FIX_Y[i+1]))
  
  next_fix_x_new <- next_fix_x-x_offset
  next_fix_y_new <- next_fix_y-y_offset
  
  #Convert to mm from pixels
  next_fix_x_new <- next_fix_x_new*pixel.h
  next_fix_y_new <- next_fix_y_new*pixel.v
  
  #Distance from (x0,y0) to current fix location
  d1 <- sqrt((current_fix_x_new-0)^2+(current_fix_y_new-0)^2)
  X_current[i]<-sqrt((current_fix_x_new-0)^2+(current_fix_y_new-0)^2)
  #Distance from eye to current fix location
  dd1 <- sqrt(straight.eye.distance^2 + d1^2)
  X_eye[i]<-sqrt(straight.eye.distance^2 + d1^2)
  
  #Distance from current fix to next fix
  d2 <- sqrt((next_fix_x_new-current_fix_x_new)^2 + (next_fix_y_new-current_fix_y_new)^2)
  X_between[i]<-sqrt((next_fix_x_new-current_fix_x_new)^2 + (next_fix_y_new-current_fix_y_new)^2)
  
  #Distance from (x0,y0) to next fix
  d3 <- sqrt((next_fix_x_new-0)^2 + (next_fix_y_new-0)^2)
  X_Next[i]<- sqrt((next_fix_x_new-0)^2 + (next_fix_y_new-0)^2)
  
  #Distance from eye to new fix
  dd2 <- sqrt(straight.eye.distance^2 + d3^2)
  X_eye_Next[i]<- sqrt(straight.eye.distance^2 + d3^2)
  
  #visual angle from current fix to next fix
  theta[i] <- acos(((dd1^2)+(dd2^2) - (d2^2))/(2*dd1*dd2)) * (360/(2*pi))
}
X_current<-append(X_current, NA)
X_between<-append(X_between, NA)
X_eye<-append(X_eye,NA)
X_Next<-append(X_Next,NA)
theta <- append(theta, NA)
X_eye_Next<-append(X_eye_Next,NA)
rawfix$X_eye_Next<-X_eye_Next
rawfix$theta <- theta
rawfix$X_Next<-X_Next
rawfix$X_eye<-X_eye
rawfix$X_between<-X_between
rawfix$X_current<-X_current
#change the format of the datasets   
rawfix=rawfix%>%
  rename(Moive=VIDEO_NAME_END,Grain=CURRENT_FIX_MSG_TEXT_1)%>%
  rename(SubjNum=RECORDING_SESSION_LABEL)
  mutate(Moive=recode_factor(Moive,"Breakfast"="Breakfast_Nonsocial_NoSound_X_XvidX.xvd", "Party"="Party_Nonsocial_NoSound_XvidX.xvd", "Wl"="wl_dv_aviconvertedX.xvd"))%>%
  #mutate(Grain=recode(Gran, "Fine"="Task Small","Passive"="Passive Passive","Coarse"="Task Large"))
  mutate(near_coarse=NA)%>%
  mutate(near_find=NA)
#figure out the nearest coarse or find boundaries 
  for (s in unique(rawfix$SubjNum)){
    for (a in unique(rawfix$Video))
      for (i in unique(rawfix$CURRENT_FIX_START))
      rawfix$near_coarse[rawfix$SubjNum==s & rawfix$Video==a & rawfix$CURRENT_FIX_START==i]=min(abs(rawfix$CURRENT_FIX_START[i][rawfix$SubjNum==s & rawfix$Video==a]-seg$MS[seg$SubjNum==s & seg$Movie==a&seg$Grain=="Corase"]))
  }
