library(tidyr)
library(dplyr)
library(readxl)

### LOAD DATA, EXTRACT COLUMNS NEEDED FROM ECOTAXA OUTPUT #######################

#ecotaxa_export <- 
  #read.delim("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/ecotaxa_export_M3_M13_.txt", header=TRUE) #MANUALLY SET THE PATH

ecotaxa_export <- read_excel("ecotaxa_export_M3_M13_QC_HG.xlsx")

sub1<-ecotaxa_export[,c(1,6,7,8,9,10,11,12,13)]#USE THIS CODE IF USING THE DATASET FROM SUPPLEMENTARY DATA 2
#sub1<-ecotaxa_export[,c(1,7,8,15,18,33:34,98,133)] #USE THIS CODE IF PULING FROM AN ECOTAXA DATASET
names(sub1)=c("Label","Min_depth","Max_depth","L_D","area","major","minor",
              "Tow_Vol","Sub_part")
sub1$num<-gsub(".*_","",sub1$Label)
sub1<-separate(sub1,Label,
         c("cruise","moc","net","fraction"), extra="drop", remove=FALSE)

### SELECT WHICH MOCNESS YOU WILL BE ANALYZING. YOU NEED TO RUN SUB1, SUB2, SUB3 FOR EACH MOC YOU WANT TO ANALYZE ################

sub1$moc<-as.factor(sub1$moc)
levels(sub1$moc)
sub2<-filter(sub1, moc == "m8") # SELECT MOCNESS NUM FROM LEVEL LIST # e.g. M8
sub2$moc=factor(sub2$moc)
str(sub2)
H<-'AE1712_M8 (July, Day)'  # INSERT CRUISE/TOW INFORMATION e.g. AE1712_M8 (July, Day) 
Tow<-'M8'                        # INSERT TOW NUMBER # e.g. M8  
DayNight<-'D'                     # INSERT N FOR NIGHT, D FOR DAY 
Tlab<-(c("8.0145","10.8157","14.3465","17.1593","18.6638", 
         "19.1972","20.1019","24.79898"))


# INSERT MEAN NET TEMPERATURES IN CELCIUS FOR SELECTED TOW n1-->n8
# FOR ACTIVE FLUX CALCUALTIONS AT A SPECIFIC MIDWATER TEMPERATURE PUT THE DESIRED TEMPERATURE FOR ALL DEPTHS 

### REFORMATTING DATA, WILL NEED TO BE RUN FOR EACH MOC YOU WANT TO ANALYZE ##########################################

{sub2$net<-as.factor(sub2$net)
sub2$fraction<-as.factor(substr(sub2$fraction, 1,2))
sub2$area_mm2<-sub2$"area"*0.00002809
sub2$major_mm<-sub2$"major"*.0053
sub2$minor_mm<-as.numeric(sub2$"minor"*.0053)
sub2$vol<-(4/3)*pi*((sub2$minor_mm*0.5)^2)*(sub2$major_mm/2)
sub2$logvol<-as.numeric(log10(sub2$vol))
sub2$DW<-(0.0267*(sub2$vol))
sub2$hdif<-(sub2$Max_depth-sub2$Min_depth)
sub2$split<-1/(sub2$Sub_part)
sub2$L_D<-as.factor(ifelse(sub2$L_D=="not-living","not-living","living"))
sub2$D_N<-as.factor(rep(DayNight, length(sub2$L_D)))
NetList<-c(levels(sub2$net))
lookup<-as.data.frame(cbind(Tlab,NetList))
sub2$temp<-as.numeric(as.character(with(lookup,Tlab[match(sub2$net,NetList)])))
sub2$O2_umol<-(exp(-0.339+(0.801*log(sub2$DW)))+0.069*(sub2$temp))/22.4
sub2$N_mg<-(exp(-3.246+(0.735*log(sub2$DW)))+0.069*(sub2$temp))
sub2$CO2<-(sub2$O2_umol)*0.87
sub2$DOC<-(sub2$CO2)*0.31
sub2$mort<-(1/24)*(sub2$DW)*0.4*0.00526*((sub2$DW)/1000)^-0.25}

sub3<-sub2[,c(1,14,2:5,15:20,24:29,22,12,6:7,21,23,8)]
head(sub3)

write.csv(sub3,file=paste ##MANUALLY SET THE PATH. cREATE FOLDER IN ADVANCE
          ("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/",Tow,"_FOR_R.csv", sep=""), 
          row.names=F)

### At this point you can run the rest of this code either off of sub3 (.CSV) or
### from the All_Tows_for_R excel sheet (to do this, save each "_FOR_R.csv" as 
### a separate sheet named M#)

library(ggplot2)
library(LaCroixColoR)
library(readxl)

# you need to manually set this path. Either the .csv from sub3 or an EXCEL 
# that has all of the sub3 outputs in one .xlsx file
All_Tows_for_R <- read_excel("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/M3_FOR_R.xlsx", 
                             #If you are using the .xlsx you need this line to 
                             #tell it which sheet to use 
                             sheet = "M3_FOR_R", guess=1000000)
M<-All_Tows_for_R
M$cruise<-as.factor(M$cruise)
M$moc<-as.factor(M$moc)
M$net<-as.factor(M$net)
M$fraction<-as.factor(M$fraction)
M$D_N<-as.factor(M$D_N)
M$L_D<-as.factor(M$L_D)
M$Tow_Vol<-as.numeric(M$Tow_Vol)
H<-'AE1614_M3 (July, Night)'     # Fill in sample info if not saved from above                
Tow<-'M3'                      # INSERT TOW NUMBER if not saved above

thewholeshabang<-function(df, na.rm=TRUE){
  

  ## Taking out the "Huge" size class and the non-living particles
  ## get rid of this code if you want to keep this information ####
  M3<-filter(M, fraction !="d1")
  M3$fraction<-factor(M3$fraction)
  Mdrop<-filter(M3, L_D !="not-living")
  Mdrop$L_D<-factor(Mdrop$L_D)
  

  ## Creating bins ####################################################
  L<-as.numeric(c(seq(-3.00, 3.00, by=0.25)))
  B<-as.character(c(10^L))
  Mdrop$bin<-cut(Mdrop$logvol, breaks=c(seq(-3.00, 3.25, by=0.25)), labels=B)
  Mdrop$bin<-factor(Mdrop$bin)
  Mdrop$Nbin<-as.numeric(as.character(Mdrop$bin))
  Mdrop$net<-as.factor(Mdrop$net)#numeric bins for conversions

  # This generates a table by net>fraction>bin that with columns for
  # the frequency (freq), tow volume (tv), normalized frequency (Nfreq),
  # biovolume/class (BV_C), and normalized biovolume (mm3 per m3)
  
  summary<-Mdrop%>%group_by(net,fraction,bin)%>%
    summarize(count=n(),depth=(mean(Min_depth)+mean(Max_depth))/2,hdif=median(hdif),split=median(split),freq=count/split,
              tv=mean(Tow_Vol), NFreq_m3=freq/tv, bin2=mean(Nbin),
              NBV_m3=(sum(vol)/split/tv),BM_m3=(sum(DW)/split/tv), oxy_m3=(sum(O2_umol))/tv/split,
              nit_m3=(sum(N_mg)/tv/split), CO2_m3=(sum(CO2)/tv/split), DOC_m3=(sum(DOC)/tv/split), Mort_m3=(sum(mort)/tv/split),
              Density_m2=(freq/tv*hdif),NBV_m2=NBV_m3*hdif, BM_m2=BM_m3*hdif, oxy_m2=oxy_m3*hdif,
              nit_m2=nit_m3*hdif, CO2_m2=CO2_m3*hdif, DOC_m2=DOC_m3*hdif, Mort_m2=Mort_m3*hdif) 
  
  summary
  head(summary)
  
  ### This writes two CSV files- one by bin and one by net ###################################################
  ### Set the paths and make sure you make the three folders 
  ### (MOC by Bin, MOC by Net, Images) in advance
  write.csv(summary,file=paste("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Bin/",Tow,"_bins.csv", sep=""), row.names = F)
  
  netsum<-summary%>%group_by(net)%>%summarize(Tot_BV_m3=sum(NBV_m3),Tot_BM_m3=sum(BM_m3), Tot_Ox_m3=sum(oxy_m3),
                  Tot_N_m3=sum(nit_m3),Tot_CO2_m3=sum(CO2_m3),Tot_DOC_m3=sum(DOC_m3),
                  Tot_Mort_m3=sum(Mort_m3),
                  Tot_BV_m2=sum(NBV_m2),Tot_BM_m2=sum(BM_m2), Tot_Ox_m2=sum(oxy_m2),
                  Tot_N_m2=sum(nit_m2),Tot_CO2_m2=sum(CO2_m2),Tot_DOC_m2=sum(DOC_m2),
                  Tot_Mort_m2=sum(Mort_m2),med_depth=mean(depth))
  as.data.frame(netsum)
  #set your path here
  write.csv(netsum,file=paste("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Net/",Tow,"_nets.csv", sep=""),row.names=F)
  
  # Creating a series of plots for each net
  # ggsave lines save plots automatically in folder on the desktop
  

  ggplot(data=summary)+
    geom_bar(aes(x=bin, y=NFreq_m3, fill=fraction), stat="identity", 
             position=position_dodge2(preserve='single'))+
    facet_wrap(~net, ncol=2, scales="free_y")+      
    scale_fill_manual(values=lacroix_palette("Apricot", n=2, type="discrete" ), labels=c("d2", "d3"))+
    labs(x=expression("Volume Bin" ~(mm^3)), y=expression("Normalized frequency"~(count~m^-2)),fill="Size Class")+
    ggtitle(paste(H,'Normalized Frequency'))+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_discrete(breaks=c("0.001", "0.01", "0.1", "1","10", "100"))+
    theme(panel.background=element_blank(),axis.line = element_line(colour = "black"))+
    ylim(0,470)
  #Set your path here
  ggsave(filename=paste(Tow,"Freq.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Images")
  
  ggplot(data=summary)+
    geom_bar(aes(x=bin, y=NBV_m3, fill=fraction), stat="identity", 
             position=position_dodge2(preserve='single'))+
    facet_wrap(~net, ncol=2, scales="free_y")+      
    scale_fill_manual(values=lacroix_palette("Apricot", n=2, type="discrete" ), labels=c("d2", "d3"))+
    labs(x=expression("Volume Bin"~ (mm^3)), y=expression("Sum Normalized BioVol" ~(mm^3*m^-3)),fill="Size Class")+
    ggtitle(paste(H,'Sum Normalized Biovolume'))+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_discrete(breaks=c("0.001", "0.01", "0.1", "1","10", "100"))+
    theme(panel.background=element_blank(),axis.line = element_line(colour = "black"))+
    ylim(0,54)
  #set your path here
  ggsave(filename=paste(Tow,"BV.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Images")
  
  
  ### BIOMASS SuMMARY
  ggplot(data=(summary), aes(x=net, y=BM_m2, fill="#D72000", alpha=fraction))+geom_bar(stat="identity")+
    scale_fill_manual(values=c("#D72000"))+
    labs(x="Net", y=expression("Biomass (Dry Weight)" ~(mg~m^-2)), title=paste(H,"Total Biomass by Net"))+
    theme(plot.title=element_text(face="bold",hjust=0.5))+
    scale_alpha_discrete(range=c(0.5,1))+
    guides(fill=FALSE)+
    ylim(0,260)
  #set your path here
  ggsave(filename=paste(Tow,"BM_bynet.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Images", width=6, height=6, units="in", dpi=300)

  ### OXYGEN USE SUMMARY
  ggplot(data=(summary), aes(x=net, y=oxy_m2, fill="#FFAD0A", alpha=fraction))+geom_bar(stat="identity")+
    scale_fill_manual(values="#FFAD0A")+
    labs(x="Net", y=expression(mu*mol~O[2]*m^-2*h^-1), title=paste(H,"Apparent Oxygen Utilization by Net"))+
    theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
    scale_alpha_discrete(range=c(0.5,1))+
    guides(fill=FALSE)+
    ylim(0,4500)
  #set your path here
  ggsave(filename=paste(Tow,"AOU_bynet.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Images", width=6, height=6, units="in", dpi=300)
  
 ### NITROGEN SUMMARY ###
   ggplot(data=(summary), aes(x=net, y=nit_m2, fill="#1BB6AF", alpha=fraction))+geom_bar(stat="identity")+
    scale_fill_manual(values="#1BB6AF")+
    labs(x="Net", y=expression(mu*g~NH[4]~m^-2*h-1), title=paste(H,"Apparent Ammonium Production by Net"))+
    theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
    scale_alpha_discrete(range=c(0.5,1))+
    guides(fill=FALSE)+
    ylim(0,101000)
   #set your path here
  ggsave(filename=paste(Tow,"Nitrogen_bynet.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Images", width=6, height=6, units="in", dpi=300)
  
}
thewholeshabang(M) #run the thang

### WATERFALL PLOTS, REQUIRES sub3 TO BE COMPLETE ############################################
library(ggpubr)
library(dplyr)
#set your path here, SELECTING THE M# THAT YOU WANT
WF<-read.csv("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Bin/M#_bins.csv")
WF$bin<-as.factor(WF$bin)
WF_sub<-filter(WF,(bin2>0.01 & bin2<100)) #SETS THE CUTOFF FOR BV PLOTTED
WF_sub2<-WF_sub%>%group_by(net,bin)%>%summarize(NFreq_m3=sum(NFreq_m3),Abund_m2=sum(Density_m2))
# FILLS IN THE DEPTH INTERVALS FOR THE SPECIFIC MOCNESS
net_labs<-c("0-40 m","40-100 m","100-150 m","150-200 m", "200-300 m",
                         "300-400 m","400-550 m", "550-600 m")
#NAME OF THE MOCNESS
Title<-"November 2019 (Night)"

#M3-M4      c("0-50 m","50-225 m","225-300 m","300-450 m", "450-600 m",
#             "600-750 m","750-900 m", "900-1000 m")
#M8-M9      c("0-50 m","50-200 m","200-275 m","275-400 m", "400-550 m",
#             "550-700 m","700-850 m", "850-1000 m")
#M10-M11    c("0-50 m","50-175 m","175-250 m","250-400 m", "400-550 m",
#             "550-700 m","700-850 m", "850-1000 m")
#M12-M13    c("0-50 m","50-200 m","200-300 m","300-400 m", "400-550 m",
#             "550-700 m","700-900 m", "900-1000 m")
#M14-M16    c("0-30 m","30-60 m","60-90 m","90-120 m", "120-150 m",
#             "150-180 m","180-220 m", "220-260 m")
#M17        c("0-40 m","40-75 m","75-150 m","150-200 m", "200-300 m",
#             "300-400 m", "400-500 m", "500-600 m")
#M18-M19    c("0-40 m","40-100 m","100-150 m","150-200 m", "200-300 m",
#             "300-400 m","400-550 m", "550-600 m")

WF_sub2$binN<-as.numeric(as.character(WF_sub2$bin))
WFplot<-ggplot(data=WF_sub2, aes(x=binN, y=NFreq_m3,color=net))+
  geom_point(size=2)+
  geom_smooth(se=F)+
  stat_regline_equation(aes(label=paste(..eq.label..,..rr.label..,sep="~~~~")),
                        show.legend=F,label.x.npc=0.7,label.y.npc=0.8, size=3)+
  scale_color_manual(values=(lacroix_palette("PeachPear", type="continuous", n=8)), labels=rev(net_labs))+
  labs(x=expression("Size class"~(mm^3)), y=expression("Density"~(particles~m^-3)),
       title=paste(Title), color="")+
  guides(color=guide_legend(reverse=T))+
  scale_y_log10(limits=c(1e-3,1e3))+
  scale_x_log10(limits=c(0.01,10000), #SETS THE CUTOFF FOR BV PLOTTED
                breaks=c(0.01,0.1,1,10,100),
                  labels=c("0.01","0.1","1","10","100"))+
  coord_cartesian(clip='off')+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=14),
        legend.position='bottom')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
WFplot
#set your path here, MAKE A FOLDER CALLED "Waterfall_plots" 
ggsave(WFplot,filename=paste(Title,"Waterfall.png", sep=" "), path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Waterfall_plots", width=9, height=5, units="in" )


### RE-RUNNING EVERYTHING... still need to figure out how to change titles ###

tab_names<-excel_sheets(path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/All Tows for R.xlsx")
lapply(tab_names, function(x)
  thewholeshebang(path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/All Tows for R.xlsx",
                  sheet=x))


## Day-Night comparison ############################
#CREATE A SET OF FOLDERS IN YOUR SAVE LOCATION CALLED /Day_Night Plots/DayNight Data/
library(gridExtra)

#SET YOUR PATH HERE, PICKING THE DAY OF A PAIR
day<-read.csv("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Net/M4_nets.csv")
#SET YOUR PATH HERE, PICKING THE NIGHT OF A PAIR
night<-read.csv("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Net/M3_nets.csv")

I="July 2016" #SET THE NAME OF THE PAIR
DayNight<-data.frame("BV_Mig"=c(abs(day$Tot_BV_m3-night$Tot_BV_m3)), "BV_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_BV_m3,night$Tot_BV_m3)))),
                    "BM_Mig"=c(abs(day$Tot_BM_m2-night$Tot_BM_m2)), "BM_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_BM_m2,night$Tot_BM_m2)))),
                     "Ox_Mig"=c(abs(day$Tot_Ox_m2-night$Tot_Ox_m2)), "Ox_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_Ox_m2,night$Tot_Ox_m2)))),
                     "NH4_Mig"=c(abs(day$Tot_N_m2-night$Tot_N_m2)), "NH4_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_N_m2,night$Tot_N_m2)))),
                     "DOC_Mig"=c(abs(day$Tot_DOC_m2-night$Tot_DOC_m2)), "DOC_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_DOC_m2,night$Tot_DOC_m2)))),
                    "Mort_Mig"=c(abs(day$Tot_Mort_m2-night$Tot_Mort_m2)), "Mort_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_Mort_m2,night$Tot_Mort_m2)))),
                     "BM_DVM"=c((day$Tot_BM_m2-night$Tot_BM_m2)),"BM_day"=c(day$Tot_BM_m2),"BM_night"=c(night$Tot_BM_m2),
                     "BV_DVM"=c((day$Tot_BV_m3-night$Tot_BV_m3)),
                     "Med_Depth"=as.factor(apply(as.data.frame(cbind(day$med_depth,night$med_depth)),1,FUN=mean)),
                     "Net"=c(day$net))
DN2<-data.frame("Net"=as.factor(c(1:8,1:8)),"M_R"=as.factor(c(rep.int("M",8), rep.int("R",8))),"BM"=c(DayNight$BM_Mig,DayNight$BM_Res), "BV"=c(DayNight$BV_Mig,DayNight$BV_Res),
                "Ox"=c(DayNight$Ox_Mig,DayNight$Ox_Res), "NH4"=c(DayNight$NH4_Mig,DayNight$NH4_Res),
                "DOC"=c(DayNight$DOC_Mig,DayNight$DOC_Res), "Mort"=c(DayNight$Mort_Mig,DayNight$Mort_Res),
                "Med_Depth"=as.factor(rep.int(DayNight$Med_Depth, 2)))
DN2
write.csv(DN2,file=paste("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/DayNight Data/",I,"_DayNightNETS.csv",sep=""),row.names=FALSE)

#INCLUDE THE DEPTH INTERVALS ASSOCIATED WITH YOUR TOW
d_labs=c("900","750","600","450","300","225","50","0")
## 3/4    c("900","750","600","450","300","225","50","0")
## 8/9    c("850","700","550","400","275","200","50","0")
## 10/11  c("850","700","550","400","250","175","50","0")
## 12/13  c("900","700","550","400","300","200","50","0")
## 14-16  c("220","180","150","120","90","60","30","0")
## 17     c("500","400","300","200","150","75","40","0")
## 18/19  c("550","400","300","200","150","100","40","0")



plot1<-ggplot(data=DN2, aes(x=Med_Depth, y=BM,fill="#D72000", alpha=M_R))+
  scale_fill_manual(values=("#D72000"))+scale_alpha_discrete(range=c(0.5,1), labels=c("Migratory","Resident"), drop=FALSE)+
  geom_col(position="stack", na.rm=FALSE)+coord_flip()+
  labs(x="Minimum Net Depth (m)", y=expression("mg Biomass"~m^-2),
       title="Dry Weight Biomass", alpha="")+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
  guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position='bottom')+
  ylim(c(0,300))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs, drop=FALSE)
plot2<-ggplot(data=DN2, aes(x=Med_Depth, y=Ox, fill="#FFAD0A", alpha=M_R))+
  scale_fill_manual(values=("#FFAD0A"))+scale_alpha_discrete(range=c(0.5,1), labels=c("Migratory","Resident"))+
  geom_col(position="stack")+coord_flip()+
  labs(x="Minimumn Net Depth (m)", y=expression(mu*mol~O[2]~m^-2*h^-1),
       title="Apparent Oxygen Usage", alpha="")+
  guides(fill=FALSE, alpha=FALSE)+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylim(c(0,4000))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)
plot3<-ggplot(data=DN2, aes(x=Med_Depth, y=NH4, fill="1BB6AF", alpha=M_R))+
  scale_fill_manual(values=("#1BB6AF"))+scale_alpha_discrete(range=c(0.5,1), labels=c("Migratory","Resident"))+
  geom_col(position="stack")+coord_flip()+
  guides(fill=FALSE, alpha=FALSE)+
  labs(x="Minimum Net Depth (m)", y=expression(mu*g~NH[4]~m^-2*h^-1),
       title="Apparent Ammonium Production", alpha="")+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylim(c(0,80000))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)
plot4<-ggplot(data=DN2, aes(x=Med_Depth, y=Mort, fill="#132157", alpha=M_R))+
  scale_fill_manual(values=("#132157"))+scale_alpha_discrete(range=c(0.5,1), labels=c("Migratory","Resident"))+
  geom_col(position="stack")+coord_flip()+
  labs(x="Minimum Net Depth (m)", y=expression(m*mol~C~m^-2*h^-1),
       title="Mortality", alpha="")+
  guides(fill=FALSE, alpha=FALSE)+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylim(c(0,0.6))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot1)

p1<-grid.arrange(arrangeGrob(plot1+theme(legend.position="none"),plot2,plot3,plot4, ncol=2, top=paste(I,"Sum Migrators and Resident Zooplankton")),
                   mylegend, nrow=2,heights=c(10, 1))
#sET YOUR PATH
ggsave(p1,filename=paste(I,"FourPlot.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/DayNight Data", width=8, height=7, units="in", dpi=300)


plot5<-ggplot(data=DayNight, aes(x=Med_Depth, y=BM_DVM, fill="#D72000"))+
  geom_col()+coord_flip()+
  labs(x="Median Net Depth (m)", y=expression("Biomass"~mg~m^-2),
       title="Biomass (Dry Weight)")+
  scale_fill_manual(values="#D72000")+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)+
  ylim(c(-100,100))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill=FALSE)
plot6<-ggplot(data=DayNight, aes(x=Med_Depth, y=BV_DVM, fill="#EE6100"))+
  geom_col()+coord_flip()+
  labs(x="Median Net Depth (m)", y=expression('Biovolume'~(mm^3*m^-3)),
       title="Biovolume")+
  scale_fill_manual(values="#EE6100")+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)+
  ylim(c(-50,50))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill=FALSE)

p2<-grid.arrange(plot5,plot6, ncol=2, top=paste(I,"Diel Vertical Migration"))
ggsave(p2,filename=paste(I,"DVM_Butterfly.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/DayNight Data/", width=8, height=7, units="in", dpi=300)

### HEAT PLOTS #########################################################
### CREATE A SET OF FOLDERS IN YOUR SAVE LOCATION CALLED /Heatmaps/Heatmap data/
library(RColorBrewer)
library(colorspace)


#SET YOUR PATH HERE, PICKING THE DAY OF A PAIR
day<-read.csv("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Net/M#_nets.csv")
#SET YOUR PATH HERE, PICKING THE NIGHT OF A PAIR
night<-read.csv("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/MOC by Net/M#_nets.csv")

J<-"July 2018" #SET THE NAME OF THE PAIR
#INCLUDE THE DEPTH INTERVALS ASSOCIATED WITH YOUR TOW
d_labs=c("850","700","550","400","250","175","50","0")
## 3/4    c("900","750","600","450","300","225","50","0")
## 8/9    c("850","700","550","400","275","200","50","0")
## 10/11  c("850","700","550","400","250","175","50","0")
## 12/13  c("900","700","550","400","300","200","50","0")
## 14-16  c("220","180","150","120","90","60","30","0")
## 17     c("500","400","300","200","150","75","40","0")
## 18/19  c("550","400","300","200","150","100","40","0")


day2$bin<-as.factor(day2$bin)
night2$bin<-as.factor(night2$bin)
dn.hm<-merge(day2,night2, by.x=c("net","bin","fraction"),by.y=c("net","bin","fraction"),all.x=TRUE,all.y=TRUE)
head(dn.hm)
str(dn.hm)
dn.hm[is.na(dn.hm)]<-0
dn.hm2<-dn.hm%>%group_by(net,bin)%>%summarize(BVm3_day=sum(NBV_m3.x), BVm3_night=sum(NBV_m3.y),
              BMm2_day=sum(BM_m2.x), BMm2_night=sum(BM_m2.y),
              Oxm2_day=sum(oxy_m2.x),Oxm2_night=sum(oxy_m2.y),
              Nitm2_day=sum(nit_m2.x),Nitm2_night=sum(nit_m2.y),
              CO2m2_day=sum(CO2_m2.x),CO2m2_night=sum(CO2_m2.y),
              DOCm2_day=sum(DOC_m2.x),DOCm2_night=sum(DOC_m2.y))

#write.csv(dn.hm2,file=paste("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/",J,"_DayNight_dnhm2.csv",sep=""),row.names=FALSE)

dn.hm3<-dn.hm2%>%mutate(BV=(BVm3_day-BVm3_night), BM=(BMm2_day-BMm2_night),
                           Ox=(Oxm2_day-Oxm2_night),Nit=(Nitm2_day-Nitm2_night),
                           CO2=(CO2m2_day-CO2m2_night),DOC=(DOCm2_day-DOCm2_night))%>%select(,c(1:2,15:20))
#write.csv(dn.hm3,file=paste("C:/PATH_TO_YOUR_SAVE_LOCATION",J,"_DayNight_dnhm3.csv",sep=""),row.names=FALSE)
dn.hm3<-as.data.frame(dn.hm3)
dn.hm3$binnum<-as.numeric(as.character(dn.hm3$bin))
dn.hm4<-filter(dn.hm3,binnum>0.01&binnum<100)

#Use these for July 2018 to drop the data from net 8 (MISSING NET)
#dn.hm5<-filter(dn.hm4, net!='n8')
#dn.hm5$net<-as.factor(dn.hm5$net)
#levels(dn.hm5$net)<-c(levels(dn.hm5$net),'n8')

#SET YOUR PATH
write.csv(dn.hm4,file=paste("C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps/Heatmap data/",J,"_DayNight.csv",sep=""),row.names=FALSE)



BV.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=BV, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-8,5),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=FALSE)+
  coord_cartesian(clip='off')+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Biovolume Shift'))+
  labs(fill=expression("Biovolume"~(mm^3/m^2)),
       x=expression("Size Class"~mm^3),
       color=expression("<-8 or >5)"~mm^3/m^2))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
BV.heatmap
ggsave(BV.heatmap,filename=paste(J,"BV Shift.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps", width=15, height=8, units="in", dpi=300)

BM.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=BM, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-18,7),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=FALSE)+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Biomass Shift'))+
  labs(fill=expression("Biomass"~(mg/m^2)),
       x=expression("Size Class"~mm^3),
       color=expression("<-18 or >7"~mg/m^2))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
BM.heatmap
#SET YOUR PATH
ggsave(BM.heatmap,filename=paste(J,"BM Shift.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps", width=15, height=8, units="in", dpi=300)

Ox.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=Ox, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-153,96),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=FALSE)+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Apparent Oxygen Usage Shift'))+
  labs(fill=expression("Oxygen"~(mu*mol~m^-2*h^-1)),
       x=expression("Size Class"~mm^3),
       color=expression("<-153 or >96"~mu*mol/m^2*h^-1))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
Ox.heatmap
#SET YOUR PATH
ggsave(Ox.heatmap,filename=paste(J,"O2 Shift.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps", width=15, height=8, units="in", dpi=300)

Nit.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=Nit, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-3405,1584),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=FALSE)+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Apparent Ammonium Production Shift'))+
  labs(fill=expression("Ammonium"~(mu*g~m^-2*h^-1)),
       x=expression("Size Class"~mm^3),
       color=expression("<-3405 or >1584"~mu*g~m^-2*h^-1))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
Nit.heatmap
#SET YOUR PATH
ggsave(Nit.heatmap,filename=paste(J,"NH4 Shift.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps", width=15, height=8, units="in", dpi=300)

CO2.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=CO2, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-133,84),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=FALSE)+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Carbon Dioxide Production Shift'))+
  labs(fill=expression("Carbon Dioxide"~(mu*mol~m^-2*h^-1)),
       x=expression("Size Class"~mm^3),
       color=expression("<-133 or >84"~mu*mol~m^-2*h^-1))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
CO2.heatmap
#SET YOUR PATH
ggsave(CO2.heatmap,filename=paste(J,"CO2 Shift.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps", width=15, height=8, units="in", dpi=300)

DOC.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=DOC, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-42,26),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=FALSE)+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton DOC Shift'))+
  labs(fill=expression("DOC"~(mu*mol~m^-2*h^-1)),
       x=expression("Size Class"~mm^3),
       color=expression("<-42 or >26"~mu*mol~m^-2*h^-1))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
DOC.heatmap
#SET YOUR PATH
ggsave(DOC.heatmap,filename=paste(J,"DOC Shift.png", sep="_"),path="C:/Users/localadmin.BIOS/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram_paper/Heatmaps", width=15, height=8, units="in", dpi=300)

