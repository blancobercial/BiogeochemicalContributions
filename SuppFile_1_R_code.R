###REFORMATTING
###LOADING NEEDED LIBRARIES
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

### LOAD DATA, EXTRACT COLUMNS NEEDED #######################

  #MANUALLY SET THE PATH
  #ecotaxa_export <- read.delim("C:ecotaxa_export.txt", header=TRUE)
ecotaxa_export <- 
  read.delim("YOUR_PATH/Supp_data_QC_AEM.tsv", header=TRUE)
sub1<-ecotaxa_export[,c(1,6,7,8,9,10,11,12,13)]#USE THIS CODE IF USING THE DATASET FROM SUPPLEMENTARY DATA 2
  #IF PULING FROM AN ECOTAXA DATASET YOU WILL NEED TO FIND THE COLUMNS ASSOCIATED WITH THE IMPORTANT MEASUREMENTS, TAXONOMY AND METADATA
  #Label is the "object_ID"
  #Min_depth is "object_depth_min", Max_Depth is "object_depth_max" These are required to generate the net interval that allow the calculation of m2 values.
  #Taxa is the taxonomy ("object_annotation_hierarchy" for this setup it represents live/dead, but it can be expanded to select for specific taxon groupings or species)
  #measurement metrics include "object_area",	"object_major",	"object_minor". Additional metrics such as "object_ESD" can be included
  #additional tow metadata including "sample_tot_vol" and	"acq_sub_part" are required to make quantitative m3 calculations
head(sub1)
names(sub1)=c("Label","Min_depth","Max_depth","Taxa","area","major","minor",
              "Tow_Vol","Sub_part")
  #The "object_ID" (now called Label) typically varies among Ecotaxa users. Generally it contains information about the contains cruise, tow, net, and size fraction
  #This information is typically (hopefully!) separated by some special character. The following code infers these splits and labels the information associated with each particle
  #You will need to change the label order if your data is included differently.
sub1$num<-gsub(".*_","",sub1$Label)
sub1<-separate(sub1,Label,
               c("cruise","moc","net","fraction"), extra="drop", remove=FALSE)

### ANALYSIS FILTERS ################
  ### SELECT WHICH MOCNESS YOU WILL BE ANALYZING. YOU NEED TO RUN EACH SUBROUTINES SUB1, SUB2, SUB3 FOR EACH MOC YOU WANT TO ANALYZE ##

sub1$moc<-as.factor(sub1$moc)
levels(sub1$moc)
sub2<-filter(sub1, moc == "m13") # SELECT MOCNESS NUM FROM LEVEL LIST # e.g. M8
sub2$moc=factor(sub2$moc)


# INSERT MEAN NET TEMPERATURES FOR SELECTED TOW
  # The following line of code links environmental data (temperature, in C) to the net tow to allow for Q10 corrections
  # INSERT MEAN NET TEMPERATURES IN CELCIUS FOR SELECTED TOW n1-->n8. This allows estimates of in situ physiology
  # FOR ACTIVE FLUX CALCUALTIONS PUT THE DESIRED MIDWATER TEMPERATURE FOR ALL DEPTHS to estimate physiology at depth of migration
Tlab<-(c("8.11799","10.56992","13.95321","16.81963","18.37317","19.21899","21.15143","24.57"))
# APPLY TOW METADATA FOR SELECTED TOW
H<-'AE1614_M3 (July, Night)'  # INSERT CRUISE/TOW INFORMATION                 
Tow<-'M3'                        # INSERT TOW NUMBER                 
DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

#Tlab<-(c("8.1096","10.5446","14.0613","16.7541","18.3510","19.3074","20.1076","24.7042"))
# APPLY TOW METADATA FOR SELECTED TOW
#H<-'AE1614_M4 (July, Day)'  # INSERT CRUISE/TOW INFORMATION                 
#Tow<-'M4'                        # INSERT TOW NUMBER                 
#DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY

# Tlab<-(c("8.0145","10.8157","14.3465","17.1593","18.6638","19.1972","20.1019","24.79898"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1712_M8 (July, Day)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M8'                        # INSERT TOW NUMBER                 
# DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY

# Tlab<-(c("7.6632","12.4439","14.3145","17.2428","18.6588","19.1529","19.9380","24.2207"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1712_M9 (July, Night)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M9'                        # INSERT TOW NUMBER                 
# DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

# Tlab<-(c("7.4169","9.9423","14.4538","17.3115","16.6889","19.3212","20.0818","24.3045"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1819_M10 (July, Day)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M10'                        # INSERT TOW NUMBER                 
# DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY

# Tlab<-(c("7.3098","10.6053","14.2498","17.0452","18.6882","19.3100","19.9853"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1819_M11 (July, Night)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M11'                        # INSERT TOW NUMBER                 
# DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

# Tlab<-(c("7.5802","10.36023","14.64107","17.33298","18.59808","19.13214","21.302998","25.18264"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1830_M12 (October, Night)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M12'                        # INSERT TOW NUMBER                 
# DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

# Tlab<-(c("7.5803","10.3602","14.6411","17.20508","18.5579","19.2203","21.3057","25.3553"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1830_M13 (October, Day)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M13'                        # INSERT TOW NUMBER                 
# DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY

### If you wish to calculate active flux using the nets in the EZ you need to apply an average midwater temperature
### They must be labled separately so they don't save over the other files (we chose to add _t17 to all names)
### We are missing the top net from M11, thus we did not make these calucations with that net pair

# Tlab<-(c("17","17","17","17","17","17","17","17"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1614_M3_t17 (July, Night)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M3_t17'                        # INSERT TOW NUMBER                 
# DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

#  Tlab<-(c("17","17","17","17","17","17","17","17"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1614_M4_t17 (July, Day)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M4_t17'                        # INSERT TOW NUMBER                 
# DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY

#  Tlab<-(c("17","17","17","17","17","17","17","17"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1712_M8_t17 (July, Day)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M8_t17'                        # INSERT TOW NUMBER                 
# DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY

#  Tlab<-(c("17","17","17","17","17","17","17","17"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1712_M9_t17 (July, Night)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M9_t17'                        # INSERT TOW NUMBER                 
# DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

#  Tlab<-(c("17","17","17","17","17","17","17","17"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1830_M12_t17 (October, Night)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M12_t17'                        # INSERT TOW NUMBER                 
# DayNight<-'n'                     # INSERT N FOR NIGHT, D FOR DAY

#  Tlab<-(c("17","17","17","17","17","17","17","17"))
# APPLY TOW METADATA FOR SELECTED TOW
# H<-'AE1830_M13_t17 (October, Day)'  # INSERT CRUISE/TOW INFORMATION                 
# Tow<-'M13_t17'                        # INSERT TOW NUMBER                 
# DayNight<-'d'                     # INSERT N FOR NIGHT, D FOR DAY



### REFORMATTING DATA ##########################################

sub2$net<-as.factor(sub2$net)
sub2$hdif<-(sub2$Max_depth-sub2$Min_depth)
sub2$split<-1/(sub2$Sub_part)
sub2$D_N<-as.factor(rep(DayNight, length(sub2$Taxa)))
NetList<-c(levels(sub2$net))
lookup<-as.data.frame(cbind(Tlab,NetList))
sub2$fraction<-as.factor(substr(sub2$fraction, 1,2))
  # This set of code converts the output from Ecotaxa (which is in pixels) into mm. 
  # Make sure the numbers are correct for you scan settings
sub2$area_mm2<-sub2$"area"*0.00002809 #check for scan settings dpi to mm
sub2$major_mm<-sub2$"major"*.0053 #check for scan settings dpi to mm
sub2$minor_mm<-as.numeric(sub2$"minor"*.0053) #check for scan settings dpi to mm
sub2$vol<-(4/3)*pi*((sub2$minor_mm*0.5)^2)*(sub2$major_mm/2) #This assumes ellipsoidal shape. Change if you want to 
sub2$logvol<-as.numeric(log10(sub2$vol)) # creates a factor which allows binning by the log of the biovolume (calculated in the previous line).
  # If you want to bin by some other metric (e.g. ESD) you would create the same line but insert ESD rather than vol in the code
  ### Biovolume to Dry Weight conversions are applied here. It can be as simple as using one equation, or taxonomic specific equations.
sub2$DW<-(0.029*(sub2$vol)) #This assumes a specific biovolume to dry weigh conversion.
  ### The code below uses the Taxa factor to apply a taxonomic specific dry weight conversion. 
  ###If the taxonomy is not explicitly listed is assumes the conversion factor of a calanoid copepod
  #sub2$DW<-with(sub2, ifelse(Taxa %in% "Calanoida", 0.055*vol,
  #                           ifelse(Taxa %in% "Chaetognatha", 0.013*vol,
  #                                  ifelse(Taxa %in% "Ostracoda", 0.052*vol,
  #                                        ifelse(Taxa %in% "Thecosomata",0.1913*vol,
  #                                                ifelse(Taxa %in% "Amphipoda",0.034*vol,
  #                                                       ifelse(Taxa %in% "Euphausiacea",0.027*vol,
  #                                                              ifelse(Taxa %in% "Cycopoida", 0.074*vol,
  #                                                                     ifelse(Taxa %in% "Foraminifera",0.142*vol,
  #                                                                            ifelse(Taxa %in% "Decapoda",0.034*vol,
  #                                                                                   0.055*vol))))))))))


sub2$temp<-as.numeric(as.character(with(lookup,Tlab[match(sub2$net,NetList)]))) # This code assigns the temperature of the net (see above) to each particle

  # The following sets of calculations estimate the physiology of each particle.
  # These equations should be tailored to the specific question under investigation
  # They can be generalized or taxon specific (They don't even have to be allometric)
sub2$O2_umol<-(exp(-0.339+(0.801*log(sub2$DW)))+0.069*(sub2$temp))/22.4 #applies an allometric equation for oxygen consumption in umol. Can be modified for species or taxonomic group
### The code below uses the Taxa factor to apply a taxonomic specific allometric oxygen consumption rate
###If the taxonomy is not explicitly listed is assumes the equation for a copepod
#sub2$O2_umol<-with(sub2, ifelse(Taxa %in% "Copepoda", ((exp(-0.399+(0.801*log(DW)))+0.069*(temp))/22.4),
  #                                ifelse(Taxa %in% "Chaetognatha", ((exp(-0.173+(0.805*log(DW)))+0.068*(temp))/22.4),
  #                                       ifelse(Taxa %in% "Amphipoda", ((exp(0.407+(0.743*log(DW)))+0.037*(temp))/22.4),
  #                                             ifelse(Taxa %in% "Euphausiacea",((exp(0.392+(0.753*log(DW)))+0.046*(temp))/22.4),
  #                                                   ifelse(Taxa %in% "Mollusca",((exp(-0.56+(0.82*log(DW)))+0.046*(temp))/22.4),
  #                                                          ((exp(-0.399+(0.801*log(DW)))+0.069*(temp))/22.4)))))))
sub2$CO2<-(sub2$O2_umol)*0.87 #Converts between O2 and CO2 using a general RQ - RQ can be changed (see text)
#### The code below uses the Taxa factor to apply a taxonomic specific RQ
###If the taxonomy is not explicitly listed is assumes the RQ for a copepod
#sub2$CO2<-with(sub2, ifelse(Taxa %in% "Copepoda", O2_umol*0.87,
  #                            ifelse(Taxa %in% "Chaetognatha", O2_umol*1.35,
  #                                 ifelse(Taxa %in% "Amphipoda", O2_umol*1.35,
  #                                        ifelse(Taxa %in% "Euphausiacea",O2_umol*1.35,
  #                                               ifelse(Taxa %in% "Mollusca",O2_umol*0.94,
  #                                                      O2_umol*0.87))))))

sub2$L_D<-as.factor(ifelse(sub2$Taxa %in%"not-living","not-living","living"))
  # Added back in the live/dead selection- although don't actually need it???
str(sub2)
  #This line of data pulls all the metadata from the previously constructed spreadsheet as well as all the subsequent calculations into one .tsv
  #If you are not sure of the column numbers you want check them by looking at sub2 in R
sub3<-sub2[,c(1,14,2:5,17:23,25:26,8,27,12,16,6:7,15)]
str(sub3)

write.csv(sub3,file=paste ##MANUALLY SET THE PATH. cREATE FOLDER IN ADVANCE
          ("YOUR_PATH/Output/",Tow,"_Classified_FOR_R.csv", sep=""),
          row.names=F)

### BINNING, REQUIRES REFORMATTING TO BE COMPLETE#########################
##########################################################################
### At this point you can run the rest of this code either off of sub3 (.CSV) or
### from the All_Tows_for_R excel sheet. To do this, save each "_FOR_R.csv" as 
### a separate sheet named M# (or whatever you designate it as)
library(ggplot2)
library(LaCroixColoR)
library(readxl)

  # All_Tows_for_R<-sub4
  # OR
All_Tows_for_R <- read.csv("YOUR_PATH/Output/M13_t17_Classified_FOR_R.csv", header=TRUE) 
  # OR
  # you need to manually set this path. Either the .csv from sub3 or an EXCEL that has all of the sub3 outputs in one .xlsx file
  #All_Tows_for_R <- read_excel("C:/Users/amy.maas/Dropbox (BIOS)/ZoopGroup_LAB/BIOSSCOPE_data analyses/Histogram/Output/All Tows for R.xlsx", 
  #                             sheet = "M9", guess=1000000)
  # The following lines tell R how to treat the data columns and assign labels to the tow
All_Tows_for_R$cruise<-as.factor(All_Tows_for_R$cruise)
All_Tows_for_R$moc<-as.factor(All_Tows_for_R$moc)
All_Tows_for_R$net<-as.factor(All_Tows_for_R$net)
All_Tows_for_R$fraction<-as.factor(All_Tows_for_R$fraction)
All_Tows_for_R$D_N<-as.factor(All_Tows_for_R$D_N)
All_Tows_for_R$Taxa<-as.factor(All_Tows_for_R$Taxa)
All_Tows_for_R$Tow_Vol<-as.numeric(All_Tows_for_R$Tow_Vol)


# FILTER FOR CLASSIFICATION LEVEL; CAN INSERT ANY VARIATION OF CLASSIFICATION LEVELS
# this is where you can run species specific information or just live/dead.
# Note that this filter is case sensitive and inclusive (living will also pick up non-living)
x<-"not-living"
#To include all of the selected group please run: 
#M<-All_Tows_for_R%>%filter(str_detect(as.character(Taxa),x))
#To exclude all of the selected group please run: 
M<-filter(All_Tows_for_R,!grepl(x, Taxa))
summary(M)
H<-'AE1830_M12 (July, Day)'     # Fill in sample info if not saved from above                
Tow<-'M13_t17'                    # INSERT TOW NUMBER if not saved above.

# AE1614_M3 (July, Night) 
# AE1614_M4 (July, Day)        
# AE1712_M8 (July, Day)
# AE1712_M9 (July, Night)
# AE1819_M10 (July, Day)
# AE1819_M11 (July, Night)
# AE1830_M12 (October, Night)
# AE1830_M13 (October, Day)

thewholeshabang<-function(df, na.rm=TRUE){
  
  ## Creating bins ####################################################
  # This bins the particles by the log biovolume. You may want to modify how to 
  # Set the upper and lower bounds of the log biovolume. Ours is set to .001 and 1000 because of our observed sampling efficiency (see text)
  # We chose to separate by 0.25 on the log scale. (this means each bin is 1.778 bigger)
  L<-as.numeric(c(seq(-3.00, 3.00, by=0.25)))
  #This puts the plot back into biovolume (rather than log biovolume)
  B<-as.character(c(10^L))
  #This controls the tick marks on the x axis.
  M$bin<-cut(M$logvol, breaks=c(seq(-3.00, 3.25, by=0.25)), labels=B)
  M$bin<-factor(M$bin)
  M$Nbin<-as.numeric(as.character(M$bin))
  M$net<-as.factor(M$net)#numeric bins for conversions
  str(M)
  # This generates a table by net>fraction>bin that with columns for
  # the frequency (freq), tow volume (tv), normalized frequency (Nfreq),
  # biovolume/class (BV_C), and normalized biovolume (mm3 per m3)
  
  summary<-M%>%group_by(net,fraction,bin)%>%
    summarize(count=n(),depth=(mean(Min_depth)+mean(Max_depth))/2,hdif=median(hdif),split=median(split),
              freq=count/split,tv=mean(Tow_Vol), Density_m3=freq/tv, bin2=mean(Nbin),
              NBV_m3=(sum(vol)/split/tv),BM_m3=(sum(DW)/split/tv), oxy_m3=(sum(O2_umol))/tv/split,CO2_m3=(sum(CO2)/tv/split),
              Abundance_m2=(freq/tv*hdif),NBV_m2=NBV_m3*hdif, BM_m2=BM_m3*hdif, oxy_m2=oxy_m3*hdif,CO2_m2=CO2_m3*hdif) 
              
  
  summary
  head(summary)
  
  ### This writes two CSV files- one by bin and one by net ###################################################
  ### Set the paths and make sure you make the Output folder 
  write.csv(summary,file=paste("YOUR_PATH/Output/",Tow,"_bins.csv", sep=""), row.names = F)
  
  netsum<-summary%>%group_by(net)%>%summarize(Tot_BV_m3=sum(NBV_m3),Tot_BM_m3=sum(BM_m3), Tot_Ox_m3=sum(oxy_m3),
                                              Tot_CO2_m3=sum(CO2_m3),Tot_BV_m2=sum(NBV_m2),
                                              Tot_BM_m2=sum(BM_m2), Tot_Ox_m2=sum(oxy_m2),
                                              Tot_CO2_m2=sum(CO2_m2),med_depth=mean(depth))
  as.data.frame(netsum)
  write.csv(netsum,file=paste("YOUR_PATH/Output/",Tow,"_nets.csv", sep=""),row.names=F)
  
  
  ### BIOMASS SuMMARY
  ggplot(data=(summary), aes(x=net, y=BM_m2, fill="#D72000", alpha=fraction))+geom_bar(stat="identity")+
    scale_fill_manual(values=c("#D72000"))+
    labs(x="Net", y=expression("Biomass (Dry Weight)" ~(mg~m^-2)), title=paste(H,"Total Biomass by Net"))+
    theme(plot.title=element_text(face="bold",hjust=0.5))+
    scale_alpha_discrete(range=c(0.5,1))+
    guides(fill=FALSE)+
    ylim(0,500)
  ggsave(filename=paste(Tow,"BM_bynet.png", sep="_"),path=paste("YOUR_PATH/Output/", sep=""), width=6, height=6, units="in", dpi=300)
  
  ### OXYGEN USE SUMMARY
  ggplot(data=(summary), aes(x=net, y=oxy_m2, fill="#FFAD0A", alpha=fraction))+geom_bar(stat="identity")+
    scale_fill_manual(values="#FFAD0A")+
    labs(x="Net", y=expression(mu*mol~O[2]*m^-2*h^-1), title=paste(H,"Apparent Oxygen Utilization by Net"))+
    theme(plot.title=element_text(face="bold",hjust=0.5, size=11))+
    scale_alpha_discrete(range=c(0.5,1))+
    guides(fill=FALSE)+
    ylim(0,5000)
  ggsave(filename=paste(Tow,"AOU_bynet.png", sep="_"),path=paste("YOUR_PATH/Output/", sep=""), width=6, height=6, units="in", dpi=300)

}
thewholeshabang(M) #run the function

### WATERFALL PLOTS (PSD), REQUIRES BINNING TO BE COMPLETE ############################################
######################################################################################################
library(ggpubr)
library(dplyr)
#set your path here, SELECTING THE M# THAT YOU WANT
WF<-read.csv("YOUR_PATH/Output/M4_bins.csv")
str(WF)
WF$bin<-as.factor(WF$bin)
WF_sub<-WF%>%group_by(net,bin)%>%summarize(Density_m3=sum(Density_m3),Abundance_m2=sum(Abundance_m2))
WF_sub$binN<-as.numeric(as.character(WF_sub$bin))
# FILLS IN THE DEPTH INTERVALS FOR THE SPECIFIC MOCNESS
#M3-M4  
net_labs<-c("0-50 m","50-200 m","200-275 m","275-400 m", "400-550 m",
                         "550-700 m","700-850 m", "850-1000 m")
#M8-M9      
#net_labs<-c("0-50 m","50-200 m","200-275 m","275-400 m", "400-550 m",
#             "550-700 m","700-850 m", "850-1000 m")
#M10-M11
#net_labs<-c("0-50 m","50-175 m","175-250 m","250-400 m", "400-550 m",
#             "550-700 m","700-850 m", "850-1000 m")
#M12-M13    
#net_labs<-c("0-50 m","50-200 m","200-300 m","300-400 m", "400-550 m",
#             "550-700 m","700-900 m", "900-1000 m")


#NAME OF THE MOCNESS
Title<-"July 2016 (Day)"

# M3 July 2016 (Night) 
# M4 July 2016 (Day)       
# M8 July 2017 (Day)
# M9 July 2017 (Night)
# M10 July 2018 (Day)
# M11 July 2018 (Night)
# M12 Oct 2018 (Night)
# M13 Oct 2018 (Day)

WFplot<-ggplot(data=WF_sub, aes(x=binN, y=Abundance_m2,color=net))+
  geom_point(size=2)+
  scale_color_manual(values=(lacroix_palette("PeachPear", type="continuous", n=8)), labels=rev(net_labs))+
  labs(x=expression("Size class"~(mm^3)), y=expression("Abundance"~(particles~m^-2)),
       title=paste(Title), color="")+
  guides(color=guide_legend(reverse=T))+
  scale_y_log10(limits=c(0.001,100000), #you may need to change your scale 
                breaks=c(0.1,1,10,100,1000,10000,100000), #you may need to change your scale
                labels=c("0.1","1","10","100","1000","10000","100000"))+
  #you can limit your scale based on what biomass you effectively sample, but you should look to see all data first
  scale_x_log10(limits=c(.001,1000),
                breaks=c(0.001,0.01,0.1,1,10,100,1000), 
                labels=c("0.001","0.01","0.1","1","10","100","1000"))+
  coord_cartesian(clip='off')+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=14),
        legend.position='bottom')+
  theme(legend.text=element_text(size=11))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=14),axis.text.y= element_text(size=14),
        axis.title=element_text(size=14),strip.text=element_text(size=14, face="bold"))

WFplot
  # set your path
ggsave(WFplot,filename=paste(Title,"Waterfall.png", sep=" "), path=paste("YOUR_PATH/Output/", sep=""), width=6, height=5, units="in" )

  ## This code gives you linear regressions and R2 of the correlation for each line.
library(data.table)
  ## set the bins to exclude the nets with poor capture (particularly on the low end)
WF_sub_trim<-WF_sub%>%filter(binN>0.1&binN<10)
dt<-data.table(WF_sub_trim, key="net")
fits<-lapply(unique(dt$net),function(z){
  summary(lm(log(Abundance_m2)~log(binN), data=dt[J(z),], y=T))
})

fits
  ## this saves the stats to a file. Set your path.
capture.output(fits,file=paste("YOUR_PATH/Output/WF_",Title,"_reg_stats.txt"))

## DAY-NIGHT PLOTS #############################
###############################################
library(gridExtra)

#SET YOUR PATH HERE, PICKING THE DAY OF A PAIR
day_bin<-read.csv("YOUR_PATH/Output/M4_bins.csv")
#SET YOUR PATH HERE, PICKING THE NIGHT OF A PAIR
night_bin<-read.csv("YOUR_PATH/Output/M3_bins.csv")

#FILTER BIN SIZES FOR BOTH DAY AND NIGHT TO SELECTED RANGE
day_bin_sub<-day_bin%>%filter(bin2 >=0.01&bin2<=100)
day<-day_bin_sub%>%group_by(net)%>%summarize(Tot_BV_m3=sum(NBV_m3),Tot_BM_m3=sum(BM_m3), Tot_Ox_m3=sum(oxy_m3),
                                                Tot_CO2_m3=sum(CO2_m3),Tot_BV_m2=sum(NBV_m2),
                                                Tot_BM_m2=sum(BM_m2), Tot_Ox_m2=sum(oxy_m2),
                                                Tot_CO2_m2=sum(CO2_m2),med_depth=mean(depth))
night_bin_sub<-night_bin%>%filter(bin2 >=0.01&bin2<=100)
night<-night_bin_sub%>%group_by(net)%>%summarize(Tot_BV_m3=sum(NBV_m3),Tot_BM_m3=sum(BM_m3), Tot_Ox_m3=sum(oxy_m3),
                                             Tot_CO2_m3=sum(CO2_m3),Tot_BV_m2=sum(NBV_m2),
                                             Tot_BM_m2=sum(BM_m2), Tot_Ox_m2=sum(oxy_m2),
                                             Tot_CO2_m2=sum(CO2_m2),med_depth=mean(depth))
I="July 2016" #SET THE NAME OF THE PAIR
# M3 July 2016 (Night) 
# M4 July 2016 (Day)       
# M8 July 2017 (Day)
# M9 July 2017 (Night)
# M10 July 2018 (Day)
# M11 July 2018 (Night)
# M12 Oct 2018 (Night)
# M13 Oct 2018 (Day)

# If you are missing a net (like in M11) use this text to block it out
#day<-filter(day,as.factor(net)!='n8')
#day$net<-as.factor(day$net)
#night<-filter(night,as.factor(net)!='n8')
#night$net<-as.factor(night$net)

DayNight<-data.frame("BV_Mig"=c(abs(day$Tot_BV_m2-night$Tot_BV_m2)), "BV_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_BV_m2,night$Tot_BV_m2)))),
                     "BM_Mig"=c(abs(day$Tot_BM_m2-night$Tot_BM_m2)), "BM_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_BM_m2,night$Tot_BM_m2)))),
                     "Ox_Mig"=c(abs(day$Tot_Ox_m2-night$Tot_Ox_m2)), "Ox_Res"=do.call(pmin,(as.data.frame(cbind(day$Tot_Ox_m2,night$Tot_Ox_m2)))),
                     "CO2_Mig"=c(abs(day$Tot_CO2_m2-night$Tot_CO2_m2)),"CO2_Mig"=do.call(pmin,(as.data.frame(cbind(day$Tot_CO2_m2,night$Tot_CO2_m2)))),
                     "BM_DVM"=c((day$Tot_BM_m2-night$Tot_BM_m2)),"BM_day"=c(day$Tot_BM_m2),"BM_night"=c(night$Tot_BM_m2),
                     "BV_DVM"=c((day$Tot_BV_m2-night$Tot_BV_m2)),
                     "Med_Depth"=as.factor(apply(as.data.frame(cbind(day$med_depth,night$med_depth)),1,FUN=mean)),
                     "Net"=c(day$net))
# This is the standard code, skip below if you have a missing net
DN2<-data.frame("Net"=as.factor(c(1:8,1:8)),"M_R"=as.factor(c(rep.int("M",8), rep.int("R",8))),"BM"=c(DayNight$BM_Mig,DayNight$BM_Res), "BV"=c(DayNight$BV_Mig,DayNight$BV_Res),
                "Ox"=c(DayNight$Ox_Mig,DayNight$Ox_Res), "CO2"=c(DayNight$CO2_Mig,DayNight$CO2_Res),
                "Med_Depth"=as.factor(rep.int(DayNight$Med_Depth, 2)))

# Use this code if you are skipping a net
#DN2<-data.frame("Net"=as.factor(c(1:7,1:7)),"M_R"=as.factor(c(rep.int("M",7), rep.int("R",7))),"BM"=c(DayNight$BM_Mig,DayNight$BM_Res), "BV"=c(DayNight$BV_Mig,DayNight$BV_Res),
#               "Ox"=c(DayNight$Ox_Mig,DayNight$Ox_Res),"CO2"=c(DayNight$CO2_Mig,DayNight$CO2_Res),
#               "Med_Depth"=as.factor(rep.int(DayNight$Med_Depth, 2)))

write.csv(DN2,file=paste("YOUR_PATH/Output/",I,"_DayNightNETS.csv",sep=""),row.names=FALSE)

d_labs=c("900","750","600","450","300","225","50","0")
## 3/4    c("900","750","600","450","300","225","50","0")
## 8/9    c("850","700","550","400","275","200","50","0")
## 10/11  c("850","700","550","400","250","175","50","0")
## 12/13  c("900","700","550","400","300","200","50","0")


plot1<-ggplot(data=DN2, aes(x=Med_Depth, y=BM,fill="grey7", alpha=M_R))+
  scale_fill_manual(values=("grey7"))+scale_alpha_discrete(range=c(0.5,1), labels=c("Migratory","Resident"), drop=FALSE)+
  geom_col(position="stack", na.rm=FALSE)+coord_flip()+
  labs(x="Minimum Net Depth (m)", y=expression("mg Biomass"~m^-2),
       title="Dry Weight Biomass", alpha="")+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=12))+
  guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=12),axis.text.y= element_text(size=12),
        axis.title=element_text(size=14),strip.text=element_text(size=14, face="bold"),
        legend.text=element_text(size=11),
        legend.position='bottom')+
  ylim(c(0,200))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs, drop=FALSE)
plot2<-ggplot(data=DN2, aes(x=Med_Depth, y=Ox, fill="grey7", alpha=M_R))+
  scale_fill_manual(values=("grey7"))+scale_alpha_discrete(range=c(0.5,1), labels=c("Migratory","Resident"))+
  geom_col(position="stack")+coord_flip()+
  labs(x="Minimumn Net Depth (m)", y=expression(mu*mol~O[2]~m^-2*h^-1),
       title="Apparent Oxygen Usage", alpha="")+
  guides(fill=FALSE, alpha=FALSE)+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),axis.text.y= element_text(size=12),
        axis.title=element_text(size=14),strip.text=element_text(size=14, face="bold"),
        legend.text=element_text(size=11),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylim(c(0,4000))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)

plot3<-ggplot(data=DayNight, aes(x=Med_Depth, y=BM_DVM, fill="grey7"))+
  geom_col()+coord_flip()+
  labs(x="Minimum Net Depth (m)", y=expression("Biomass"~mg~m^-2),
       title="Biomass (Dry Weight)")+
  scale_fill_manual(values="grey7")+
  theme(plot.title=element_text(face="bold",hjust=0.5, size=12))+
  scale_x_discrete(limits=rev(levels(DN2$Med_Depth)),labels=d_labs)+
  ylim(c(-100,100))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),axis.text.y= element_text(size=12),
        axis.title=element_text(size=14),strip.text=element_text(size=14, face="bold"),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill=FALSE)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot1)

p1<-grid.arrange(arrangeGrob(plot1+theme(legend.position="none"),plot2,plot3, ncol=3, top=paste(I,"Migrators and Resident Zooplankton")),
                 mylegend, nrow=2,heights=c(10, 1))

ggsave(p1,filename=paste(I,"DNPlot.png", sep="_"),path="YOUR_PATH/Output/", width=8, height=7, units="in", dpi=300)


### MIGRATION HEATMAPS #########################################################
################################################################################
library(RColorBrewer)
library(colorspace)
library(ggplot2)

#SET YOUR PATH HERE, PICKING THE DAY OF A PAIR
day2<-read.csv("YOUR_PATH/Output/M13_t17_bins.csv")
#SET YOUR PATH HERE, PICKING THE NIGHT OF A PAIR
night2<-read.csv("YOUR_PATH/Output/M12_t17_bins.csv")

# M3 July 2016 (Night) 
# M4 July 2016 (Day)       
# M8 July 2017 (Day)
# M9 July 2017 (Night)
# M10 July 2018 (Day)
# M11 July 2018 (Night)
# M12 Oct 2018 (Night)
# M13 Oct 2018 (Day)

J<-"Oct 2018_t17" #SET THE NAME OF THE PAIR
#SET THE DEPTH INTERVALS FOR THE PAIR
d_labs=c("900","700","550","400","300","200","50","0")
## 3/4    c("900","750","600","450","300","225","50","0")
## 8/9    c("850","700","550","400","275","200","50","0")
## 10/11  c("850","700","550","400","250","175","50","0")
## 12/13  c("900","700","550","400","300","200","50","0")


day2$bin<-as.factor(day2$bin)
night2$bin<-as.factor(night2$bin)
dn.hm<-merge(day2,night2, by.x=c("net","bin","fraction"),by.y=c("net","bin","fraction"),all.x=TRUE,all.y=TRUE)
head(dn.hm)
summary(dn.hm)
dn.hm[is.na(dn.hm)]<-0
dn.hm2<-dn.hm%>%group_by(net,bin)%>%summarize(BVm3_day=sum(NBV_m3.x), BVm3_night=sum(NBV_m3.y),
                                              BVm2_day=sum(NBV_m2.x), BVm2_night=sum(NBV_m2.y),
                                              BMm2_day=sum(BM_m2.x), BMm2_night=sum(BM_m2.y),
                                              Oxm2_day=sum(oxy_m2.x),Oxm2_night=sum(oxy_m2.y),
                                              CO2m2_day=sum(CO2_m2.x),CO2m2_night=sum(CO2_m2.y))

#write.csv(dn.hm2,file=paste("C:/Users/amy.maas/Desktop/Zooscan R Codes/",J,"_DayNight_dnhm2.csv",sep=""),row.names=FALSE)

dn.hm3<-dn.hm2%>%mutate(BV_m3=(BVm3_day-BVm3_night), BV_m2=(BVm2_day-BVm2_night),BM_m2=(BMm2_day-BMm2_night),
                        Ox=(Oxm2_day-Oxm2_night),CO2=(CO2m2_day-CO2m2_night)) %>%select(,c(1:2,13:17))  
                    
#write.csv(dn.hm3,file=paste("C:/Users/amy.maas/Desktop/Zooscan R Codes/",J,"_DayNight_dnhm3.csv",sep=""),row.names=FALSE)
dn.hm3<-as.data.frame(dn.hm3)
dn.hm3$binnum<-as.numeric(as.character(dn.hm3$bin))
dn.hm4<-filter(dn.hm3,binnum>0.01&binnum<100)

#Use these for July 2018 to drop the data from net 8
#dn.hm4<-filter(dn.hm4, net!='n8')
#dn.hm4$net<-as.factor(dn.hm4$net)
#levels(dn.hm4$net)<-c(levels(dn.hm4$net),'n8')


write.csv(dn.hm4,file=paste("YOUR_PATH/Output/",J,"_DayNight.csv",sep=""),row.names=FALSE)


BV.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=BV_m2, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-1000,1000),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=TRUE)+
  coord_cartesian(clip='off')+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Biovolume Shift'))+
  labs(fill=expression("Biovolume"~(mm^3/m^2)),
       x=expression("Size Class"~mm^3),
       color=expression("<-1000"~mm^3/m^2))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
BV.heatmap
ggsave(BV.heatmap,filename=paste(J,"BV_Shift.png", sep="_"),path=paste("YOUR_PATH/Output/",sep=""), width=8, height=5, units="in", dpi=300)

CO2.heatmap<-ggplot(data=dn.hm4, mapping=aes(x=bin, y=net, fill=CO2, color=""))+
  geom_tile()+
  xlab(label="Size Class (mm^3)")+
  ylab(label="Minimum Net Depth (m)")+
  scale_fill_continuous_divergingx(na.value="gray45",limits=c(-80,80),palette = 'RdBu',
                                   rev=TRUE, mid =0, l3 = 0, p1=0.4, p3 = .4, p4 = .5)+
  scale_x_discrete(breaks=c("0.01", "0.1", "1","10", "100"), 
                   labels=c("0.01", "0.1", "1","10", "100"), drop=TRUE)+
  coord_cartesian(clip='off')+
  scale_y_discrete(labels=d_labs, drop=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        axis.title= element_text(size=14),
        axis.text.y=element_text(size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5,face="bold",size=16))+
  ggtitle(paste(J,'(Day-Night) Plankton Respiratory Shift'))+
  labs(fill=expression("CO2"~(mu*mol~m^-2*h^-1)),
       x=expression("Size Class"~mm^3),
       color=expression("<-80"~mu*mol~m^-2*h^-1))+
  scale_colour_manual(values=NA) +   
  guides(fill=guide_colorbar(order=1))+
  guides(color=guide_legend(order=2, override.aes=list(fill="gray45")))
CO2.heatmap
ggsave(CO2.heatmap,filename=paste(J,"CO2_Shift.png", sep="_"),path=paste("YOUR_PATH/Output/",sep=""), width=8, height=5, units="in", dpi=300)

