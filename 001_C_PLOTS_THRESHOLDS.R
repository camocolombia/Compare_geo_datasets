library("reshape2")
library("ggplot2")

dir_analysis   <-   "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/TRAFFIC_LIGHT"; if(!file.exists(dir_analysis)){dir.create(dir_analysis)}
out_dir   <-   "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"; if(!file.exists(out_dir)){dir.create(out_dir)}
out_dir_plot  <- paste0(out_dir,"/","THR_PLOTS");if(!file.exists(out_dir_plot)){dir.create(out_dir_plot)}

#s  <-  seq(from = 10, to = 2000, by = 10)
s   <-   seq(from = 0, to = 100, by = 10)
s2   <-   seq(from = 0, to = 100, by = 10)
######################################################################################
######################################################################################

######################################################################################
SCORES_FILES2  <-  lapply(1:length(s),function(i) {
  
  cat("reading csv file for ",s[[i]]," Km","\n")
  X   <-   read.csv(paste0(dir_analysis,"/","SCORES_",s[[i]],".csv"),header=T,sep="|")
  return(X)
  
});gc()
######################################################################################
######################################################################################  


THR_ACC_RAW  <-  as.data.frame(matrix(nrow=length(SCORES_FILES2),ncol=4))
colnames(THR_ACC_RAW)  <-  c("THRESHOLD","Outside the threshold","Inside the Threshold","Not Coordinates/Excluded")


for(i in 1:length(SCORES_FILES2)){
  THR_ACC_RAW[i,1]  <-  s[[i]]
  THR_ACC_RAW[i,2]  <-  sum(SCORES_FILES2[[i]]$THR_ACC_RAW==0,na.rm=T)
  THR_ACC_RAW[i,3]  <-  sum(SCORES_FILES2[[i]]$THR_ACC_RAW==1,na.rm=T)
  THR_ACC_RAW[i,4]  <-  sum(is.na(SCORES_FILES2[[i]]$THR_ACC_RAW))
  
  
};rm(i);gc()
######################################################################################
######################################################################################
THR_ACC_RDGI  <-  as.data.frame(matrix(nrow=length(SCORES_FILES2),ncol=4))
colnames(THR_ACC_RDGI)  <-  c("THRESHOLD","Outside the threshold","Inside the Threshold","Not Coordinates/Excluded")

for(i in 1:length(SCORES_FILES2)){
  THR_ACC_RDGI[i,1]  <-  s[[i]]
  THR_ACC_RDGI[i,2]  <-  sum(SCORES_FILES2[[i]]$THR_ACC_RDGI==0,na.rm=T)
  THR_ACC_RDGI[i,3]  <-  sum(SCORES_FILES2[[i]]$THR_ACC_RDGI==1,na.rm=T)
  THR_ACC_RDGI[i,4]  <-  sum(is.na(SCORES_FILES2[[i]]$THR_ACC_RDGI))
  
  
};rm(i);gc()
######################################################################################
######################################################################################
THR_ACC_IR  <-  as.data.frame(matrix(nrow=length(SCORES_FILES2),ncol=4))
colnames(THR_ACC_IR)  <-  c("THRESHOLD","Outside the threshold","Inside the Threshold","Not Coordinates/Excluded")

for(i in 1:length(SCORES_FILES2)){
  THR_ACC_IR[i,1]  <-  s[[i]]
  THR_ACC_IR[i,2]  <-  sum(SCORES_FILES2[[i]]$THR_ACC_IR==0,na.rm=T)
  THR_ACC_IR[i,3]  <-  sum(SCORES_FILES2[[i]]$THR_ACC_IR==1,na.rm=T)
  THR_ACC_IR[i,4]  <-  sum(is.na(SCORES_FILES2[[i]]$THR_ACC_IR))
  
  
};rm(i);gc()
######################################################################################
######################################################################################
THR_RAW_RDGI  <-  as.data.frame(matrix(nrow=length(SCORES_FILES2),ncol=4))
colnames(THR_RAW_RDGI)  <-  c("THRESHOLD","Outside the threshold","Inside the Threshold","Not Coordinates/Excluded")

for(i in 1:length(SCORES_FILES2)){
  THR_RAW_RDGI[i,1]  <-  s[[i]]
  THR_RAW_RDGI[i,2]  <-  sum(SCORES_FILES2[[i]]$THR_RAW_RDGI==0,na.rm=T)
  THR_RAW_RDGI[i,3]  <-  sum(SCORES_FILES2[[i]]$THR_RAW_RDGI==1,na.rm=T)
  THR_RAW_RDGI[i,4]  <-  sum(is.na(SCORES_FILES2[[i]]$THR_RAW_RDGI))
  
  
};rm(i);gc()
######################################################################################
######################################################################################
THR_RAW_IR  <-  as.data.frame(matrix(nrow=length(SCORES_FILES2),ncol=4))
colnames(THR_RAW_IR)  <-  c("THRESHOLD","Outside the threshold","Inside the Threshold","Not Coordinates/Excluded")

for(i in 1:length(SCORES_FILES2)){
  THR_RAW_IR[i,1]  <-  s[[i]]
  THR_RAW_IR[i,2]  <-  sum(SCORES_FILES2[[i]]$THR_RAW_IR==0,na.rm=T)
  THR_RAW_IR[i,3]  <-  sum(SCORES_FILES2[[i]]$THR_RAW_IR==1,na.rm=T)
  THR_RAW_IR[i,4]  <-  sum(is.na(SCORES_FILES2[[i]]$THR_RAW_IR))
  
  
};rm(i);gc()
######################################################################################
######################################################################################
THR_RDGI_IR  <-  as.data.frame(matrix(nrow=length(SCORES_FILES2),ncol=4))
colnames(THR_RDGI_IR)  <-  c("THRESHOLD","Outside the threshold","Inside the Threshold","Not Coordinates/Excluded")

for(i in 1:length(SCORES_FILES2)){
  THR_RDGI_IR[i,1]  <-  s[[i]]
  THR_RDGI_IR[i,2]  <-  sum(SCORES_FILES2[[i]]$THR_RDGI_IR==0,na.rm=T)
  THR_RDGI_IR[i,3]  <-  sum(SCORES_FILES2[[i]]$THR_RDGI_IR==1,na.rm=T)
  THR_RDGI_IR[i,4]  <-  sum(is.na(SCORES_FILES2[[i]]$THR_RDGI_IR))
  
  
};rm(i);gc()
######################################################################################
######################################################################################
THR_ACC_RAW_DATA  <-   melt(THR_ACC_RAW, id="THRESHOLD")  # convert to long format
THR_ACC_RAW_DATA$variable  <-  factor(THR_ACC_RAW_DATA$variable,unique(THR_ACC_RAW_DATA$variable))


THR_ACC_RAW_PLOT  <-  ggplot(data=THR_ACC_RAW_DATA,
       aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
ggtitle("ACCESS VS GRIN GLOBAL")+
theme(panel.background = element_rect(fill = "gray95"),
      text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
      axis.text.y  = element_text(size=40,colour="black"),
      legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_ACC_RAW_PLOT",".pdf"),THR_ACC_RAW_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
THR_ACC_RDGI_DATA  <-   melt(THR_ACC_RDGI, id="THRESHOLD")  # convert to long format
THR_ACC_RDGI_DATA$variable  <-  factor(THR_ACC_RDGI_DATA$variable,unique(THR_ACC_RDGI_DATA$variable))


THR_ACC_RDGI_PLOT  <-  ggplot(data=THR_ACC_RDGI_DATA,
                         aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
  ggtitle("ACCESS VS GEO IMPROVED")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_ACC_RDGI_PLOT",".pdf"),THR_ACC_RDGI_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
THR_ACC_IR_DATA  <-   melt(THR_ACC_IR, id="THRESHOLD")  # convert to long format
THR_ACC_IR_DATA$variable  <-  factor(THR_ACC_IR_DATA$variable,unique(THR_ACC_IR_DATA$variable))


THR_ACC_IR_PLOT  <-  ggplot(data=THR_ACC_IR_DATA,
                          aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
  ggtitle("ACCESS VS HIJMANS")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_ACC_IR_PLOT",".pdf"),THR_ACC_IR_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()

######################################################################################
THR_RAW_RDGI_DATA  <-   melt(THR_RAW_RDGI, id="THRESHOLD")  # convert to long format
THR_RAW_RDGI_DATA$variable  <-  factor(THR_RAW_RDGI_DATA$variable,unique(THR_RAW_RDGI_DATA$variable))


THR_RAW_RDGI_DATA_PLOT  <-  ggplot(data=THR_RAW_RDGI_DATA,
                        aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
  ggtitle("GRIN GLOBAL VS GEO IMPROVED")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_RAW_RDGI_DATA_PLOT",".pdf"),THR_RAW_RDGI_DATA_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
THR_RAW_IR_DATA  <-   melt(THR_RAW_IR, id="THRESHOLD")  # convert to long format
THR_RAW_IR_DATA$variable  <-  factor(THR_RAW_IR_DATA$variable,unique(THR_RAW_IR_DATA$variable))


THR_RAW_IR_PLOT  <-  ggplot(data=THR_RAW_IR_DATA,
                               aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
  ggtitle("GRIN GLOBAL VS HIJMANS")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_RAW_IR_PLOT",".pdf"),THR_RAW_IR_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
THR_RDGI_IR_DATA  <-   melt(THR_RDGI_IR, id="THRESHOLD")  # convert to long format
THR_RDGI_IR_DATA$variable  <-  factor(THR_RDGI_IR_DATA$variable,unique(THR_RDGI_IR_DATA$variable))


THR_RDGI_IR_PLOT  <-  ggplot(data=THR_RDGI_IR_DATA,
                        aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
  ggtitle("GEO IMPROVED DATA VS HIJMANS")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_RDGI_IR_PLOT",".pdf"),THR_RDGI_IR_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
######################################################################################

TOTAL  <-  cbind(THR_ACC_RAW_DATA,
             THR_ACC_RDGI_DATA[,3],
             THR_ACC_IR_DATA[,3],
             THR_RAW_RDGI_DATA[,3],
             THR_RAW_IR_DATA[,3],
             THR_RDGI_IR_DATA[,3]
             )
colnames(TOTAL)  <-  c("THRESHOLD","variable","THR_ACC_RAW","THR_ACC_RDGI","THR_ACC_IR",
                   "THR_RAW_RDGI","THR_RAW_IR","THR_RDGI_IR")
write.table(TOTAL,paste0(out_dir,"/","SUMMARY.csv"),row.names = F,quote = F,sep="|")


TOTAL2  <-  read.csv(paste0(out_dir,"/","SUMMARY.csv"),header = T,sep="|")



######################################################################################
TOTAL_PLOT  <-  ggplot(data=TOTAL, aes(x=THRESHOLD,y=THR_ACC_RAW,group=variable,colour="red"))+
  
  
  annotate("rect", xmin=0,xmax=10,ymin=-10,ymax=Inf,alpha=0.16,fill="green") +
  annotate("rect", xmin=10.001,xmax=50,ymin=-10,ymax=Inf, alpha=0.16,fill="orange") +
  annotate("rect", xmin=50.01,xmax=100,ymin=-10,ymax=Inf, alpha=0.16,fill="red") +
  
  geom_line(aes(x=THRESHOLD,y=THR_ACC_RAW,group=variable,colour="red"),TOTAL) + 
  geom_point(size=5,aes(x=THRESHOLD,y=THR_ACC_RAW,shape=variable,colour="red"))+
  
  
  
  geom_line(aes(x=THRESHOLD,y=THR_ACC_RDGI,group=variable,colour="lightgoldenrod4"),TOTAL) + 
  geom_point(size=5,aes(x=THRESHOLD,y=THR_ACC_RDGI,shape=variable,colour="lightgoldenrod4"))+
  
  
  geom_line(aes(x=THRESHOLD,y=THR_ACC_IR,group=variable,colour="blue4"),TOTAL) + 
  geom_point(size=5,aes(x=THRESHOLD,y=THR_ACC_IR,shape=variable,colour="blue4"))+
  
  geom_line(aes(x=THRESHOLD,y=THR_RAW_RDGI,group=variable,colour="purple"),TOTAL) + 
  geom_point(size=5,aes(x=THRESHOLD,y=THR_RAW_RDGI,shape=variable,colour="purple"))+
  
  geom_line(aes(x=THRESHOLD,y=THR_RAW_IR,group=variable,colour="green"),TOTAL) + 
  geom_point(size=5,aes(x=THRESHOLD,y=THR_RAW_IR,shape=variable,colour="green"))+
  
  geom_line(aes(x=THRESHOLD,y=THR_RDGI_IR,group=variable,colour="orange"),TOTAL) + 
  geom_point(size=5,aes(x=THRESHOLD,y=THR_RDGI_IR,shape=variable,colour="orange"))+
  
  scale_colour_manual(name="COMPARISON",values=c("red","lightgoldenrod4","blue4","purple","green","orange"),
                      labels=c("GRIN 2007 Vs GRIN GLOBAL 2017",
                               "GRIN 2007 Vs. IRRI IMP DATA",
                               "GRIN 2007 Vs. IRRI FINAL COORDS",
                               "GRIN GLOBAL 2017 Vs. IRRI IMP DATA",
                               "GRIN GLOBAL 2017 Vs. IRRI FINAL COORDS",
                               "IRRI IMP DATA Vs. IRRI FINAL COORDS"))+# + scale_fill_manual(name="Bar",values=cols) +
  
  xlab("Km") + ylab("Occurrences") + 
  ggtitle("THRESHOLDS PERFORMANCE")+
  guides(colour = guide_legend(override.aes = list(shape = 27)))+
  theme(panel.background = element_rect(fill = "gray95"),
                                          text=element_text(size=60),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
                                          axis.text.y  = element_text(size=40,colour="black"),
                                          legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

  
ggsave(paste0(out_dir_plot,"/","TOTAL_PLOT",".pdf"),TOTAL_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()


# convert to long format
THR_RDGI_IR_DATA$variable  <-  factor(THR_RDGI_IR_DATA$variable,unique(THR_RDGI_IR_DATA$variable))


THR_RDGI_IR_PLOT  <-  ggplot(data=THR_RDGI_IR_DATA,
                         aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("Occurrences")+ xlab("Km")+
  ggtitle("GEO IMPROVED DATA VS HIJMANS")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir_plot,"/","THR_RDGI_IR_PLOT",".pdf"),THR_RDGI_IR_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
######################################################################################




# RAW_RDGI_TH_PLOT
# 
# RAW_IR_TH_PLOT
# RDGI_IR_TH_PLOT
#################################################
RAW_RDGI_TH  <-  THR_RAW_RDGI # NEW

RAW_IR_TH  <-  THR_RAW_IR    # NEW

RDGI_IR_TH  <-  THR_RDGI_IR  # NEW
#################################################
RAW_RDGI_TH_2  <-  RAW_RDGI_TH
RAW_RDGI_TH_2[,2]  <-  (RAW_RDGI_TH_2[,2]/85186)*100
RAW_RDGI_TH_2[,3]  <-  (RAW_RDGI_TH_2[,3]/85186)*100
RAW_RDGI_TH_2[,4]  <-  (RAW_RDGI_TH_2[,4]/85186)*100
#################################################
RAW_IR_TH_2  <-  RAW_IR_TH
RAW_IR_TH_2[,2]  <-  (RAW_IR_TH_2[,2]/85186)*100
RAW_IR_TH_2[,3]  <-  (RAW_IR_TH_2[,3]/85186)*100
RAW_IR_TH_2[,4]  <-  (RAW_IR_TH_2[,4]/85186)*100
#################################################
RDGI_IR_TH_2  <-  RDGI_IR_TH
RDGI_IR_TH_2[,2]  <-  (RDGI_IR_TH_2[,2]/85186)*100
RDGI_IR_TH_2[,3]  <-  (RDGI_IR_TH_2[,3]/85186)*100
RDGI_IR_TH_2[,4]  <-  (RDGI_IR_TH_2[,4]/85186)*100
#################################################
gc()
####################################################################################################################################################################################################
####################################################################################################################################################################################################
RAW_RDGI_TH_2_DATA   <-   melt(RAW_RDGI_TH_2, id="THRESHOLD")  # convert to long format
RAW_RDGI_TH_2_DATA$variable  <-  factor(RAW_RDGI_TH_2_DATA$variable,unique(RAW_RDGI_TH_2_DATA$variable))


RAW_RDGI_TH_PLOT_2  <-  ggplot(data=RAW_RDGI_TH_2_DATA,
                         aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("% of total")+ xlab("Km")+
  #ylim(c(0,60))+
  ggtitle("RAW DATA FROM ACCESS DATABASE  Vs IRRI IMPROVED GEOGRAPHY")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
    scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)+
    scale_y_continuous(limits = c(0,60),breaks=c(seq(0,60,by=5)))

ggsave(paste0(out_dir_plot,"/","RAW_RDGI_TH_PLOT_PER",".pdf"),RAW_RDGI_TH_PLOT_2,dpi=600,width =120,height=50,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################
RAW_IR_TH_2_DATA   <-   melt(RAW_IR_TH_2, id="THRESHOLD")  # convert to long format
RAW_IR_TH_2_DATA$variable  <-  factor(RAW_IR_TH_2_DATA$variable,unique(RAW_IR_TH_2_DATA$variable))

RAW_IR_TH_PLOT_2  <-  ggplot(data=RAW_IR_TH_2_DATA,
                       aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  
  ylab("% of total")+ xlab("Km")+
  #ylim(c(0,60))+
  ggtitle("RAW DATA FROM ACCESS DATABASE  Vs HIJMANS RESULTS")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)+
scale_y_continuous(limits = c(0,60),breaks=c(seq(0,60,by=5)))

ggsave(paste0(out_dir_plot,"/","RAW_IR_TH_PLOT_PER",".pdf"),RAW_IR_TH_PLOT_2,dpi=600,width =120,height=50,units = "cm",scale=1.2,limitsize = FALSE)
gc()
######################################################################################

RDGI_IR_TH_2_DATA   <-   melt(RDGI_IR_TH_2, id="THRESHOLD")  # convert to long format
RDGI_IR_TH_2_DATA$variable  <-  factor(RDGI_IR_TH_2_DATA$variable,unique(RDGI_IR_TH_DATA$variable))

RDGI_IR_TH_PLOT_2  <-  ggplot(data=RDGI_IR_TH_2_DATA,
                        aes(x=THRESHOLD, y=value, colour=variable)) +
  geom_line(size=2,alpha=0.8)+
  geom_point()+
  ylab("% of total")+ xlab("Km")+
  #ylim(c(0,60))+
  ggtitle("IRRI IMPROVED GEOGRAPHY Vs HIJMANS RESULTS ")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())+
  scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)+
scale_y_continuous(limits = c(0,60),breaks=c(seq(0,60,by=5)))

cat("DONE!","/n")

ggsave(paste0(out_dir_plot,"/","RDGI_IR_TH_PLOT_PER",".pdf"),RDGI_IR_TH_PLOT_2,dpi=600,width =120,height=50,units = "cm",scale=1.2,limitsize = FALSE)
gc()
##################################################################################################
TOTAL_2  <-  cbind(RAW_RDGI_TH_2_DATA,RAW_IR_TH_2_DATA[,3],RDGI_IR_TH_2_DATA[,3])
colnames(TOTAL_2)  <-  c("THRESHOLD","variable","RAW_RDGI","RAW_IRRI","RDGI_IRRI")
write.table(TOTAL_2,paste0(out_dir_plot,"/","SUMMARY_PER.csv"),row.names = F,quote = F,sep="|")
gc()

