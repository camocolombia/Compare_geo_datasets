library("reshape2")
library("ggplot2")
library("RColorBrewer")

dir_analysis <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/TRAFFIC_LIGHT"
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"; if(!file.exists(out_dir)){dir.create(out_dir)}
out_dir_TH <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/THRESHOLD"; if(!file.exists(out_dir_TH)){dir.create(out_dir_TH)}

library(RColorBrewer)
myColors  <-  brewer.pal(14,"Paired")


THR_MEAN_FILE <- read.csv(paste0(out_dir_TH,"/","THR_METRICS_2.csv"),header=T,sep="|")
THR_MEAN_FILE[,4:14][which(THR_MEAN_FILE$COORD_STATUS=="NO LOCALITY (CENTROID)"),] <- NA
THR_MEAN_FILE[,4:14][which(THR_MEAN_FILE$COORD_STATUS=="GEOREF BY HAND"),] <- NA
THR_MEAN_FILE[,4:14][which(THR_MEAN_FILE$COORD_STATUS=="SOS"),] <- NA
THR_MEAN_FILE[,4:14][which(THR_MEAN_FILE$COORD_STATUS=="GRIN GLOBAL"),] <- NA

##############################

THR_MEAN_FILE_DATA  <-  melt(THR_MEAN_FILE[-c(1,2,15)], id="COORD_STATUS")

##############################

THR_MEAN_FILE_DATA_2 <- THR_MEAN_FILE_DATA
THR_MEAN_FILE_DATA_2$value <- as.numeric(THR_MEAN_FILE_DATA_2$value)
THR_MEAN_FILE_DATA_2$value <- THR_MEAN_FILE_DATA_2$value*100
THR_MEAN_FILE_DATA_2 <- THR_MEAN_FILE_DATA_2[which(THR_MEAN_FILE_DATA_2$variable!="TRAFFIC_LIGHT"),]
THR_MEAN_FILE_DATA_2 <- THR_MEAN_FILE_DATA_2[which(!is.na(THR_MEAN_FILE_DATA_2$value)),]

##############################
THR_MEAN_FILE_DATA_2$variable <- as.character(THR_MEAN_FILE_DATA_2$variable)
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_0")] <- 0
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_10")] <- 10
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_20")] <- 20
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_30")] <- 30
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_40")] <- 40
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_50")] <- 50
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_60")] <- 60
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_70")] <- 70

THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_80")] <- 80
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_90")] <- 90
THR_MEAN_FILE_DATA_2$variable[which(as.character(THR_MEAN_FILE_DATA_2$variable)=="Km_100")] <- 100
THR_MEAN_FILE_DATA_2$variable <- factor(THR_MEAN_FILE_DATA_2$variable,unique(THR_MEAN_FILE_DATA_2$variable))
##############################


lis_sum <- list()

for(i in 4:14){
  j <- i-3;cat(j,"\n")
  xx <- as.data.frame(table(THR_MEAN_FILE[,i]))
  xx$km <- colnames(THR_MEAN_FILE)[i]
  xx$Var1 <- as.numeric(as.character(xx$Var1))
  xx$Var1 <- xx$Var1*100
  xx$Var1 <- round(xx$Var1,2)
  xx$km <- as.numeric(sub("Km_","",xx$km))
  
  
  xx <- xx[,c(3,1,2)]
  
  lis_sum[[(j)]] <- xx

}  

sum_do <- do.call(rbind,lis_sum)
  # THR_MEAN_FILE_DATA <- THR_MEAN_FILE_DATA[order(
# THR_MEAN_FILE_DATA$COORD_STATUS== "1",
# THR_MEAN_FILE_DATA$COORD_STATUS== "2",
# THR_MEAN_FILE_DATA$COORD_STATUS== "3",
# THR_MEAN_FILE_DATA$COORD_STATUS== "4",
# 
# THR_MEAN_FILE_DATA$COORD_STATUS== "SOS",
# THR_MEAN_FILE_DATA$COORD_STATUS== "NO LOCALITY (CENTROID)",
# THR_MEAN_FILE_DATA$COORD_STATUS== "GRIN GLOBAL",
# THR_MEAN_FILE_DATA$COORD_STATUS== "GEOREF BY HAND",

# decreasing = T),]
# 
sum_do$Var1 <- factor(sum_do$Var1,unique(sum_do$Var1))
sum_do$Var1 <- factor(sum_do$Var1,unique(sum_do$Var1))
sum_do$km <- factor(sum_do$km,unique(sum_do$km)) 


sum_do <- sum_do[order(
sum_do$Var1== "0",
sum_do$Var1== "8.33",
sum_do$Var1== "12.5",
sum_do$Var1== "16.67",
sum_do$Var1== "25",
sum_do$Var1== "33.33",
sum_do$Var1== "37.5",
sum_do$Var1== "50",
sum_do$Var1== "58.33",
sum_do$Var1== "62.5",
sum_do$Var1== "66.67",
sum_do$Var1== "75",
sum_do$Var1== "83.33",
sum_do$Var1== "100",
decreasing = T),]

sum_do$Var1 <- factor(sum_do$Var1,unique(sum_do$Var1))

# THR_MEAN_FILE_DATA <- THR_MEAN_FILE_DATA[complete.cases(THR_MEAN_FILE_DATA),]
# 
# 
# 


set3  <-  colorRampPalette(brewer.pal('Set3',n=12))

THR_MEAN_FILE_PLOT <- ggplot(data=sum_do,
                           aes(x=Var1,y=Freq,fill=km,color=km)) +
 
  geom_bar(position="dodge",stat="identity")+
 # geom_line(aes(x=as.numeric(as.character(sum_do$Var1)),y=as.numeric(as.character(Freq)))+
  ylab("Count")+ xlab("% matches for 4 datasets")+
  ggtitle("Threshold status")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_text(size=40,colour="black"))+
  labs(fill = "Threshold (Km)")
#+
 # scale_fill_manual(values = c("green", "blue", "red","purple","yellow","lightgreen","pink","darkgreen"))
  #+
  #scale_y_continuous(limits = c(0,100),breaks=seq(from = 0, to = 100, by = 10))
#scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

ggsave(paste0(out_dir,"/","THR_MEAN_FILE_PLOT",".pdf"),THR_MEAN_FILE_PLOT,dpi=600,width =120,height=50,units = "cm",scale=1,limitsize = FALSE)
gc()



set3  <-  colorRampPalette(brewer.pal('Set3',n=12))

p1a <- ggplot(data=THR_MEAN_FILE_DATA_2,aes(x=variable,y=value))+
  geom_boxplot(aes(fill=variable)) +
  stat_boxplot(geom ='errorbar') +
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point")+
  guides(fill=FALSE)+
  #ylim(c(0,100))+
  ylab("% Matches among datasets")+ xlab("Km")+
  ggtitle("Georeferencing Threshold Status (EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),legend.position="none")+
  labs(fill = "Threshold (Km)")



#+



ggsave(paste0(out_dir,"/","THR_BP_FILE_PLOT",".pdf"),p1a,dpi=600,width =120,height=50,units = "cm",scale=1,limitsize = FALSE)

set3  <-  colorRampPalette(brewer.pal('Set3',n=12))

p1 <- ggplot(data=THR_MEAN_FILE_DATA_2,aes(x=value,colour=variable))+
 # geom_histogram(aes(y=..density..), alpha=0.5, 
  #               position="dodge")+
  geom_density(alpha = 0.1)+
  #xlim(c(0,100))+
  ylab("Density")+ xlab("% matches among datasets")+
  ggtitle("Georeferencing uncertainty (NO EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),legend.title=element_text(size=40,colour="black"))+
  labs(fill = "Threshold (Km)")+
  scale_x_continuous(limits = c(0,100),breaks=seq(0,100,10))

#+
 # scale_fill_manual(values=setNames(set3(11), levels(THR_MEAN_FILE_DATA_2$variable)),name = "Matches' mean")

ggsave(paste0(out_dir,"/","THR_DENSITY_FILE_PLOT",".pdf"),p1,dpi=600,width =120,height=50,units = "cm",scale=1,limitsize = FALSE)


####
THR_MEAN_FILE_DATA_3 <- THR_MEAN_FILE_DATA_2
THR_MEAN_FILE_DATA_3 <- THR_MEAN_FILE_DATA_3[which(THR_MEAN_FILE_DATA_3$variable==c(0,10,50,100)),]






set3  <-  colorRampPalette(brewer.pal('Set3',n=12))

p11 <- ggplot(data=THR_MEAN_FILE_DATA_3,aes(x=value,colour=variable))+
  # geom_histogram(aes(y=..density..), alpha=0.5, 
  #               position="dodge")+
  geom_density(alpha = 0.1)+
  #xlim(c(0,100))+
  ylab("Density")+ xlab("% matches among datasets")+
  ggtitle("Georeferencing uncertainty (NO EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),legend.title=element_text(size=40,colour="black"))+
 
  scale_x_continuous(limits = c(0,100),breaks=seq(0,100,10)) +
  labs(fill = "Threshold (Km)")
 # scale_x_continuous(limits = c(0,100),breaks=seq(0,100,10)) #+
# scale_fill_manual(values=setNames(set3(11), levels(THR_MEAN_FILE_DATA_2$variable)),name = "Matches' mean")

ggsave(paste0(out_dir,"/","THR_DENSITY_0-50_FILE_PLOT",".pdf"),p11,dpi=600,width =120,height=50,units = "cm",scale=1,limitsize = FALSE)



# THR_SUM <- as.data.frame(matrix(nrow = 14,ncol=12))
# 
# colnames(THR_SUM) <- c(
#   "MATCHES",0,10,20,30,40,50,60,70,80,90,100
#   
# )
# THR_SUM[1,1] <- "NO AVAILABLE"
# THR_SUM[2,1] <- "0"
# THR_SUM[3,1] <- "0.25"
# THR_SUM[4,1] <- "0.5"
# THR_SUM[5,1] <- "0.75"
# THR_SUM[6,1] <- "1"
# THR_SUM[7,1] <- "1.25"
# THR_SUM[8,1] <- "1.5"
# THR_SUM[9,1] <- "1.75"
# THR_SUM[10,1] <- "2"
# THR_SUM[11,1] <- "2.25"
# THR_SUM[12,1] <- "2.5"
# THR_SUM[13,1] <- "2.75"
# THR_SUM[14,1] <- "3"
# 
# for(i in 4:14){
#   
#   j=i-2
# cat("COL: ",i,"| ROW: ",j,"\n")
# THR_SUM[1, j] <- sum(is.na(THR_MEAN_FILE[,i]))
# THR_SUM[2, j] <- sum(THR_MEAN_FILE[,i]==0,na.rm = T)
# THR_SUM[3, j] <- sum(THR_MEAN_FILE[,i]==0.25,na.rm = T)
# THR_SUM[4, j] <- sum(THR_MEAN_FILE[,i]==0.5,na.rm = T)
# THR_SUM[5, j] <- sum(THR_MEAN_FILE[,i]==0.75,na.rm = T)
# THR_SUM[6, j] <- sum(THR_MEAN_FILE[,i]==1,na.rm = T)
# THR_SUM[7, j] <- sum(THR_MEAN_FILE[,i]==1.25,na.rm = T)
# THR_SUM[8, j] <- sum(THR_MEAN_FILE[,i]==1.5,na.rm = T)
# THR_SUM[9, j] <- sum(THR_MEAN_FILE[,i]==1.75,na.rm = T)
# THR_SUM[10,j] <- sum(THR_MEAN_FILE[,i]==2,na.rm = T)
# THR_SUM[11,j] <- sum(THR_MEAN_FILE[,i]==2.25,na.rm = T)
# THR_SUM[12,j] <- sum(THR_MEAN_FILE[,i]==2.5,na.rm = T)
# THR_SUM[13,j] <- sum(THR_MEAN_FILE[,i]==2.75,na.rm = T)
# THR_SUM[14,j] <- sum(THR_MEAN_FILE[,i]==3,na.rm = T)
#   }
# 
# 
# write.table(THR_SUM,paste0(out_dir_TH,"/","THR_SUM.csv"),row.names = F,sep="|")
# 
# 
# 
# THR_THR_SUM_FILE_DATA  <-  melt(THR_SUM,id="MATCHES") 
# THR_THR_SUM_FILE_DATA$variable <- factor(THR_THR_SUM_FILE_DATA$variable,unique(THR_THR_SUM_FILE_DATA$variable))
# THR_THR_SUM_FILE_DATA$MATCHES <- factor(THR_THR_SUM_FILE_DATA$MATCHES,unique(THR_THR_SUM_FILE_DATA$MATCHES))
# THR_THR_SUM_FILE_DATA <- THR_THR_SUM_FILE_DATA[which(THR_THR_SUM_FILE_DATA$MATCHES!="NO AVAILABLE"),]
# THR_THR_SUM_FILE_DATA <- THR_THR_SUM_FILE_DATA

# colourCount = length(unique(THR_THR_SUM_FILE_DATA$variable))
# getPalette = colorRampPalette(brewer.pal(11, "Blues"))


# 
#   THR_THR_SUM_FILE_PLOT <- ggplot(data=THR_MEAN_FILE_DATA,
#                           # aes(x=variable, y=value,fill=MATCHES)) +
#                           aes(x=variable, y=value,group=COORD_STATUS,colour=COORD_STATUS)) +
# 
#   
#   # geom_rect(data=THR_THR_SUM_FILE_DATA,)+
#   # geom_rect(data=THR_THR_SUM_FILE_DATA, xmin=2,xmax=6,ymin=0,ymax=Inf,fill="yellow", alpha=0.1)+
#   # geom_rect(data=THR_THR_SUM_FILE_DATA, xmin=6,xmax=11,ymin=0,ymax=Inf, fill="red",alpha=0.1)+
# 
#   #geom_bar(position="dodge",stat="identity")+
#   geom_line(size=5)+
#   geom_point(size = 8)+
#   # annotate("rect", xmin=0,xmax=2,ymin=-10,ymax=Inf,alpha=0.16,fill="green") +
#   # annotate("rect", xmin=2.001,xmax=6,ymin=-10,ymax=Inf, alpha=0.16,fill="orange") +
#   # annotate("rect", xmin=6.01,xmax=11,ymin=-10,ymax=Inf, alpha=0.16,fill="red") +
#   geom_line(size=5)+
#   geom_point(size = 8)+
#   
#   
#     ylab("Occurences")+ 
#   xlab("Km")+
#   ggtitle("Threshold status")+
#   theme(panel.background = element_rect(fill = "gray95"),
#         text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
#         axis.text.y  = element_text(size=40,colour="black"),
#         legend.title=element_blank())+
#     scale_color_manual(values=setNames(set3(14), levels(THR_MEAN_FILE_DATA$COORD_STATUS)),name = "Matches' mean")


  #annotate(xmin=0,xmax=2,ymin=0,ymax=15000, alpha=0.1,fill="green")
  # annotate(xmin=2,xmax=6,ymin=0,ymax=15000, alpha=0.1,fill="green")+ 
  # annotate(xmin=6,xmax=11,ymin=0,ymax=15000, alpha=0.1,fill="green")
  


  #scale_colour_hue("clarity")
#+
# scale_fill_manual(values = c("green", "blue", "red","purple","yellow","lightgreen","pink","darkgreen"))
#+
#scale_y_continuous(limits = c(0,100),breaks=seq(from = 0, to = 100, by = 10))
#scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

# ggsave(paste0(out_dir,"/","THR_MEAN_SUM_PLOT",".png"),THR_THR_SUM_FILE_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
# gc()
#   
