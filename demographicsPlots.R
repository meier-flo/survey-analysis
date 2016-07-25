require(tidyr)
source("multiplot.R")
#survey_data<- read.csv("~/Dropbox/DISS_NEU/Twitter-Survey/ANALYSIS/survey-analysis/survey-analysis/survey_data_fusion.csv", sep=";",na.strings=c(""," ","NA"))
#demographics<-survey_data%>%select(X1demo_gender:X1demo_usage_other)


##### Which social networks do people use?
demographics.snetworks<-demographics%>%select(contains("snetworks"))%>%plyr::rename(c("X1demo_snetworks_facebook"="Facebook","X1demo_snetworks_googleplus"="Google+","X1demo_snetworks_linkedIn"="LinkedIn","X1demo_snetworks_myspace"="MySpace","X1demo_snetworks_pinterest"="Pinterest","X1demo_snetworks_other"="Other(s)"))%>%summarise_each(funs(sum(!is.na(.))))%>%gather("snetworks","count",1:6)%>%mutate(percent=(count/604)*100)%>%arrange(percent)
###### Here comes the plot
<<<<<<< HEAD
c<-ggplot(demographics.snetworks,aes(x=snetworks,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=14))+xlab("c) What other social networks\n do you use?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,10,30,50,70,90,100),labels=c("0","10","30","50","70","90","100"),limits = c(0,100))+coord_flip()


#Which access methods do people use?
demographics.usage<-demographics%>%select(contains("usage"))%>%plyr::rename(c("X1demo_usage_twitterweb"="Twitter Website","X1demo_usage_twitterclientsmart"="Official\n Smartphone Client","X1demo_usage_tweetdeck"="TweetDeck","X1demo_usage_tweeterific"="Tweeterific","X1demo_usage_ubersocial"="UberSocial","X1demo_usage_mac"="DesktopClient","X1demo_usage_other"="Other(s)"))%>%summarise_each(funs(sum(!is.na(.))))%>%gather("usage","count",1:7)%>%mutate(percent=(count/604)*100)%>%arrange(percent)
####### Here comes the plot
d<-ggplot(demographics.usage,aes(x=usage,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=14))+xlab("d) Which of the following are \n common means for you to use Twitter?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,5,10,30,50,70,90,100),labels=c("0","5","10","30","50","70","90","100"),limits = c(0,100))+coord_flip()
=======
c<-ggplot(demographics.snetworks,aes(x=snetworks,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=16))+xlab("What other social networks\n do you use?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,10,30,50,70,90,100),labels=c("0","10","30","50","70","90","100"),limits = c(0,100))+coord_flip()


#Which access methods do people use?
demographics.usage<-demographics%>%select(contains("usage"))%>%plyr::rename(c("X1demo_usage_twitterweb"="Twitter Website","X1demo_usage_twitterclientsmart"="Official Smartphone Client","X1demo_usage_tweetdeck"="TweetDeck","X1demo_usage_tweeterific"="Tweeterific","X1demo_usage_ubersocial"="UberSocial","X1demo_usage_mac"="DesktopClient","X1demo_usage_other"="Other(s)"))%>%summarise_each(funs(sum(!is.na(.))))%>%gather("usage","count",1:7)%>%mutate(percent=(count/604)*100)%>%arrange(percent)
####### Here comes the plot
d<-ggplot(demographics.usage,aes(x=usage,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=16))+xlab("Which of the following are \ncommon means to use Twitter?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,5,10,30,50,70,90,100),labels=c("0","5","10","30","50","70","90","100"),limits = c(0,100))+coord_flip()
>>>>>>> 73e5cc62a9406832f9dcd9d11fb354efadc655bc


############
### When did they post their post tweet ?
levels(demographics$X1demo_firstpost)<-list("about a month ago"=c("monthago","about a month ago"),"about half a year ago"=c("about half a year ago","halfyearago"),"about a year ago"=c("yearormore","about a year or more"),"about two years ago"=c("about 2 years or more","2yearsormore"),"about five years ago"=c("about 5 years or more","5yearsoremore"))
demographics.firstpost<-demographics%>%group_by(X1demo_firstpost)%>%summarise(count=n())%>%filter(!is.na(X1demo_firstpost))%>%mutate(percent=(count/604)*100)
# Here comes the plot
<<<<<<< HEAD
b<-ggplot(demographics.firstpost,aes(x=X1demo_firstpost,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=14))+xlab("b) How long has it been since\n you posted your first Tweet?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,10,20,30,40,50),labels=c("0","10","20","30","40","50"),limits = c(0,50))+coord_flip()
=======
b<-ggplot(demographics.firstpost,aes(x=X1demo_firstpost,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=16))+xlab("How long has it been since\n you posted your first Tweet?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,10,20,30,40,50),labels=c("0","10","20","30","40","50"),limits = c(0,50))+coord_flip()
>>>>>>> 73e5cc62a9406832f9dcd9d11fb354efadc655bc

#############
### What is the highest level of education you achieved or working towards 
levels(demographics$X1demo_edu)<-list("School Qualification"=c("schooledu","school qualifications (i.e. highschool)"),"Technical Training"=c("vocational","vocational education (i.e. technical training)"),"Bachelor's Degree"=c("bachelor","bachelor's degree"),"Master's Degree"=c("master's degree","master"),"Ph.D."=c("phd","doctoral degree"))
demographics.edu<-demographics%>%group_by(X1demo_edu)%>%summarise(count=n())%>%mutate(percent=(count/604)*100)
<<<<<<< HEAD
a<-ggplot(demographics.edu,aes(x=X1demo_edu,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=14))+xlab("a) What is the highest level of education\n you achieved or working towards?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,10,20,30,40,50),labels=c("0","10","20","30","40","50"),limits = c(0,50))+coord_flip()
=======
a<-ggplot(demographics.edu,aes(x=X1demo_edu,y=percent))+geom_bar(colour="black",stat="identity")+theme(text=element_text(size=16))+xlab("What is the highest level of education\n you achieved or working towards?")+ylab("Percentage (%)")+scale_y_continuous(breaks=c(0,10,20,30,40,50),labels=c("0","10","20","30","40","50"),limits = c(0,50))+coord_flip()
>>>>>>> 73e5cc62a9406832f9dcd9d11fb354efadc655bc

multiplot(a,b,c,d,cols=2)
