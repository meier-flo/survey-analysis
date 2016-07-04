require(tidyr)
#survey_data<- read.csv("~/Dropbox/DISS_NEU/Twitter-Survey/ANALYSIS/survey-analysis/survey-analysis/survey_data_fusion.csv", sep=";",na.strings=c(""," ","NA"))
#demographics<-survey_data%>%select(X1demo_gender:X1demo_usage_other)


##### Which social networks do people use?
demographics.snetworks<-demographics%>%select(contains("snetworks"))%>%plyr::rename(c("X1demo_snetworks_facebook"="Facebook","X1demo_snetworks_googleplus"="Google+","X1demo_snetworks_linkedIn"="LinkedIn","X1demo_snetworks_myspace"="MySpace","X1demo_snetworks_pinterest"="Pinterest","X1demo_snetworks_other"="Other(s)"))%>%summarise_each(funs(sum(!is.na(.))))%>%gather("snetworks","count",1:6)%>%mutate(percent=count/604)
###### Here comes the plot



#Which access methods do people use?
demographics.usage<-demographics%>%select(contains("usage"))%>%plyr::rename(c("X1demo_usage_twitterweb"="Twitter Website","X1demo_usage_twitterclientsmart"="Official Smartphone Client","X1demo_usage_tweetdeck"="TweetDeck","X1demo_usage_tweeterific"="Tweeterific","X1demo_usage_ubersocial"="UberSocial","X1demo_usage_mac"="DesktopClient","X1demo_usage_other"="Other(s)"))%>%summarise_each(funs(sum(!is.na(.))))%>%gather("usage","count",1:7)%>%mutate(percent=count/604)
####### Here comes the plot
ggplot(demographics.usage,aes(x=usage,y=percent))+geom_bar(stat="identity")


demogrpahics. 