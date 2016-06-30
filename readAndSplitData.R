
########################
### helper stuff ######
require(dplyr)
require(likert)
likert.style<-theme_bw(base_size = 16)+theme(legend.position="bottom",legend.title=element_blank())
surveyDataFactorCleaner <-function(data.frame){
  for(i in seq_along(data.frame)){
    if(nlevels(data.frame[[i]])==10){
      levels(data.frame[[i]])<-list(never="never",rarely="rarely","a few times a month"=c("aftmonth","a few times a month"),weekly="weekly","a few times a week"=c("aftweek","a few times a week"),daily="daily","multiple times per day"=c("multipleday","multiple times per day"))
    }else if(nlevels(data.frame[[i]])==7){
      levels(data.frame[[i]])<-list("strongly disagree"=c("strdisag","strongly disagree"),"disagree"="disagree",neutral="neutral",agree="agree","strongly agree"=c("stragree","strongly agree"))
    }else{
      data.frame[[i]]<-data.frame[[i]]
    }
  }
  return(data.frame)
}

######################


survey_data<- read.csv("~/Dropbox/DISS_NEU/Twitter-Survey/ANALYSIS/survey-analysis/survey-analysis/survey_data_fusion.csv", sep=";",na.strings=c(""," ","NA"))
demographics<-survey_data%>%select(X1demo_gender:X1demo_usage_other)
# We will make data.frames for conceptually close columns and for analysis we merge them again

#faving
faving<-survey_data%>%select(X2fav_knowfavoption:X2fav_unfavreason)
faving<-faving%>%select(-c(X2fav_favreason,X2fav_favtweetexample,X2fav_favreasonown,X2fav_unfavreason))

#preserving  own stuff is not used makes no sense imho
preserving<-survey_data%>%select(X3preserv_freq:X3preserv_notreat)
preserving<-preserving%>%select(-X3preserv_reason)

#refinding
refinding<-survey_data%>%select(X4refind_freq:X4refind_long_timegap)
refinding<-refinding%>%select(-contains("_own"))
refinding<-refinding%>%select(-c(X4refind_exampleTweet,X4refind_reasons,X4refind_difficulty))

#twitterrating
rating <-survey_data%>%select(X5sum_preservingopt:X5sum_refind_frust,X4refind_difficulty)

# preserving likert plot 
preserving<-surveyDataFactorCleaner(preserving)
preserving<-preserving%>%select(-X3preserv_freq)
preserving<-plyr::rename(preserving,c("X3preserv_fav"="PreserveStrategy:Favouriting","X3preserv_retweet"="PreserveStrategy:Retweeting","X3preserv_addstore"="PreserveStrategy:SeparateStore","X3preserv_notreat"="PreserveStrategy:NoSpecial"))
preserving.plot<-likert(preserving)
likert.bar.plot(preserving.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style 


# refinding likert plot
refinding<-surveyDataFactorCleaner(refinding)
refinding <- refinding%>%select(X4refind_searchtimeline:X4refind_lookstore)
refinding<-plyr::rename(refinding,c("X4refind_searchtimeline"="RefindingStrategy:OwnTimeline","X4refind_searchfavlist"="RefindingStrategy:FavouritesList","X4refind_searchtimeline_person"="RefindingStrategy:UserProfile","X4refind_query"="RefindingStrategy:TwitterSearch","X4refind_searchengine"="RefindingStrategy:SearchEngine","Xrefind_lookstore"="RefindingStrategy:ExternalStore"))
refinding.plot<-likert(refinding)
likert.bar.plot(refinding.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style 

# User who frequently preserv also frequently refind?
preserv.refind.freq<-survey_data%>%select(X3preserv_freq,X4refind_freq)
preserv.refind.freq<-surveyDataFactorCleaner(preserv.refind.freq)

preserv.refind.freq$X3preserv_freq<-as.numeric(preserv.refind.freq$X3preserv_freq)
preserv.refind.freq$X4refind_freq<-as.numeric(preserv.refind.freq$X4refind_freq)

#Spearman Rho for Correlation with ordinal data
cor(preserv.refind.freq,use="complete.obs",method="spearman")
#r=0.46  relativ starke Beziehung zwischen Aufbewahren und Wiederfinden


###############
# Haben Nutzer die nie oder selten Tweets aufbewahren anderen Strategien wie Nutzer die häufig aufbewahren?

refinding.preserv.relation<-bind_cols(refinding,preserv.refind.freq)
# re-code preserving frequency to rarely and often
refinding.preserv.relation<-refinding.preserv.relation%>%mutate(preserv.freq=ifelse(X3preserv_freq<3,"never/rarely","often"))
refinding.preserv.relation<-refinding.preserv.relation%>%select(-c(X4refind_freq,X3preserv_freq))
refinding.preserv.relation$preserv.freq<-as.factor(refinding.preserv.relation$preserv.freq) 
refinding.preserv.relation.plot<-likert(refinding.preserv.relation,grouping = refinding.preserv.relation$preserv.freq) 

table(refinding.preserv.relation$`RefindingStrategy:FavouritesList`,refinding.preserv.relation$preserv.freq)

refinding.preserv.relation%>%group_by()%>%summarise(count=n())
