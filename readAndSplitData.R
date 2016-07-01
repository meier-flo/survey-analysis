
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
   }else if(nlevels(data.frame[[i]])==8){
     levels(data.frame[[i]])<-list("very poor"=c("verypoor","very poor"),"poor"="poor","neutral"=c("neutral","ok"),"good"="good","very good"=c("very good","very goo","verygood"))
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
rating<-surveyDataFactorCleaner(rating)

str(survey_data$X5sum_preservingopt)
str(rating$X5sum_preservingopt)

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

############
##  Korrelation preserving <-> refinding 
###########
# User who frequently preserv also frequently refind?
preserv.refind.freq<-survey_data%>%select(X3preserv_freq,X4refind_freq)
preserv.refind.freq<-surveyDataFactorCleaner(preserv.refind.freq)
preserv.refind.freq$X3preserv_freq<-as.numeric(preserv.refind.freq$X3preserv_freq)
preserv.refind.freq$X4refind_freq<-as.numeric(preserv.refind.freq$X4refind_freq)
#Spearman Rho for Correlation with ordinal data
cor(preserv.refind.freq,use="complete.obs",method="spearman")
#r=0.46  relativ starke Beziehung zwischen Aufbewahren und Wiederfinden


###############
### Haben Nutzer die nie oder selten Tweets aufbewahren anderen Strategien wie Nutzer die häufig aufbewahren?
#############

refinding.preserv.relation<-bind_cols(refinding,preserv.refind.freq)
# re-code preserving frequency to rarely and often
refinding.preserv.relation<-refinding.preserv.relation%>%mutate(preserv.freq=ifelse(X3preserv_freq<2,"never","often"))
refinding.preserv.relation<-refinding.preserv.relation%>%select(-c(X4refind_freq,X3preserv_freq))
refinding.preserv.relation$preserv.freq<-as.factor(refinding.preserv.relation$preserv.freq) 
refinding.preserv.relation<-refinding.preserv.relation[complete.cases(refinding.preserv.relation),]

# somehow it doesn't work with the grouping varialbe also in the same data frame so 
freq.group<-refinding.preserv.relation$preserv.freq
refinding.preserv.relation<-refinding.preserv.relation%>%select(-preserv.freq)

refinding.preserv.relation.plot<-likert(refinding.preserv.relation,grouping = freq.group) 
likert.bar.plot(refinding.preserv.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style 

######################
#### Haben Nutzer die sagen wiederfinden ist einfach andere Strategien als Nutzer die sagen Wiederfinden ist schwer 
######################

refinding.difficulty.relation <-bind_cols(refinding,rating)
#make difficulty numeric
refinding.difficulty.relation$X4refind_difficulty<-as.numeric(refinding.difficulty.relation$X4refind_difficulty)
#re-code difficulty level 
refinding.difficulty.relation<-refinding.difficulty.relation%>%mutate(rf.difficult=ifelse(X4refind_difficulty<4,"yes","no"))
refinding.difficulty.relation<-refinding.difficulty.relation%>%select(-(X5sum_preservingopt:X4refind_difficulty))

#refinding.difficulty.relation%>%group_by(rf.difficult)%>%summarise(count=n())
refinding.difficulty.relation$rf.difficult<-as.factor(refinding.difficulty.relation$rf.difficult)
refinding.difficulty.relation<-refinding.difficulty.relation[complete.cases(refinding.difficulty.relation),]

diff.freq.group<-refinding.difficulty.relation$rf.difficult
refinding.difficulty.relation<-refinding.difficulty.relation%>%select(-rf.difficult)

#### Plot the stuff 
refinding.difficulty.relation.plot<-likert(refinding.difficulty.relation,grouping=diff.freq.group)
likert.bar.plot(refinding.difficulty.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style 

#########################
##### Nutzer die sagen sie waren noch nie oder seltenfrustriert andere RF-Strategien?
#########################
refinding.frustration.relation <-bind_cols(refinding,rating)
#make frustration numeric
refinding.frustration.relation$X5sum_refind_frust<-as.numeric(refinding.frustration.relation$X5sum_refind_frust)  
#re-code frustration level 
refinding.frustration.relation<-refinding.frustration.relation%>%mutate(rf.frust=ifelse(X5sum_refind_frust<4,"no","yes"))
refinding.frustration.relation<-refinding.frustration.relation%>%select(-(X5sum_preservingopt:X4refind_difficulty))

#refinding.frustration.relation%>%group_by(rf.frust)%>%summarise(count=n())

refinding.frustration.relation$rf.frust<-as.factor(refinding.frustration.relation$rf.frust)
refinding.frustration.relation<-refinding.frustration.relation[complete.cases(refinding.frustration.relation),]

frust.freq.group<-refinding.frustration.relation$rf.frust
refinding.frustration.relation<-refinding.frustration.relation%>%select(-rf.frust)
refinding.frustration.relation.plot<-likert(refinding.frustration.relation,grouping=frust.freq.group)

#plot(refinding.frustration.relation.plot)
likert.bar.plot(refinding.frustration.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style 

######################################
########### Nutzer die sagen die Aufbewahrensfunktionen sind gut - welche Nutzen die ?
#####################################
preserving.option.relation<-bind_cols(preserving,rating)
#make frustration numeric
preserving.option.relation$X5sum_preservingopt<-as.numeric(preserving.option.relation$X5sum_preservingopt)

