
########################
### helper stuff ######
require(dplyr)
require(plyr)
require(likert)
require(corrplot)
likert.style<-theme_bw(base_size = 14)+theme(legend.position="bottom",legend.title=element_blank())

surveyDataFactorCleaner <-function(data.frame){
  for(i in seq_along(data.frame)){
    if(nlevels(data.frame[[i]])>8){
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

#Helper Function for building a Summary Table to plot with tableGrob
summaryTable<-function(data){
  #names<-c("Min.","1st Quantile","Median","Mean","3rd Quantile","Max.","SD")
  #values<-c(min(data),quantile(data,0.25),median(data),mean(data),quantile(data,0.75),max(data),sd(data))
  #summary<-data.frame(names,values)
  summary<-data.frame()  
  values<-c(min(data,na.rm=TRUE),quantile(data,0.25,na.rm=TRUE),median(data,na.rm=TRUE),mean(data,na.rm=TRUE),quantile(data,0.75,na.rm=TRUE),max(data,na.rm=TRUE),sd(data,na.rm=TRUE))
  values<-round(values,2)
  summary<-rbind(summary,values)
  colnames(summary)<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","SD")
  return(summary)
}

# Theme for Tables in Plots 
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 1.5)),
  colhead = list(fg_params=list(cex = 1.0)),
  rowhead = list(fg_params=list(cex = 1.0))
)
######
#Plot Pics in res  830 x 400
#Plot pics with groups 830x 600
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



# preserving likert plot 
preserving<-surveyDataFactorCleaner(preserving)
preserving<-preserving%>%select(-X3preserv_freq)
preserving<-plyr::rename(preserving,c("X3preserv_fav"="Favourite","X3preserv_retweet"="Retweet","X3preserv_addstore"="Use Separate Store","X3preserv_notreat"="No Special Action"))
#preserving<-preserving[complete.cases(preserving),]
preserving.plot<-likert(preserving)
likert.bar.plot(preserving.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("When you want to preserve a Tweet for later, do you ...\n (n=",nrow(preserving),")"))


# refinding likert plot
refinding <-refinding%>%select(X4refind_searchtimeline:X4refind_lookstore)
refinding<-surveyDataFactorCleaner(refinding)
refinding<-plyr::rename(refinding,c("X4refind_searchtimeline"="Scan Own Timeline","X4refind_searchfavlist"="Scan Favourites List","X4refind_searchtimeline_person"="Scan Sender Profile","X4refind_query"="Use Twitter Search","X4refind_searchengine"="Use Search Engine","X4refind_lookstore"="Scan External Store"))
#refinding<-refinding[complete.cases(refinding),]
refinding.plot<-likert(refinding)
likert.bar.plot(refinding.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("When you want to find a Tweet again, do you ...\n(n=",nrow(refinding),")"))


###############
### Haben Nutzer die nie oder selten Tweets aufbewahren anderen Strategien wie Nutzer die häufig aufbewahren?
#############

refinding.preserv.relation<-bind_cols(refinding,preserv.refind.freq.corr)
# re-code preserving frequency to rarely and often
refinding.preserv.relation<-refinding.preserv.relation%>%mutate(preserv.freq=ifelse(X3preserv_freq<2,"never","at least rarely"))
refinding.preserv.relation<-refinding.preserv.relation%>%select(-c(X3preserv_freq:X4refind_freqown))
refinding.preserv.relation$preserv.freq<-as.factor(refinding.preserv.relation$preserv.freq) 
refinding.preserv.relation<-refinding.preserv.relation[complete.cases(refinding.preserv.relation),]

# somehow it doesn't work with the grouping varialbe also in the same data frame so 
freq.group<-refinding.preserv.relation$preserv.freq
refinding.preserv.relation<-refinding.preserv.relation%>%select(-preserv.freq)

freq.group.table<-table(freq.group)

#### Plot that stuff
refinding.preserv.relation.plot<-likert(refinding.preserv.relation,grouping = freq.group) 
likert.bar.plot(refinding.preserv.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("Re-finding Strategies Grouped by Preserve Frequency\n(at least rarely: n=",freq.group.table[1]," | never: n=",freq.group.table[2],")")) 

######################
#### Haben Nutzer die sagen wiederfinden ist einfach andere Strategien als Nutzer die sagen Wiederfinden ist schwer 
######################

refinding.difficulty.relation <-bind_cols(refinding,rating)
#make difficulty numeric
refinding.difficulty.relation$X4refind_difficulty<-as.numeric(refinding.difficulty.relation$X4refind_difficulty)
#re-code difficulty level 
refinding.difficulty.relation<-refinding.difficulty.relation%>%mutate(rf.difficult=ifelse(X4refind_difficulty<4,"hard","easy"))
refinding.difficulty.relation<-refinding.difficulty.relation%>%select(-(X5sum_preservingopt:X4refind_difficulty))

#refinding.difficulty.relation%>%group_by(rf.difficult)%>%summarise(count=n())
refinding.difficulty.relation$rf.difficult<-as.factor(refinding.difficulty.relation$rf.difficult)
refinding.difficulty.relation<-refinding.difficulty.relation[complete.cases(refinding.difficulty.relation),]

diff.freq.group<-refinding.difficulty.relation$rf.difficult
refinding.difficulty.relation<-refinding.difficulty.relation%>%select(-rf.difficult)

diff.freq.group.table<-table(diff.freq.group)

#### Plot the stuff 
refinding.difficulty.relation.plot<-likert(refinding.difficulty.relation,grouping=diff.freq.group)
likert.bar.plot(refinding.difficulty.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("Re-finding Strategies Grouped by Re-finding Difficulty\n(easy: n=",diff.freq.group.table[1]," | hard: n=",diff.freq.group.table[2],")")) 

#########################
##### Nutzer die sagen sie waren noch nie oder seltenfrustriert andere RF-Strategien?
#########################
refinding.frustration.relation <-bind_cols(refinding,rating)
#make frustration numeric
refinding.frustration.relation$X5sum_refind_frust<-as.numeric(refinding.frustration.relation$X5sum_refind_frust)  
#re-code frustration level 
refinding.frustration.relation<-refinding.frustration.relation%>%mutate(rf.frust=ifelse(X5sum_refind_frust<2,"never","at least rarely"))
refinding.frustration.relation<-refinding.frustration.relation%>%select(-(X5sum_preservingopt:X4refind_difficulty))

#refinding.frustration.relation%>%group_by(rf.frust)%>%summarise(count=n())

refinding.frustration.relation$rf.frust<-as.factor(refinding.frustration.relation$rf.frust)
refinding.frustration.relation<-refinding.frustration.relation[complete.cases(refinding.frustration.relation),]

frust.freq.group<-refinding.frustration.relation$rf.frust
refinding.frustration.relation<-refinding.frustration.relation%>%select(-rf.frust)

frust.freq.group.table<-table(frust.freq.group)

#### Plot that stuff 
refinding.frustration.relation.plot<-likert(refinding.frustration.relation,grouping=frust.freq.group)
likert.bar.plot(refinding.frustration.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("Re-finding Strategies Grouped by Frustration Experience\n(at least rarely: n=",frust.freq.group.table[1]," | never: n=",frust.freq.group.table[2],")")) 

######################################
########### Nutzer die sagen die Aufbewahrensfunktionen sind gut - welche Nutzen die ?
#####################################
preserving.option.relation<-bind_cols(preserving,rating)
#make frustration numeric
preserving.option.relation$X5sum_preservingopt<-as.numeric(preserving.option.relation$X5sum_preservingopt)
#recode People say good/bad preserving options
preserving.option.relation<-preserving.option.relation%>%mutate(preserve.opt=ifelse(X5sum_preservingopt<4,"not satisfied","satisfied"))
preserving.option.relation<-preserving.option.relation%>%select(-(X5sum_preservingopt:X4refind_difficulty))

#preserving.option.relation%>%group_by(preserve.opt)%>%summarise(count=n())

preserving.option.relation$preserve.opt<-as.factor(preserving.option.relation$preserve.opt)
preserving.option.relation<-preserving.option.relation[complete.cases(preserving.option.relation),]

preserveopt.freq.group<-preserving.option.relation$preserve.opt
preserving.option.relation<-preserving.option.relation%>%select(-preserve.opt)

preserveopt.freq.group.table<-table(preserveopt.freq.group)

###### Plot that stuff 
preserving.option.relation.plot<-likert(preserving.option.relation,grouping = preserveopt.freq.group)
likert.bar.plot(preserving.option.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("Preserve Strategies Grouped by Preserve Options Satisfaction \n(not satisfied: n=",preserveopt.freq.group.table[1]," | satisfied: n=",preserveopt.freq.group.table[2],")"))

###################################################
############### Nutzer die sagen re-finding optionen sind gut, welche Nutzen die ?
##################################################
refinding.option.relation<-bind_cols(refinding,rating)
refinding.option.relation$X5sum_refindingopt<-as.numeric(refinding.option.relation$X5sum_refindingopt)

# recode people say bad re-finding options good re-finding options 
refinding.option.relation<-refinding.option.relation%>%mutate(rf.option=ifelse(X5sum_refindingopt<4,"not satisfied","satisfied"))
# drop the other columns
refinding.option.relation<-refinding.option.relation%>%select(-(X5sum_preservingopt:X4refind_difficulty))
# turn it into factor 
refinding.option.relation$rf.option<-as.factor(refinding.option.relation$rf.option)
refinding.option.relation<-refinding.option.relation[complete.cases(refinding.option.relation),]

refinding.opt.group<-refinding.option.relation$rf.option
refinding.option.relation<-refinding.option.relation%>%select(-rf.option)

refinding.opt.group.table<-table(refinding.opt.group)

##### Plot that stuff
refinding.option.relation.plot<-likert(refinding.option.relation,grouping = refinding.opt.group)
likert.bar.plot(refinding.option.relation.plot,low.color = "#E69F00",high.color = "#56B4E9",text.size = rel(3.5))+likert.style+ggtitle(paste0("Re-finding Strategies Grouped by Re-finding Options Satisfaction \n(not satisfied: n=",refinding.opt.group.table[1]," | satisfied: n=",refinding.opt.group.table[2],")")) 


###############################################
####  Korrelation of a lot of stuff 
###############################################
#require(corrplot)
# User who frequently preserv also frequently refind?
preserv.refind.freq<-survey_data%>%select(X3preserv_freq,X4refind_freq,X4refind_freqown)
preserv.refind.freq<-surveyDataFactorCleaner(preserv.refind.freq)
preserv.refind.freq.corr<-as.data.frame(lapply(preserv.refind.freq,as.numeric))

# The numerics of Rating Columns
rating.corr <- as.data.frame(lapply(rating,as.numeric))
# bind the two data frames 
corr.plot<-bind_cols(preserv.refind.freq.corr,rating.corr)
corr.plot<-plyr::rename(corr.plot,c("X3preserv_freq"="Preserve\nFrequency","X4refind_freq"="Refinding\nFrequency","X4refind_freqown"="Re-finding\nFrequency\nOwn Tweets","X5sum_preservingopt"="Preserve\nOptions\nSatisfaction","X5sum_refindingopt"="Re-finding\nOptions\nSatisfaction","X5sum_refind_frust"="Re-finding\nFrustration","X4refind_difficulty"="Re-finding\nDifficulty"))

#### Plot that stuff 
cor.object<-cor(corr.plot,use="complete.obs",method="spearman")
corr.color<-colorRampPalette(c("#E69F00","grey","#56B4E9"))
corrplot.mixed(cor.object,lower="circle",upper="number",col=corr.color(10),tl.col="black")

#corrplot(cor.object,type="upper",method = "number",col = corr.color(10),)


##############################################
############ Preserve Frequency

#plot<-ggplot(preserv.refind.freq.corr,aes(y=X4refind_freq,x=factor(0)))+geom_boxplot(outlier.size = 3)+xlab("")+ylab("Preserve Frequency")+coord_flip()+theme_set(theme_grey(base_size = 21))
#table<-tableGrob(summaryTable(preserv.refind.freq.corr$X4refind_freq),rows=NULL,theme=mytheme)
#grid.arrange(plot,table,heights=c(1,0.5))

#testi<-preserv.refind.freq.corr%>%group_by(X4refind_freq)%>%summarise(count=n())


