require(dplyr)
require(likert)

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
levels(preserving$X3preserv_freq)<-list(never="never",rarely="rarely","a few times a month"=c("aftmonth","a few times a month"),weekly="weekly","a few times a week"=c("aftweek","a few times a week"),daily="daily","multiple times per day"=c("multipleday","multiple times per day"))
levels(preserving$X3preserv_fav)<-list("strongly disagree"=c("strdisag","strongly disagree"),"disagree"="disagree",neutral="neutral",agree="agree","strongly agree"=c("stragree","strongly agree"))

levels(testi$X3preserv_fav)<-list("strongly disagree"=c("strdisag","strongly disagree"),"disagree"="disagree",neutral="neutral",agree="agree","strongly agree"=c("stragree","strongly agree"))
nlevels(testi$X3preserv_freq)

surveyDataFactorCleaner <-function(data.frame){
  for(i in seq_along(data.frame)){
    if(nlevels(data.frame[[i]])==10){
      levels(data.frame[[i]])<-list(never="never",rarely="rarely","a few times a month"=c("aftmonth","a few times a month"),weekly="weekly","a few times a week"=c("aftweek","a few times a week"),daily="daily","multiple times per day"=c("multipleday","multiple times per day"))
    }else if(nlevels(data.frame[[i]])==7){
      levels(data.frame[[i]])<-list("strongly disagree"=c("strdisag","strongly disagree"),"disagree"="disagree",neutral="neutral",agree="agree","strongly agree"=c("stragree","strongly agree"))
    }
  }
  return(data.frame)
}


