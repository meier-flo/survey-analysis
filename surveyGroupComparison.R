#Here we will do significance testing via bootstrapped confidence intervalls not containg the null effect

##### Function for doing the bootstrapping and means subtracting 
calcDiffMeanCI<-function(session.data.RF,session.data.NORF){
  booty.data<-data.frame("Feature"=character(),"Mean"=double(),"LowerCI"=double(),"UpperCI"=double(),stringsAsFactors=FALSE)
  #booty.data<-data.frame(stringsAsFactors = FALSE)
  #print(str(booty.data))
  
  sampleSize <-ifelse(nrow(session.data.RF)<nrow(session.data.NORF),nrow(session.data.NORF),nrow(session.data.RF))
  
  for (i in colnames(session.data.RF)){
    DiffMean<-c()
    norfvector<-data.frame()
    rfvector<-data.frame()
    vectorToProcess.RF<-session.data.RF[[i]]
    vectorToProcess.NORF<-session.data.NORF[[i]]
    
    doShuffleAndMean<-function(vectorToProcess.RF,vectorToProcess.NORF){ 
      sampleRF<-sample(vectorToProcess.RF,sampleSize,replace=TRUE)
      sampleNORF<-sample(vectorToProcess.NORF,sampleSize,replace=TRUE)
      diffMean=(mean(sampleRF,na.rm=TRUE)-mean(sampleNORF,na.rm=TRUE))
      return(diffMean)
    }
    
    resultVector<-replicate(10000,doShuffleAndMean(vectorToProcess.RF,vectorToProcess.NORF))
    ##### This is the Vector for not rf bootstrap sample 
    norfvector<-data.frame("Feature"=i,"Mean"=mean(resultVector),"LowerCI"=quantile(resultVector,0.025),"UpperCI"=quantile(resultVector,0.975))
    print(norfvector)
    booty.data<-rbind(booty.data,norfvector)
  }
  #colnames(booty.data)<-c("Feature","Mean","LowerCI","UpperCI","Group")
  return(booty.data)
}


######### Prepare the data.frames for the comparison #######
#### First do users who keep rarely/frequently have significantly different strategies?
# We need the two data frames  "refinding" and "preserv.refind.freq.corr"  respectively the refinding.preserv.relation
# 
# Turn to numeric
refinding.preserv.relation.numeric<-as.data.frame(lapply(refinding.preserv.relation,as.numeric))

#make one data.frame for each group
refind.diff.no.preserv <- refinding.preserv.relation.numeric%>%filter(preserv.freq==1)
refind.diff.often.preserv<- refinding.preserv.relation.numeric%>%filter(preserv.freq>1)
# delete the two freq columns
refind.diff.no.preserv<-refind.diff.no.preserv%>%select(-(X4refind_freqown:preserv.freq))
refind.diff.often.preserv<-refind.diff.often.preserv%>%select(-(X4refind_freqown:preserv.freq))

##### Calc Bootstrapped Difference 
diffMeansBoot.refinding.preserv.freq<-calcDiffMeanCI(refind.diff.often.preserv,refind.diff.no.preserv)

##### Plotting to visualise difference 
ggplot(diffMeansBoot.refinding.preserv.freq,aes(x=1,y=Mean))+geom_errorbar(aes(ymin=LowerCI,ymax=UpperCI),width=.1)+geom_point(size=5)+facet_wrap(~Feature,scales="free_y")+geom_hline(aes(yintercept=0),color="red",linetype="dashed")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+xlab("")
##################################################################################


##################################################################################
### Do people who say re-finding is easy have significant different strategies?###
### refinding.difficulty.relation  data.frame is needed

#turn to numeric
refinding.difficulty.relation.numeric<-as.data.frame(lapply(refinding.difficulty.relation,as.numeric))
#make the two data frames for the two groups
refinding.strategies.rf.easy<-refinding.difficulty.relation.numeric%>%filter(rf.difficult==1)
refinding.strategies.rf.hard<-refinding.difficulty.relation.numeric%>%filter(rf.difficult==2)
#strip the grouping variable

refinding.strategies.rf.easy<-refinding.strategies.rf.easy%>%select(-rf.difficult)
refinding.strategies.rf.hard<-refinding.strategies.rf.hard%>%select(-rf.difficult)

##### Calc Bootstrapped Difference 
diffMeansBoot.refinding.difficulty<-calcDiffMeanCI(refinding.strategies.rf.easy,refinding.strategies.rf.hard)

##### Plotting to visualise difference 
ggplot(diffMeansBoot.refinding.difficulty,aes(x=1,y=Mean))+geom_errorbar(aes(ymin=LowerCI,ymax=UpperCI),width=.1)+geom_point(size=5)+facet_wrap(~Feature,scales="free_y")+geom_hline(aes(yintercept=0),color="red",linetype="dashed")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+xlab("")


##################################################
######### Do users who say they have never been frustrated when re-finding other strategies 
### we need data.frame refinding.frustration.relation

refinding.frustration.relation.numeric<-as.data.frame(lapply(refinding.frustration.relation,as.numeric))

#make the two data frames for the two groups 
refinding.frustration.relation.never.frust<-refinding.frustration.relation.numeric%>%filter(rf.frust==1)
refinding.frustration.relation.sometimes.frust<-refinding.frustration.relation.numeric%>%filter(rf.frust==2)

# strip the grouping variable
refinding.frustration.relation.never.frust<-refinding.frustration.relation.never.frust%>%select(-rf.frust)
refinding.frustration.relation.sometimes.frust<-refinding.frustration.relation.sometimes.frust%>%select(-rf.frust)

##### Calc Bootstrapped Difference 
diffMeansBoot.refinding.frustration<-calcDiffMeanCI(refinding.frustration.relation.never.frust,refinding.frustration.relation.sometimes.frust)

##### Plotting to visualise difference 
ggplot(diffMeansBoot.refinding.frustration,aes(x=1,y=Mean))+geom_errorbar(aes(ymin=LowerCI,ymax=UpperCI),width=.1)+geom_point(size=5)+facet_wrap(~Feature,scales="free_y")+geom_hline(aes(yintercept=0),color="red",linetype="dashed")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+xlab("")

#####################################################
######## What preserv strategies do people use who say Preserve options are good 
### we need data.frame preserving.option relation

preserving.option.relation.numeric<-as.data.frame(lapply(preserving.option.relation,as.numeric))

#make the two groups
preserving.options.good<-preserving.option.relation.numeric%>%filter(preserve.opt==1)
preserving.options.bad<-preserving.option.relation.numeric%>%filter(preserve.opt==2)

#cut grouping variable 
preserving.options.good<-preserving.options.good%>%select(-preserve.opt)
preserving.options.bad<-preserving.options.bad%>%select(-preserve.opt)

##### Calc Bootstrapped Difference 
diffMeansBoot.preserving.option<-calcDiffMeanCI(preserving.options.good,preserving.options.bad)

##### Plotting to visualise difference 
ggplot(diffMeansBoot.preserving.option,aes(x=1,y=Mean))+geom_errorbar(aes(ymin=LowerCI,ymax=UpperCI),width=.1)+geom_point(size=5)+facet_wrap(~Feature,scales="free_y")+geom_hline(aes(yintercept=0),color="red",linetype="dashed")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+xlab("")
