#####################################
# processing read data
#######################################

readAutomata<- function(fileName){
  dataList <- list()
  i<-1;
  myFile<-file(fileName, open = "r")
  #myFile<-file("finite_state_machine.txt", open = "r")
  while( length(oneLine<-readLines(myFile,n=1, warn=FALSE))>0){
    myVector <- (strsplit(oneLine, " "))
    if(i<=3 & i>=1){dataList<-c(dataList, as.numeric(myVector))}
    else {dataList<- c(dataList, myVector);}
    i<-i+1;
  }
  close(myFile)
  if(length(dataList)==0) {print("Warning. Empty file"); return(NULL)}
  dataList
  fsm<-data.frame()# finite 
  if(length(dataList)>=5){
    for( i in 5:length(dataList)){
      fsm<-rbind(fsm,data.frame(from = dataList[[i]][1],
                                symbol = dataList[[i]][2],to = dataList[[i]][3]
      ))
    }
    
    if(length(levels(fsm$to))>length(levels(fsm$from)))
      state.factor<-(fsm$to) else state.factor<-(fsm$from)# not on a new line
      
      merged.fsm<-merge(levels(state.factor), levels(fsm$symbol), by=NULL)
      merged.fsm<-data.frame(from = merged.fsm$x, symbol=merged.fsm$y)
      sub.fsm<-subset(fsm, select=1:2)
      
      library(dplyr)
      new.factor=floor(runif(1, -999, -1))
      # создание тупикового состояния
      if(nrow(anti_join(merged.fsm,sub.fsm))!=0){
        fsm<-rbind(fsm,data.frame(anti_join(merged.fsm,sub.fsm),
                                  to = as.factor(new.factor)), 
                   data.frame(from=rep(as.factor(new.factor),
                                       times=length(levels(fsm$symbol))),symbol=levels(fsm$symbol),
                              to=rep(as.factor(new.factor),times=length(levels(fsm$symbol)))))
      }
      
      final<-as.factor(dataList[[4]][2:length(dataList[[4]])])
      all<-as.factor(levels(fsm$from))
      start<-as.factor(dataList[[3]])
      
      states<-list(start=start,all=all,final=final)
  } else {
    
    fsm<-data.frame(from=NULL,symbol=NULL, to=NULL)
    final<-as.factor(dataList[[4]][2:length(dataList[[4]])])
    all<-as.factor(levels(fsm$from))
    start<-as.factor(dataList[[3]])
    
    states<-list(start=start,all=all,final=final)
  }
  list(fsm = fsm,states = states, voc = dataList[[1]])
}
##################
###################
# length of fsm#####
###################

non.zero.length.fsm<-function(fsm1, fsm2)
{
  if(length(fsm1$fsm)==0) ans<-c(FALSE) else ans<-c(TRUE)
  if(length(fsm2$fsm)==0) ans<-c(ans,FALSE) else ans<-c(ans, TRUE)
  return (ans)
}
############
myFunc2<-function(fsm1,fsm2){
  ans<-non.zero.length.fsm(fsm1=fsm1, fsm2=fsm2)
  l<-length(ans[ans==FALSE])
  if(l==2) TRUE else if(l==1) FALSE else NULL
}



#######################
#######################
########################
# check equivalence myFunc()
########################

myFunc<-function(fsm1.states=fsm1.states,fsm2.states=fsm2.states,
                 fsm1.auto=fsm1.auto,fsm2.auto=fsm2.auto){
  alphabet<-levels(fsm1.auto$symbol)
  
  q0= union(fsm1.states$start, fsm2.states$start)
  stack.pair<-list(q0)
  set<-list(q0)
  
  while(length(stack.pair)!=0){
    
    temp.q<-stack.pair[[length(stack.pair)]]
    stack.pair<-stack.pair[-length(stack.pair)]
    
    for(i in 1:length(alphabet)){
      
      r1<-subset(fsm1.auto$to,fsm1.auto$from ==temp.q[1]
                 & fsm1.auto$symbol==alphabet[i])

      r2<-subset(fsm2.auto$to,fsm2.auto$from ==temp.q[2]
                 & fsm2.auto$symbol==alphabet[i])

      
      
      subs1<-getSet(set,r1)
      subs2<-getSet(set,r2)
      if(!is.null(subs1)) {subs<-subs1} else 
      {subs<-subs2}

      if(length(subs1)!=0 & length(subs2)!=0 ) {r<-union(subs1$set,subs2$set);
      if(subs1$index!=subs2$index) set<-set[-subs2$index]} else
        r=union(r1,r2)
      
      if(is.null(subs)) # only one class exist
      {
        set[length(set)+1]<-list(r)
        stack.pair[length(stack.pair)+1]<-list(union(r1,r2))
      } else {
        set[subs$index]<-list(union(subs$set, r))
        if(!(identical(subs1,subs2))){
          stack.pair[length(stack.pair)+1]<-list(union(r1,r2))} 
        
      } 
    }
  }
  set
}
##################
##################
###################
getSet<-function(set,elem){
  for( i in 1: length(set))
  {
    if (elem %in% set[[i]]) return(list(set=set[[i]],index=i))
  }
  return(NULL)
}




################################
################################
### accepting the same language?############
####################################

is.equal.auto<- function(mySet=set.states, final1=fsm1.states$final,
                         final2=fsm2.states$final){
  
  for( i in 1: length(mySet))
  {
    bool1<-(final1 %in% mySet[[i]])
    bool2<-(final2 %in% mySet[[i]])
    amount<-length(bool1[bool1==TRUE])+length(bool2[bool2==TRUE])
    if(amount!= length(mySet[[i]]) & amount!=0){
      return(FALSE)        
    }
  }
  return(TRUE)
}
####################
####################
####################

check<- function(fsm1=fsm1, fsm2=fsm2){
  if(is.null(fsm1) | is.null(fsm2)) return(FALSE)
  if(fsm1$voc!=fsm2$voc) {return(FALSE)} 

  if(length(fsm1$states$all[fsm1$states$all %in% fsm2$states$all])!=0)
  {print("Please check your auto. Warning"); return(FALSE);}
  return(TRUE);
}
  
