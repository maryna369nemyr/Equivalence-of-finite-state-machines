source("functions.R")
#fsm1<-readAutomata("finite_state_machine.txt")
#fsm1<-readAutomata("1.txt")
#fsm1<-readAutomata("3.txt")
#fsm1<-readAutomata("empty.txt")
#fsm1<-readAutomata("0.txt")
fsm1<-readAutomata("new11.txt")
                              #fsm1.auto<-as.data.frame(fsm1$fsm)
                              #fsm1.states<-fsm1$states

#fsm2<-readAutomata("finite_state_machine_2.txt")
#fsm2<-readAutomata("2.txt")
#fsm2<-readAutomata("00.txt")
#fsm2<-readAutomata("4.txt")

fsm2<-readAutomata("new22.txt")
                              #fsm2.auto<-as.data.frame(fsm2$fsm)
                              #fsm2.states<-fsm2$states


if(check(fsm1=fsm1,fsm2=fsm2)){
  temp<-myFunc2(fsm1=fsm1, fsm2=fsm2)
 if(is.null(temp)){
set.states<-myFunc(fsm1.states = fsm1$states,fsm2.states = fsm2$states,
                   fsm1.auto = fsm1$fsm,fsm2.auto = fsm2$fsm)
#set.states<-myFunc(fsm1.states = fsm1.states,fsm2.states = fsm2.states,
#                   fsm1.auto = fsm1.auto,fsm2.auto = fsm2.auto)

is.equal.auto(mySet = set.states,final1=fsm1$states$final, final2=fsm2$states$final)
 }  else temp
}else FALSE





