Sys.getlocale()
Sys.setlocale("LC_ALL", "C")
set.seed(123)
install.packages('cem')
install.packages('readxl')
install.packages("tidyverse")
install.packages('Matching')
install.packages('sem')
install.packages('utf8')
install.packages('AER')
install.packages('LARF')
library(cem)
library(readxl)
library(tidyverse)
library(Matching)
library(sem)
library(utf8)
library(AER)
library(LARF)

# Observational study
# Data cleaning
#pdata=read.csv('parent_all_pay.csv', sep=",",row.names=NULL,encoding="UTF-8",stringsAsFactors=FALSE,na.strings=c(""))
pdata=read_excel('parent_all_pay1.xlsx', col_names=FALSE)
names(pdata)=c('ParentID','IsAudition','IsPay','StudentID','Correct','Complete','Rectify','CityTier','GoodSchool','Grade','ActivePenetration','PayPenetration','AuditionSubject','AuditionType','PaySubject','PayType','Price','PayDate','PayTime','PayGap','ActiveGap','Audition','ProductActive','ProductComplete')
pdata=pdata[-nrow(pdata),]
str(pdata)
pdata$PayDate=as.Date(pdata$PayDate,"%y-%m-%d")
pdata$Audition[is.na(pdata$Audition)]=0
summary(pdata)
sapply(pdata,function(x) sum(is.na(x)))
sapply(pdata,function(x) length(unique(x)))


# Internal dataviz
# IsAudition: Whether participated, Audition: whether offered
sum(pdata$IsAudition==1&pdata$IsPay==0)
sum(pdata$IsAudition==1)
sum(pdata$Audition==1)
sum(pdata$IsPay==1)
sum(pdata$IsPay==0)
hist(pdata$IsAudition)
hist(pdata$IsPay)
hist(pdata$Correct)
hist(pdata$Complete)
hist(pdata$Rectify)
barplot(table(pdata$CityTier))
hist(pdata$GoodSchool)
hist(pdata$Grade)
hist(pdata$ActivePenetration)
hist(pdata$PayPenetration)
barplot(table(pdata$AuditionSubject))
hist(pdata$Price)
hist(pdata$PayDate,'months')
hist(pdata$PayTime)
hist(pdata$PayGap)
hist(pdata$ActiveGap)
hist(pdata$Audition)
hist(pdata$ProductActive)
hist(pdata$ProductComplete)

#imbalance(group=pdata$Price, drop=c('IsPay','Price','ActiveGap'),data=pdata)
#qdata=cem(treatment='Price', drop=c('IsPay','Price','ActiveGap'),data=pdata)
#str(qdata)

#pdatatr=pdata[c(1:200000),]
#glm1=glm(ContentPay~Correct+Rectify+Complete,data=pdata,family='binomial')

#glm2=glm(IsPay~IsAudition+Correct+Complete+Rectify+GoodSchool+Grade+ActivePenetration+PayPenetration+PayTime+PayGap,data=pdata,family='binomial',maxit=100)
glm1=glm(IsPay~IsAudition+Correct+Complete+Rectify+GoodSchool+Grade+ActivePenetration+PayPenetration,data=pdata,family='binomial')
summary(glm1)
glm2=glm(IsPay~Audition+Correct+Complete+Rectify+GoodSchool+Grade+ActivePenetration+PayPenetration,data=pdata,family='binomial')
summary(glm2)
glm3=glm(IsPay~IsAudition,data=pdata,family='binomial')
summary(glm3)
glm4=glm(IsPay~Audition,data=pdata,family='binomial')
summary(glm4)

plot(pdata$IsAudition,predict(glm2,type='response'))
#What does this mean? How to represent regression graphically?

mout=Match(Y=pdata$IsPay,Tr=pdata$IsAudition,X=cbind(pdata$Correct,pdata$Complete,pdata$Rectify,pdata$GoodSchool,pdata$Grade,pdata$ActivePenetration,pdata$PayPenetration),exact=FALSE,estimand="ATT",M=1,BiasAdjust=FALSE,ties=FALSE)
#City tier?
mb=MatchBalance(IsAudition~Correct+Complete+Rectify+GoodSchool+Grade+ActivePenetration+PayPenetration,data=pdata,match.out=mout,nboots=100)
summary(mout)

#Gugong experiment data
gdata_raw=read_excel('gugong67.xlsx', col_names=TRUE)
names(gdata_raw)=c('ParentID','IsPay','PayAmount','IsRefund','IsAudition','IsParticipation','IsComplete','ViewDate','ViewTime','AuditionDate','AuditionTime','ParticipationDate','ParticipationTime','PayDate','PayTime','CourseDate','CourseTime','GPaymentView','GPaymentAudition','GPaymentParticipation','GCoursePayment','TotalPay','TotalPayAmount','TotalActive','TotalActiveTime','Correct','Complete','Rectify','City','GoodSchool','Grade','ChildAge','ParentGender','ChildGender')
summary(gdata_raw)
str(gdata_raw)

#Experimental study
#Data cleaning
gdata=gdata_raw
sapply(gdata,function(x) sum(is.na(x)))
sum(is.na(gdata$IsAudition))
gdata$IsAudition[gdata$ParentID%%2==0]=1
sum(is.na(gdata$IsParticipation))
gdata$IsParticipation[gdata$IsAudition==0]=0
gdata$IsParticipation[is.na(gdata$IsParticipation)==1]=0
sum(is.na(gdata$IsPay))
sum(is.na(gdata$IsRefund))
gdata$IsRefund[is.na(gdata$IsRefund)==1]=0
sum(is.na(gdata$IsComplete))
gdata$IsComplete[gdata$IsAudition==0]=0
gdata$IsComplete[gdata$IsParticipation==0]=0
gdata$IsComplete[is.na(gdata$IsComplete)==1]=0
sum(is.na(gdata$PayAmount))
gdata$PayAmount[is.na(gdata$PayAmount)==1]=0
gdata$ParentGender[gdata$ParentGender==utf8_normalize("女")]=as.numeric(0)
gdata$ParentGender[gdata$ParentGender==utf8_normalize("男")]=as.numeric(1)
gdata$ChildGender[gdata$ChildGender==utf8_normalize("女")]=as.numeric(0)
gdata$ChildGender[gdata$ChildGender==utf8_normalize("男")]=as.numeric(1)
gdata$ParentGender=ifelse(gdata$ParentGender==utf8_normalize("1"),1,0)
gdata$ChildGender=ifelse(gdata$ChildGender==utf8_normalize("0"),1,0)
unique(gdata$PayAmount)
unique(gdata$ChildAge)
gdata$GoodSchool[is.na(gdata$GoodSchool)==1]=0
gdata$Grade[is.na(gdata$Grade)==1]=1
gdata$ChildAge[is.na(gdata$ChildAge)==1]=7
sapply(gdata,function(x) length(unique(x)))

#Internal Dataviz
sum(gdata$IsAudition==1&gdata$IsParticipation==1)
sum(gdata$IsAudition==1&gdata$IsParticipation==0)
sum(gdata$IsAudition==0&gdata$IsParticipation==0)

hist(gdata$Correct)
hist(gdata$Complete)
hist(gdata$Rectify)
hist(gdata$TotalPay)
hist(gdata$TotalPayAmount)
hist(gdata$TotalActiveTime)
hist(gdata$TotalActive)
hist(gdata$Grade)

#Payers
payers=gdata[which(gdata$IsPay==1&gdata$PayAmount!=0),]
payerse=payers[which(payers$IsAudition==1),]
payersn=payers[which(payers$IsAudition==0),]
payerset=payers[which(payers$IsAudition==1&payers$IsParticipation==1),]
nrow(payers)
npayerse=nrow(payerse)
npayersn=nrow(payersn)
npayerset=nrow(payerset)
meanpayerse=mean(payerse$PayAmount)
meanpayersn=mean(payersn$PayAmount)
#Refund adjustment
payersnr=gdata[which(gdata$IsPay==1&gdata$PayAmount!=0&gdata$IsRefund==0),]
payersnre=payersnr[which(payersnr$IsAudition==1),]
payersnrn=payersnr[which(payersnr$IsAudition==0),]
npayersnr=nrow(payersnr)
npayersnre=nrow(payersnre)
npayersnrn=nrow(payersnrn)
meanpayersnre=mean(payersnre$PayAmount)
meanpayersnrn=mean(payersnrn$PayAmount)

#Proportion of compliers
encouragement=gdata[which(gdata$IsAudition==1),]
nonencouragement=gdata[which(gdata$IsAudition==0),]
compliers=gdata[which(gdata$IsAudition==1&gdata$IsParticipation==1),]
defiers=gdata[which(gdata$IsAudition==1&gdata$IsParticipation==0),]
meancomp=sapply(compliers,function(x) mean(x))
meandef=sapply(defiers,function(x) mean(x))

#No always-takers
ncompliers=nrow(compliers)
nencouragement=nrow(encouragement)
nnonencouragement=nrow(nonencouragement)
(nencouragement-nnonencouragement)/nencouragement
propofcompliers=ncompliers/nencouragement
propofcompliers

#2SLS
#ITT and LATE, LATE=ATT
meana=sapply(gdata,function(x) mean(x))
meane=sapply(encouragement,function(x) mean(x))
meann=sapply(nonencouragement,function(x) mean(x))
encouragementdash=encouragement[which(is.na(encouragement$ChildAge)==0),]
meanedash=sapply(encouragementdash,function(x) mean(x))
meanedash/meann
paye=mean(encouragement$PayAmount)
payn=mean(nonencouragement$PayAmount)
sdpaye=sd(encouragement$PayAmount)
sdpayn=sd(nonencouragement$PayAmount)
ITT=paye-payn
LATE=ITT/propofcompliers

iv1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata)
summary(iv1,vconv=sandwich,diagnostics=TRUE)

#Refund adjustment
encouragementnr=encouragement[which(encouragement$IsRefund==0),]
nonencouragementnr=nonencouragement[which(nonencouragement$IsRefund==0),]
meanenr=sapply(encouragementnr,function(x) mean(x))
meannnr=sapply(nonencouragementnr,function(x) mean(x))
payenr=mean(encouragementnr$PayAmount)
paynnr=mean(nonencouragementnr$PayAmount)
ITTnr=payenr-paynnr
LATEnr=ITTnr/propofcompliers

#OLS
glm9=glm(PayAmount~IsParticipation,data=gdata)
summary(glm9)

#Weak instrument leading to unstable causal effects estimate?
#Matching just for the encouragement group; 1000 treated vs 5000 control for genetic matching
gent=compliers[sample(nrow(compliers),1000),]
genc=defiers[sample(nrow(defiers),5000),]
gendata=rbind(gent,genc)
gendata=gendata[is.na(gendata$GoodSchool)==0,]
sapply(gendata,function(x) sum(is.na(x)))
Tr=gendata$IsParticipation
X=cbind(gendata$TotalPay,gendata$TotalPayAmount,gendata$TotalActive,gendata$TotalActiveTime,gendata$GoodSchool,gendata$Grade,gendata$ChildAge,gendata$ChildGender,gendata$ParentGender)
Y=gendata$PayAmount
genout1=GenMatch(Tr=Tr,X=X,pop.size=200)
mout1=Match(Y=Y,Tr=Tr,X=X,Weight.matrix = genout1)
summary(mout1)
mb1=MatchBalance(IsPay~TotalPay+TotalPayAmount+TotalActive+TotalActiveTime+GoodSchool+Grade+ChildAge+ChildGender+ParentGender,data=gendata,match.out=mout1,nboots=100)

#LARF
Y=gdata$PayAmount
X=as.matrix(gdata[,c('TotalPay','TotalPayAmount','TotalActive','TotalActiveTime','GoodSchool','Grade','ChildAge','ParentGender','ChildGender')])
D=gdata$IsParticipation
Z=gdata$IsAudition
larf1=larf(Y~D|Z,D,Z,gdata)
summary(larf1)
larf2=larf(PayAmount~TotalPay+TotalPayAmount+TotalActive+TotalActiveTime+GoodSchool+Grade+ChildAge+ParentGender+ChildGender,treatment=gdata$IsParticipation,instrument=gdata$IsAudition,data=gdata)
summary(larf2)

#gdata backup
gdata_all=gdata

#Maximum treatment effects of subsets
glm5=glm(PayAmount~TotalPay+TotalPayAmount+TotalActive+TotalActiveTime+GoodSchool+Grade+ChildAge+ParentGender+ChildGender,data=gdata)
summary(glm5)

#Encouragement design function
encourage = function (gdata){
  encouragement=gdata[which(gdata$IsAudition==1),]
  nonencouragement=gdata[which(gdata$IsAudition==0),]
  compliers=gdata[which(gdata$IsAudition==1&gdata$IsParticipation==1),]
  defiers=gdata[which(gdata$IsAudition==1&gdata$IsParticipation==0),]
  meancomp=sapply(compliers,function(x) mean(x))
  meandef=sapply(defiers,function(x) mean(x))
  
  #No always-takers
  ncompliers=nrow(compliers)
  nencouragement=nrow(encouragement)
  nnonencouragement=nrow(nonencouragement)
  (nencouragement-nnonencouragement)/nencouragement
  propofcompliers=ncompliers/nencouragement
  propofcompliers
  
  #2SLS
  #ITT and LATE, LATE=ATT
  meana=sapply(gdata,function(x) mean(x))
  meane=sapply(encouragement,function(x) mean(x))
  meann=sapply(nonencouragement,function(x) mean(x))
  encouragementdash=encouragement[which(is.na(encouragement$ChildAge)==0),]
  meanedash=sapply(encouragementdash,function(x) mean(x))
  meanedash/meann
  paye=mean(encouragement$PayAmount)
  payn=mean(nonencouragement$PayAmount)
  sdpaye=sd(encouragement$PayAmount)
  sdpayn=sd(nonencouragement$PayAmount)
  ITT=paye-payn
  LATE=ITT/propofcompliers
  #Refund adjustment
  encouragementnr=encouragement[which(encouragement$IsRefund==0),]
  nonencouragementnr=nonencouragement[which(nonencouragement$IsRefund==0),]
  meanenr=sapply(encouragementnr,function(x) mean(x))
  meannnr=sapply(nonencouragementnr,function(x) mean(x))
  payenr=mean(encouragementnr$PayAmount)
  paynnr=mean(nonencouragementnr$PayAmount)
  ITTnr=payenr-paynnr
  LATEnr=ITTnr/propofcompliers
  return(c(ITTnr,LATEnr))
}

#Subset Analysis
#TotalPay
ivtp1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPay>=1& gdata$TotalPay<=2,])
ivtp2=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPay>2 & gdata$TotalPay<=4,])
ivtp3=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPay>4 & gdata$TotalPay<=6,])
ivtp4=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPay>6 & gdata$TotalPay<=24,])
summary(ivtp1,vconv=sandwich,diagnostics=TRUE)
summary(ivtp2,vconv=sandwich,diagnostics=TRUE)
summary(ivtp3,vconv=sandwich,diagnostics=TRUE)
summary(ivtp4,vconv=sandwich,diagnostics=TRUE)
#TotalPayAmount
ivtpa1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPayAmount>=0& gdata$TotalPay<=1,])
ivtpa2=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPayAmount>1& gdata$TotalPay<=9.9,])
ivtpa3=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPayAmount>9.9& gdata$TotalPay<=10,])
ivtpa4=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalPayAmount>10& gdata$TotalPay<=3200,])
summary(ivtpa1,vconv=sandwich,diagnostics=TRUE)
summary(ivtpa2,vconv=sandwich,diagnostics=TRUE)
summary(ivtpa3,vconv=sandwich,diagnostics=TRUE)
summary(ivtpa4,vconv=sandwich,diagnostics=TRUE)
#TotalActive
ivta1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActive>=0& gdata$TotalPay<=14,])
ivta2=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActive>14& gdata$TotalPay<=19,])
ivta3=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActive>19& gdata$TotalPay<=23,])
ivta4=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActive>23& gdata$TotalPay<=360,])
summary(ivta1,vconv=sandwich,diagnostics=TRUE)
summary(ivta2,vconv=sandwich,diagnostics=TRUE)
summary(ivta3,vconv=sandwich,diagnostics=TRUE)
summary(ivta4,vconv=sandwich,diagnostics=TRUE)
#TotalActiveTime
ivtat1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActiveTime==0,])
ivtat2=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActiveTime>0& gdata$TotalPay<=30,])
ivtat3=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActiveTime>30& gdata$TotalPay<=197,])
ivtat4=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$TotalActiveTime>197& gdata$TotalPay<=2344,])
summary(ivtat1,vconv=sandwich,diagnostics=TRUE)
summary(ivtat2,vconv=sandwich,diagnostics=TRUE)
summary(ivtat3,vconv=sandwich,diagnostics=TRUE)
summary(ivtat4,vconv=sandwich,diagnostics=TRUE)
#GoodSchool
ivgs0=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$GoodSchool==0,])
ivgs1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$GoodSchool==1,])
summary(ivgs0,vconv=sandwich,diagnostics=TRUE)
summary(ivgs1,vconv=sandwich,diagnostics=TRUE)
#Grades
encourage(gdata[gdata$Grade==1,])
encourage(gdata[gdata$Grade==2,])
encourage(gdata[gdata$Grade==3,])
encourage(gdata[gdata$Grade==4,])
encourage(gdata[gdata$Grade==5,])
encourage(gdata[gdata$Grade==6,])
ivg1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$Grade==1,])
ivg2=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$Grade==2,])
ivg3=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$Grade==3,])
ivg4=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$Grade==4,])
ivg5=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$Grade==5,])
ivg6=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$Grade==6,])
summary(ivg1,vconv=sandwich,diagnostics=TRUE)
summary(ivg2,vconv=sandwich,diagnostics=TRUE)
summary(ivg3,vconv=sandwich,diagnostics=TRUE)
summary(ivg4,vconv=sandwich,diagnostics=TRUE)
summary(ivg5,vconv=sandwich,diagnostics=TRUE)
summary(ivg6,vconv=sandwich,diagnostics=TRUE)
#ChildGender
encourage(gdata[gdata$ChildGender==0,])
encourage(gdata[gdata$ChildGender==1,])
ivc0=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildGender==0,])
ivc1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildGender==1,])
summary(ivc0,vconv=sandwich,diagnostics=TRUE)
summary(ivc1,vconv=sandwich,diagnostics=TRUE)
#ParentGender
encourage(gdata[gdata$ParentGender==0,])
encourage(gdata[gdata$ParentGender==1,])
ivp0=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ParentGender==0,])
ivp1=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ParentGender==1,])
summary(ivp0,vconv=sandwich,diagnostics=TRUE)
summary(ivp1,vconv=sandwich,diagnostics=TRUE)
#ChildAge
ivc7=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildAge==7,])
ivc8=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildAge==8,])
ivc9=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildAge==9,])
ivc10=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildAge==10,])
ivc11=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildAge==11,])
ivc12=ivreg(PayAmount~IsParticipation|IsAudition,data=gdata[gdata$ChildAge==12,])
summary(ivc7,vconv=sandwich,diagnostics=TRUE)
summary(ivc8,vconv=sandwich,diagnostics=TRUE)
summary(ivc9,vconv=sandwich,diagnostics=TRUE)
summary(ivc10,vconv=sandwich,diagnostics=TRUE)
summary(ivc11,vconv=sandwich,diagnostics=TRUE)
summary(ivc12,vconv=sandwich,diagnostics=TRUE)
#Three different trials


glm6=glm(IsParticipation~IsAudition,data=gdata)
summary(glm6)


