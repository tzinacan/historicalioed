library(nlme)
library(pastecs)
library(Hmisc)
library (ggplot2)
library(dplyr)
library (foreign)
library(multcomp)
library(r2glmm)
library(lm.beta)
library(Rmisc)
library(MASS)
library(xlsx)
library(lavaan)
library(ltm)
library(lsr)
library(reghelper)
library(psych)
library(EnvStats)
library(reshape)

#deseabilidad
deseabilidad3.2<-read.xlsx("experimento3.2.xlsx",as.data.frame=TRUE,header=TRUE, sheetName="Sheet1",colNames=TRUE)

deseabilidad3.2$ID<-factor(deseabilidad3.2$ID)

deseabilidad3.2$desparapolitica<-(deseabilidad3.2$parapoliticav+deseabilidad3.2$parapoliticap+deseabilidad3.2$parapoliticapos+
                                    deseabilidad3.2$parapoliticaneg)/4
deseabilidad3.2$desconstituyente<-(deseabilidad3.2$constituyentev+deseabilidad3.2$constituyentep+deseabilidad3.2$constituyentepos+
                                     deseabilidad3.2$constituyenteneg)/4
deseabilidad3.2$desexterminioup<-(deseabilidad3.2$expterminioupv+deseabilidad3.2$exterminioupp+deseabilidad3.2$exterminiouppos+
                                    deseabilidad3.2$exterminioupneg)/4
deseabilidad3.2$despcolombia<-(deseabilidad3.2$pcolombiav+deseabilidad3.2$pcolombiap+deseabilidad3.2$pcolombiapos+
                                    deseabilidad3.2$pcolombianeg)/4
deseabilidad3.2$desfarc<-(deseabilidad3.2$farcv+deseabilidad3.2$farcp+deseabilidad3.2$farcpos+deseabilidad3.2$farcneg)/4
deseabilidad3.2$desparamil<-(deseabilidad3.2$paramilv+deseabilidad3.2$paramilp+deseabilidad3.2$paramilpos+deseabilidad3.2$paramilneg)/4
deseabilidad3.2$desconflargo<-(deseabilidad3.2$conflargov+deseabilidad3.2$conflargop+deseabilidad3.2$conflargopos+
                                    deseabilidad3.2$conflargoneg)/4
deseabilidad3.2$desconfrural<-(deseabilidad3.2$confruralv+deseabilidad3.2$confruralp+deseabilidad3.2$confruralpos+
                                    deseabilidad3.2$confruralneg)/4
View(deseabilidad3.2)

#Social desirability descriptives

stat.desc(deseabilidad3.2$desfarc, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desfarc)
stat.desc(deseabilidad3.2$desparamil, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desparamil)
stat.desc(deseabilidad3.2$desconflargo, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desconflargo)
stat.desc(deseabilidad3.2$desconfrural, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desconfrural)
stat.desc(deseabilidad3.2$desparapolitica, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desparapolitica)
stat.desc(deseabilidad3.2$desconstituyente, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desconstituyente)
stat.desc(deseabilidad3.2$desexterminioup, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$desexterminioup)
stat.desc(deseabilidad3.2$despcolombia, basic=FALSE,norm=TRUE)
CI(deseabilidad3.2$despcolombia)

##Paired t-test for desirability items
#preparation
deseab2amelted<-deseabilidad3.2[c("ID","sexo","edad","desfarc","desparamil","desconflargo","desconfrural","desparapolitica","desconstituyente","desexterminioup","despcolombia")]
deseab2amelted<-melt(deseab2amelted,id=c("ID","sexo","edad"),measured=c("desfarc","desparamil","desconflargo","desconfrural","desparapolitica","desconstituyente","desexterminioup","despcolombia"))
names(deseab2amelted)<-c("ID","sexo","edad","tema","deseabilidad")
deseab2amelted$ID<-as.factor(deseab2amelted$ID)
deseab2amelted$tema<-as.factor(deseab2amelted$tema)
deseab2amelted$condicion[deseab2amelted$tema=="desfarc"|deseab2amelted$tema=="desparamil"|deseab2amelted$tema=="desconflargo"|deseab2amelted$tema=="desconfrural"]<-"Alta"
deseab2amelted$condicion[deseab2amelted$tema=="desparapolitica"|deseab2amelted$tema=="desconstituyente"|deseab2amelted$tema=="desexterminioup"|deseab2amelted$tema=="despcolombia"]<-"Baja"
View(deseab2amelted)

#Descriptives desirability*condition
by(deseab2amelted$deseabilidad,deseab2amelted$condicion,stat.desc,basic=FALSE,norm=TRUE)
by(deseab2amelted$deseabilidad,deseab2amelted$condicion,CI)

#Desirability*condition t test
t.test(deseabilidad ~ condicion, data = deseab2amelted, paired = TRUE)
cohensD(deseabilidad ~ condicion, data = deseab2amelted, method ="pooled")

#alphas
cronbach.alpha(subset(deseabilidad3.2,select=c("parapoliticav","parapoliticap","parapoliticapos","parapoliticaneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("constituyentev","constituyentep","constituyentepos","constituyenteneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("expterminioupv","exterminioupp","exterminiouppos","exterminioupneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("pcolombiav","pcolombiap","pcolombiapos","pcolombianeg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("farcv","farcp","farcpos","farcneg"),CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("paramilv","paramilp","paramilpos","paramilneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("conflargov","conflargop","conflargopos","conflargoneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("confruralv","confruralp","confruralpos","confruralneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))


cronbach.alpha(subset(deseabilidad3.2,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
                                               "jaqueover","jgarzonover","elnogalover","pjusticiaover","mesaelnover","firmapazover","bojayaover","cesefuegover"),
                                              CI=TRUE,B=1000))

cronbach.alpha(subset(deseabilidad3.2,select=c("uribeoverf","manizalesoverf","periodistasoverf","araucaoverf"),
                                               CI=TRUE,B=1000))

#Unidimensionality analysis Overclaiming
overclaiminghits<-subset(deseabilidad3.2,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
                                                 "jaqueover","jgarzonover","elnogalover","pjusticiaover","mesaelnover","firmapazover","bojayaover","cesefuegover"))
overclaiminghitscor<-cor(overclaiminghits)
KMO(overclaiminghitscor)

pcahits<-principal(overclaiminghits,nfactors=16,rotate="none")

overclaimingfoils<-subset(deseabilidad3.2,select=c("uribeoverf","manizalesoverf","periodistasoverf","araucaoverf"))
pcafoils<-principal(overclaimingfoils,nfactors=4,rotate="none")

overclaimingfoilscor<-cor(overclaimingfoils)
KMO(overclaimingfoilscor)

#SEM overclaiming
cfaoverclaiming<-subset(deseabilidad3.2,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
                                                 "jaqueover","jgarzonover","elnogalover","pjusticiaover","mesaelnover","firmapazover","bojayaover","cesefuegover",
                                                 "uribeoverf","manizalesoverf","periodistasoverf","araucaoverf"))
defaultmodel <- ' Hitrate  = ~ ingridover + ralitover + asambleaover + fpositivosover + bogotazover + ctorresover + raulreyesover + oslover + jaqueover + jgarzonover + 
                               elnogalover + pjusticiaover + mesaelnover + firmapazover + bojayaover + cesefuegover+uribeoverf + manizalesoverf + periodistasoverf + araucaoverf'
cfadefault<- cfa(defaultmodel, data=cfaoverclaiming)

summary(cfadefault, fit.measures=TRUE)

overclaimingmodel <- ' Hitrate  =~ ingridover + ralitover + asambleaover + fpositivosover + bogotazover + ctorresover + raulreyesover + oslover + jaqueover + 
                                   jgarzonover + elnogalover + pjusticiaover + mesaelnover + firmapazover + bojayaover + cesefuegover
                       Falsealarm =~ uribeoverf + manizalesoverf + periodistasoverf + araucaoverf'
cfaover<- cfa(overclaimingmodel, data=cfaoverclaiming)
summary(cfaover, fit.measures=TRUE)

anova(cfadefault,cfaover)

#CFA deseabilidad

#Parapolitica

parapoliticades<-subset(deseabilidad3.2,select=c("parapoliticav","parapoliticap","parapoliticapos","parapoliticaneg"))
desparapoliticazero <- ' desparapolitica =~ parapoliticav + parapoliticap + parapoliticapos + parapoliticaneg'
cfdesparapoliticazero<- cfa(desparapoliticazero, data=parapoliticades)
summary(cfdesparapoliticazero, fit.measures=TRUE)

desparapolitica2 <- ' desnegparapolitica =~ parapoliticav + parapoliticaneg
                      desposparapolitica =~ parapoliticap + parapoliticapos'

cfdesparapolitica2<- cfa(desparapolitica2, data=parapoliticades)
summary(cfdesparapolitica2, fit.measures=TRUE)

anova(cfdesparapoliticazero,cfdesparapolitica2)

#Constituyente

constituyentedes<-subset(deseabilidad3.2,select=c("constituyentev","constituyentep","constituyentepos","constituyenteneg"))
desconstituyentezero <- ' desconstituyente =~ constituyentev + constituyentep + constituyentepos + constituyenteneg'
cfdesconstituyentezero<- cfa(desconstituyentezero, data=constituyentedes)
summary(cfdesconstituyentezero, fit.measures=TRUE)

desconstituyente2 <- ' desnegconstituyente =~ constituyentev + constituyentep
                       desposconstituyente =~ constituyentepos + constituyenteneg'

cfdesconstituyente2<- cfa(desconstituyente2, data=constituyentedes)
summary(cfdesconstituyente2, fit.measures=TRUE)

anova(cfdespcolombiazero,cfdespcolombia2)

#Exterminio UP

exterminioupdes<-subset(deseabilidad3.2,select=c("expterminioupv","exterminioupp","exterminiouppos","exterminioupneg"))
desexterminioupzero <- ' desconstituyente =~ expterminioupv + exterminioupp + exterminiouppos + exterminioupneg'
cfdesexterminioupzero<- cfa(desexterminioupzero, data=exterminioupdes)
summary(cfdesexterminioupzero, fit.measures=TRUE)

desexterminioup2 <- ' desnegexterminioup =~ expterminioupv + exterminioupp
                      desposexterminioup =~ exterminiouppos + exterminioupneg'

cfdesexterminioup2<- cfa(desexterminioup2, data=exterminioupdes)
summary(cfdesexterminioup2, fit.measures=TRUE)

anova(cfdesexterminioupzero,cfdesexterminioup2)

#Plan Colombia

pcolombiades<-subset(deseabilidad3.2,select=c("pcolombiav","pcolombiap","pcolombiapos","pcolombianeg"))
despcolombiazero <- ' despcolombia =~ pcolombiav + pcolombiap + pcolombiapos + pcolombianeg'
cfdespcolombiazero<- cfa(despcolombiazero, data=pcolombiades)
summary(cfdespcolombiazero, fit.measures=TRUE)

despcolombia2 <- ' desnegpcolombia =~ pcolombiav + pcolombiap
                   despospcolombia =~ pcolombiapos + pcolombianeg'

cfdespcolombia2<- cfa(despcolombia2, data=pcolombiades)
summary(cfdespcolombia2, fit.measures=TRUE)

anova(cfdespcolombiazero,cfdespcolombia2)

#FARC
farcdes<-subset(deseabilidad3.2,select=c("farcv","farcp","farcpos","farcneg"))
desfarczero <- ' desfarc =~ farcv + farcp + farcpos + farcneg'
cfdesfarczero<- cfa(desfarczero, data=farcdes)
summary(cfdesfarczero, fit.measures=TRUE)

desfarc2 <- 'desposfarc =~ farcp + farcv  
             desnegfarc =~ farcpos + farcneg'

cfdesfarc2<- cfa(desfarc2, data=farcdes)
summary(cfdesfarc2, fit.measures=TRUE)

anova(cfdesfarczero,cfdesfarc2)

#Paramilitarismo

paramildes<-subset(deseabilidad3.2,select=c("paramilv","paramilp","paramilpos","paramilneg"))
desparamilzero <- ' desparamil =~ paramilv + paramilp + paramilpos + paramilneg'
cfdesparamilzero<- cfa(desparamilzero, data=paramildes)
summary(cfdesparamilzero, fit.measures=TRUE)

desparamil2 <- 'desposparamil =~ paramilp + paramilv  
                desnegparamil =~ paramilpos + paramilneg'

cfdesparamil2<- cfa(desparamil2, data=paramildes)
summary(cfdesparamil2, fit.measures=TRUE)

anova(cfdesparamilzero,cfdesparamil2)

#conflargo
conflargodes<-subset(deseabilidad3.2,select=c("conflargov","conflargop","conflargopos","conflargoneg"))
desconflargozero <- ' desconflargo =~ conflargov + conflargop + conflargopos + conflargoneg'
cfdesconflargozero<- cfa(desconflargozero, data=conflargodes)
summary(cfdesconflargozero, fit.measures=TRUE)

desconflargo2 <- 'desposconflargo =~ conflargop + conflargov  
                  desnegconflargo =~ conflargopos + conflargoneg'

cfdesconflargo2<- cfa(desconflargo2, data=conflargodes)
summary(cfdesconflargo2, fit.measures=TRUE)

anova(cfdesconflargozero,cfdesconflargo2)

#confrural
confruraldes<-subset(deseabilidad3.2,select=c("confruralv","confruralp","confruralpos","confruralneg"))
confruralzero <- ' desconfrural =~ confruralv + confruralp + confruralpos + confruralneg'
cfconfruralzero<- cfa(confruralzero, data=confruraldes)
summary(cfconfruralzero, fit.measures=TRUE)

confrural2 <- ' desnegconfrural =~ confruralv + confruralp
                desposconfrural =~  confruralpos + confruralneg'

cfconfrural2<- cfa(confrural2, data=confruraldes)
summary(cfconfrural2, fit.measures=TRUE)

anova(cfconfruralzero,cfconfrural2)


#Main analysis
#preparation
ipexpto3.2<-read.xlsx("experimento3.2.xlsx",as.data.frame=TRUE,header=TRUE, sheetName="Sheet1")
View(ipexpto3.2)
ipexpto3.2$ID<-factor(ipexpto3.2$ID)
ipexpto3.2$condicion<-factor(ipexpto3.2$condicion,levels=c(0,1), labels = c("baja","alta"))
ipexpto3.2$item<-factor(ipexpto3.2$item,levels=c(1,2,3,4,5,6,7,8), labels = c("farc","paramil","conflargo","confrural","parapol","constituyente","exterminioup","pcolombia"))
ipexpto3.2$chistoria1y0<-ifelse(ipexpto3.2$cursohistoria>1,c(0),c(1))

##Transformations

#Log transformation
ipexpto3.2$Falsealarmratelog<-log10(ipexpto3.2$FalseAlarmRate+1)
#Inv transformation
ipexpto3.2$Falsealarmrateinv<-1/((max(ipexpto3.2$FalseAlarmRate)-ipexpto3.2$FalseAlarmRate)+1)
#sqrt transformation
ipexpto3.2$Falsealarmratesqrt<-sqrt(ipexpto3.2$FalseAlarmRate+1)
#calculating bias
ipexpto3.2$Bias<-(ipexpto3.2$AveragedHitRate+ipexpto3.2$FalseAlarmRate)/2

##Descriptives
#IPE
stat.desc(ipexpto3.2$ipe)
CI(ipexpto3.2$ipe)
#IPExCondicion
by(ipexpto3.2$ipe,list(ipexpto3.2$condicion),stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3.2$ipe,list(ipexpto3.2$condicion),CI)
#IPExItem
by(ipexpto3.2$ipe,ipexpto3.2$item,stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3.2$ipe,ipexpto3.2$item,CI)
#Pre/PostxItem
by(ipexpto3.2$comprevia,list(ipexpto3.2$item),stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3.2$compost,list(ipexpto3.2$item),stat.desc,basic=FALSE)

#Normality tests
#IPE
shapiro.test(ipexpto3.2$ipe)
hist(ipexpto3.2$ipe)

#Desirability
shapiro.test(ipexpto3.2$deseabporitem)
hist(ipexpto3.2$deseabporitem)

#IPExDesirability
desebaja2a<-subset(ipexpto3.2, ipexpto3.2$condicion=="baja")
desebalta2a<-subset(ipexpto3.2, ipexpto3.2$condicion=="alta")

#Low desirability
hist.lowdesirability2a<-ggplot(desebaja2a, aes(deseabporitem)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebaja2a$deseabporitem,na.rm=TRUE),
                                     sd=sd(desebaja2a$deseabporitem, na.rm=TRUE)), colour="black", size=1)+
                                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
hist.lowdesirability2a

#High desirability
hist.highdesirability2a<-ggplot(desebalta2a, aes(deseabporitem)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebalta2a$deseabporitem,na.rm=TRUE),
                                     sd=sd(desebalta2a$deseabporitem, na.rm=TRUE)), colour="black", size=1)+
                                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
hist.highdesirability2a

#Low desabirility x IPE
hist.ipelowdesirability2a<-ggplot(desebaja2a, aes(ipe)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebaja2a$ipe,na.rm=TRUE),
                                     sd=sd(desebaja2a$ipe, na.rm=TRUE)), colour="black", size=1)+
                                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
hist.ipelowdesirability2a

#High desirability x IPE

hist.ipehighdesirability2a<-ggplot(desebalta2a, aes(ipe)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebalta2a$ipe,na.rm=TRUE),
                                     sd=sd(desebalta2a$ipe, na.rm=TRUE)), colour="black", size=1)+
                                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
hist.ipehighdesirability2a

##Q plots
#IPE
qplot(sample=ipexpto3.2$ipe, stat="qq")
#Desirability
qplot(sample=ipexpto3$deseabporitempre, stat="qq")

#descriptive graphs
#social desirability
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$condicion, ipexpto3.2$ipe))
line + stat_summary(fun.y = mean, geom = "point", aes(group = 1),colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Deseabilidad", y = "IPE")+theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Social desirability 
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$item, ipexpto3.2$deseabporitem))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Deseabilidad social")+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#pre
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$condicion, ipexpto3.2$comprevia))
line + stat_summary(fun.y = mean,geom = "point", aes(group = 1),colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Desirability", y = "Previous Understanding")+theme(panel.grid.major = element_blank(), 
                                             panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#pre x item
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$item, ipexpto3.2$comprevia))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Comprensi?n Previa")+theme(panel.grid.major = element_blank(), 
                                             panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                             axis.text = element_text(size=10),axis.title=element_text(size=13)) +
                                             scale_y_continuous(breaks=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5))+expand_limits(y=c(1,5.5))
#post
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$condicion, ipexpto3.2$compost))
line + stat_summary(fun.y = mean, geom = "point", aes(group = 1),colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Desirability", y = "Post Understanding")+theme(panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#post x item
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$item, ipexpto3.2$compost))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Comprensi?n Post")+theme(panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                            axis.text = element_text(size=10),axis.title=element_text(size=13)) +
                                            scale_y_continuous(breaks=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5))+expand_limits(y=c(1,5.5))
#pre_post x item
line <- ggplot(ipexpto3.2, aes(ipexpto3.2$item, ipexpto3.2$ipe))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Comprensi?n Post")+theme(panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                 axis.text = element_text(size=10),axis.title=element_text(size=13)) +
                                                 scale_y_continuous(breaks=c(-0.5,0,0.5,1,1.5,2))+expand_limits(y=c(-0.5,2))

#contrasts
require(multcomp)
clargovscrural<-c(0,0,-1,1)
pcolombiavsrpinilla<-c(1,-1,0,0)
rpinillavsall<-c(0,0,0,1)
contrasts(ipexpto3$temacondicion)<-cbind(clargovscrural,pcolombiavsrpinilla)
ipexpto3$temacondicion

#correlations desirability

cor.test(ipexpto3.2$deseabporitem,ipexpto3.2$comprevia)
cor.test(ipexpto3.2$deseabporitem,ipexpto3.2$compost)
cor.test(ipexpto3.2$deseabporitem,ipexpto3.2$ipe)
cor.test(ipexpto3.2$deseabporitem,ipexpto3.2$FalseAlarmRate)

#main effect of IPE
t.test(ipexpto3.2$ipe,mu=0)
stat.desc(ipexpto3.2$ipe, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
cohensD(ipexpto3.2$ipe,mu=0)

##as GLM
#model with condicion (TFA)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modelcondicionexp<-update(modelzero,.~. + condicion)
modeloverclaim<-update(modelcondicionexp,.~. + FalseAlarmRate)
modelcondover<-update(modeloverclaim,.~. + condicion:FalseAlarmRate)
anova(modelzero,modelcondicionexp,modeloverclaim,modelcondover)
summary(modelcondover)

#model with condicion (AHR)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modelcondicionexp<-update(modelzero,.~. + condicion)
modeloverclaim<-update(modelcondicionexp,.~. + AveragedHitRate)
modelcondover<-update(modeloverclaim,.~. + condicion:AveragedHitRate)
anova(modelzero,modelcondicionexp,modeloverclaim,modelcondover)
summary(modelcondover)

#model with condicion (ACC)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modelcondicionexp<-update(modelzero,.~. + condicion)
modeloverclaim<-update(modelcondicionexp,.~. + Accuracy)
modelcondover<-update(modeloverclaim,.~. + condicion:Accuracy)
anova(modelzero,modelcondicionexp,modeloverclaim,modelcondover)
summary(modelovercondover)

#model with condicion (Bias)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modelcondicionexp<-update(modelzero,.~. + condicion)
modeloverclaim<-update(modelcondicionexp,.~. + Bias)
modelcondover<-update(modeloverclaim,.~. + condicion:Bias)
anova(modelzero,modelcondicionexp,modeloverclaim,modelcondover)
summary(modeloverclaim)

#model with condicion (chistory)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modelcondicionexp<-update(modelzero,.~. + condicion)
modeloverclaim<-update(modelcondicionexp,.~. + chistoria1y0)
modelcondover<-update(modeloverclaim,.~. + condicion:chistoria1y0)
anova(modelzero,modelcondicionexp,modeloverclaim,modelcondover)
summary(modelcondover)

#models with desirability (TFA)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modeldesitem<-update(modelzero,.~. + deseabporitem)
modeloverclaimd<-update(modeldesitem,.~. + FalseAlarmRate)
modeldesover<-update(modeloverclaimd,.~. + deseabporitem:FalseAlarmRate)
anova(modelzero,modeldesitem,modeloverclaimd,modeldesover)
summary(modeldesover)

#models with desirability (AHR)
modelzero<-lm(ipe ~ 1,data = ipexpto3.2)
modeldesitem<-update(modelzero,.~. + deseabporitem)
modeloverclaimd<-update(modeldesitem,.~. + AveragedHitRate)
modeldesover<-update(modeloverclaimd,.~. + deseabporitem:AveragedHitRate)
anova(modelzero,modeldesitem,modeloverclaimd,modeldesover)
summary(modeloverclaimd)

#models with desirability (ACC)
modelzero<-lme(ipe ~ 1, data = ipexpto3.2)
modeldesitem<-update(modelzero,.~. + deseabporitem)
modeloverclaimd<-update(modeldesitem,.~. + Accuracy)
modeldesover<-update(modeloverclaimd,.~. + deseabporitem:Accuracy)
anova(modelzero,modeldesitem,modeloverclaimd,modeldesover)
summary(modeloverclaimd)

#models with desirability (Bias)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modeldesitem<-update(modelzero,.~. + deseabporitem)
modeloverclaimd<-update(modeldesitem,.~. + Bias)
modeldesover<-update(modeloverclaimd,.~. + deseabporitem:Bias)
anova(modelzero,modeldesitem,modeloverclaimd,modeldesover)
summary(modeldesover)

#models with desirability (chistory)
modelzero<-lm(ipe ~ 1, data = ipexpto3.2)
modeldesitem<-update(modelzero,.~. + deseabporitem)
modeloverclaimd<-update(modeldesitem,.~. + chistoria1y0)
modeldesover<-update(modeloverclaimd,.~. + deseabporitem:chistoria1y0)
anova(modelzero,modeldesitem,modeloverclaimd,modeldesover)
summary(modeldesover)

#chistory as predictor
modelhistoTFA<-lm(FalseAlarmRate ~ chistoria1y0, data = ipexpto3.2)
summary(modelhistoTFA)

modelhistoAHR<-lm(AveragedHitRate ~ chistoria1y0, data = ipexpto3.2)
summary(modelhistoAHR)

modelhistoACC<-lm(Accuracy ~ chistoria1y0, data = ipexpto3.2)
summary(modelhistoACC)

modelhistoBIAS<-lm(Bias ~ chistoria1y0, data = ipexpto3.2)
summary(modelhistBIAS)

#to report
modelzero<-lm(ipe ~ 1,data = ipexpto3.2)
modelcondicionexp<-update(modelzero,.~. + condicion)
modelcondover<-update(modelcondicionexp,.~. + FalseAlarmRate)
summary(modelzero)
summary(modelcondicionexp)
summary(modelcondover)

modelcondexphist<-update(modelcondicionexp,.~. + chistoria1y0)
modelcondicionexphist<-update(modelcondexphist,.~. + condicion:chistoria1y0)
anova(modelzero,modelcondicionexp,modelcondexphist,modelcondicionexphist)
summary(modelcondicionexphist)

#as HLM
modelzerohlm<-lme(ipe ~ 1, random = ~1|ID/item, data = ipexpto3.2, method = "ML")
modelcondicionhlm<-update(modelzerohlm,.~. + condicion)
modelcondoverhlm<-update(modelcondicionhlm,.~. + FalseAlarmRate)
modelcondoverinthlm<-update(modelcondoverhlm,.~. + condicion:FalseAlarmRate)
anova(modelzerohlm,modelcondicionhlm,modelcondoverhlm,modelcondoverinthlm)
summary(modelzerohlm)
summary(modelcondicionhlm)
summary(modelcondoverhlm)

#Check: correlation desirability-ipe
cor.test(ipexpto3.2$deseabporitem,ipexpto3.2$ipe)

#check: interaction desirability:item
modeldesirabitem<-update(modelcondicionexp,.~. + item + condicion:item)
anova(modelcondicionexp,modeldesirabitem)

#standardized coefficients

beta(modelzero,x=TRUE,y=TRUE)
beta(modelcondicionexp,x=TRUE,y=TRUE,skip='condicion')
beta(modeldesitem,x=TRUE,y=TRUE)
beta(modeloverclaim,x=TRUE,y=TRUE)
beta(modeldespreover,x=TRUE,y=TRUE)

r2beta(modelcondicion,method= 'nsj')
r2beta(modeldespre,method= 'nsj')
r2beta(modeloverclaim,method= 'nsj')
r2beta(modeldespreover,method= 'nsj')

#model with pre
modelzeropre<-lm(comprevia ~ 1, data = ipexpto3.2)
modelcondicionexpre<-update(modelzeropre,.~. + condicion)
modeloverclaimpre<-update(modelcondicionexpre,.~. + FalseAlarmRate)
modelcondoverpre<-update(modeloverclaimpre,.~. + condicion:FalseAlarmRate)
anova(modelzeropre,modelcondicionexpre,modeloverclaimpre,modelcondoverpre)
summary(modeloverclaimpre)

beta(modelzeropre,x=TRUE,y=TRUE)
beta(modelcondicionexpre,x=TRUE,y=TRUE,skip='condicion')
beta(modeloverclaimpre,x=TRUE,y=TRUE)

#model with post
modelzeropost<-lm(compost ~ 1, data = ipexpto3.2)
modelcondicionexpost<-update(modelzeropost,.~. + condicion)
modeloverclaimpost<-update(modelcondicionexpost,.~. + FalseAlarmRate)
modelcondoverpost<-update(modeloverclaimpost,.~. + condicion:FalseAlarmRate)
anova(modelzeropost,modelcondicionexpost,modeloverclaimpost,modelcondoverpost)
summary(modeloverclaimpost)

beta(modelzeropost,x=TRUE,y=TRUE)
beta(modelcondicionexpost,x=TRUE,y=TRUE,skip='condicion')
beta(modeloverclaimpost,x=TRUE,y=TRUE)

#IPE condicion*cursohistoria (Graph)
moderationchistoria<-lm(ipe ~ condicion + chistoria1y0 + condicion*chistoria1y0, data = ipexpto3.2)

plot_model(moderationchistoria, type="int", terms = c("deseabporitem","chistoria1y0"))+
  labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post")+
  ggtitle("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=13),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 15),
                                                                                  legend.title = element_text(size = 16))

#Overclaiming as predictor of social desirability
modelzerodeseab<-lm(deseabporitem ~ 1, data = ipexpto3.2)
modeloverclaimdeseab<-update(modelzerodeseab,.~. + FalseAlarmRate)
modeldeseabtheme<-update(modeloverclaimdeseab,.~. + item)
modelovertheme<-update(modeldeseabtheme,.~. + item:FalseAlarmRate)
anova(modelzerodeseab,modeloverclaimdeseab,modeldeseabtheme,modelovertheme)
summary(modeldeseabtheme)

#History course as predictor of Overclaiming
t.test(ipexpto3.2$Accuracy~ipexpto3.2$chistoria1y0)

##Overclaiming comparison between experiments 2 and 2a
#Preparation
overclaiming2<-cbind.data.frame(ipexpto3$Falsealarmrate,ipexpto3$Averagedhitrate,ipexpto3$Accuracy)
overclaiming2a<-cbind.data.frame(ipexpto3.2$FalseAlarmRate,ipexpto3.2$AveragedHitRate,ipexpto3.2$Accuracy)
names(overclaiming2)<-c("TFA","AHR","ACC")
names(overclaiming2a)<-c("TFA","AHR","ACC")
overclaiming<-rbind(overclaiming2,overclaiming2a)
overclaiming$ID<-1:nrow(overclaiming)
overclaiming$experimento[overclaiming$ID<=314]<-"Experimento 2"
overclaiming$experimento[overclaiming$ID>314]<-"Experimento 2a"
View(overclaiming)

#t-test
t.test(overclaiming$TFA ~ overclaiming$experimento ,var.equal=FALSE)
t.test(overclaiming$AHR ~ overclaiming$experimento ,var.equal=FALSE)
cohensD(AHR ~ experimento, data = overclaiming, method ="pooled")
t.test(overclaiming$ACC ~ overclaiming$experimento ,var.equal=FALSE)

#Mediation Desirability Condition -> Desirability -> IPE
mediationdesirability<-'deseabporitem ~ condicion
                        ipe ~ deseabporitem + condicion'
fitmedesirability<-sem(mediationdesirability,data=ipexpto3.2)
summary(fitmedesirability,standardized=T,fit.measures=T,rsq=T)

mediationdesirability2<-'deseabporitem ~ a*condicion
                         ipe ~ b*deseabporitem + c*condicion
                         indirect:= a*b
                         direct:= c
                         total:= c + (a*b)'
fitmedesirability2<-sem(mediationdesirability2,data=ipexpto3.2)
summary(fitmedesirability2,standardized=T,fit.measures=T,rsq=T)