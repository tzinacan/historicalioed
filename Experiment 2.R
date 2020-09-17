library(nlme)
library(pastecs)
library(Hmisc)
library(reshape2)
library (ggplot2)
library(dplyr)
library (foreign)
library(multcomp)
library(lsr)
library(r2glmm)
library(lm.beta)
library(xlsx)
library(Rmisc)
library(MASS)
library(reghelper)
library(EnvStats)
library(ltm)
library(ppcor)
library(lavaan)

#deseabilidad pre
deseabilidad3.1<-read.xlsx("Base Expto 3.1.xlsx",as.data.frame=TRUE,header=TRUE, sheetName="Sheet1",colNames=TRUE)
deseabilidad3.1$ID<-factor(deseabilidad3.1$ID)
deseabilidad3.1$desfarc<-(deseabilidad3.1$farcv+deseabilidad3.1$farcp+deseabilidad3.1$farcpos+deseabilidad3.1$farcneg)/4
deseabilidad3.1$desparamil<-(deseabilidad3.1$paramilv+deseabilidad3.1$paramilp+deseabilidad3.1$paramilpos+deseabilidad3.1$paramilneg)/4
deseabilidad3.1$desparapolitica<-(deseabilidad3.1$parapoliticav+deseabilidad3.1$parapoliticap+deseabilidad3.1$parapoliticapos+
                                    deseabilidad3.1$parapoliticaneg)/4
deseabilidad3.1$desconflargo<-(deseabilidad3.1$conflargov+deseabilidad3.1$conflargop+deseabilidad3.1$conflargopos+
                                 deseabilidad3.1$conflargoneg)/4
deseabilidad3.1$desconfrural<-(deseabilidad3.1$confruralv+deseabilidad3.1$confruralp+deseabilidad3.1$confruralpos+
                                 deseabilidad3.1$confruralneg)/4
deseabilidad3.1$pcolombia<-(deseabilidad3.1$pcolombiav+deseabilidad3.1$pcolombiap+deseabilidad3.1$pcolombiapos+
                              deseabilidad3.1$pcolombianeg)/4
deseabilidad3.1$creacioneln<-(deseabilidad3.1$creacionelnv+deseabilidad3.1$creacionelnp+deseabilidad3.1$creacionelnpos+
                                deseabilidad3.1$creacionelneg)/4
deseabilidad3.1$rpinilla<-(deseabilidad3.1$rpinillav+deseabilidad3.1$rpinillap+deseabilidad3.1$rpinillapos+
                             deseabilidad3.1$rpinillaneg)/4
View(deseabilidad3.1)

#alphas
cronbach.alpha(subset(deseabilidad3.1,select=c("farcv","farcp","farcpos","farcneg"),CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("paramilv","paramilp","paramilpos","paramilneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("parapoliticav","parapoliticap","parapoliticapos","parapoliticaneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("conflargov","conflargop","conflargopos","conflargoneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("confruralv","confruralp","confruralpos","confruralneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("pcolombiav","pcolombiap","pcolombiapos","pcolombianeg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("creacionelnv","creacionelnp","creacionelnpos","creacionelneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.1,select=c("rpinillav","rpinillap","rpinillapos","rpinillaneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))

cronbach.alpha(subset(deseabilidad3.1,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
                                               "jaqueover","jgarzonover","elnogalover","pjusticiaover","mesaelnover","firmapazover","bojayaover","cesefuegover"),
                                               CI=TRUE,B=1000))

cronbach.alpha(subset(deseabilidad3.1,select=c("uribeoverf","manizalesoverf","periodistasoverf","araucaoverf"),
                                               CI=TRUE,B=1000))

#Unidimensionality analysis Overclaiming
overclaiminghits<-subset(deseabilidad3.1,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
                                                  "jaqueover","jgarzonover","elnogalover","pjusticiaover","mesaelnover","firmapazover","bojayaover","cesefuegover"))
overclaiminghitscor<-cor(overclaiminghits)
KMO(overclaiminghitscor)

pcahits<-principal(overclaiminghits,nfactors=16,rotate="none")

overclaimingfoils<-subset(deseabilidad3.1,select=c("uribeoverf","manizalesoverf","periodistasoverf","araucaoverf"))
pcafoils<-principal(overclaimingfoils,nfactors=4,rotate="none")

overclaimingfoilscor<-cor(overclaimingfoils)
KMO(overclaimingfoilscor)

#CFA overclaiming
cfaoverclaiming<-subset(deseabilidad3.1,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
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

#sem deseabilidad
#Conflicto largo
conflargodes<-subset(deseabilidad3.1,select=c("conflargov","conflargop","conflargopos","conflargoneg"))
desconflargozero <- ' desconflargo =~ conflargov + conflargop + conflargopos + conflargoneg'
cfdesconflargozero<- cfa(desconflargozero, data=conflargodes)
summary(cfdesconflargozero, fit.measures=TRUE)

desconflargo2 <- 'desposconflargo =~ conflargop + conflargov  
                  desnegconflargo =~ conflargopos + conflargoneg'

cfdesconflargo2<- cfa(desconflargo2, data=conflargodes)
summary(cfdesconflargo2, fit.measures=TRUE)

anova(cfdesconflargozero,cfdesconflargo2)

#Conflicto rural
confruraldes<-subset(deseabilidad3.1,select=c("confruralv","confruralp","confruralpos","confruralneg"))
confruralzero <- ' desconfrural =~ confruralv + confruralp + confruralpos + confruralneg'
cfconfruralzero<- cfa(confruralzero, data=confruraldes)
summary(cfconfruralzero, fit.measures=TRUE)

confrural2 <- ' desnegconfrural =~ confruralv + confruralp
                desposconfrural =~  confruralpos + confruralneg'

cfconfrural2<- cfa(confrural1, data=confruraldes)
summary(cfconfrural2, fit.measures=TRUE)

anova(cfconfruralzero,cfconfrural2)

#Plan Colombia

pcolombiades<-subset(deseabilidad3.1,select=c("pcolombiav","pcolombiap","pcolombiapos","pcolombianeg"))
despcolombiazero <- ' despcolombia =~ pcolombiav + pcolombiap + pcolombiapos + pcolombianeg'
cfdespcolombiazero<- cfa(despcolombiazero, data=pcolombiades)
summary(cfdespcolombiazero, fit.measures=TRUE)

despcolombia2 <- ' desnegpcolombia =~ pcolombiav + pcolombiap
                   despospcolombia =~ pcolombiapos + pcolombianeg'

cfdespcolombia2<- cfa(despcolombia2, data=pcolombiades)
summary(cfdespcolombia2, fit.measures=TRUE)

anova(cfdespcolombiazero,cfdespcolombia2)

#Rojas Pinilla

crpinillades<-subset(deseabilidad3.1,select=c("rpinillav","rpinillap","rpinillapos","rpinillaneg"))
desrpinillazero <- ' desrpinilla =~ rpinillav + rpinillap + rpinillapos + rpinillaneg'
cfdesrpinillazero<- cfa(desrpinillazero, data=crpinillades)
summary(cfdesrpinillazero, fit.measures=TRUE)

desrpinilla2 <- ' desnegrpinilla =~ rpinillav + rpinillap
                  despospinilla =~ rpinillapos + rpinillaneg'

cfdesrpinilla2<- cfa(desrpinilla2, data=crpinillades)
summary(cfdesrpinilla2, fit.measures=TRUE)

anova(cfdesrpinillazero,cfdesrpinilla2)

#Labelling and preparation
ipexpto3<-read.spss("baselargaexpto3(sin1s).sav",use.value.labels = TRUE,to.data.frame = TRUE)
View(ipexpto3)
ipexpto3$ID<-factor(ipexpto3$ID)
ipexpto3$condicion<-factor(ipexpto3$condicion,levels=c(0,1), labels = c("Baja","Alta"))
ipexpto3$orden<-factor(ipexpto3$orden,levels=c(0,1), labels = c("Tema1/Tema2","Tema2/Tema1"))
ipexpto3$Tema<-factor(ipexpto3$Tema,levels=c(0,1), labels = c("Primero","Segundo"))
ipexpto3$sexo<-factor(ipexpto3$sexo,levels=c(0,1), labels = c("Hombre","Mujer"))
ipexpto3$histocol<-factor(ipexpto3$histocol,levels=c(0,1), labels = c("no","si"))
ipexpto3$temacondicion<-factor(ipexpto3$temacondicion,levels=c(1:4), labels = c("Plan Colombia","Dictadura","Conflicto Largo","Conflicto Rural"))

#familiarity descriptives
ipexpto3.1<-read.xlsx("Base Expto 3.1.xlsx",as.data.frame=TRUE,header=TRUE, sheetName="Sheet1",colNames=TRUE)
ipexpto3.1$ID<-factor(ipexpto3.1$ID)

stat.desc(ipexpto3.1$farcf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$paramilf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$caguanf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$parapoliticaf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$conflargof, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$confruralf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$pcolombiaover, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$exterminioupf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$elnf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$constituyentef, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$rpinillaf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$m19f, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$quintinlamef, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$cesefuegof, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$ragrariaf, basic=FALSE,norm=TRUE)
stat.desc(ipexpto3.1$eseguridadf, basic=FALSE,norm=TRUE)

#familiarity graph
familiaridadmelted<-ipexpto3.1[c(1,22:37)]
familiaridadmelted<-melt(familiaridadmelted,id="ID")
familiaridadmelted$variable<-revalue(familiaridadmelted$variable, c("farcf"="F", "paramilf"="PM","caguanf"="CG","parapoliticaf"="PP","conflargof"="CL","confruralf"="CR",
                                                                       "pcolombiaover"="PC","exterminioupf"="UP","elnf"="ELN","constituyentef"="C","rpinillaf"="RP","m19f"="M19","quintinlamef"="QL","cesefuegof"="CF",
                                                                       "ragrariaf"="RA","eseguridadf"="ES"))

line <- ggplot(familiaridadmelted, aes(variable, value))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = variable)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Tema", y = "Familiaridad") +
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#quantiles familiarity
quantile(familiaridadmelted$value,c(.30,.50,.75))

#alphas
cronbach.alpha(subset(deseabilidad3.2,select=c("farcv","farcp","farcpos","farcneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("paramilv","paramilp","paramilpos","paramilneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("parapoliticav","parapoliticap","parapoliticapos","parapoliticaneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("conflargov","conflargop","conflargopos","conflargoneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("confruralv","confruralp","confruralpos","confruralneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("pcolombiav","pcolombiap","pcolombiapos","pcolombianeg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("creacionelnv","creacionelnp","creacionelnpos","creacionelneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))
cronbach.alpha(subset(deseabilidad3.2,select=c("rpinillav","rpinillap","rpinillapos","rpinillaneg"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))

cronbach.alpha(subset(deseabilidad3.2,select=c("ingridover","ralitover","asambleaover","fpositivosover","bogotazover","ctorresover","raulreyesover","oslover",
                                               "jaqueover","jgarzonover","elnogalover","pjusticiaover","mesaelnover","firmapazover","bojayaover","cesefuegover"),
                      CI=TRUE,B=500))

cronbach.alpha(subset(deseabilidad3.2,select=c("uribeoverf","manizalesoverf","periodistasoverf","araucaoverf"),
                      CI=TRUE,B=1000))

#subset desirability
ipexpto3deseabaja<-subset(ipexpto3,condicion=="Baja")
ipexpto3deseabalta<-subset(ipexpto3,condicion=="Alta")

#subset themes (first and second)
ipexpto3temas1<-subset(ipexpto3,Tema=="Primero")
ipexpto3temas2<-subset(ipexpto3,Tema=="Segundo")

##Transformations

#Log transformation
ipexpto3$Falsealarmratelog<-log10(ipexpto3$Falsealarmrate+1)
#Inv transformation
ipexpto3$Falsealarmrateinv<-1/((max(ipexpto3$Falsealarmrate)-ipexpto3$Falsealarmrate)+1)
#sqrt transformation
ipexpto3$Falsealarmratesqrt<-sqrt(ipexpto3$Falsealarmrate+1)
#Calculating Bias
ipexpto3$Bias<-(ipexpto3$Averagedhitrate+ipexpto3$Falsealarmrate)/2

#descriptives

stat.desc(ipexpto3$IPE)
CI(ipexpto3$IPE)

by(ipexpto3$IPE,list(ipexpto3$condicion),stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3$IPE,list(ipexpto3$condicion),CI)

by(ipexpto3$IPE,list(ipexpto3$temacondicion),stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3$IPE,list(ipexpto3$temacondicion),CI)

by(ipexpto3$famporitem,ipexpto3$temacondicion,stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3$famporitem,ipexpto3$temacondicion,CI)

stat.desc(ipexpto3$Falsealarmrate,basic=FALSE,norm=TRUE)
CI(ipexpto3$Falsealarmrate)

by(ipexpto3$compre,list(ipexpto3$temacondicion),stat.desc,basic=FALSE,norm=TRUE)
by(ipexpto3$deseabporitempre,list(ipexpto3$temacondicion),stat.desc,basic=FALSE)

##Normality tests

#IPE
shapiro.test(ipexpto3$IPE)
hist(ipexpto3$IPE)
qplot(sample=ipexpto3$IPE, stat="qq")

#Desirability pre
shapiro.test(ipexpto3$deseabporitempre)

#IPExDesirability Pre

#Low desirability
hist.lowdesirability2<-ggplot(ipexpto3deseabaja, aes(deseabporitempre)) + theme(legend.position ="none") + 
                       geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
                       stat_function(fun=dnorm, args=list(mean = mean(ipexpto3deseabaja$deseabporitempre,na.rm=TRUE),
                       sd=sd(ipexpto3deseabaja$deseabporitempre, na.rm=TRUE)), colour="black", size=1)+ 
                       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

hist.lowdesirability2

#High desirability
hist.highdesirability2<-ggplot(ipexpto3deseabalta, aes(deseabporitempre)) + theme(legend.position ="none") + 
                        geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
                        stat_function(fun=dnorm, args=list(mean = mean(ipexpto3deseabalta$deseabporitempre,na.rm=TRUE),
                        sd=sd(ipexpto3deseabalta$deseabporitempre, na.rm=TRUE)), colour="black", size=1)+
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

hist.highdesirability2

#Low desirability (IPE)

hist.ipelowdesirability2<-ggplot(ipexpto3deseabaja, aes(IPE)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(ipexpto3deseabaja$IPE,na.rm=TRUE),
                                     sd=sd(ipexpto3deseabaja$IPE, na.rm=TRUE)), colour="black", size=1)+
                                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
hist.ipelowdesirability2

#High desirability (IPE)

hist.ipehighdesirability2<-ggplot(ipexpto3deseabalta, aes(IPE)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(ipexpto3deseabalta$IPE,na.rm=TRUE),
                                     sd=sd(ipexpto3deseabalta$IPE, na.rm=TRUE)), colour="black", size=1)+
                                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
hist.ipehighdesirability2

#Q plots
#IPE
qplot(sample=ipexpto3$IPE, stat="qq")
#desirability
qplot(sample=ipexpto3$deseabporitempre, stat="qq")

#check correlations Desirability-IPE

cor.test(ipexpto3$deseabporitempre,ipexpto3$deseabporitempost)
cor.test(ipexpto3$deseabporitempre,ipexpto3$compre)
cor.test(ipexpto3$deseabporitempre,ipexpto3$IPE)

#check correlation Desirability-Familiarity
cor.test(ipexpto3$deseabporitempre,ipexpto3$famporitem)

#check correlation Desirability-Overclaiming
cor.test(ipexpto3$deseabporitempre,ipexpto3$Falsealarmrate)
cor.test(ipexpto3$deseabporitempre,ipexpto3$Accuracy)
cor.test(ipexpto3$deseabporitempre,ipexpto3$Bias)

#descriptive graphs
#topic
line <- ggplot(ipexpto3, aes(ipexpto3$temacondicion, ipexpto3$IPE))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Black", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "IPE")+theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#desirability pre
line <- ggplot(ipexpto3, aes(ipexpto3$temacondicion, ipexpto3$deseabporitempre))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Black", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Previous Desirability")+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#desirability post
line <- ggplot(ipexpto3, aes(ipexpto3$temacondicion, ipexpto3$deseabporitempost))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Black", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Topic", y = "Post Desirability")+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#familiarity pre
line <- ggplot(ipexpto3, aes(ipexpto3$temacondicion, ipexpto3$famporitem))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Black", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Topic", y = "Familiarity")+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#pre
line <- ggplot(ipexpto3, aes(ipexpto3$temacondicion, ipexpto3$compre))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Black", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Previous Understanding")+theme(panel.grid.major = element_blank(), 
                                             panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                                             scale_y_continuous(breaks=c(1.5,2,2.5,3,3.5,4,4.5,5))+expand_limits(y=c(1.5,5))
#post
line <- ggplot(ipexpto3, aes(ipexpto3$temacondicion, ipexpto3$compost))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Black", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Post Understanding")+theme(panel.grid.major = element_blank(), 
                                                        panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                                                        scale_y_continuous(breaks=c(1.5,2,2.5,3,3.5,4,4.5,5))+expand_limits(y=c(1.5,5))
#contrasts
require(multcomp)
clargovscrural<-c(0,0,-1,1)
pcolombiavsrpinilla<-c(1,-1,0,0)
rpinillavsall<-c(0,0,0,1)
contrasts(ipexpto3$temacondicion)<-cbind(clargovscrural,pcolombiavsrpinilla)
ipexpto3$temacondicion

#as GLM
#main effects
t.test(ipexpto3$IPE,mu=0)
stat.desc(ipexpto3$IPE, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
CI(ipexpto3$IPE)
cohensD(ipexpto3$IPE,mu=0)

model0<-lme(IPE ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modelcondicion<-update(model0,.~. + condicion)
modeldespre<-update(model0,.~. + deseabporitempre)
modeloverclaim<-update(modeldespre,.~. + Falsealarmrate)
modeldespost<-update(modeloverclaim,.~. + deseabporitempost)
modelorden<-update(modeldespost,.~. + orden)
anova(model0,modelcondicion,modeldespre,modeloverclaim,modeldespost,modelorden)

#interactions
modelitem<-update(modelorden,.~. + temacondicion)
modeldespreitem<-update(modelitem,.~. + deseabporitempre:temacondicion)
modeldespreover<-update(modeldespreitem,.~. + deseabporitempre:Falsealarmrate)
modelitemover<-update(modeldespreover,.~. + temacondicion:Falsealarmrate)
modelallinteract<-update(modelitemover,.~. + temacondicion:deseabporitempre:Falsealarmratesummary(de))
anova(model0,modelcondicion,modeldespre,modeloverclaim,modeldespost,modelorden,modelitem,modeldespreitem,
      modeldespreover,modelitemover,modelallinteract)
summary(modeldespreitem)
r2beta(modeldespreitem,method= 'nsj')

#definitivemodel
model0<-lme(IPE ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modeldespre<-update(model0,.~. + deseabporitempre)
modeloverclaim<-update(modeldespre,.~. + Falsealarmrate)
modeldespreover<-update(modeloverclaim,.~. + deseabporitempre:Falsealarmrate)
modelorden<-update(modeldespreover,.~. + orden)
modelitem<-update(modeldespreover,.~. + temacondicion)
modeldespreitem<-update(modelitem,.~. + temacondicion:deseabporitempre)
modelitemover<-update(modeldespreitem,.~. + temacondicion:Falsealarmrate)

anova(model0,modeldespre,modeloverclaim,modeldespreover,modelorden,modelitem,modeldespreitem,modelitemover)
summary(modelitem)
r2beta(modeldespreitem,method= 'nsj')

#modelstoreport
modelzero<-lme(IPE ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modelcondicion<-update(modelzero,.~. + condicion)
modeldespre<-update(modelzero,.~. + deseabporitempre)
modeloverclaim<-update(modeldespre,.~. + Falsealarmrate)
modeldespreover<-update(modeloverclaim,.~. + deseabporitempre:Falsealarmrate)
anova(modelzero,modeldespre,modeloverclaim,modeldespreover)

summary(modelzero)
summary(modelcondicion)
summary(modeldespre)
summary(modeloverclaim)
summary(modeldespreover)


##check: interaction Desirability*Theme

#Low desirability
modelow<-lme(IPE ~ 1, random = ~1|ID/Tema, data = ipexpto3deseabaja, method = "ML")
modelowfull<-update(modelow,.~. + deseabporitempre + Falsealarmrate + deseabporitempre:Falsealarmrate)
modelowtema<-update(modelowfull,.~. + Tema)
modelowdestema<-update(modelowtema,.~.+ deseabporitempre:Tema)
anova(modelow,modelowfull,modelowtema,modelowdestema)
summary(modelowdestema)

#High desirability
modelhigh<-lme(IPE ~ 1, random = ~1|ID/Tema, data = ipexpto3deseabalta, method = "ML")
modelhighfull<-update(modelhigh,.~. + deseabporitempre + Falsealarmrate + deseabporitempre:Falsealarmrate)
modelhightema<-update(modelhighfull,.~. + Tema)
modelhighdestema<-update(modelhightema,.~.+ deseabporitempre:Tema)
anova(modelhigh,modelhighfull,modelhightema,modelhighdestema)
summary(modeldestema)

#check: interaction Desirability*Order

modelorden<-update(modeldespreover,.~. + orden)
modeldeseaborden<-update(modelorden,.~.+ deseabporitempre:orden)
anova(modelzero,modeldespreover,modelorden,modeldeseaborden)
summary(modeldeseaborden)

#models to report (alternative)
modelzero<-lme(IPE ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modeltema<-update(modelzero,.~. + Tema)
modelcondicion<-update(modeltema,.~. + condicion)
modeldespre<-update(modeltema,.~. + deseabporitempre)
modeloverclaim<-update(modeldespre,.~. + Falsealarmrate)
modeldespreover<-update(modeloverclaim,.~. + deseabporitempre:Falsealarmrate)
anova(modelzero,modeltema,modeldespre,modeloverclaim,modeldespreover)

##Overclaiming indexes as predictors

#AHR

modeloverclaim1<-update(modeldespre,.~. + Averagedhitrate)
modeloverclaim2<-update(modeloverclaim1,.~. + deseabporitempre:Averagedhitrate)
anova(modeldespre,modeloverclaim1,modeloverclaim2)

#Accuracy
modeloverclaim1<-update(modeldespre,.~. + Accuracy)
modeloverclaim2<-update(modeloverclaim1,.~. + deseabporitempre:Accuracy)
anova(modeldespre,modeloverclaim1,modeloverclaim2)

#Bias
modeloverclaim1<-update(modeldespre,.~. + Bias)
modeloverclaim2<-update(modeloverclaim1,.~. + deseabporitempre:Bias)
anova(modeldespre,modeloverclaim1,modeloverclaim2)

#chistory as predictor

#chistoria
modeloverclaim1<-update(modeldespre,.~. + histocol)
modeloverclaim2<-update(modeloverclaim1,.~. + deseabporitempre:histocol)
anova(modeldespre,modeloverclaim1,modeloverclaim2)
summary(modeloverclaim2)

modelhistTFA<-lm(Falsealarmrate ~ histocol, data = ipexpto3)
summary(modelhistTFA)

modelhistAHR<-lm(Averagedhitrate ~ histocol, data = ipexpto3)
summary(modelhistAHR)

modelhistACC<-lm(Accuracy ~ histocol, data = ipexpto3)
summary(modelhistACC)

modelhistBIAS<-lm(Bias ~ histocol, data = ipexpto3)
summary(modelhistBIAS)

#sex effect
modelsex<-update(modeldespre,.~. + sexo)
summary(modelsex)

#models with standardized coefficients

beta(modelzero,x=TRUE,y=TRUE)
beta(modelcondicion,x=TRUE,y=TRUE,skip=c('Tema','condicion'))
beta(modeldespre,X=TRUE,y=TRUE,skip='Tema')
beta(modeloverclaim,X=TRUE,y=TRUE,skip='Tema')
beta(modeldespreover,X=TRUE,y=TRUE,skip='Tema')

r2beta(modelcondicion,method= 'nsj')
r2beta(modeldespre,method= 'nsj')
r2beta(modeloverclaim,method= 'nsj')
r2beta(modeldespreover,method= 'nsj')

#models (pre)
modelzeropre<-lme(compre ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modelcondicionpre<-update(modelzeropre,.~. + condicion)
modeldesspre<-update(modelzeropre,.~. + deseabporitempre)
modeloverclaimpre<-update(modeldesspre,.~. + Falsealarmrate)
modeldespreoverpre<-update(modeloverclaimpre,.~. + deseabporitempre:Falsealarmrate)
anova(modelzeropre,modeldesspre,modeloverclaimpre,modeldespreoverpre)

#descriptives (pre)

cor.test(ipexpto3$deseabporitempre,ipexpto3$compre)
cor.test(ipexpto3$Falsealarmrate,ipexpto3$compre)
      
#models (post)
modelzeropost<-lme(compost ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modelcondicionpost<-update(modelzeropost,.~. + condicion)
modeldespost<-update(modelzeropost,.~. + deseabporitempre)
modeloverclaimpost<-update(modeldespost,.~. + Falsealarmrate)
modeldespreoverpost<-update(modeloverclaimpost,.~. + deseabporitempre:Falsealarmrate)
anova(modelzeropost,modeldespost,modeloverclaimpost,modeldespreoverpost)

#descriptives (post)

cor.test(ipexpto3$deseabporitempre,ipexpto3$compost)

##Graphs
#TFA
ipexpto3$FA <-cut(ipexpto3$Falsealarmrate,
                         breaks=c(-Inf,mean(ipexpto3$Falsealarmrate),+Inf),
                         labels=c("Low","High"))
table(ipexpto3$FA)

#TFA Low desirability
ipexpto3deseabaja$FA <-cut(ipexpto3deseabaja$Falsealarmrate,
                        breaks=c(-Inf,mean(ipexpto3deseabaja$Falsealarmrate),+Inf),
                       labels=c("Low","High"))

#TFA High desirability
ipexpto3deseabalta$FA <-cut(ipexpto3deseabalta$Falsealarmrate,
                            breaks=c(-Inf,mean(ipexpto3deseabalta$Falsealarmrate),+Inf),
                            labels=c("Low","High"))

#graph despreover
ggplot(ipexpto3, aes(x = deseabporitempre, y = IPE, group = FA,shape=FA,linetype=FA)) +
  geom_point(color="grey") +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,color=1) +
  labs(x = "Social desirability", y = "IOED (Difference Pre-Post)", colour = "FA") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),axis.text = element_text(size=14),axis.title=element_text(size=16))+
        theme(legend.text = element_text(size = 14),legend.title = element_text(size = 14))

#graph despreover (alternative)

library(sjPlot)
library(sjmisc)
theme_set(theme_sjplot())

modelmoderation3<-lme(IPE ~ deseabporitempre + FA + deseabporitempre * FA,  random = ~1|ID/Tema, data = ipexpto3)

plot_model(modelmoderation3, type = "int", terms = c("IOED (Difference Pre-Post)", "FA"))+
  labs(x = "Social Desirability", y = "IOED (Pre-Post Difference)")+
  ggtitle("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=13),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 15),
                                                                                  legend.title = element_text(size = 16))
#Item effects (graph)
ggplot(ipexpto3, aes(x = deseabporitempre, y = IPE, group = temacondicion,shape=temacondicion,colour=temacondicion)) +
  geom_point(color="grey",show.legend = FALSE) +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE) +
  labs(x = "Deseabilidad Social (Pre)", y = "IPE", colour = "Tema") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values=c("twodash","dashed","dotted"))

#First/Second effects

#IPE*First/Second (graph)
line <- ggplot(ipexpto3, aes(Tema,IPE))
line + stat_summary(fun.y = mean, geom = "point", aes(group = 1),colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Tema", y = "Diferencia Pre-Post")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
                                                    axis.line = element_line(colour = "black"),axis.text = element_text(size=14),axis.title=element_text(size=16))+
  scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1,1.5,2))+scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 16))+expand_limits(y=c(-1,2))

#IPE*First/Second*Condition (graph)
Temadeseab<-ggplot(ipexpto3, aes(condicion, IPE, linetype = Tema))
Temadeseab<-Temadeseab + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post", linetype = "Tema") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size=11),axis.title=element_text(size=12))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta")) +
  theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3))+expand_limits(y=c(-2,3))
Temadeseab

##IPE~Desirability*First/Second
#Overall graph
modelmoderationtheme<-lme(IPE ~ deseabporitempre + Tema + deseabporitempre*Tema + TFA,  random = ~1|ID/Tema, data = ipexpto3)

plot_model(modelmoderationtheme, type="int", terms = c("deseabporitempre","Tema"))+
  labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post")+
  ggtitle("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=13),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 15),
                                                                                  legend.title = element_text(size = 16))
#Low desirability graph
modelmoderationthemeb<-lme(IPE ~ deseabporitempre + Tema + deseabporitempre*Tema + TFA,  random = ~1|ID/Tema, data = ipexpto3deseabaja)

plot_model(modelmoderationthemeb, type = "int", terms = "deseabporitempre")+
  labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post")+
  ggtitle("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=13),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 15),
                                                                                  legend.title = element_text(size = 16))
#High desirability graph
modelmoderationthemea<-lme(IPE ~ deseabporitempre + Tema + deseabporitempre*Tema + TFA,  random = ~1|ID/Tema, data = ipexpto3deseabalta)

plot_model(modelmoderationthemea, type = "int", terms = "deseabporitempre")+
  labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post")+
  ggtitle("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=13),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 15),
                                                                                  legend.title = element_text(size = 16))
#Overclaiming as predictor of social desirability
modeldesitem<-lme(deseabporitempre ~ 1, random = ~1|ID/Tema, data = ipexpto3, method = "ML")
modelfalarms<-update(modeldesitem,.~. + Falsealarmrate)
modeltema<-update(modelfalarms,.~. + temacondicion)
modelfatopic<-update(modeltema,.~. + temacondicion:Falsealarmrate)
anova(modeldesitem,modelfalarms,modeltema,modelfatopic)

#History course as predictor of Overclaiming
t.test(ipexpto3$Averagedhitrate~ipexpto3$histocol)

#Mediation Desirability Condition -> Desirability -> IPE
mediationdeseabilidad<-'deseabporitempre ~ condicion
                        IPE ~ deseabporitempre + condicion'
fitmedeseabilidad<-sem(mediationdeseabilidad,data=ipexpto3)
summary(fitmedeseabilidad,standardized=T,fit.measures=T,rsq=T)

mediationdeseabilidad2<-'deseabporitempre ~ a*condicion
                         IPE ~ b*deseabporitempre + c*condicion
                         indirect:= a*b
                         direct:= c
                         total:= c + (a*b)'
fitmedeseabilidad2<-sem(mediationdeseabilidad2,data=ipexpto3)
summary(fitmedeseabilidad2,standardized=T,fit.measures=T,rsq=T)
