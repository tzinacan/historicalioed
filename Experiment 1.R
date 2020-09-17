library(nlme)
library(pastecs)
library (ggplot2)
library(foreign)
library(stats)
library(Rmisc)
library(reshape2)
library(memisc)
library(multicon)
library(plyr)
library(lsr)
library(psych)
library(reghelper)
library(r2glmm)
library(ggpubr)
library(EnvStats)

#social desirability pretest
deseabilidadpre<-read.csv("deseabilidadexpto1.csv",header=TRUE,sep=";")
deseabilidadpremelted<-melt(deseabilidadpre,id=c("ID","sexo","edad","estrato","neducativo"),measured=c("largaduracion","creacionfarc","inflacion","bolsavalores","canapescar","jet"))
deseabilidadpremelted$variable<-factor(deseabilidadpremelted$variable, levels=c("largaduracion","creacionfarc","inflacion","bolsavalores","canapescar","jet"))
deseabilidadpremelted$variable<-revalue(deseabilidadpremelted$variable, c("largaduracion"="DC", "creacionfarc"="CF","inflacion"="I","bolsavalores"="BV","canapescar"="CP","jet"="TJ"))
names(deseabilidadpremelted)[names(deseabilidadpremelted)=="variable"] <- "Tema"
names(deseabilidadpremelted)[names(deseabilidadpremelted)=="value"] <- "Deseabilidad"
View(deseabilidadpremelted)

#graph social desirability pretest
line <- ggplot(deseabilidadpremelted, aes(Tema, Deseabilidad))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = Tema)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Tema", y = "Deseabilidad social") +
                  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                  scale_y_continuous(breaks=c(2,2.5,3,3.5,4,4.5,5,5.5))+expand_limits(y=c(2,5,5.5))

#preparation
ipexpto1<-data.frame(as.data.set(spss.system.file("IPE184simp.sav"),use.value.labels = TRUE,to.data.frame = TRUE))
ipexpto1$prepostbaja<-ipexpto1$previabaja-ipexpto1$postbaja
ipexpto1$prepostalta<-ipexpto1$previalta-ipexpto1$postalta
ipexpto1$previabaja<-NULL
ipexpto1$postbaja<-NULL
ipexpto1$previalta<-NULL
ipexpto1$postalta<-NULL
ipe1melted<-melt(ipexpto1,id=c("puesto","categoria","orden"),measured=c("prepostbaja","prepostalta"))
View(ipe1melted)
names(ipe1melted)<-c("ID","dominio","orden","deseabilidad","comprension")
ipe1melted$dominio<-factor(ipe1melted$dominio,labels=c("Historico","Economico","Dispositivos"))
ipe1melted$orden<-factor(ipe1melted$orden,labels=c("Baja/Alta","Alta/Baja"))
ipe1melted$ID<-factor(ipe1melted$ID)

#contrasts domain
require(multcomp)
contrasteHistDisp<-c(1,0,0)
contrasteEcoDisp<-c(0,1,0)
contrasts(ipe1melted$dominio)<-cbind(contrasteHistDisp,contrasteEcoDisp)
ipe1melted$dominio
domconstrast<-aov(comprension~dominio, data=ipe1melted)
summary.lm(domconstrast)

#recode domain
ipe1melted$dispo[ipe1melted$dominio=="Dispositivos"]<-"Dispositivos"
ipe1melted$dispo[ipe1melted$dominio!="Dispositivos"]<-"Hist/Eco"

#descriptives

stat.desc(ipe1melted$comprension)
CI(ipe1melted$comprension)

by(data=ipe1melted$comprension,ipe1melted$deseabilidad,stat.desc)
by(data=ipe1melted$comprension,ipe1melted$deseabilidad,CI)

by(data=ipe1melted$comprension,ipe1melted$dominio,stat.desc)
by(data=ipe1melted$comprension,ipe1melted$dominio,CI)

by(data=ipe1melted$comprension,list(ipe1melted$dominio,ipe1melted$deseabilidad),stat.desc)
by(data=ipe1melted$comprension,list(ipe1melted$dominio,ipe1melted$deseabilidad),CI)

#normality

shapiro.test(ipe1melted$comprension)

hist.ipelowdesirability1<-ggplot(ipexpto1, aes(prepostbaja)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(ipexpto1$prepostbaja,na.rm=TRUE),
                                     sd=sd(ipexpto1$prepostbaja, na.rm=TRUE)), colour="black", size=1)
hist.ipelowdesirability1

hist.ipehighdesirability1<-ggplot(ipexpto1, aes(prepostalta)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(ipexpto1$prepostalta,na.rm=TRUE),
                                     sd=sd(ipexpto1$prepostalta, na.rm=TRUE)), colour="black", size=1)
hist.ipehighdesirability1

qplot(sample=ipe1melted$comprension, stat="qq")

#Main effect of IPE
t.test(ipe1melted$comprension,mu=0)
stat.desc(ipe1melted$comprension, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
cohensD(ipe1melted$comprension,mu=0)

#Domain with ANOVA
dominioaov<-aov(comprension ~ dominio, data=ipe1melted)
summary(dominioaov)
pairwise.t.test(ipe1melted$comprension, ipe1melted$dominio)

#mixed anova
library(ez)
iperepetidas<-ezANOVA(data=ipe1melted,dv=.(comprension),wid=.(ID),within = .(deseabilidad),between =.(dominio,orden),detailed=TRUE, type=2)
iperepetidas

#as GLM
initialmodel<-gls(comprension ~ 1,data = ipe1melted, method = "ML")
baselinemodel<-lme(comprension ~ 1, random = ~1|ID/deseabilidad, data = ipe1melted, method = "ML")
deseabmodel<-update(baselinemodel,.~. + deseabilidad)
dommodel<-update(deseabmodel,.~. + dominio)
deseabdommodel<-update(dommodel,.~. + dominio:deseabilidad)
ordenmodel<-update(deseabdommodel,.~. + orden)
ordprepost<-update(ordenmodel,.~. + orden:deseabilidad)
allinmodel<-update(ordprepost,.~. + dominio:orden:deseabilidad)
anovamodel<-update(deseabdommodel,.~. + orden:deseabilidad + dominio:orden:deseabilidad)
anova(initialmodel,baselinemodel,deseabmodel,dommodel,deseabdommodel,ordenmodel,ordprepost,allinmodel,anovamodel)

summary(baselinemodel)
summary(deseabmodel)
summary(dommodel)
summary(deseabordmodel)
summary(allinmodel)

#as GLM to report

baselinemodel<-lme(comprension ~ 1, random = ~1|ID/deseabilidad, data = ipe1melted, method = "ML")
deseabmodel<-update(baselinemodel,.~. + deseabilidad)
dommodel<-update(deseabmodel,.~. + dominio)
ordmodel<-update(dommodel,.~. + orden)
deseabdommodel<-update(ordmodel,.~. + deseabilidad:dominio)
deseabordmodel<-update(deseabdommodel,.~. + deseabilidad:orden)
allinmodel<-update(deseabordmodel,.~. + deseabilidad:orden:dominio)
anova(baselinemodel,deseabmodel,dommodel,ordmodel, deseabdommodel,deseabordmodel,allinmodel)

summary(baselinemodel)
summary(deseabmodel)
summary(dommodel)
summary(ordmodel)
summary(deseabdommodel)
summary (deseabordmodel)
summary(allinmodel)

#models with standardized coefficients

beta(baselinemodel,x=TRUE,y=TRUE,skip=NULL)
beta(deseabmodel,x=FALSE,y=FALSE,skip=NULL)
beta(dommodel,x=FALSE,y=TRUE,skip=NULL)
beta(deseabdommodel,x=FALSE,y=TRUE,skip=NULL)
beta(deseabordmodel,x=FALSE,y=TRUE,skip=NULL)
beta(allinmodel,x=FALSE,y=TRUE,skip=NULL)

r2beta(deseabmodel,method= 'nsj')
r2beta(dommodel,method= 'nsj')
r2beta(deseabdommodel,method= 'nsj')
r2beta(deseabordmodel,method= 'nsj')
r2beta(allinmodel,method= 'nsj')

##Separated models in pre and post ratings

ipexpto1preypost<-data.frame(as.data.set(spss.system.file("IPE184simp.sav"),use.value.labels = TRUE,to.data.frame = TRUE))
View(ipexpto1preypost)
ipexpto1preypostmelted<-melt(ipexpto1preypost,id=c("puesto","categoria","orden"),measured=c("previabaja","previalta","postbaja","postalta"))
View(ipexpto1preypostmelted)
names(ipexpto1preypostmelted)<-c("ID","dominio","orden","destime","comprension")
ipexpto1preypostmelted$dominio<-factor(ipexpto1preypostmelted$dominio,labels=c("Hist?rico","Econ?mico","Dispositivos"))
ipexpto1preypostmelted$orden<-factor(ipexpto1preypostmelted$orden,labels=c("Baja/Alta","Alta/Baja"))
ipexpto1preypostmelted$ID<-factor(ipexpto1preypostmelted$ID)
ipexpto1preypostmelted$dispo[ipe1melted$dominio=="Dispositivos"]<-"Dispositivos"
ipexpto1preypostmelted$dispo[ipe1melted$dominio!="Dispositivos"]<-"Hist/Eco"

ipexpto1preypostmelted$destimecat[ipexpto1preypostmelted$destime=="previabaja"] <- "0"
ipexpto1preypostmelted$destimecat[ipexpto1preypostmelted$destime=="previalta"] <- "0"
ipexpto1preypostmelted$destimecat[ipexpto1preypostmelted$destime=="postbaja"] <- "1"
ipexpto1preypostmelted$destimecat[ipexpto1preypostmelted$destime=="postalta"] <- "1"
ipexpto1preypostmelted$destimecat<-factor(ipexpto1preypostmelted$destimecat,labels=c("Previa","Post"))

ipexpto1preypostmelted$deseabilidad[ipexpto1preypostmelted$destime=="previabaja"] <- "0"
ipexpto1preypostmelted$deseabilidad[ipexpto1preypostmelted$destime=="previalta"] <- "1"
ipexpto1preypostmelted$deseabilidad[ipexpto1preypostmelted$destime=="postbaja"] <- "0"
ipexpto1preypostmelted$deseabilidad[ipexpto1preypostmelted$destime=="postalta"] <- "1"
ipexpto1preypostmelted$deseabilidad<-factor(ipexpto1preypostmelted$deseabilidad,labels=c("Baja","Alta"))
View(ipexpto1preypostmelted)

## models for pre ratings

ipexpto1premelted<-subset(ipexpto1preypostmelted,destimecat=="Previa")
View(ipexpto1premelted)

baselinemodel<-lme(comprension ~ 1, random = ~1|ID/deseabilidad, data = ipexpto1premelted, method = "ML")
deseabmodel<-update(baselinemodel,.~. + deseabilidad)
dommodel<-update(deseabmodel,.~. + dispo)
deseabdommodel<-update(dommodel,.~. + deseabilidad:dispo)
deseabordmodel<-update(deseabdommodel,.~. + deseabilidad:orden)
allinmodel<-update(deseabordmodel,.~. + deseabilidad:orden:dispo)
anova(baselinemodel,deseabmodel,dommodel,deseabdommodel,deseabordmodel,allinmodel)

#descriptives for pre ratings

by(data=ipexpto1premelted$comprension,ipexpto1premelted$deseabilidad,stat.desc)
by(data=ipexpto1premelted$comprension,ipexpto1premelted$deseabilidad,CI)

by(data=ipexpto1premelted$comprension,list(ipexpto1premelted$dominio,ipexpto1premelted$deseabilidad),stat.desc)
by(data=ipexpto1premelted$comprension,list(ipexpto1premelted$dominio,ipexpto1premelted$deseabilidad),CI)

#graph desirability x domain (pre)

line <- ggplot(ipexpto1premelted, aes(deseabilidad, comprension, linetype = dominio),scale_linetype_manual(values=c("dotdash","longdash","dotted")))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = dominio)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Deseabilidad Social", y = "Comprension Pre", linetype = "Dominio")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=12),axis.title=element_text(size=16))+ 
  scale_linetype_manual(values = c(1,3,5))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 14))+theme(legend.title = element_text(size=14))+scale_y_continuous(breaks=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5))+expand_limits(y=c(1,5.5))

## models for post ratings

ipexpto1postmelted<-subset(ipexpto1preypostmelted,destimecat=="Post")
View(ipexpto1postmelted)

baselinemodel<-lme(comprension ~ 1, random = ~1|ID/deseabilidad, data = ipexpto1postmelted, method = "ML")
deseabmodel<-update(baselinemodel,.~. + deseabilidad)
dommodel<-update(deseabmodel,.~. + dispo)
deseabdommodel<-update(dommodel,.~. + deseabilidad:dispo)
deseabordmodel<-update(deseabdommodel,.~. + deseabilidad:orden)
allinmodel<-update(deseabordmodel,.~. + deseabilidad:orden:dispo)
anova(baselinemodel,deseabmodel,dommodel,deseabdommodel,deseabordmodel,allinmodel)

#descriptives for post ratings

by(data=ipexpto1postmelted$comprension,list(ipexpto1postmelted$dominio,ipexpto1postmelted$deseabilidad),stat.desc)
by(data=ipexpto1postmelted$comprension,list(ipexpto1postmelted$dominio,ipexpto1postmelted$deseabilidad),CI)

#graph desirability x domain (post)

line <- ggplot(ipexpto1postmelted, aes(deseabilidad, comprension, linetype = dominio),scale_linetype_manual(values=c("dotdash","longdash","dotted")))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = dominio)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Deseabilidad Social", y = "Comprension Post", linetype = "Dominio")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=12),axis.title=element_text(size=14))+ 
  scale_linetype_manual(values = c(1,3,5))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 14))+theme(legend.title = element_text(size=14))+scale_y_continuous(breaks=c(1.5,2,2.5,3,3.5,4))+expand_limits(y=c(1.5,4))

#graph order x desirability x domain (post)

ipexpto1postmeltedhistorico<-subset(ipexpto1postmelted,dominio=="Hist?rico")
deshistorico<-ggplot(ipexpto1postmeltedhistorico, aes(deseabilidad, comprension, linetype = orden))
deshistorico<-deshistorico + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Comprensi?n Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=11),axis.title=element_text(size=12))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(1,1.5,2,2.5,3,3.5,4,4.5,5))+expand_limits(y=c(1,5))
#orden_deseabilidad_economico
ipexpto1postmeltedeconomico<-subset(ipexpto1postmelted,dominio=="Econ?mico")
deseconomico <- ggplot(ipexpto1postmeltedeconomico, aes(deseabilidad, comprension, linetype = orden))
deseconomico <- deseconomico + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Comprensi?n Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=11),axis.title=element_text(size=12))+scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(1,1.5,2,2.5,3,3.5,4,4.5,5))+expand_limits(y=c(1,5))
#orden_deseabilidad_dispositivos
ipexpto1postmelteddispo<-subset(ipexpto1postmelted,dominio=="Dispositivos")
desdispo <-ggplot(ipexpto1postmelteddispo, aes(deseabilidad, comprension, linetype = orden))
desdispo<-desdispo + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Comprensi?n Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=11),axis.title=element_text(size=12))+scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(1,1.5,2,2.5,3,3.5,4,4.5,5))+expand_limits(y=c(1,5))

ggarrange(
  deshistorico,deseconomico, desdispo,labels = c("H", "E","D"),
  common.legend = TRUE, legend = "bottom", nrow=1,ncol=3, font.label =10)

##Graphs
#deseabilidad
line <- ggplot(ipe1melted, aes(ipe1melted$deseabilidad, ipe1melted$comprension))
#layer_with_means_and_points
line + stat_summary(fun.y = mean, geom = "point") 
#line_connecting_colour_dashed
+ stat_summary(fun.y = mean, geom = "line", aes(group = 1), linetype = "dashed") 
#error_bars_and_labels
+ stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Measure Timing", y = "Comprension")
#altogether
line <- ggplot(ipe1melted, aes(deseabilidad,comprension))
line + stat_summary(fun.y = mean, geom = "point", aes(group = 1),colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=14),axis.title=element_text(size=16))+scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1,1.5,2))+scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 16))+expand_limits(y=c(-1,2))

#IPE_dominio
line <- ggplot(ipe1melted, aes(dominio, comprension))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = dominio)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Dominio", y = "IPE", linetype = "Dominio")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#dominio_deseabilidad
line <- ggplot(ipe1melted, aes(deseabilidad, comprension, linetype = dominio),scale_linetype_manual(values=c("dotdash","longdash","dotted")))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = dominio)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post", linetype = "Dominio")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=14),axis.title=element_text(size=16))+ scale_linetype_manual(values = c(1,3,5))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 14))+theme(legend.title = element_text(size=14))+scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1,1.5,2))+expand_limits(y=c(-1,2))
#orden_deseabilidad
line <- ggplot(ipe1melted, aes(deseabilidad, comprension, linetype = orden))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=16),axis.title=element_text(size=16))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 16))+theme(legend.title = element_text(size=16))+scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1,1.5,2))+expand_limits(y=c(-1,2))
#orden_deseabilidad_historico
ipe1meltedhistorico<-subset(ipe1melted,dominio=="Hist?rico")
deshistorico<-ggplot(ipe1meltedhistorico, aes(deseabilidad, comprension, linetype = orden))
deshistorico<-deshistorico + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=11),axis.title=element_text(size=12))+ scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3))+expand_limits(y=c(-2,3))
#orden_deseabilidad_economico
ipe1meltedeconomico<-subset(ipe1melted,dominio=="Econ?mico")
deseconomico <- ggplot(ipe1meltedeconomico, aes(deseabilidad, comprension, linetype = orden))
deseconomico <- deseconomico + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=11),axis.title=element_text(size=12))+scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3))+expand_limits(y=c(-2,3))
#orden_deseabilidad_dispositivos
ipe1melteddispo<-subset(ipe1melted,dominio=="Dispositivos")
desdispo <-ggplot(ipe1melteddispo, aes(deseabilidad, comprension, linetype = orden))
desdispo<-desdispo + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = orden)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post", linetype = "Orden")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(size=11),axis.title=element_text(size=12))+scale_x_discrete(labels=c("prepostbaja" = "Baja", "prepostalta" = "Alta"))+theme(legend.text = element_text(size = 13))+theme(legend.title = element_text(size=13))+scale_y_continuous(breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3))+expand_limits(y=c(-2,3))

ggarrange(
  deshistorico,deseconomico, desdispo,labels = c("H", "E","D"),
  common.legend = TRUE, legend = "bottom", nrow=1,ncol=3, font.label =10)

#descriptives_interaction
by(data=ipe1melted$comprension,list(ipe1melted$dominio,ipe1melted$deseabilidad,ipe1melted$orden),stat.desc)
by(data=ipe1melted$comprension,list(ipe1melted$dominio,ipe1melted$deseabilidad,ipe1melted$orden),CI)

by(data=ipe1meltedhistorico$comprension,list(ipe1meltedhistorico$deseabilidad,ipe1meltedhistorico$orden),stat.desc)
by(data=ipe1meltedhistorico$comprension,list(ipe1meltedhistorico$deseabilidad,ipe1meltedhistorico$orden),CI)

by(data=ipe1meltedeconomico$comprension,list(ipe1meltedeconomico$deseabilidad,ipe1meltedeconomico$orden),stat.desc)
by(data=ipe1meltedeconomico$comprension,list(ipe1meltedeconomico$deseabilidad,ipe1meltedeconomico$orden),CI)
by(data=ipe1melteddispo$comprension,list(ipe1melteddispo$deseabilidad,ipe1melteddispo$orden),CI)

#catseye
catseye(ipe1melted$comprension)
catseye(ipe1melted$comprension, grp=ipe1melted$deseabilidad)
catseye(ipe1melted$comprension, grp=ipe1melted$dominio)