library(nlme)
library(pastecs)
library (ggplot2)
library (foreign)
library(multcomp)
library(r2glmm)
library(Rmisc)
library(Hmisc)
library(reshape2)
library(EnvStats)
library(xlsx)
library(ltm)

#desirability scale (allitems)
ipexpto2deseab<-read.spss("Experimento2Parte1.sav",use.value.labels = FALSE,to.data.frame = TRUE)
View(ipexpto2deseab)

ipexpto2deseab$ID<-factor(ipexpto2deseab$ID)
ipexpto2deseab$StartDate<-NULL
ipexpto2deseab$EndDate<-NULL
names(ipexpto2deseab)[names(ipexpto2deseab) == 'agrariasum'] <- 'RA'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'baldiosum'] <- 'B'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'desmovsum'] <- 'DM'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'abortosum'] <- 'AB'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'pilopagasum'] <- 'PP'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'colcienciasum'] <- 'C'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'licenciaturasum'] <- 'L'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'gaseosasum'] <- 'G'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'drogasum'] <- 'D'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'policiasum'] <- 'P'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'ubersum'] <- 'U'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'pagotronicosum'] <- 'PE'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'proceresum'] <- 'PC'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'actoressum'] <- 'A'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'eimpuestosum'] <-'IE'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'victimasum'] <- 'V'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'presidentesum'] <- 'PR'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'cuotasum'] <- 'CG'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'cplasticasum'] <-'CP'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'minternasum'] <- 'MI'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'saldosum'] <- 'SB'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'ncongresistasum'] <- 'NC'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'pescadoresum'] <-'PS'
names(ipexpto2deseab)[names(ipexpto2deseab) == 'equilibriosum'] <- 'EP'

deseabexpto2<-ipexpto2deseab[c(1,185:209)]
deseabexpto2$LOCAT6<-NULL
View(deseabexpto2)

#graph desirability

deseabexpto2melted<-melt(deseabexpto2,id="ID")
View(deseabexpto2melted)

line <- ggplot(deseabexpto2melted, aes(variable, value))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = variable)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Tema", y = "Deseabilidad Social") +
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#desirability scale (6 items)
ipexpto2<-read.spss("Experimento2largaCRT.sav",use.value.labels = FALSE,to.data.frame = TRUE)
View(ipexpto2)

ipexpto2$deseabilidad<-factor(ipexpto2$deseabilidad,levels=c(0,1), labels = c("Baja","Alta"))
ipexpto2$item<-factor(ipexpto2$item,levels=c(0,1), labels = c("primero","segundo"))
ipexpto2$tema[ipexpto2$item =="primero" & ipexpto2$deseabilidad=="Baja"] <- 1
ipexpto2$tema[ipexpto2$item == "segundo" & ipexpto2$deseabilidad == "Baja"] <- 2
ipexpto2$tema[ipexpto2$item == "primero"& ipexpto2$deseabilidad == "Alta"] <- 3
ipexpto2$tema[ipexpto2$item == "segundo"& ipexpto2$deseabilidad == "Alta"]<- 4
ipexpto2$tema<-factor(ipexpto2$tema,levels=c(1,2,3,4), labels = c("actores","cplastica","avictimas","acdrogas"))

#desirability scale (4 items)
ipexpto2<-read.spss("Experimento2largaCRT (4items).sav",use.value.labels = FALSE,to.data.frame = TRUE)
View(ipexpto2)

ipexpto2$deseabilidad<-factor(ipexpto2$deseabilidad,levels=c(0,1), labels = c("Baja","Alta"))
ipexpto2$item<-factor(ipexpto2$item,levels=c(0,1), labels = c("primero","segundo"))
ipexpto2$tema[ipexpto2$item =="primero" & ipexpto2$deseabilidad=="Baja"] <- 1
ipexpto2$tema[ipexpto2$item == "segundo" & ipexpto2$deseabilidad == "Baja"] <- 2
ipexpto2$tema[ipexpto2$item == "primero"& ipexpto2$deseabilidad == "Alta"] <- 3
ipexpto2$tema[ipexpto2$item == "segundo"& ipexpto2$deseabilidad == "Alta"]<- 4
ipexpto2$tema<-factor(ipexpto2$tema,levels=c(1,2,3,4), labels = c("actores","cplastica","avictimas","acdrogas"))

#desirability comparisons
CI((subset(ipexpto2$sumadeseab,ipexpto2$tema=="avictimas")),ci = 0.95)
CI((subset(ipexpto2$sumadeseab,ipexpto2$tema=="acdrogas")),ci = 0.95)
CI((subset(ipexpto2$sumadeseab,ipexpto2$tema=="actores")),ci = 0.95)
CI((subset(ipexpto2$sumadeseab,ipexpto2$tema=="cplastica")),ci = 0.95)

t.test((subset(ipexpto2$sumadeseab,ipexpto2$tema=="avictimas")),(subset(ipexpto2$sumadeseab,ipexpto2$tema=="acdrogas")),paired=TRUE)
t.test((subset(ipexpto2$sumadeseab,ipexpto2$tema=="avictimas")),(subset(ipexpto2$sumadeseab,ipexpto2$tema=="actores")),paired=TRUE)
t.test((subset(ipexpto2$sumadeseab,ipexpto2$tema=="avictimas")),(subset(ipexpto2$sumadeseab,ipexpto2$tema=="cplastica")),paired=TRUE)
t.test((subset(ipexpto2$sumadeseab,ipexpto2$tema=="acdrogas")),(subset(ipexpto2$sumadeseab,ipexpto2$tema=="actores")),paired=TRUE)
t.test((subset(ipexpto2$sumadeseab,ipexpto2$tema=="acdrogas")),(subset(ipexpto2$sumadeseab,ipexpto2$tema=="cplastica")),paired=TRUE)
t.test((subset(ipexpto2$sumadeseab,ipexpto2$tema=="actores")),(subset(ipexpto2$sumadeseab,ipexpto2$tema=="cplastica")),paired=TRUE)

expto2deseab<-read.csv("expto2deseab.csv",sep=";",header=TRUE)
View(expto2deseab)
expto2deseab$ID<-factor(expto2deseab$ID)
expto2deseabmelted<-melt(expto2deseab,id="ID",measured=c("V","D","AC","CP"))
View(expto2deseabmelted)

line <- ggplot(expto2deseabmelted, aes(variable, value))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = variable)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Tema", y = "Deseabilidad social") +
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Reliability CRT
ipexpto3CRT<-read.xlsx("Experimento3(sesiones1y2).xlsx",as.data.frame=TRUE,header=TRUE, sheetName="Sheet1")
ipexpto3CRT$CRTbate<-factor(ipexpto3CRT$CRTbate)
ipexpto3CRT$CRTmaquinas<-factor(ipexpto3CRT$CRTmaquinas)
ipexpto3CRT$CRTalgas<-factor(ipexpto3CRT$CRTalgas)
View(ipexpto3CRT)
cronbach.alpha(subset(ipexpto3CRT,select=c("CRTbate","CRTmaquinas","CRTalgas"),
                      CI=TRUE,probs = c(0.025, 0.975),B=1000))

#Normality tests
#IPE
shapiro.test(ipexpto2$prepost)
hist(ipexpto2$prepost)
qplot(sample=ipexpto2$prepost, stat="qq")
#desirability
shapiro.test(ipexpto2$sumadeseab)
hist(ipexpto2$sumadeseab)
#IPE x desirability
desebaja3<-subset(ipexpto2, ipexpto2$deseabilidad=="Baja")
desebalta3<-subset(ipexpto2, ipexpto2$deseabilidad=="Alta")

#low desirability

hist.lowdesirability3<-ggplot(desebaja3, aes(sumadeseab)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebaja3$sumadeseab,na.rm=TRUE),
  sd=sd(desebaja3$sumadeseab, na.rm=TRUE)), colour="black", size=1)
hist.lowdesirability3

#high desirability

hist.highdesirability3<-ggplot(desebalta3, aes(sumadeseab)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebalta3$sumadeseab,na.rm=TRUE),
  sd=sd(desebalta3$sumadeseab, na.rm=TRUE)), colour="black", size=1)
hist.highdesirability3

#low desirability (IPE)

hist.ipelowdesirability3<-ggplot(desebaja3, aes(prepost)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebaja3$prepost,na.rm=TRUE),
                                     sd=sd(desebaja3$prepost, na.rm=TRUE)), colour="black", size=1)
hist.ipelowdesirability3

#high desirability (IPE)

hist.ipehighdesirability3<-ggplot(desebalta3, aes(prepost)) + theme(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour="black",binwidth = 1) + labs(x = "Deseabilidad Social", y = "Densidad") + 
  stat_function(fun=dnorm, args=list(mean = mean(desebalta3$prepost,na.rm=TRUE),
                                     sd=sd(desebalta3$prepost, na.rm=TRUE)), colour="black", size=1)
hist.ipehighdesirability3

#qplots

qplot(sample=ipexpto2$prepost, stat="qq")
qplot(sample=ipexpto2$sumadeseab, stat="qq")


#main effect of IPE
t.test(ipexpto2$prepost,mu=0)
stat.desc(ipexpto2$prepost, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
CI(ipexpto2$prepost)
cohensD(ipexpto2$prepost,mu=0)

##check correlations desirability

cor.test(ipexpto2$sumadeseab,ipexpto2$comprensionpre)
cor.test(ipexpto2$sumadeseab,ipexpto2$comprensionpost)
cor.test(ipexpto2$sumadeseab,ipexpto2$prepost)

#as GLM
model0<-lme(prepost ~ 1, random = ~1|ID/item, data = ipexpto2, method = "ML")
modelcondicion<-update(model0,.~. + deseabilidad)
modeldeseab<-update(model0,.~. + sumadeseab)
modelcrt<-update(modeldeseab,.~. + sumcrt)
modeldeseabcrt<-update(modelcrt,.~. + sumadeseab:sumcrt)
anova(model0,modeldeseab,modelcrt,modeldeseabcrt)

summary(model0)
summary(modeldeseab)
summary(modelcrt)
summary(modeldeseabcrt)

#check: interaction desirability:item
modelitem<-update(modeldeseab,.~. + item)
modeldesitem<-update(modelitem,.~. + sumadeseab:item)
anova(model0,modeldeseab,modelitem,modeldesitem)

#standardized coefficients

beta(model0,x=TRUE,y=TRUE)
beta(modelcondicion,x=TRUE,y=TRUE,skip='condicion')
beta(modeldeseab,X=TRUE,y=TRUE)
beta(modelcrt,X=TRUE,y=TRUE)
beta(modeldeseabcrt,X=TRUE,y=TRUE)

r2beta(modeldeseab,method= 'nsj')
summary(modeldeseab)
r2beta(modelcrt,method= 'nsj')
summary(modelcrt)
r2beta(modeldeseabcrt,method= 'nsj')
summary(modeldeseabcrt)

#as moderation
library(psych)
deseabilidad<-lm(prepost ~ sumadeseab, data = ipexpto2)
reflective<-lm(prepost ~ sumadeseab + CRT, data = ipexpto2)
moderation<-lm(prepost ~ sumadeseab*CRT, data = ipexpto2)
summary(reflective)
summary(moderation)

## models for pre ratings

model0pre<-lme(comprensionpre ~ 1, random = ~1|ID/item, data = ipexpto2, method = "ML")
modelcondicionpre<-update(model0pre,.~. + deseabilidad)
modeldeseabpre<-update(model0pre,.~. + sumadeseab)
modelcrtpre<-update(modeldeseabpre,.~. + sumcrt)
modeldeseabcrtpre<-update(modelcrtpre,.~. + sumadeseab:sumcrt)
anova(model0pre,modeldeseabpre,modelcrtpre,modeldeseabcrtpre)

#descriptives for pre ratings

by(data=ipexpto2$comprensionpre,ipexpto2$deseabilidad,stat.desc)
by(data=ipexpto2$comprensionpre,ipexpto2$deseabilidad,CI)

## models for post ratings

model0post<-lme(comprensionpost ~ 1, random = ~1|ID/item, data = ipexpto2, method = "ML")
modelcondicionpost<-update(model0post,.~. + deseabilidad)
modeldeseabpost<-update(model0post,.~. + sumadeseab)
modelcrtpost<-update(modeldeseabpost,.~. + sumcrt)
modeldeseabcrtpost<-update(modelcrtpost,.~. + sumadeseab:sumcrt)
anova(model0post,modeldeseabpost,modelcrtpost,modeldeseabcrtpost)

#descriptives for post ratings

by(data=ipexpto2$comprensionpost,ipexpto2$deseabilidad,stat.desc)
by(data=ipexpto2$comprensionpost,ipexpto2$deseabilidad,CI)

#descriptives for prepost ratings
by(data=ipexpto2$prepost,ipexpto2$tema,stat.desc)
by(data=ipexpto2$prepost,ipexpto2$tema,CI)

#graph
ipexpto2$CRT <-cut(ipexpto2$sumcrt,
                   breaks=c(-Inf, 0, +Inf),
                   labels=c("Bajo","Alto"))

ggplot(ipexpto2, aes(x=sumadeseab, y=prepost, shape=CRT,linetype=CRT)) + geom_point(color="grey") +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,color=1)+
  labs(x = "Deseabilidad Social", y = "Diferencia Pre-Post")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=12),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 16),
                                                                                 legend.title = element_text(size = 16))
#alternative graph

library(sjPlot)
library(sjmisc)
theme_set(theme_sjplot())

# model with interaction
ipexpto2$CRT<-case_when(
  ipexpto2$sumcrt==0~"Low",
  ipexpto2$sumcrt>0~"High")
ipexpto2$CRT<-factor(ipexpto2$CRT)

modelinteraction2<-lme(prepost ~ sumadeseab + CRT + sumadeseab * CRT, random = ~1|ID/item, data=ipexpto2)

plot_model(modelinteraction2, type = "int", terms = c("Social desirability", "CRT"))+
  labs(x = "Social Desirability", y = "IOED (Pre-Post Difference)")+
  ggtitle("")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
      axis.line = element_line(colour = "black"),
      axis.text = element_text(size=13),axis.title=element_text(size=16))+theme(legend.text = element_text(size = 15),
                                                                                legend.title = element_text(size = 16))
