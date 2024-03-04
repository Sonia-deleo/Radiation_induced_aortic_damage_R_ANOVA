


#REMOVE THIS CHUNK IF LIBRARY INSTALLATION WANTS TO BE RUN MANUALLY

#instals requierRments and rstudioapi packages if not already installed
if(!require(requiRements)) install.packages("requiRements",repos = "http://cran.us.r-project.org")
if(require(!rstudioapi)) install.packages("rstudioapi",repos = "http://cran.us.r-project.org")
library(requiRements)
library(rstudioapi)

#set working directory to location of the current script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#installs any missing requirements listed in Requirements2.txt. It Does not update any already installed packages (upgrade='never).
requirements_path<-'./Requirements2.txt'
requiRements::install(path_to_requirements = requirements_path, upgrade='never') 


#pkgSnapshot("mySnapshot.txt", standAlone = TRUE)


library(rstudioapi)
library(tidyverse)
library(ggplot2)
library(GGally)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


#print(getwd()) #checking current working directory.

df_aorta=read.table('aorta2.txt', header = TRUE)

df_aorta<-df_aorta %>% rename('Id'= X)
df_aorta<-df_aorta %>% rename_with(str_to_title)
df_aorta<-df_aorta  %>% rename('Diet'=Dieta, 'Radiation'=Radiacion)
df_aorta<-df_aorta  %>% rename('Mouse'=Raton)

df_aorta$Diet<-as.factor(df_aorta$Diet)
df_aorta$Radiation<-as.factor(df_aorta$Radiation)
df_aorta$Diet<-ifelse(df_aorta$Diet=='estandar',0,1)
df_aorta$Diet=factor(df_aorta$Diet, levels=c(0,1), labels= c('standard','high-fat'))
df_aorta$Id<-as.factor(df_aorta$Id)
df_aorta$Mouse<-as.factor(df_aorta$Mouse)


head(df_aorta)

colnames(df_aorta)


variables<-colnames(df_aorta)

#Id
class_id<-class(df_aorta$Id)
levels_id<-paste(unique(df_aorta$Id)[1],tail(unique(df_aorta$Id),1), sep='to')
type_id<-typeof(df_aorta$Id)

#Mouse
class_m<-class(df_aorta$Mouse)
levels_m<-paste(unique(df_aorta$Mouse)[1],tail(unique(df_aorta$Mouse),1), sep='to')
type_m<-typeof(df_aorta$Mouse)

#Radiation
class_r<-class(df_aorta$Radiation)
levels_r<-toString(unique(df_aorta$Radiation))
type_r<-typeof(df_aorta$Radiation)

#Diet
class_d<-class(df_aorta$Diet)
levels_d<-toString(unique(df_aorta$Diet))
type_d<-typeof(df_aorta$Diet)

#Area
class_a<-class(df_aorta$Area)
rango_a<-paste(min(df_aorta$Area),max(df_aorta$Area),sep='-')
type_a<-typeof(df_aorta$Area)


levels<-c(levels_id,levels_m,levels_r,levels_d,rango_a)
class<-c(class_id,class_m,class_r,class_d,class_a)
type<-c(type_id, type_m, type_r, type_d, type_a)

#summary table with types of variables and ranges/levels
tabla<-rbind(class,type, levels)
colnames(tabla)<-variables
rownames(tabla)<-c('Type of variable', 'Data type','Levels-Range')

tabla<-as.data.frame(tabla)
tabla

summary(df_aorta)


my_colors <- c('deeppink4','darkseagreen') 
my_colors2<-c('turquoise','turquoise2','turquoise3','turquoise4')

ggpairs(df_aorta, #legend=1 # not working after package update
        lower = list(discrete = "count", combo = "dot_no_facet"),
        columns=c(3,4,5),
        #legend=1,
        aes(color= Radiation), 
        #upper='blank',
        title='Data Exploratory Analysis for Variable relationship')+
  scale_color_manual(values=my_colors2)+ #changing default colors
  scale_fill_manual(values=my_colors2) + #changing default colors
  theme(plot.title=element_text(hjust = 0.5))+
  theme(legend.position = "bottom")

  # + labs(caption='Data Exploratory Analysis between pair of relevant variables')



resumen<-df_aorta%>% select(Area, Diet, Radiation) %>% group_by(Diet, Radiation) %>%
  summarise(
    n=length(Area), 
    Mean=round(mean(Area),3),
    SD=round(sd(Area),3),
    SE=round(sd(Area)/sqrt(length(Area)),3),
    Var=round(var(Area),3), 
    Min=round(min(Area),3), 
    Max=round(max(Area),3))

p1 <- ggplot(resumen, 
             aes(x=Radiation, y=Mean, colour=Diet, group=Diet))+
  geom_line(size=1) +
  geom_point(size=3)+
  ylab("Average Aortic Area (um^2)")+
  scale_color_manual(values=my_colors)+ #changing default colors
  scale_fill_manual(values=my_colors) #changing default colors

#resumen
p2 <- ggplot(resumen, 
              aes(x=Diet, y=Mean, colour=Radiation, group=Radiation))+
  geom_line( size=1) +
  geom_point(size=3) +
  ylab("Average Aortic Area (um^2)")+
  scale_color_manual(values=my_colors2)+ #changing default colors
  scale_fill_manual(values=my_colors2) #changing default colors

p1 + p2 + plot_annotation('Profiles Plot', theme=theme(plot.title=element_text(hjust=0.5)) )


resumen

library(lme4)
library(lmerTest) #para obtener los pvalores
m1= lmer(Area ~ Radiation*Diet +(1|Mouse),df_aorta)
anova1<-anova(m1)
anova1
#summary(m1)


#Evaluación de supuestos del modelo 
#install.packages('ggplot2')
library(car)

residuals_m1=rstudent(m1)
#install.packages('qqplotr')
library(qqplotr)
#qqPlot(residuals_m1) #decided to show with ggplot for graph consistency ji

r <- data.frame(res = residuals_m1)
# Normal Q-Q plot of Normal data
gg <- ggplot(data = r, mapping = aes(sample = res)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point()+
  labs(x = "Residual Quantiles", y = "Sample Quantiles")+
  scale_fill_manual(values='darkseagreen')

gg

#qqnorm(residuals_m1)
#qqline(residuals_m1)




shapiro.test(residuals_m1)



ggplot(data=df_aorta, aes(x=fitted.values(m1), y=residuals_m1))+
         geom_point(color='deeppink4')+
  geom_hline(yintercept = 0) + 
  labs(y='Sample Residuals', y='Fitted values')


print(leveneTest(Area~Radiation*Diet, data=df_aorta, center=mean))


random_effects<-ranef(m1)

#shapiro.test(ranef(m1)$Mouse[,1])

r<-data.frame(ran=ranef(m1)$Mouse[,1])

gg <- ggplot(data = r, mapping = aes(sample = ran), legend=1) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point()+
  labs(x = "Random Effect Quantiles", y = "Sample Quantiles")+
  scale_fill_manual(values='darkseagreen')

gg
#qqPlot(ranef(m1)$Mouse[,1])


shapiro.test(ranef(m1)$Mouse$'(Intercept')

## #install.packages('rstatix')
## library(rstatix)
## #ezDesign(df_aorta)
## df<-df_aorta
## m2<-anova_test(data=df,
##                dv=Area,
##                wid = Id,
##                between = c(Radiation, Diet),
##                within= Mouse)
## 

## library(rstatix)
## str(df_aorta)
## df_aorta %>% anova_test(Area ~ Diet*Radiation + Error(Id/Mouse))
## 

outliers<-outlierTest(m1)
outliers

#install.packages('sjPlot')
library(sjPlot)
sjPlot::tab_model(m1, show.df = TRUE, show.est = TRUE, show.se = TRUE, string.ci='CI (95%)',
                  pred.labels = c('0 Gy - Standard', '4 Gy','8 Gy', '12 Gy', 'High-fat', '4 Gy: High-fat', '8 Gy: High-fat', '12 Gy: High-fat'), digits=3, digits.p = 3, digits.re=3)

## fixed_eff<-data.frame(fixef(m1))
## colnames(fixed_eff)<-c('Effect')
## rownames(fixed_eff)<-c('0 Gy - Standard','4 Gy', '8 Gy', '12 Gy', 'High-Fat', '4 Gy - High Fat','8 Gy - High Fat', '12 Gy - High Fat' )
## round(fixed_eff,3)

re<-round(ranef(m1)$Mouse[,1],3)
mouse<-seq(1,40,1)
re_df<-data.frame(Mouse=mouse, Mouse_Effect=re)


re_df

#install.packages('insight')
library(insight)
ran<-get_variance_random(m1, verbose = TRUE)
var<-get_variance_residual(m1, verbose = TRUE)
icc<-ran/(ran+var)
print(icc)

library(emmeans)

#all tukey comparisons
comp1<-emmeans(m1,  pairwise ~ Diet*Radiation)
s<-summary(comp1$contrasts)

tukey<-data.frame(s)

#confidence intervals for means and comparisons
ci_m<-data.frame(confint(comp1)$emmeans) #means
ci_c<-data.frame(confint(comp1)$contrasts) #comparisons

#adding significance column to tukey comparisons
len<-length(tukey$p.value)
i=1
vector_s<-c('NA')
while (i<=len) {
  if (tukey[i,6] <0.05){
    vector_s[i]<-c('S') 
  }
  else{ vector_s[i]<-c('NS') }
  i=i+1
}
tukey$sig<-vector_s

#presenting confidence intervals for comparisons in a dataframe
tabla<-cbind(ci_c$contrast,round(confint(comp1$contrasts)$estimate,3), round(ci_c$lower.CL,3), round(ci_c$upper.CL,3), round(tukey$p.value,3), tukey$sig)
tabla<-as.data.frame(tabla)
colnames(tabla)<-c('Contrasts', ' Mean diff', 'Lower (95% CI)', 'Upper (95% CI)', 'p', 'S')


#presenting confidence intervals for means in a dataframe
tabla2<-data.frame(ci_m$Diet, ci_m$Radiation, round(ci_m$emmean,3),round(ci_m$SE,3), round(ci_m$lower.CL, 3), round(ci_m$upper.CL,3))

colnames(tabla2)<-c('Diet','Radiation','Mean','SE','Lower (95% CI)', 'Upper (95% CI)')

tabla2

#comp


#multimple comparisons table with confidence intervals and S
tabla

#simple effects for Radiation

comp3<-emmeans(m1, pairwise~Diet|Radiation, adjust='Tukey')

#making the table nicer
tabla3<-as.data.frame(comp3$contrasts)
tabla3[,c('estimate','SE','t.ratio','p.value')]<-tabla3%>%select(estimate, SE, t.ratio, p.value)%>%round(3)

#adding a Significance column (S, NS)
len<-length(tabla3$p.value)
i=1
vector_s<-c('NA')
while (i<=len) {
  if (tabla3[i, 'p.value'] <0.05){
    vector_s[i]<-c('S') 
  }
  else{ vector_s[i]<-c('NS') }
  i=i+1
}
tabla3$S<-vector_s
names(tabla3)[names(tabla3) == 'p.value']<-'p'

tabla3





#simple effects for Diet
comp4<-emmeans(m1, pairwise~Radiation|Diet, adjust='Tukey')

#making the table nicer
tabla4<-as.data.frame(comp4$contrasts)
tabla4[,c('estimate','SE','t.ratio','p.value')]<-tabla4%>%select(estimate, SE, t.ratio, p.value)%>%round(3)

#adding a Significance Column (S, NS)
len<-length(tabla4$p.value)
i=1
vector_s<-c('NA')
while (i<=len) {
  if (tabla4[i, 'p.value'] <0.05){
    vector_s[i]<-c('S') 
  }
  else{ vector_s[i]<-c('NS') }
  i=i+1
}
tabla4$S<-vector_s
names(tabla4)[names(tabla4) == 'p.value']<-'p'
tabla4

df_medias<-data.frame(comp1$emmeans)
ggplot(df_medias, 
       aes(x=Radiation,y=emmean, group=Diet, colour=Diet))+
         geom_point(shape=20,size=4)+
        geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=0.2)+
  scale_color_manual(values=my_colors)+ #changing default colors
  scale_fill_manual(values=my_colors)+ #changing default colors
  labs(y='Mean Aortic Area (um^2)', y='Radiation (Gy)')+
  annotate("text",x=c(1,1,2,2,3,3,4,4),y=c(1.9,3.1,2.3,5.4,4,8,4,11.2),label=c("A","A","A","B","A","B","A","B"))
  
#ggplot(df_medias, 
#       aes(x=Diet,y=emmean, group=Radiation, colour=Radiation))+
#         geom_point(shape=20,size=4)+
#        geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=0.2)+
#  scale_color_manual(values=my_colors2)+ #changing default colors
#  scale_fill_manual(values=my_colors2)+ #changing default colors
#  labs(y='Mean Aortic Area (um^2)', y='Radiation (Gy)')#+
  #annotate("text",x=c(1,1,2,2,3,3,4,4),y=c(1.9,3.1,2.3,5.4,4,8,4,11.2),label=c("A","A","A","B","A","B","A","B"))

