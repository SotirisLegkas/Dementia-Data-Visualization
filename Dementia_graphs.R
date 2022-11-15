##############################################################################################
dataset <- read.csv("C:/Users/sotir/Desktop/hlth_cd_asdr2_1_Data.csv", stringsAsFactors=FALSE)
dataset$Value=as.numeric(dataset$Value)
dataset$GEO[dataset$GEO == 'Czechia'] = 'Czech Rep.'
dataset$GEO[dataset$GEO == 'Germany (until 1990 former territory of the FRG)'] = 'Germany'
dataset$GEO[dataset$GEO == 'European Union - 28 countries (2013-2020)'] = 'European Union'

library(dplyr)
library(ggplot2)
library(ggthemes)

##############################################################################################
#Dementia Death Rate in Greece (2011-2019)

df1=subset(dataset,subset= GEO=='Greece' & SEX=='Total'& AGE=='Total')

ggplot(df1,aes(x=factor(TIME),y=Value))+
  theme_stata()+
  xlab('Year')+ylab('Death Rate')+
  ggtitle('Standardised Dementia death rate in Greece (2011-2019)')+
  theme(axis.title = element_text(size=20),
        plot.title=element_text(hjust=0.5,size=20),
        title = element_text(face='bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        plot.background=element_rect(fill='white'))+
  geom_point()+
  labs(caption='Source: Eurostat')+
  scale_y_continuous(breaks=c(0:14))+
  geom_col(aes(label=Value),fill='steelblue4')+
  geom_line(group=1,size=2)+
  geom_text(aes(label=Value),vjust=-1.1,size=5)

##############################################################################################
#Dementia Death Rate in Greece vs European Union per Gender (2011-2019) 

library(ggrepel)

df100=subset(dataset,subset=(GEO=='Greece' | GEO=='European Union') & (SEX=='Males'| SEX=='Females') & AGE=='Total')

ggplot(df100,aes(x=factor(TIME),y=Value))+
  theme_stata()+
  scale_color_stata()+
  scale_fill_stata()+
  theme(axis.title = element_text(size=20),
        strip.text.x=element_text(size=20),
        plot.title=element_text(hjust=0.5,size=20),
        title = element_text(face='bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.text = element_text(size=20),
        plot.background=element_rect(fill='white'),
        legend.title=element_text(size=20,face='bold'))+
  xlab('Year')+ylab('Death Rate')+
  ggtitle('Standardised Dementia death rate per Gender (2011-2019)')+
  scale_y_continuous(breaks=seq(0,40,by=4))+
  geom_col(aes(fill=SEX),width=0.9,position='dodge')+
  labs(caption= "Source: Eurostat",fill="Gender")+
  geom_text(aes(label=Value,x=factor(TIME),group=SEX),size=4,position=position_dodge(width=0.9),vjust=-0.8,hjust=0.45)+
  facet_wrap(~GEO,ncol=2)


##############################################################################################
#Dementia Death Rate in Greece vs European Union per Age Groups (2011-2019) 

df200=subset(dataset,subset=(GEO=='Greece' | GEO=='European Union') & (SEX=='Total') & (AGE=='65 years or over'))
df201=subset(dataset,subset=(GEO=='Greece' | GEO=='European Union') & (SEX=='Total') & ( AGE=='Less than 65 years'))


ggplot(df200,aes(x=factor(TIME),y=Value,color=GEO))+
  theme_stata()+
  scale_color_stata()+
  xlab('Year')+ylab('Death Rate')+
  ggtitle('Standardised Dementia death rate (2011-2019)')+
  theme(axis.title = element_text(size=20),
        plot.title=element_text(hjust=0.5,size=20),
        title = element_text(face='bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        legend.position = c(.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.text = element_text(size=16),
        legend.title=element_text(size=16,face='bold'),
        legend.background = element_rect(fill='NA',color="NA"),
        plot.background=element_rect(fill='white'),
        plot.subtitle = element_text(size=18))+
  geom_point()+
  geom_text(aes(label=Value),vjust=-0.5,size=6,color='black')+
  labs(color="",caption= "Source: Eurostat",subtitle = '65 Years or Over Age Group')+
  scale_y_continuous(breaks=c(seq(0,202,by=8)))+
  geom_line(aes(group=GEO),size=2)

ggplot(df201,aes(x=factor(TIME),y=Value,color=GEO))+
  theme_stata()+
  scale_color_stata()+
  xlab('Year')+ylab('Death Rate')+
  ggtitle('Standardised Dementia death rate (2011-2019)')+
  theme(axis.title = element_text(size=20),
        plot.title=element_text(hjust=0.5,size=20),
        title = element_text(face='bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        legend.position = c(.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.text = element_text(size=16),
        legend.background = element_rect(fill='NA',color="NA"),
        plot.background=element_rect(fill='white'),
        plot.subtitle = element_text(size=18),
        legend.title=element_text(size=16,face='bold'))+
  geom_point()+
  geom_text_repel(aes(label=Value,color='GEO'),size=6,direction = 'y',color='black')+
  labs(caption= "Source: Eurostat",color="",subtitle = 'Less than 65 Years Age Group')+
  scale_y_continuous(breaks=c(seq(0,1,by=0.1)))+
  geom_line(aes(group=GEO),size=2)

##############################################################################################
#Dementia Death Rate in Greece vs European Union (2011-2017)

df4=subset(dataset,subset= TIME<2018 & (GEO=='Greece' | GEO=='European Union') & AGE=='Total' & SEX=='Total')
df41=subset(dataset,subset= TIME<2018 & (GEO=='Greece') & AGE=='Total' & SEX=='Total')
df42=subset(dataset,subset= TIME<2018 & (GEO=='European Union') & AGE=='Total' & SEX=='Total')

ggplot(df4,aes(x=factor(TIME)))+
  theme_stata()+
  scale_color_stata()+
  scale_fill_stata()+
  xlab('Year')+ylab('Death Rate')+
  ggtitle('Standardised Dementia death rate (2011-2017)')+
  theme(axis.title = element_text(size=20),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        legend.position = c(.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.title=element_text(size=18,face='bold'),
        title = element_text(face='bold'),
        legend.background = element_rect(fill='NA',color="NA"),
        plot.background=element_rect(fill='white'),
        legend.text = element_text(size=18),
        plot.title=element_text(hjust=0.5,size=20))+
  scale_y_continuous(breaks=c(seq(0,44,by=3)))+
  labs(fill = "",caption= "Source: Eurostat")+
  geom_col(data=df42,aes(fill=GEO,y=Value),width=0.9)+
  geom_text(data=df42,aes(label=Value,y=Value,x=factor(TIME)),vjust=-0.6,colour='black',size=6)+
  geom_col(data=df41,aes(fill=GEO,y=Value),width=0.9)+
  geom_text(data=df41,aes(label=Value,y=Value,x=factor(TIME)),vjust=-0.6,colour='white',size=6)

##############################################################################################
#Dementia Death Rate (Trellis Plot) (2011-2019)
# https://cran.r-project.org/web/packages/trelliscopejs/vignettes/trelliscopejs.html

library(trelliscopejs)
library(gapminder)

df5=subset(dataset,subset= AGE=='Total' & SEX=='Total' & GEO!='European Union')

str(gapminder)
ggplot(aes(factor(TIME), Value), data = df5) +
  xlab('Year')+ylab('Death Rates')+
  ggtitle('Standardised Dementia death rate (2011-2019)')+
  theme_classic()+
  theme(axis.title = element_text(size=20),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=15),
        plot.background=element_rect(fill='white'),
        title = element_text(face='bold',size=20),
        plot.title=element_text(hjust=0.5))+
  geom_line(group=1,fill='black',size=1)+
  geom_area( fill="steelblue", alpha=0.4,group=1,position='stack')+
  geom_point(color=ifelse(df5$GEO=='Greece','darkred','black'))+
  scale_x_discrete(breaks=c(seq(2012,2019,by=2)))+
  scale_y_continuous(breaks=c(seq(0,75,by=20)))+
  labs(caption= "Source: Eurostat")+
  facet_wrap(~ GEO,ncol=5)

##############################################################################################
#Dementia Death Rate in Countries Similar to Greece (2011 vs 2017)

df9 = subset(dataset, SEX=="Total" & AGE == "Total" & (TIME == 2011 | TIME == 2017) &
                        (GEO == "France" | GEO == "Greece"  | GEO=='Turkey'|
                         GEO == "Spain" | GEO == "Italy" | GEO=='Bulgaria' | GEO=='Serbia'| GEO=='Cyprus'|
                         GEO == "Croatia" | GEO == "Malta" | GEO == "Slovenia" | GEO=='Romania'|GEO=='Portugal'))
df9<-as.data.frame(df9)
df9 = df9[order(df9$GEO),]
df9$paired = rep(1:(26/2),each=2)

ggplot(df9,aes(x= Value, y= reorder(GEO,Value))) +
  theme_stata()+
  ggtitle('Standardised Dementia death rate in Mediterranean-Balkan Countries (2011-2017)')+
  scale_color_stata()+
  geom_line(aes(group = paired),size=1,color='black')+
  geom_point(aes(color=factor(TIME)), size=6) +
  labs(y="Country",colour='Death Rate',caption= "Source: Eurostat")+
  scale_x_continuous(breaks=c(seq(0,200,by=5)))+
  theme(axis.title = element_text(size=20),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        legend.position = c(.1, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.title=element_text(size=18,face='bold'),
        plot.background=element_rect(fill='white'),
        title = element_text(face='bold'),
        legend.text = element_text(size=18),
        plot.title=element_text(hjust=0.5,size=20))

##############################################################################################
#Average Dementia Death Rate (Map)
#https://egallic.fr/en/european-map-using-r/

library(rgeos)
library(data.table)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)

df8=subset(dataset,subset=SEX=='Total'& AGE=='Total')
df81=aggregate(df8$Value, list(factor(df8$GEO)), FUN=mean, na.rm=TRUE)
names(df81)[1]='GEO'
names(df81)[2]='Value'
df81$GEO=factor(df81$GEO,levels=(df81$GEO)[order(df81$Value)])
df82=subset(dataset,subset=SEX=='Total'& AGE=='Total' & TIME==2011)
df83=aggregate(df8$Value, list(factor(df8$GEO)), FUN=max, na.rm=TRUE)
names(df83)[1]='GEO'
names(df83)[2]='Value'

df6=subset(dataset,subset= TIME==2017 & AGE=='Total' & SEX=='Total')

world_map <- ne_countries(scale = 50, returnclass = 'sf')

european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",'Ukraine','Moldova',
                    "Portugal","Romania","Slovakia","Slovenia","Spain",'Turkey','Iceland',
                    "Sweden","United Kingdom","Albania",'Switzerland','Macedonia','Serbia','Bosnia and Herz.','Montenegro','Kosovo','Norway','Belarus')
european_union_map <- world_map %>% filter(name %in% european_union)

bbox_europe <- st_bbox(c(xmin = -25, ymin = 35, xmax = 50, ymax = 70), crs=st_crs(european_union_map))
european_union_map_cropped <- st_crop(european_union_map, bbox_europe)

map <- european_union_map_cropped %>% left_join(df81, by = c("name" = "GEO"))

ggplot(data = map) +
  geom_sf(mapping = aes(fill = Value,colour="")) +
  theme_stata()+
  theme(legend.background = element_rect(fill='NA',color="NA"),
        legend.text = element_text(size=10),
        legend.title=element_text(size=10,face='bold'),
        legend.position='right',plot.title=element_text(hjust=0.5))+
  scale_fill_gradient(name = "Value",na.value = 'white',low = "#56B1F7",high = "#132B43") +
  labs(title = " Average Standardised Dementia Death Rate (2011-2019)",caption= "Source: Eurostat")+
  theme(axis.title = element_text(size=14),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        legend.title=element_text(size=16,face='bold'),
        title = element_text(face='bold'),
        legend.text = element_text(size=16),
        plot.title=element_text(hjust=0.5),
        plot.background=element_rect(fill='white'),
        plot.title.position = "plot")+
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("No data", override.aes=list(fill="white")))

##############################################################################################
#Average Dementia Death Rate (Barplot)

ggplot(df81,aes(x=reorder(factor(GEO),desc(Value)),y=Value))+
  theme_stata()+
  xlab('Country')+ylab('Death Rates')+
  ggtitle('Average Standardised Dementia Death Rate (2011-2019)')+
  labs(caption= "Source: Eurostat")+
  geom_col(data=df81,aes(x=reorder(factor(GEO),Value),y=Value),fill=ifelse(df81$GEO=="Greece" | df81$GEO=="European Union","red3",'steelblue4'))+
  geom_text(aes(label=round(Value,digits=2),x=factor(GEO)),vjust=0.27,hjust=-0.2,size=4.2)+
  coord_flip()+
  theme(axis.title = element_text(size=20),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=16),
        plot.background=element_rect(fill='white'),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=14),
        title = element_text(face='bold'),
        plot.title=element_text(hjust=0.5,size=20))

##############################################################################################
#Dementia Death Rate (Heatmap) (2011-2019)

ggplot(df8,aes(x=factor(TIME),y=factor(GEO),fill=Value))+
  theme_stata()+
  ylim(levels(df81$GEO))+
  xlab('Year')+ylab('Country')+
  ggtitle('Standardised Dementia Death Rate (2011-2019)')+
  geom_tile()+
  labs(caption= "Source: Eurostat")+
  scale_fill_gradient(high = "red3",low = "yellow2",na.value='white')+
  geom_text(data=df81,aes(label=round(Value,digits = 2),y=GEO,x=factor(' AVG')),size=3.8,color=ifelse((df81$GEO=='Greece'|df81$GEO=='European Union'),'darkred','black'))+
  geom_text(data=df82,aes(label=round(Value,digits = 2),y=GEO,x=factor(' MIN')),size=3.8,color=ifelse((df82$GEO=='Greece'|df82$GEO=='European Union'),'darkred','black'))+  
  geom_text(data=df83,aes(label=round(Value,digits = 2),y=GEO,x=factor('MAX')),size=3.8,color=ifelse((df83$GEO=='Greece'|df83$GEO=='European Union'),'darkred','black'))+
  theme(legend.background = element_rect(fill='NA',color="NA"),
        legend.position = "right",
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=12),
        legend.title=element_text(size=16,face='bold'),
        title = element_text(face='bold'),
        legend.text = element_text(size=16),
        plot.title=element_text(hjust=0.5),
        plot.background=element_rect(fill='white'),
        plot.title.position = "plot")

##############################################################################################
#Dementia Death Rate in Countries Similar to Greece (Correlation Plot)

library(GGally)
library(tidyr)

df7=subset(dataset,subset=SEX=='Total'& AGE=='Total' & (( GEO == "Greece" | 
                                                            GEO == "Spain" | GEO == "Italy" | GEO=='Cyprus'|
                                                            GEO == "Croatia" | GEO == "Malta"  | GEO=='Portugal')),select=-c(SEX,AGE,UNIT,ICD10))
df7$TIME=factor(df7$TIME)
long=spread(df7,GEO,Value)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, color="steelblue",fill=NA)
  p
}


ggpairs(long,columns=c(2:8),title="Standardised Dementia Death Rate Country correlation",lower = list(continuous = my_fn),
        upper = list(continuous = wrap("cor", size = 4))) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=10),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        title = element_text(face='bold'),
        plot.title=element_text(hjust=0.5),
        plot.background=element_rect(fill='white'),
        plot.title.position = "plot")+
  labs(caption= "Source: Eurostat")

##############################################################################################
#Percentage Change in Dementia Death Rate in Greece vs European Union (2011-2017)

library(scales)

change=df4 %>%
  group_by(GEO) %>% 
  arrange(TIME, .by_group = TRUE) %>%
  mutate(pct_change = (Value/lag(Value) - 1))

change1=subset(change,subset=GEO=='Greece'& TIME!=2011)
change2=subset(change,subset=GEO=='European Union'& TIME!=2011)
change100=subset(change,subset= TIME!=2011)

ggplot(change100,aes(x=factor(TIME),y=pct_change))+
  theme_stata()+
  xlab('Year')+ylab('Percentage Change in Value')+
  ggtitle('Percentage change in Standardised Dementia death rate (2011-2019)')+
  theme(axis.title = element_text(size=12),
        plot.title=element_text(hjust=0.5),
        title = element_text(face='bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12),
        plot.background=element_rect(fill='white'),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=12))+
  labs(caption='Source: Eurostat')+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format())+
  geom_col(aes(label=pct_change),fill=ifelse(change100$GEO=="Greece" ,"darkred",'steelblue4'))+
  geom_text(aes(label=percent_format()(pct_change)),size=5,hjust=-0.2)+
  facet_wrap(~GEO,ncol = 1)

##############################################################################################
#Percentage Change in Dementia Death Rate (2011 vs 2019)

df40=subset(dataset,subset= (TIME==2011 | TIME==2017) & AGE=='Total' & SEX=='Total' & GEO !='Portugal')
change3=df40 %>%
  group_by(GEO) %>% 
  arrange(TIME, .by_group = TRUE) %>%
  mutate(pct_change = (Value/lag(Value) - 1))

change3=subset(change3,subset=TIME==2017)

ggplot(change3,aes(x=reorder(factor(GEO),desc(pct_change)),y=pct_change))+
  theme_stata()+
  xlab('Country')+ylab('Percentage Change in Value')+
  ggtitle('Percentage change in Standardised Dementia death rate (2011-2017)')+
  labs(caption= "Source: Eurostat")+
  geom_col(data=change3,aes(x=reorder(factor(GEO),pct_change),y=pct_change),fill=ifelse(change3$GEO=="Greece" | change3$GEO=="European Union","red3",'steelblue4'))+
  geom_text(aes(label=percent_format()(pct_change),x=factor(GEO)),vjust=0.27,hjust=-0.2,size=4.2)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format())+
  theme(axis.title = element_text(size=12),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=12),
        plot.background=element_rect(fill='white'),
        title = element_text(face='bold'),
        plot.title=element_text(hjust=0.5))