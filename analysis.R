rm(list = ls())
require("readr")
require('data.table')
require('ggplot2')


#Set working directory with all the necessary files
WD <- getwd()
if (!is.null(WD)) setwd(WD)

#Read data
dark_data <- read_delim('DreamSensationStudy.tab',  "\t", escape_double = FALSE, trim_ws = TRUE)

#filter data
dark_data<-dark_data[which(dark_data$BLACKNESS %in% c('full','part')),]
dark_data<-dark_data[complete.cases(dark_data),]
colnames(dark_data)[c(1,2)]<-c('USER_ID','REPORT_ID')

#check uniqueness
length(unique(dark_data$USER_ID))
length(unique(dark_data$REPORT_ID))

#read control data
seconds_data<-read_delim('seconds.csv',  "\t", escape_double = FALSE, trim_ws = TRUE)
seconds_data<-seconds_data[complete.cases(seconds_data),]
length(unique(seconds_data$REPORT_ID))
length(unique(seconds_data$USER_ID))


#make another column, uniting seconds calculation approaches in both studies
dd_to_sec<-c()
for (i in c(1:nrow(dark_data)))
{
  if (dark_data$continued_end[i] == 'continued')
  {
    if (as.numeric(as.character(dark_data$BLACKNESS_t[i])) != 4)
    {
      dd_to_sec<-append(dd_to_sec,as.numeric(as.character(dark_data$BLACKNESS_t[i]))+1)
    }
    else
    {
      dd_to_sec<-append(dd_to_sec,as.numeric(as.character(dark_data$BLACKNESS_t[i])))
    }
  }
  else
  {
    dd_to_sec<-append(dd_to_sec,as.numeric(as.character(dark_data$BLACKNESS_t[i])))
  }
}

dark_data<- cbind(dark_data,dd_to_sec)


sec_to_dd<-c()
for (i in c(1:nrow(seconds_data)))
{
  if (as.numeric(as.character(seconds_data$QUANTITY[i])) >=4)
  {
    sec_to_dd<-append(sec_to_dd,4)
  }
  else
  {
    sec_to_dd<-append(sec_to_dd,as.numeric(as.character(seconds_data$QUANTITY[i])))
  }
}
seconds_data<- cbind(seconds_data,sec_to_dd)


#check if both control and experimental data is similar by phase experience
pro<-table(dark_data[,'PHASE'])/sum(table(dark_data[,'PHASE']))
chisq.test(x = table(seconds_data[,'PHASE']), p = pro)
# answer is NO p-value = 1.178e-05

#check if both control and experimental data is similar by gender distribution
pro<-table(dark_data[,'SEX'])/sum(table(dark_data[,'SEX']))
chisq.test(x = table(seconds_data[,'SEX']), p = pro)
# answer is NO p-value = 0.01725

#check if the data is normal by Shapiro-Wilk test
shapiro.test(dd_to_sec)
#normally distributed p-value = 1.572e-08
shapiro.test(sec_to_dd)
#normally distributed p-value = 3.454e-13

#Overall data non-paired comparison by non-paired T-test
t.test(sec_to_dd,dd_to_sec, paired=FALSE)
#p-val 4.46e-10


#Control and experimental data in overall are not comparable 
#due to different gender and phase distribution. However, we 
#still can make a paired analysis, since there are people who 
#passed both experiments


#Merge data by USER ID
full_data<-merge(seconds_data,dark_data, by='USER_ID', all=TRUE)

#Leave only those, who completed both experiments
inter_data<-full_data[complete.cases(full_data),]
# n = 29

#Test normality
shapiro.test(inter_data$dd_to_sec)
#normally distributed  p-value = 0.0009697
shapiro.test(inter_data$sec_to_dd) 
#p-value = 0.0001345

#Apply paired t-test
t.test(inter_data$sec_to_dd,inter_data$dd_to_sec,paired=TRUE)
#p-value = 0.0009068

#Make a result visualization
df<-as.data.frame(melt(inter_data[,c('sec_to_dd','dd_to_sec')]))
plotBP <- ggplot(df, aes(x=variable, y=value )) + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE) +scale_x_discrete(labels=c("Counting seconds",'Counting seconds in blackness'), name=c('Experiment')) +scale_y_continuous(name='Seconds',labels=c('0-10','11-30','31-60','60+'))  + theme_bw()

plotBP

#Building linear models to find dependencies on gender and phase experience
tr<-lm(as.numeric(as.vector(dark_data$dd_to_sec))~dark_data$SEX+as.numeric(as.vector(dark_data$PHASE)))
summary(tr)[['coefficients']]
#p-value for sex and phase experience are 0.108 and 0.506 correspondingly, no dependency in dark_data

tr<-lm(as.numeric(as.vector(seconds_data$sec_to_dd))~seconds_data$SEX+as.numeric(as.vector(seconds_data$PHASE)))
summary(tr)[['coefficients']]
#p-value for sex and phase experience are 0.00369 and 0.75077 correspondingly, there is a slight dependency on gender

