#clear all objects
rm(list = ls())

#load libraries
library(dplyr)
library(ggplot2)
library(xtable)
library(lazyeval)
#helper function to trim leading and ending spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

summarize_Var<-function(df, var){
        print(var)
        df %>% 
        group_by(var) %>% 
        summarise(count = n())
          }

#get wd
wd<-trim(getwd())
wd<-"C:/Coursera/duke_probability"

#open the Rdata file that was previously unzipped
#load puts the Rdata into a dataframe named brfss2013
load(paste(wd,"/brfss2013.Rdata",sep=""))
names(brfss2013)

#limit the data to MN
brfss2013_MN<-filter(brfss2013, X_state == "Minnesota")
#get only the following 16 columns of interest
col_names=c(
  "physhlth",
  "menthlth",
  "poorhlth",
  "veteran3",
  "marital",
  "children",
  "educa",
  "employ1",
  "X_incomg",
  "sex",
  "X_race",
  "seatbelt",
  "flushot6",
  "flshtmy2",
  "tetanus",
  "pneuvac3"
    )

#pull out the columns of interest
brfss2013_MN2<-brfss2013_MN[, col_names]

#now take a look at the data
#str(brfss2013_MN2)
write.csv(brfss2013_MN2,paste(wd,"/brfss2013_MN2.csv",sep=""))

#add new variables as clean up some of the levels
brfss2013_MN2$gender<-brfss2013_MN2$sex

brfss2013_MN2$tetanusYN[grepl("Yes",brfss2013_MN2$tetanus)]<-"Yes"
brfss2013_MN2$tetanusYN[grepl("No",brfss2013_MN2$tetanus)]<-"No"

brfss2013_MN2$ethnic[grepl("White",brfss2013_MN2$X_race)]<-"White"
brfss2013_MN2$ethnic[grepl("Black",brfss2013_MN2$X_race)]<-"Black"
brfss2013_MN2$ethnic[grepl("Hawaiian",brfss2013_MN2$X_race)]<-"Hawaiian"
brfss2013_MN2$ethnic[grepl("Asian",brfss2013_MN2$X_race)]<-"Asian"
brfss2013_MN2$ethnic[grepl("Other",brfss2013_MN2$X_race)]<-"Other"
brfss2013_MN2$ethnic[grepl("Native",brfss2013_MN2$X_race)]<-"Native"
brfss2013_MN2$ethnic[grepl("Multi",brfss2013_MN2$X_race)]<-"Multi"

#remove verbosity
brfss2013_MN2$ed_level[grepl("Never",brfss2013_MN2$educa)]<-"Never"
brfss2013_MN2$ed_level[grepl("Elementary",brfss2013_MN2$educa)]<-"Elem"
brfss2013_MN2$ed_level[grepl("Some High School",brfss2013_MN2$educa)]<-"Some HS"
brfss2013_MN2$ed_level[grepl("High school graduate",brfss2013_MN2$educa)]<-"HS"
brfss2013_MN2$ed_level[grepl("Other",brfss2013_MN2$educa)]<-"Other"
brfss2013_MN2$ed_level[grepl("Some college",brfss2013_MN2$educa)]<-"Some Colleg"
brfss2013_MN2$ed_level[grepl("College graduate",brfss2013_MN2$educa)]<-"College"

#remove thousands place and verbosity
brfss2013_MN2$income_level<-gsub(",000", "", brfss2013_MN2$X_incomg)
brfss2013_MN2$income_level<-gsub(" to less than ", "-", brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("\\$", "", brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub(" or more", "50+", brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("5055+", "50+", brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("Less than ", "",brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("15-25", "<25",brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("15", "<15",brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("25-35", "<35",brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("35-50", "<50",brfss2013_MN2$income_level)
brfss2013_MN2$income_level<-gsub("5050", "50",brfss2013_MN2$income_level)

#more detail about the selected variables can be found by looking at a previously generated file
df_vars=read.csv("https://raw.githubusercontent.com/wer61537/Duke/master/vars.csv")
df_vars[1:16,1:3]
#the more detail list of variables, description and levels is in the writeup.

#=========================================
#summarize each var
#=========================================
brfss2013_MN2 %>% 
  group_by(physhlth) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = physhlth)) + geom_bar(aes(fill = physhlth))

brfss2013_MN2 %>% 
  group_by(menthlth) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = menthlth)) + geom_bar(aes(fill = menthlth))



brfss2013_MN2 %>% 
  group_by(poorhlth) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = poorhlth)) + geom_bar(aes(fill = poorhlth))


brfss2013_MN2 %>% 
  group_by(veteran3) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


ggplot(data = brfss2013_MN2, mapping = aes(x = veteran3)) + geom_bar(aes(fill = veteran3))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>% 
  group_by(marital) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


ggplot(data = brfss2013_MN2, mapping = aes(x = marital)) + geom_bar(aes(fill = marital))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



brfss2013_MN2 %>% 
  group_by(children) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = children)) + geom_bar(aes(fill = children))

brfss2013_MN2 %>% 
  group_by(ed_level) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = ed_level)) + geom_bar(aes(fill = ed_level))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


brfss2013_MN2 %>% 
  group_by(employ1) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = employ1)) + geom_bar(aes(fill = employ1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>% 
  group_by(income_level) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = income_level)) + geom_bar(aes(fill = income_level))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>% 
  group_by(gender) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = gender)) + geom_bar(aes(fill = gender))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>% 
  group_by(ethnic) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = ethnic)) + geom_bar(aes(fill = ethnic))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>%
  group_by(seatbelt) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = seatbelt)) + geom_bar(aes(fill = seatbelt))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>%
  group_by(flushot6) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = flushot6)) + geom_bar(aes(fill = flushot6))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

brfss2013_MN2 %>%
  group_by(flshtmy2) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = flshtmy2)) + geom_bar(aes(fill = flshtmy2))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


brfss2013_MN2 %>%
  group_by(tetanus) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

t<-brfss2013_MN2 %>%
  group_by(tetanus, ethnic, ed_level) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(t, "c:/temp/ethic_tetanus_ed.csv")


ggplot(data = brfss2013_MN2, mapping = aes(x = tetanus)) + geom_bar(aes(fill = tetanus))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


brfss2013_MN2 %>%
  group_by(pneuvac3) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = brfss2013_MN2, mapping = aes(x = pneuvac3)) + geom_bar(aes(fill = pneuvac3))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#=========================================
#summarize each var
#=========================================

#=========================================
#tetanus immunization by variable
#=========================================

brfss2013_MN2 %>% 
  group_by(ethnic, tetanusYN ) %>% 
  summarise(count = n())

brfss2013_MN2 %>% 
  group_by(ed_level, tetanusYN ) %>% 
  summarise(count = n())

brfss2013_MN2 %>% 
  group_by(income_level, tetanus  ) %>% 
  summarise(count = n())


brfss2013_MN2 %>% 
  group_by(gender, tetanusYN) %>% 
  summarise(count = n())
#=========================================
#tetanus immunization by variable
#=========================================

#=========================================
#seatbelt use  by variable
#=========================================

brfss2013_MN2 %>% 
  group_by(ethnic, seatbelt ) %>% 
  summarise(count = n())

brfss2013_MN2 %>% 
  group_by(ed_level, seatbelt ) %>% 
  summarise(count = n())

brfss2013_MN2 %>% 
  group_by(income_level, seatbelt  ) %>% 
  summarise(count = n())


brfss2013_MN2 %>% 
  group_by(gender, seatbelt) %>% 
  summarise(count = n())

#=========================================
#seatbelt use  by variable
#=========================================


#=========================================
#days of bad mental health use  by variable
#=========================================

brfss2013_MN2 %>% 
  group_by(ethnic, menthlth ) %>% 
  summarise(count = n())

brfss2013_MN2 %>% 
  group_by(ed_level, menthlth ) %>% 
  summarise(count = n())

brfss2013_MN2 %>% 
  group_by(income_level, menthlth  ) %>% 
  summarise(count = n())


brfss2013_MN2 %>% 
  group_by(gender, menthlth) %>% 
  summarise(count = n())

#=========================================
# days of bad mental health use  by variable
#=========================================

#==========================================
# graphs
#==========================================

#==========================================
# tetanus graphs
#==========================================

ggplot(brfss2013_MN2, aes(ethnic, fill=tetanus)) + geom_bar() +
  facet_grid(. ~ ed_level) + theme(text = element_text(size=12),
                                   axis.text.x = element_text(angle=90, vjust=1)) 

ggplot(brfss2013_MN2, aes(ethnic, fill=tetanus)) + geom_bar() +
  facet_grid(. ~ income_level) + theme(text = element_text(size=12),
                                       axis.text.x = element_text(angle=90, vjust=1)) 

ggplot(brfss2013_MN2, aes(ethnic, fill=tetanus)) + geom_bar() +
  facet_grid(. ~ gender) + theme(text = element_text(size=12),
                                 axis.text.x = element_text(angle=90, vjust=1)) 

#==========================================
# tetanus graphs
#==========================================


#==========================================
# seatbelt graphs
#==========================================

ggplot(brfss2013_MN2, aes(ethnic, fill=seatbelt)) + geom_bar() +
  facet_grid(. ~ ed_level) + theme(text = element_text(size=12),
                                 axis.text.x = element_text(angle=90, vjust=1)) 

ggplot(brfss2013_MN2, aes(ethnic, fill=seatbelt)) + geom_bar() +
  facet_grid(. ~ income_level) + theme(text = element_text(size=12),
                                       axis.text.x = element_text(angle=90, vjust=1)) 
ggplot(brfss2013_MN2, aes(ethnic, fill=seatbelt)) + geom_bar() +
    facet_grid(. ~ gender) + theme(text = element_text(size=12),
                                   axis.text.x = element_text(angle=90, vjust=1)) 

#==========================================
# seatbelt graphs
#==========================================


#==========================================
# menthlth graphs
#==========================================
ggplot(brfss2013_MN2, aes(x=ethnic, y=menthlth), na.rm=TRUE) + 
      geom_boxplot(aes(fill=ed_level)) + 
      facet_wrap(~gender)

ggplot(brfss2013_MN2, aes(x=ethnic, y=menthlth)) + 
  geom_boxplot(aes(fill=income_level)) + 
  facet_wrap(~gender)
#==========================================
# menthlth graphs
#==========================================
