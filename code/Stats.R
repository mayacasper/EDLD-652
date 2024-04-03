library('pacman')
p_load(haven, tidyverse, modelsummary, here, ggplot2,dplyr)


medexam<-read.csv('final/MedicalExaminerTableau_2020_05_Migrated Data.csv')



Mal<-medexam %>%
  filter(County=='MALHEUR') %>%
  filter(Time.Period == '2011')

medexam1<- medexam %>%
  select(County, Drug.Type,Time.Measure,Count, Time.Period, Rate)

str(medexam1)
datasummary_skim(medexam1)

Lane<-medexam1 %>%
  filter(County=='LANE')
Lane

Rates<- medexam1 %>%
  select(Rate, County, Time.Period, Drug.Type) %>%
  filter(Drug.Type =='All Drugs') %>%
  filter(County =='OREGON STATEWIDE')


models

datasummary_skim(medexam)


oregonsw<-medexam %>%
  filter(County == "OREGON STATEWIDE") %>%
  filter(Drug.Type =='All Drugs') %>%
  filter(Time.Measure == 'Year') %>%
  filter(Count.rate== 'Rate')

oregonsw
overall <- group_by(oregonsw, Time.Period)
tally (overall)
yearly <-mutate(overall, sum(Count))

ggplot(data=final, aes(x=Time.Period, y=sum(Count)))+
  geom_line()+
  geom_point()
?mutate

overall %>%
  mutate(final = yearly)
yearly <- (overall, sum(Count))


final <-yearly %>%
  filter(Manner=='Suicide')
ggplot()
