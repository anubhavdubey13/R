getwd()
setwd("C:/Users/admin/Desktop")
library(dplyr)
library(ggplot2)
suicides<- read.csv("Suicides in India 2001-2012.csv")
str(suicides)
north <- suicides %>% filter(State %in% c("Jammu & Kashmir","Punjab",
                                          "Himachal Pradesh", "Haryana", "Uttarakhand", "Delhi (Ut)"))
north$State <- droplevels(north$State)
north$Year <- as.factor(north$Year)
str(north)

# aggregate(Total~State,data=north,sum)

#Bar plot of State vs Total (considering the duplication in data, filter has been used)
north %>% filter(Type_code == "Causes") %>%
  ggplot(aes(x = State, y = Total)) + geom_col() + coord_flip()

avg_deaths_per_state = sum(north$Total)/30
north %>% ggplot(aes(x = State, y = Total/5)) + geom_col()  +
  geom_hline(yintercept = avg_deaths_per_state) + ylab("Total")
#C: Max = Haryana followed by Delhi, min = J&K (Could be due to popn size)
#Note that the graphs obtained are same

#Table of suicides in every state during 2001-12
deaths_per_state <- north %>% filter(Type_code =="Causes") %>%
  group_by(State) %>% summarise(total_suicides = sum(Total))
prop_suicides <- deaths_per_state$total_suicides/sum(deaths_per_state$total_suicides)
deaths_per_state <- cbind(deaths_per_state, prop_suicides)
deaths_per_state
#C: 43.5% suicides in north happened in Haryana
pie(deaths_per_state$prop_suicides,labels = deaths_per_state$State,
    main = "Pie chart for proportion of suicides across different States")
#add percentage



#Total vs Age faceted by State
north %>% filter(Type_code == "Causes") %>%
  ggplot(aes(x = Age_group, y = Total)) + facet_wrap(~State) +
  geom_col() 
#C: Maximum no. of suicides in the age group 15-29

#Problem : Readability issue due to difference in the total no of deaths across states
#Hence, we can make individual plots of states
#Example : 
north %>% filter(Type_code == "Causes", State == "Himachal Pradesh") %>%
  ggplot(aes(x = Age_group, y = Total)) + scale_y_continuous(limits = c(0,2200)) +
  geom_col() + labs(title = "Himachal Pradesh")  

#State wise table for Age_group vs Total deaths
library(knitr)
library(tidyr)
north %>% filter(Type_code == "Causes") %>% group_by(State, Age_group) %>%
  summarise(total_suicides = sum(Total)) %>% spread(Age_group,total_suicides) %>% kable()



#table only for 15-29 Age_group
tab2_State_vs_age <- north %>% filter(Type_code == "Causes", Age_group == "15-29") %>% 
  group_by(State, Age_group) %>%
  summarise(total_suicides = sum(Total))
tab2_State_vs_age

#Total vs State faceted by Age
north %>% filter(Type_code == "Causes") %>%
  ggplot(aes(x = State, y = Total)) + facet_wrap(~Age_group) + geom_col() +coord_flip()




#--------------------------------ERROR(Dodge)---------------------------
# State vs Total filled by Gender
north %>% filter(Type_code == "Causes") %>%
  ggplot(aes(x = State, y  = Total, fill = Gender)) + 
  geom_col(position = "fill") + coord_flip() 
#C: Male deaths are higher in absolute terms except J&K
#Problem: Look at the scale of x-axis
#----------------------------------------------------------------


#Table of male and female suicides per state
state_suicides <- north %>% filter(Type_code == "Causes") %>%
  group_by(State) %>% summarise(total_suicides = sum(Total)) %>% pull(total_suicides)

tab_state_vs_mgender <- north %>% filter(Type_code == "Causes", Gender == "Male") %>% group_by(State) %>%
  summarise(Male = sum(Total)) 
tab_state_vs_fgender <- north %>% filter(Type_code == "Causes", Gender == "Female") %>% group_by(State) %>%
  summarise(Female = sum(Total)) 
Female <- tab_state_vs_fgender$Female
Male <- tab_state_vs_mgender$Male
M_prop <- Male/state_suicides
F_prop <- Female/state_suicides
tab_state_vs_gender <- cbind(tab_state_vs_mgender, Female, Total=state_suicides,
                             M_prop, F_prop)
tab_state_vs_mgender
tab_state_vs_fgender
tab_state_vs_gender
#C: 76.3% of suicides by Males in Punjab
#C: 54% of suicides by Males in J&K
#Can we apply t-test to test M_prop=0.5 for the given data of different states?



#State vs Type_Code

#1. Causes
#Delhi
north %>% filter(Type_code == "Causes", State == "Delhi (Ut)") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + labs(title = "Delhi")

#Haryana
north %>% filter(Type_code == "Causes", State == "Haryana") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + labs(title = "Haryana")

#Himachal Pradesh
north %>% filter(Type_code == "Causes", State == "Himachal Pradesh") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + labs(title = "Himachal Pradesh")

#Jammu & Kashmir
north %>% filter(Type_code == "Causes", State == "Jammu & Kashmir") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + labs(title = "J & K")

#Punjab
north %>% filter(Type_code == "Causes", State == "Punjab") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + labs(title = "Punjab")

#Uttarakhand
north %>% filter(Type_code == "Causes", State == "Uttarakhand") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + labs(title = "Uttarakhand")

#C: Family problems is the leading cause of suicides among the known causes accross all
# states except Punjab which has *Insanity/Mental Illness* as the major cause
#Merge the columns of Not having children

#2. Education_Status
north %>% filter(Type_code == "Education_Status") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + facet_wrap(~State) +
  labs(title = "Education_Status vs Total accross States")
#Arrange in order


#table:  
north %>% filter(Type_code == "Education_Status") %>% 
  group_by(State, Type) %>%
  summarise(total_suicides = sum(Total)) %>% 
  spread(State,total_suicides) %>%
  kable()

#C: Maximum suicides in Secondary education level(Except J&K and Punjab where no education has 
#the highest no of suicides)  


#3. Means adopted

#Delhi
north %>% filter(Type_code == "Means_adopted", State == "Delhi (Ut)") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + 
  labs(title = "Means_adopted vs Total (Delhi)")
#C: Max: By hanging
#Haryana
north %>% filter(Type_code == "Means_adopted", State == "Haryana") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + 
  labs(title = "Means_adopted vs Total (Haryana)")
#C: Max: By consuming poison (among the known means)
#Himachal Pradesh
north %>% filter(Type_code == "Means_adopted", State == "Himachal Pradesh") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + 
  labs(title = "Means_adopted vs Total (Himachal Pradesh)")
#C: Max: By consuming poison
#Jammu & Kashmir
north %>% filter(Type_code == "Means_adopted", State == "Jammu & Kashmir") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + 
  labs(title = "Means_adopted vs Total (Jammu & Kashmir)")
#C: Max: By consuming poison (among the known means)
#Punjab
north %>% filter(Type_code == "Means_adopted", State == "Punjab") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + 
  labs(title = "Means_adopted vs Total (Punjab)")
#C: Max: By coming under running vehicles
#Uttarakhand
north %>% filter(Type_code == "Means_adopted", State == "Uttarakhand") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + 
  labs(title = "Means_adopted vs Total (Uttarakhand)")
#C: Max: By hanging

#table:  
north %>% filter(Type_code == "Means_adopted") %>% 
  group_by(State, Type) %>%
  summarise(total_suicides = sum(Total)) %>% spread(State,total_suicides) %>%
  kable()



#4. Professional_Profile
north %>% filter(Type_code == "Professional_Profile") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + facet_wrap(~State) +
  labs(title = "Professional_Profile vs Total accross States")
#C: max: House wives 
#proportion of house wives among female suicides (across state) ************************

#Table

#No of suicides in every state
state_suicides <- north %>% filter(Type_code == "Causes") %>%
  group_by(State) %>% summarise(total_suicides = sum(Total)) %>% pull(total_suicides)

tab_State_vs_prof <- north %>% filter(Type_code == "Professional_Profile", Type == "House Wife") %>% 
  group_by(State, Type) %>%
  summarise(total_suicides = sum(Total))
prop_suicides <- (tab_State_vs_prof %>% pull(total_suicides))/state_suicides 
tab_State_vs_prof <- cbind(tab_State_vs_prof, total_state_suicides = state_suicides, 
                           proportion_suicides = prop_suicides)
tab_State_vs_prof
#C: 22.5% of the total suicides are committed by house wives in Uttarakhand 
# Haryana despite having the highest no of suicides of house wives, has lowest proportion of 14.5%


## Housewife suicides as a prop of female suicides grouped by state
state_suicides_female <- north %>% filter(Type_code == "Causes",Gender == "Female") %>%
  group_by(State) %>% summarise(total_suicides = sum(Total)) %>% pull(total_suicides)

tab_fem_vs_prof <- north %>% filter(Type_code == "Professional_Profile", Type == "House Wife") %>% 
  group_by(State, Type) %>%
  summarise(total_suicides = sum(Total))
prop_suicides <- (tab_fem_vs_prof %>% pull(total_suicides))/state_suicides_female 
tab_fem_vs_prof <- cbind(tab_fem_vs_prof, total_fem_suicides = state_suicides_female, 
                           proportion_suicides = prop_suicides)
tab_fem_vs_prof

#surprise in Punjab again @ 62.7% of females who committed suicide were housewives



#5.Social_Status
north %>% filter(Type_code == "Social_Status") %>%
  ggplot(aes(x = Type, y = Total)) + geom_col() + coord_flip() + facet_wrap(~State) +
  labs(title = "Social_Status vs Total accross States")

#C: Max : Married (absolute terms)

