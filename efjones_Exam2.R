rm(list=ls())

# Eric Jones
# Exam 2

#getwd()
#setwd("C:/Users/ericf/OneDrive/Documents/R")


#################################################### Question 1 ###

part1<- read.csv("states.csv")
head(part1)

part2<- read.csv("midwest.csv")
head(part2)

names(part2)[names(part2)=="Colleges"] <- "Universities"
part2$State_Tree <- NULL
part2$Region <- "Unkown"

states<-rbind(part1, part2)
str(states)


#################################################### Question 2 ###

pets_politics <- table(states$Top_Pet, states$Governor)
pets_politics

plot_pets <- table(states$Population, states$Top_Pet)
barplot(plot_pets, main="Pet Preference", xlab="Top Pet",
        ylab="Population (In Millions)", col = "orange")


#################################################### Question 3 ###

rankings <- read.csv("rankings.txt", sep="#")  
head(rankings)

rankings["State_Name"][rankings["State_Name"] == "Washington DC"] <- "District of Columbia"

state_rankings <- merge(states,rankings, by.x = "State", by.y = "State_Name", all.y = TRUE)
head(state_rankings)

#################################################### Question 4 ###

#normal distribution quantile-quantile

qqnorm(state_rankings$Overall_Score)
qqline(state_rankings$Overall_Score, col="blue")

#linear regression
attach(state_rankings)
score_model <- lm(Overall_Score ~Percent_Poverty+Universities+Covid_Rate+Diversity_Score+Beach, data = state_rankings)
summary(score_model)
exp(-0.16127)-1 #the Percent_Poverty decreases by 14 percent when overall score increases by 1
exp(0.81457)-1 #the Universities increases by 125% percent when overall score increases by 1
exp(-0.08056)-1 #the Covid_Rate decreases by 7% percent when overall score increases by 1
exp(0.95366)-1 #the Diversity Score increases by 159% percent when overall score increases by 1
exp(1.58091)-1 #the Beach increases by 386% percent when overall score increases by 1

#pvalue of F-statitics is very low, rejecting the H0: intercept only is better than our model 
#Overall_Score = Beta_0

library(dplyr)

region_group <- group_by(state_rankings, Region, Largest_Employer)
region_group

score_summary <- summarise(region_group,Total_States=n(), Average_Score = mean(Overall_Score))
score_summary

library(reshape2)

score_pivot <- dcast(score_summary, Largest_Employer~Region, value.var = "Average_Score")
score_pivot


#################################################### Question 5

state_rankings$Republican_Binary <- ifelse(state_rankings$Republican_Score>=50,1,0)
state_rankings$Republican_Binary

republican_model <- glm(state_rankings$Republican_Binary~state_rankings$Density+state_rankings$Incarceration_Rate+state_rankings$Governor, 
                        family =binomial(link='logit'))
summary(republican_model)

exp(-0.0046591)-1 #If one unit of the Intercept increases then the chance of Density decreases by .4%

exp(0.0005612)-1 #If one unit of the Intercept increases then the chance of Incarceration_Rate increases by .0005

exp(1.3021014)-1 #If one unit of the Intercept increases then the chance of GovernorRepublican increases by 267%























