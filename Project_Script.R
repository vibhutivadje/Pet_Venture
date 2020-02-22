pets_data <- data.frame(read.csv('catsvdogs.csv'))

#Excluding 'Distric of Columnbia' state becaue of low HH count
pets_data <- pets_data[pets_data$Location != 'District of Columbia',]

catvsdog <- pets_data

library(usmap)
library(ggplot2)
library(dplyr)
library(psych)
library(plotly)

#****************Part (a) Descriptive Statistics*******************
#summary 
summary(pets_data)
describe(pets_data)

#Histogram of proportion of pets, dogs and cats per HH
#Pets
hist(pets_data$Percentage.of.households.with.pets, col = "orange" , xlab= "% of HH with Pets",main = '% of HH with Pets')
#Dogs
hist(pets_data$Percentage.of.Dog.Owners, col = "blue" , xlab= "% of HH",main = '% of HH with Cats vs. Dogs')
hist(pets_data$Percentage.of.Cat.Owners, col = "green" , xlab= "% of HH",main = '% of HH with Cats vs. Dogs',add = TRUE)

#Histogram of mean dogs and cats per HH
#Dogs
hist(pets_data$Mean.Number.of.Dogs.per.household, col = "blue" , xlab= "Avg. per HH",main = 'Avg. # of Cats vs. Dogs per HH')
#Cats
hist(pets_data$Mean.Number.of.Cats, col = "green" , xlab= "Avg. per HH", main = 'Avg. # of Cats vs. Dogs per HH' , add = TRUE)


#Scatter charts
scatter <- plot_ly(data = pets_data, x = ~Percentage.of.Dog.Owners, y = ~Percentage.of.Cat.Owners,
                   marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(252, 0, 0, .8)',width = 2))) %>%
        layout(title = 'Scatter Chart', yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE))
scatter

#between mean of dogs and cats per hh
scatter <- plot_ly(data = pets_data, x = ~Mean.Number.of.Dogs.per.household, y = ~Mean.Number.of.Cats,
                   marker = list(size = 10,color = 'rgba(155, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 2))) %>%
  layout(title = 'Scatter Chart', yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE))
scatter

#Getting top 10 states with the highest % of HH with pets
pets_data['Rank.with.pets'] <- dense_rank(-pets_data$Percentage.of.households.with.pets)
top_states_with_pets <- pets_data[pets_data$Rank.with.pets<=10,]
top_states_with_pets <- top_states_with_pets[c(1,2,4)]
top_states_with_pets

#Getting top 10 states with the least % of HH with pets
pets_data['Rank.with.pets.bottom'] <- dense_rank(pets_data$Percentage.of.households.with.pets)
bottom_states_with_pets <- pets_data[pets_data$Rank.with.pets.bottom<=10,]
bottom_states_with_pets <- bottom_states_with_pets[c(2,4)]
bottom_states_with_pets

#map for top 10 states with least % of HH with pets
names(bottom_states_with_pets) <- c('state','Percentage.of.households.with.pets')
plot_usmap(data = bottom_states_with_pets, values = "Percentage.of.households.with.pets", color = "black",labels = TRUE) + 
  scale_fill_continuous(low = "red",high = "green", name = "% of HH with pets", label = scales::comma) + 
  theme(legend.position = "right")

#Getting top 10 states with the highest % of dogs
pets_data['Rank.with.dogs'] <- dense_rank(-pets_data$Percentage.of.Dog.Owners)
top_states_with_dogs <- pets_data[pets_data$Rank.with.dogs<=10,]
top_states_with_dogs <- top_states_with_dogs[c(1,2,6)]
top_states_with_dogs

#Getting top 10 states with the least % of dogs
pets_data['Rank.with.dogs.bottom'] <- dense_rank(pets_data$Percentage.of.Dog.Owners)
bottom_states_with_dogs <- pets_data[pets_data$Rank.with.dogs.bottom<=10,]
bottom_states_with_dogs <- bottom_states_with_dogs[c(1,2,6)]
bottom_states_with_dogs

#map for top 10 states with least % of HH with dogs
map_data2<- data.frame(bottom_states_with_dogs[c(2,3)])
names(map_data2) <- c('state','Percentage.of.households.with.dogs')
plot_usmap(data = map_data2, values = "Percentage.of.households.with.dogs", color = "grey",labels = TRUE) + 
  scale_fill_continuous(low = "white",high = "green", name = "Percentage of households with dogs", label = scales::comma) + 
  theme(legend.position = "right")

#Getting top 10 states with the highest % of cats
pets_data['Rank.with.cats'] <- dense_rank(-pets_data$Percentage.of.Cat.Owners)
top_states_with_cats <- pets_data[pets_data$Rank.with.cats<=10,]
top_states_with_cats <- top_states_with_cats[c(1,2,10)]
top_states_with_cats

#Getting top 10 states with the least % of cats
pets_data['Rank.with.cats.bottom'] <- dense_rank(pets_data$Percentage.of.Cat.Owners)
bottom_states_with_cats <- pets_data[pets_data$Rank.with.cats.bottom<=10,]
bottom_states_with_cats <- bottom_states_with_cats[c(1,2,10)]
bottom_states_with_cats

#map for top 10 states with the least  % of HH with cats
map_data3<- data.frame(bottom_states_with_cats[c(2,3)])
names(map_data3) <- c('state','Percentage.of.households.with.cats')
plot_usmap(data = map_data3, values = "Percentage.of.households.with.cats", color = "grey",labels = TRUE) + 
  scale_fill_continuous(low = "white",high = "orange", name = "Percentage of households with cats", label = scales::comma) + 
  theme(legend.position = "right")



#***************Sampling Distribution & Hypothesis Testing**************************************

#at overall level
pecentage_of_pets <- sum(pets_data$Number.of.Pet.Households..in.1000.)/sum(pets_data$Number.of.Households..in.1000.)
pecentage_of_dogs <- sum(pets_data$Dog.Owning.Households..1000s.)/sum(pets_data$Number.of.Households..in.1000.)
pecentage_of_cats <- sum(pets_data$Cat.Owning.Households)/sum(pets_data$Number.of.Households..in.1000.)

#Develop 95% confidence interval of proportions-
#1. % of HH with Pets
#2. % of HH with Dogs
#3. % of HH with Cats
#4. Difference in % of HH with Dogs & Cats at overall level (in all the states)
#5. Difference Cats vs. Dogs owning HH's (matched sample)


#1.
p1 <-pecentage_of_pets
n <- 48
zhalfa <- qnorm(0.975,0,1) 
error <- sqrt(p1*(1-p1)/n)*zhalfa
p1 + c(-error, error)

#2.
p2 <-pecentage_of_dogs
n <- 48
zhalfa <- qnorm(0.975,0,1) 
error <- sqrt(p2*(1-p2)/n)*zhalfa
p2 + c(-error, error)

#3.
p3 <- pecentage_of_cats
n <- 48
zhalfa <- qnorm(0.975,0,1) 
error <- sqrt(p3*(1-p3)/n)*zhalfa
p3 + c(-error, error)

#4.
p1 <- pecentage_of_dogs
p2 <- pecentage_of_cats
n1 <- 48
n2 <- 48
zhalfa <- qnorm(0.975,0,1) #calculate zalpha/2 
error <- sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)*zhalfa #sigma * zalpha/2
p1-p2 + c(-error, error)

#5.
s1 <- sd(data1)
n1 <- length(data1)
s2 <- sd(data2)
n2 <- length(data2)
testF <- s2^2/s1^2 #Ensure this number is bigger than 1, else consider the reciprocal
2*(1-pf(testF,n1-1,n2-1))

#Hypothesis Testing
# Ho:  HH with Dogs is equal or less than Cats
# Ha:  HH with Dogs is > than Cats  --- Upper tailed
stdError <- sd(diff)/sqrt(n)
tstat <- mean(diff)/stdError
(1-pt(tstat,n-1)) 

# Ho:  Proportion of HH with Dogs is equal to Cats
# Ha:  Proportion of HH with Dogs is not equal to Dogs  --- 2 tailed test
p1 <- pecentage_of_dogs
p2 <- pecentage_of_cats
n1 <- 48
n2 <- 48
pooledp <- (n1*p1+n2*p2)/(n1+n2)
stdError <- sqrt(pooledp*(1-pooledp)*(1/n1+1/n2))
2*(1-pnorm(p1-p2,0,stdError)) 
#p-value = 0.26, greater than >0.05 therefore, can't reject Null Hypo.

#Ho: Variance of % of HH with Cats vs. Dogs is same
s1 <- sd(pets_data$Percentage.of.Dog.Owners)
n1 <- length(pets_data$Percentage.of.Dog.Owners)
s2 <- sd(pets_data$Percentage.of.Cat.Owners)
n2 <- length(pets_data$Percentage.of.Cat.Owners)
testF <- s1^2/s2^2 
2*(1-pf(testF,n1-1,n2-1))


#************************Mean number of Dogs and Cats per HH*************************************
#Develop 95% confidence interval of mean
#1. Mean number of Dogs per HH
#2. Mean number of Cats per HH
#3. Mean of difference of Avg. number of Dogs & Cats calculated at state level (matched sample case)

#at overall level
mean_of_dogs <- mean(pets_data$Mean.Number.of.Dogs.per.household)
mean_of_cats <- mean(pets_data$Mean.Number.of.Cats)
sd_dogs <- sd(pets_data$Mean.Number.of.Dogs.per.household)
sd_cats <- sd(pets_data$Mean.Number.of.Cats)

#1.
deoff <- 48-1
thalfa <- qt(0.975,df=deoff) #calculate talpha/2 and degree of freedom = n-1
error <- sd_dogs/sqrt(deoff)*thalfa #sample sd * talpha/2
mean_of_dogs + c(-error, error)

#2.
deoff <- 48-1
thalfa <- qt(0.975,df=deoff) #calculate talpha/2 and degree of freedom = n-1
error <- sd_cats/sqrt(deoff)*thalfa #sample sd * talpha/2
mean_of_cats + c(-error, error)

#3.
n <- 48
diff <- catvsdog$Mean.Number.of.Dogs.per.household - catvsdog$Mean.Number.of.Cats
mean(diff)
sd(diff)
degoff <- n-1
thalfa <- qt(0.975,df=degoff) 
error <- sd(diff)/sqrt(n)*thalfa
mean(diff) + c(-error, error)

#Hypothesis Testing
# Ho:  Mean number of Dogs per HH is equal or greater than Cats
# Ha:  Mean number of Dogs per HH is lower than Cats  --- Lower tail test
n <- 48
diff <-  catvsdog$Mean.Number.of.Dogs.per.household - catvsdog$Mean.Number.of.Cats
mean(diff)
sd(diff)
degoff <- n-1
thalfa <- qt(0.975,df=degoff) 
error <- sd(diff)/sqrt(n)*thalfa 
stdError <- sd(diff)/sqrt(n)
tstat <- mean(diff)/stdError
pt(tstat,degoff)

#******************Final Analysis*****************************************88
# Taking Mean of % of Dog Households = 36.51
mean_pecentage_of_dogs <- sum(pets_data$Dog.Owning.Households..1000s.)/sum(pets_data$Number.of.Households..in.1000.)
mean_pecentage_of_dogs

#Creating New Data Frame with Columns of Interest and Calculated Columns
new1 <- data.frame(catvsdog$State,catvsdog$Number.of.Households..in.1000.,catvsdog$Dog.Population..in.1000.,catvsdog$Percentage.of.Dog.Owners,catvsdog$Dog.Population..in.1000./catvsdog$Number.of.Households..in.1000.)

#Ranking Columns based of Number of Households
new1['Rank'] <- dense_rank(-new1$catvsdog.Number.of.Households..in.1000.)
new1 <- new1[new1$catvsdog.Number.of.Households..in.1000. >= 5000 & new1$catvsdog.Number.of.Households..in.1000. <= 15000,]

#Renaming Columns
names(new1) <- c("State", "No of HH", "Dog Population(in 1000)", "% of Dog Population", "Dog_PP/No_of_HH", "Dog_HH/No_of_HH_Rank")
new1
