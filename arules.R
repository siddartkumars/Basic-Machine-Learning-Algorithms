load("~/R working directory/Assignments/titanic.raw (1).rdata")
titanic_data <- titanic.raw
View(titanic_data)
titanic_data$Survived <- as.character(titanic_data$Survived)

#Step 1: Descriptive Stats
#1Compute the percentage of people that survived.  
survive <- titanic_data[titanic_data$Survived=="Yes",]
sum_travelers <- nrow(titanic_data)
survivors <- nrow(survive)
sum_travelers
survive_percent <- (survivors/sum_travelers)*100
survive_percent

#2)	Compute the percentage of people that were children

children <- titanic_data[titanic_data$Age=="Child",]
n_children <- nrow(children)
adult_cat <- nrow(titanic_data[titanic_data$Age=="Adult",])
children_percent <- (n_children/sum_travelers)*100
children_percent

#3)Compute the percentage of people that were female

female <-nrow( titanic_data[titanic_data$Sex=="Female",])
male <- nrow( titanic_data[titanic_data$Sex=="Male",])
female_percent <- (female/sum_travelers)*100
female_percent

#4)Finally, compute the percentage of people that were in first class

first_class <- titanic_data[titanic_data$Class=="1st",]
n_firstclass <- nrow(first_class)
first_class_percent <- (n_firstclass/sum_travelers)*100
first_class_percent

##STEP 2 Step 2: More Descriptive Stats
#1)	What percentage of children survived?
View(survive)
n_child <- nrow(survive[survive$Age=="Child",])
child_survive <- (n_child/n_children)*100
child_survive

adult <- nrow(survive[survive$Age=="Adult",])
#2) What percentage of female survived?

n_femal <- nrow(survive[survive$Sex=="Female",])
female_survive <- (n_femal/female)*100
female_survive


#3)	What percentage of first class people survived?
n_firstclasses <- nrow(survive[survive$Class=="1st",])
firstclass_survive <- (n_firstclasses/n_firstclass)*100
firstclass_survive

#4) What percentage of 3rd class people survived?
n_thirdclass <- nrow(survive[survive$Class=="3rd",])
n_thrdclases <- nrow(titanic_data[titanic_data$Class=="3rd",])
thirdclass_survive <- (n_thirdclass/n_thrdclases)*100
thirdclass_survive

##Step 3: Writing a Function

#1)	Write a function that returns the a new dataframe of people that satisfy the specified criteria of sex, age, class and survived as parameters

create_dataframe <- function(Sex,Age,Class,Survived)
{
  return(data.frame(Sex,Age,Class,Survived))
 
}

#2)	Write a function, using the previous function, that calculates the percentage (who lives, who dies) for a specified (parameters) of age, class and sex.

survive$Class <- as.character(survive$Class)
survive$Sex <- as.character(survive$Sex)
survive$Age <- as.character(survive$Age)
titanic_data$Age <- as.factor(titanic_data$Age)
titanic_data$Class <- as.factor(titanic_data$Class)
titanic_data$Sex <- as.factor(titanic_data$Sex)
library(sqldf)
library(dplyr)



operations <- function(y)
{
  
  percent_lives_age <- nrow( survive[survive$Age==y$Age,])
  percent_age <- (percent_lives_age/adult_cat)*100
  
  percent_lives_gen <- nrow( survive[survive$Sex==y$Sex,])
  percent_gen <- (percent_lives_gen/male)*100
  
  percent_lives_class <- nrow( survive[survive$Class==y$Class,])
  percent_class <- ((percent_lives_class)/survivors)*100
  
  xyz <- survive[survive$Age==y$Age,]
  abc <- xyz[xyz$Sex==y$Sex,]
  mno <- nrow(abc[abc$Class==y$Class,])
  
  perc <- (mno/nrow(survive))*100
  return(list("Percentage of survival based on age"= percent_age,
               "Percentage of Survival based on sex"=percent_gen,
               "Percentage of Survival based on class"=percent_class,
               "Percentage based on condtion"=perc
               ))
}

#1)	Use the function to compare age & 3rd class male survival rates


y <- create_dataframe ("Male","Adult","3rd","Yes")
operations(y)
z <- create_dataframe ("Female","Adult","1st","Yes")
operations(z)


#Step 4
library(arules)
rules <- apriori(titanic.raw,parameter = list(minlen=2, supp=0.005, conf=0.8), 
                 appearance = list(rhs=c("Survived=Yes"),
                                   default="lhs"),control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)


library(arulesViz)
plot(rules, method="graph", control=list(type="items"))


#A lift value greater than 1 indicates that X and Y appear more often together than expected; this means that the occurrence of X has a positive effect on the occurrence of Y or that X is positively correlated with Y.


# We select rules with the highest Lift value
#2nd class Female Chile has highest lift value and indicates higher survival value
#1st class female  have higher survival percent
#1st class adult female have higher surivival percent
#2nd class adult male has good survival 



#1)	How does this compare to the descriptive analysis we did on the same dataset? 
# This is different because the rules are association mapping , It often describes when this event occurs and even that event occurs at this percent of times , Then if the same condition prevails it is bound to happen again
#THe arules has two parts, an antecedent (if) and a consequent (then). An antecedent is an item found in the data. A consequent is an item that is found in combination with the antecedent.
#Association rules are created by analyzing data for frequent if/then patterns and using the criteria support and confidence to identify the most important relationships.
#Support is an indication of how frequently the items appear in the database
