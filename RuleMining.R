

# Step 1
load(url('https://sites.google.com/a/rdatamining.com/www/data/titanic.raw.rdata?attredirects=1'))
t <-titanic.raw
total <-nrow(t)

#Compute the percentage of people that survived.
surv <-sum(t$Survived =="Yes")
percsurvived <-((surv/total)*100)
percsurvived

#Compute the percentage of people that were children.
childcount <-sum(t$Age =="Child")
childpct <-((childcount/total)*100)
childpct

#Compute the percentage of people that were female.
femalecount <-sum(t$Sex =="Female")
femalepct <-((femalecount/total)*100)
femalepct

#Finally, compute the percentage of people that were in first class.
fccount <-sum(t$Class =="1st")
fcpct <- ((fccount/total)*100)
fcpct

# Step 2
#What percentage of children survived?
childrenSurvived <- sum(t$Age=="Child"&t$Survived=="Yes")/sum(t$Age=="Child")*100
childrenSurvived

#What percentage of female survived?
femalesurvived <- sum(t$Sex=="Female"&t$Survived=="Yes")/sum(t$Sex=="Female")*100
femalesurvived

#What percentage of first-class people survived?
firstSurvived <- sum(t$Class=="1st"&t$Survived=="Yes")/sum(t$Class=="1st")*100
firstSurvived

#What percentage of third-class people survived?
Thirdsurvived <- sum(t$Class=="3rd"&t$Survived=="Yes")/sum(t$Class=="3rd")*100
Thirdsurvived

# Step 3 writing a function
# fucking no clue...

myFunction <- function(class,sex,age,surv)
{
  i <- 0
  for (i in length(t)){
    if(class=='1st' && sex=='Female' && age=='Adult' && surv=='Yes'){
      df <- data.frame(class,sex,age,surv)
      return(df)
    }
  }
}

#Function calling with different arguments to check the functionality of it
myFunction('1st','Female','Adult','Yes')
myFunction('1st','Male','Child','Yes')
myFunction('3rd','Female','Adult','No')


percentFunction <- function()
{
  num <- length(df)
  return((num/2201)*100)
}

percentFunction()

#step 4 using aRules

library(DT)
library(data.table)
library(arules)

titanic <- as(t, 'transactions')
summary( itemFrequency(titanic) )
# train apriori 
rules <- apriori(titanic,parameter = list(support = 0.05, confidence = 0.7, minlen = 2, maxlen = 5), appearance = list(rhs = c('Survived=No', 'Survived=Yes'), default = 'lhs'),control = list(verbose = FALSE))

# convert object of class rules to data frame
rules_dt <- data.table(lhs = labels(lhs(rules)), rhs = labels(rhs(rules)), quality(rules) )[order(-lift),]
DT::datatable(rules_dt)

# 
