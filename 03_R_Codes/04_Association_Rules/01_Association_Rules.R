df = read.csv('F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\AssociationRules\\Groceries.csv')
library(arules)
df$id = as.factor(df$id)

library(reshape2)
df_new = dcast(df,id~product,length)
View(df_new)
write.csv(df_new, 'F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\AssociationRules\\Groceries1.csv')
#remove X,all zeros in the csv file and read the file
rm(df)
### data
str(df_new)
df_new = read.csv('F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\AssociationRules\\Groceries1.csv')


for(i in 1:ncol(df_new)){
  df_new[,i] = as.factor(df_new[,i])
}

df_new$id = NULL

library(arules)

##Converting to Transactions Object
df_trans = as(df_new,'transactions')
inspect(head(df_trans,6))


##creating rules
rules = apriori(df_trans,parameter = list(supp = 0.5, conf = 0.5))
rules
inspect(head(rules,20))

## Subsetting Rules
rules_beer = subset(rules,subset = rhs %pin% "beer")
inspect(rules_beer)
