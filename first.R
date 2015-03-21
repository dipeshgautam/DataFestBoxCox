library(dplyr)
library(data.table)
library(bit64)
config <- suppressWarnings(tbl_df(fread("DF2015 Data/configuration.csv", stringsAsFactors = FALSE)))
lead <- suppressWarnings(tbl_df(fread("DF2015 Data/leads.csv", stringsAsFactors = FALSE)))
visitor <- suppressWarnings(tbl_df(fread("DF2015 Data/visitor.csv", stringsAsFactors = FALSE)))
trans <- suppressWarnings(tbl_df(fread("DF2015 Data/transactions.csv", stringsAsFactors = FALSE))) 

visitor$visitor_key=as.character(visitor$visitor_key)
lead$visitor_key=as.character(lead$visitor_key)

z <- inner_join(visitor, lead)




visitor1=as.dataframe(visitor)
leads$visitor_key=as.character(leads$visitor_key)
visitor1$visitor_key=as.character(visitor1$visitor_key)
visitor2=visitor1[1:10000,]
 
group=as.data.frame(plyr::count(leads$visitor_key))
 
final=inner_join(group,visitor2)
final = inner_join(leads,visitor2)
lm(final$freq~final$first_device_model+final$clk_total)
final$first_device_model1 = as.factor(final$first_device_model)
 #final$clk_total1 = as.factor(final$clk_total)
final$first_agg_search_type1 = as.factor(final$first_agg_search_type)
final$make1 = as.factor(tolower(final$make))
final$zip1 = as.factor(final$zip)
tree1 = randomForest(freq~first_device_model1+clk_total+first_agg_search_type1
                 , data = final, importance=T)
tree1 = tree(session_count~first_device_model1+clk_total+first_agg_search_type
          1+msrp, data = final)
 
mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within 
groups sum of squares")
