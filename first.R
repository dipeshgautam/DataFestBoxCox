check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}
check_packages(c("data.table","lubridate","XML","dplyr","bit64","dummies","zipcode","sp","rgdal","maptools","rgeos","raster","stringr","rjson","reshape"))

config <- suppressWarnings(tbl_df(fread("~/DataFest2015/configuration.csv", stringsAsFactors = FALSE)))
lead <- suppressWarnings(tbl_df(fread("~/DataFest2015/leads.csv", stringsAsFactors = FALSE)))
visitor <- suppressWarnings(tbl_df(fread("~/DataFest2015/visitor.csv", stringsAsFactors = FALSE)))
trans <- suppressWarnings(tbl_df(fread("~/DataFest2015/transactions.csv", stringsAsFactors = FALSE))) 
income <- suppressWarnings(tbl_df(fread("~/DataFest2015/12zpallnoagi.csv", stringsAsFactors = FALSE))) 
states <- suppressWarnings(tbl_df(fread("~/DataFest2015/us_states.csv", stringsAsFactors = FALSE, header=F))) 
colnames(states) = c('state1', 'state', 'STATE')
states$STATE1 = NULL



theurl <- "http://www.gasbuddy.com/GB_Price_List.aspx?cntry=USA"
tables <- readHTMLTable(theurl)
prices = tables[2]$'NULL'
prices=prices[-6,c('V1','V2')]
colnames(prices) = c('state', 'price')

prices = inner_join(states, prices)
prices$state1=NULL
prices$price = as.numeric(levels(prices$price))[prices$price]


visitor1 = as.data.frame(visitor[,c('visitor_key',"first_agg_search_type","first_platform_type","new_flag","used_flag","cpo_flag","preprod_flag","zip")])
visitor1 = visitor1[visitor1$first_agg_search_type!="",]
visitor1 = visitor1[visitor1$first_platform_type!="",]
visitor1 = visitor1[visitor1$zip!="",]
visitor1$zip = as.integer(visitor1$zip)
visitor1 = visitor1[visitor1$zip!=0,]
visitor1$visitor_key = as.character(visitor1$visitor_key)

dummy_first_platform = dummy(visitor1$first_platform_type)
visitor1 = cbind(visitor1,dummy_first_platform)
visitor1$first_platform_type=NULL

dummy_first_agg_search_type = dummy(visitor1$first_agg_search_type)
visitor1 = cbind(visitor1,dummy_first_agg_search_type)
visitor1$first_agg_search_type=NULL

income = income[,c("ZIPCODE","A00100",'STATE','N1')]
colnames(income) <- c("zip", "AGI",'STATE','N1')
income$avg_AGI = income$AGI/income$N1

visitor1 = inner_join(visitor1,income)
visitor1 = inner_join(visitor1,prices)#
visitor1$visitor_key = as.character(visitor1$visitor_key)

visitor1$zip = visitor1$N1 = visitor1$AGI  = NULL
visitor1$first_platform_typeNA = NULL
visitor1$first_agg_search_typeNA = NULL

#dummy_Location = dummy(visitor1$Location)
#visitor1 = cbind(visitor1,dummy_Location)
#visitor1$Location=NULL
visitor1$STATE=NULL
visitor1$state=NULL


mydata = scale(visitor1[,-1], center=FALSE)
wss = matrix(0,nrow=20,ncol=10)
for (j in 1:10){
wss[1,j] <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:20) {wss[i,j] <- sum(kmeans(mydata,centers=i)$withinss)}
}
ws_mean = rowMeans(wss)
plot(1:20, ws_mean, type="b", xlab="Number of Clusters", ylab="Within 
groups sum of squares")

cluster = as.data.frame(kmeans(mydata,centers=15)$cluster)
visitor1=cbind(visitor1,cluster)

visitor1 <- rename(visitor1, replace=c("kmeans(mydata, centers = 15)$cluster" = "cluster"))


#group=as.data.frame(plyr::count(lead$visitor_key))
#colnames(group) = c("visitor_key", "count")

trucks=rep(0,15)
for(i in 1:15)
{
  data1=visitor1[visitor1$cluster==i,]
  lea=lead[,c('visitor_key','bodytype')]
  final1=inner_join(data1,lea)
  group=as.data.frame(plyr::count(final1$bodytype))
  trucks[i]=(group[group$x=='HB',]$freq)/sum(group[group$x!="",]$freq)
}
  

#vis=visitor[,c('visitor_key')]
#vis$visitor_key = as.character(vis$visitor_key)
#fin1=inner_join(vis,final1)
#reg1=lm(fin1$count~fin1$clk_total)
#summary(reg1)
#cor1=cor(fin1$count,fin1$clk_total)
#cor1

data1=visitor1[visitor1$cluster==14,]
final1=inner_join(group,data1)
vis=visitor[,c('visitor_key','clk_total')]
vis$visitor_key = as.character(vis$visitor_key)
fin1=inner_join(vis,final1)
reg1=lm(fin1$count~fin1$clk_total)
cor1=cor(fin1$count,fin1$clk_total)
cor1





