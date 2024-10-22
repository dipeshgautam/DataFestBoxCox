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

#config <- suppressWarnings(tbl_df(fread("~/DataFestBoxCox/configuration.csv", stringsAsFactors = FALSE)))
lead <- suppressWarnings(tbl_df(fread("~/DataFestBoxCox/leads.csv", stringsAsFactors = FALSE)))
visitor <- suppressWarnings(tbl_df(fread("~/DataFestBoxCox/visitor.csv", stringsAsFactors = FALSE)))
#trans <- suppressWarnings(tbl_df(fread("~/DataFestBoxCox/transactions.csv", stringsAsFactors = FALSE))) 
income <- suppressWarnings(tbl_df(fread("~/DataFestBoxCox/12zpallnoagi.csv", stringsAsFactors = FALSE))) 
states <- suppressWarnings(tbl_df(fread("~/DataFestBoxCox/us_states.csv", stringsAsFactors = FALSE, header=F))) 
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
png('SSE.png')
plot(1:20, ws_mean, type="b", xlab="Number of Clusters", ylab="Sum of Squared Error")
dev.off()

cluster = as.data.frame(kmeans(mydata,centers=15)$cluster)
visitor1=cbind(visitor1,cluster)

visitor1 <- rename(visitor1, replace=c("kmeans(mydata, centers = 15)$cluster" = "cluster"))
vis=visitor1[,c('visitor_key','cluster')]
vis$visitor_key = as.character(vis$visitor_key)

#group=as.data.frame(plyr::count(lead$visitor_key))
#colnames(group) = c("visitor_key", "count")

#for trucks or any bodytype
# 
# lea=lead[,c('visitor_key','bodytype')]
# lea$visitor_key = as.character(lea$visitor_key)
# final1=inner_join(vis,lea)
# 
# trucks=rep(0,15)
# for(i in 1:15)
# {
#   data1=final1[final1$cluster==i,]
#   group=as.data.frame(plyr::count(data1$bodytype))
#   trucks[i]=(group[group$x=='Truck',]$freq)/sum(group[group$x!="",]$freq)
# }
#   
#for msrp

lea=lead[,c('visitor_key','msrp')]
lea$visitor_key = as.character(lea$visitor_key)
final1=inner_join(vis,lea)

fin1=final1[!is.na(final1$msrp),]
n=rep(0,nrow(fin1))
fin1=cbind(fin1,n)
fin1=as.data.frame(fin1)

for(i in 1:nrow(fin1)){
  if(fin1[i,3]>50000)
  {
    fin1[i,4]=1
  }
}
 
msr=rep(0,15)
for(i in 1:15)
{
  data1=fin1[fin1$cluster==i,]
  group=as.data.frame(plyr::count(data1$n))
  msr[i]=((group[group$x=='1',]$freq)/sum(group[group$x!="",]$freq))*100
}
png('msr.png')
plot(msr, type ="b", col ="blue", main ="Percentage of people buying luxury cars", xlab="Cluster", ylab ="%age")
dev.off()
#for session count (to see customer retention)

visi=visitor[,c('visitor_key','session_count')]
visi$visitor_key = as.character(visi$visitor_key)
final1=inner_join(vis,visi)

fin1=final1[!is.na(final1$session_count),]
n=rep(0,nrow(fin1))
fin1=cbind(fin1,n)
fin1=as.data.frame(fin1)

for(i in 1:nrow(fin1)){
  if(fin1[i,3]>5)
  {
    fin1[i,4]=1
  }
}

ses=rep(0,15)
for(i in 1:15)
{
  data1=fin1[fin1$cluster==i,]
  group=as.data.frame(plyr::count(data1$n))
  ses[i]=((group[group$x=='1',]$freq)/sum(group[group$x!="",]$freq))*100
}
png('ses.png')
plot(ses, Main="Percentage of people with more than 5 site visits", xlab="Cluster", ylab="%age")
dev.off()

#total dwell time

visi=visitor[,c('visitor_key','tot_dwell_time')]
visi$visitor_key = as.character(visi$visitor_key)
final1=inner_join(vis,visi)

fin1=final1[!is.na(final1$tot_dwell_time),]
n=rep(0,nrow(fin1))
fin1=cbind(fin1,n)
fin1=as.data.frame(fin1)

for(i in 1:nrow(fin1)){
  if(fin1[i,3]>5000)
  {
    fin1[i,4]=1
  }
}

time=rep(0,15)
for(i in 1:15)
{
  data1=fin1[fin1$cluster==i,]
  group=as.data.frame(plyr::count(data1$n))
  time[i]=((group[group$x=='1',]$freq)/sum(group[group$x!="",]$freq))*100
}

#time per session

visi=visitor[,c('visitor_key','total_dwell_time','session_count')]
visi$visitor_key = as.character(visi$visitor_key)

final1=inner_join(vis,visi)

fin1=final1[!is.na(final1$session_count),]
fin1=fin1[!is.na(fin1$total_dwell_time),]
n=rep(0,nrow(fin1))
fin1=cbind(fin1,n)
fin1=as.data.frame(fin1)

a=rep(0,nrow(fin1))
for(i in 1:nrow(fin1))
{
  a=fin1$total_dwell_time/final1$session_count
}

for(i in 1:nrow(fin1)){
  if(fin1[i,3]>5)
  {
    fin1[i,4]=1
  }
}

ses=rep(0,15)
for(i in 1:15)
{
  data1=fin1[fin1$cluster==i,]
  group=as.data.frame(plyr::count(data1$n))
  ses[i]=((group[group$x=='1',]$freq)/sum(group[group$x!="",]$freq))*100
}
png('all.png')
plot(time, ylim = c(0,70),type="l",col="blue",main="Cluster-wise trends", xlab="Cluster", ylab="%age")
lines(seq(1,15),msr, col = "red")
lines(seq(1,15), ses, col = "green")
legend("top", c("Total time spent", "Luxury Vehicles", "Site visits>=5"), lty=1, col=c("blue", "red", "green"))
dev.off()

png('all.png')
par(mfrow=c(2,2))
plot(msr, type ="b", col ="blue", main ="People buying luxury cars", xlab="Cluster", ylab ="%age")
plot(ses, type="b", col= "blue", main="People with more than 5 site visits", xlab="Cluster", ylab="%age")
plot(time,type="b",col="blue",main="People spending more than 5000s", xlab="Cluster", ylab="%age")
plot(time, ylim = c(0,70),type="l",col="black",main="Cluster-wise trends", xlab="Cluster", ylab="%age")
lines(seq(1,15),msr, col = "red")
lines(seq(1,15), ses, col = "green")
legend("topleft", c("Total time spent", "Luxury vehicles", "Site visits>=5"), bty="n", lty=1, col=c("black", "red", "green"))
dev.off()

all = cbind(seq(1,15),msr,time,ses)

#effect of time on leads

lead$visitor_key = as.character(lead$visitor_key)
group=as.data.frame(plyr::count(lead$visitor_key))
joint=inner_join(visi,group,by=c("visitor_key"="x"))

reg=lm(joint$freq~joint$tot_dwell_time)
summary(reg)

cor(joint$freq,joint$tot_dwell_time)*100

#NOTES:

#Target based ads using clustering

#1)Outside data for customers' purchasing power parity (gas price as price index + income)

#2)K-means clustering based on platform, entry chanel, New/Used..., purchasing power
#(we get 15 as stable # of clusters using graph)

#3)Established that more the ad clicks more leads
# +More dwell time more leads but correlation 5% only (Later we see that in clusters
#where they buy more high-priced cars rget have less dwell time) 

#4)Certain clusters have more high-msrp (>50000) leads so target those ads for them

#5)Awesome trend in session_count (>5)

#6)Negative corrleation between price and session count (20%)

#7)Also saw same trend in total dwell time (98% cor between session count and dwell time)
#25% negative correl with price

#If you find any anamoly like sudden drop in leads of a particular brand in a particular
#cluster then display more of those ads in that cluster






