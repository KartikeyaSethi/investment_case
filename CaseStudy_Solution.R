
#To load the library for exporting excel data in to data sets
library(XLConnect)
setwd("D:\\Kartikeya\\UpGrad\\Case Study_20171203")

#Load companies data in dfcompanies data set
dfcompanies <- read.delim("companies.txt",header = TRUE)
dfcompanies$permalink<-sapply(dfcompanies$permalink,tolower)


#Load companies funding information in dfRounds data set
dfRounds <- read.csv("rounds2.csv",header=TRUE,stringsAsFactors = FALSE)
dfRounds$company_permalink<-sapply(dfRounds$company_permalink,tolower)

library(dplyr)

#=============Table 1.1

#To calculate distinct companies in the data set rounds2
nrow(distinct(dfRounds,company_permalink))

#To calculate distinct companies in the data set companies
nrow(distinct(dfcompanies,permalink))

#Merge two data frames in one, namely "master_frame"
master_frame <- merge(x=dfcompanies,y=dfRounds,by.x="permalink", by.y="company_permalink", all.y=TRUE)
nrow(master_frame)

#==========Table 2.1


df_average_funding_amount<-aggregate(raised_amount_usd ~ funding_round_type,data = dfRounds,mean)

#=================Average funding amount of venture type
#11748949.1

#==================Average funding amount of angel type
#958694.5

#==================Average funding amount of seed type
#719818

#==================Average funding amount of private equity type
#73308593

#==================which investment type is the most suitable for them
#By above figures we cam say that "venture" funding type is more suitable


#==========================Table 3.1
#Now we narrow down the data frame on the basis of selected funding type "venture"

df_master_frame_venture <- filter(master_frame,funding_round_type=="venture")

#Find out top nine countries which have received the highest total funding 

df_countries_group <- group_by(df_master_frame_venture,country_code)
df_summarise_contries <- summarise(df_countries_group,sum(raised_amount_usd,na.rm = TRUE))
names(df_summarise_contries)[2] <- paste("sum_raised")
top_9 <- arrange(df_summarise_contries,desc(sum_raised))

#China does not have english as its offical language so
#====Top English speaking countries are
#USA
#====Second English speaking country
#UK
#====Third English speaking country
#India


#============================Table 4.1

library(tidyr)

#Extract the primary sector 
df_master_frame_primary_sector <- separate(df_master_frame_venture,category_list,into=c("Primary_Sector"),sep="[|]",remove=FALSE)

#load mapping file
df_mapping<- read.csv("mapping.csv",header = TRUE,na.strings = c("","NA") ,check.names = FALSE)

#convert from wide to long format

df_mapping_wide_to_long<-gather(df_mapping,main_sector,main_sector_val,'Automotive & Sports':'Social, Finance, Analytics, Advertising',na.rm = T)

df_mapping_wide_to_long<-df_mapping_wide_to_long[!(df_mapping_wide_to_long$main_sector_val==0),]
df_mapping_wide_to_long<-df_mapping_wide_to_long[,-3]

df_merged_data<-merge(df_mapping_wide_to_long,df_master_frame_primary_sector,by="category_list")

#===========================================================================================================
#Table 5.1
#===========================================================================================================


#Separate data frames of three selected countries on the observation of selected funding type

D1 <- filter(df_merged_data,country_code == "USA",raised_amount_usd <= 15000000,raised_amount_usd >= 5000000)
D2 <- filter(df_merged_data,country_code == "GBR",raised_amount_usd <= 15000000,raised_amount_usd >= 5000000)
D3 <- filter(df_merged_data,country_code == "IND",raised_amount_usd <= 15000000,raised_amount_usd >= 5000000)

library(sqldf)

#Adding total number of investments and total amount invested in each data frame

D1_Group <- sqldf("select main_sector,count(raised_amount_usd) total_number_investment,sum(raised_amount_usd) total_investment from D1 group by main_sector")
D1 <- merge(D1,D1_Group,by="main_sector",all = TRUE)

D2_Group <- sqldf("select main_sector,count(raised_amount_usd) total_number_investment,sum(raised_amount_usd) total_investment from D2 group by main_sector")
D2 <- merge(D2,D2_Group,by="main_sector",all = TRUE)

D3_Group <- sqldf("select main_sector,count(raised_amount_usd) total_number_investment,sum(raised_amount_usd) total_investment from D3 group by main_sector")
D3 <- merge(D3,D3_Group,by="main_sector",all = TRUE)

#Total number of investment
sum(D1$total_number_investment)
sum(D2$total_number_investment)
sum(D3$total_number_investment)

#Total amount of investment
sum(D1$total_investment)
sum(D2$total_investment)
sum(D3$total_investment)

#After analysis D1_Group,D2_Group & D3_Group 


#Top sector count wise for D1
df_company_investment_first_Secto_D1 <- sqldf("select name,sum(total_investment) Company_investment from D1 where main_sector='Others' group by name order by Company_investment")
#Second best sector wise for D1
df_company_investment_Sec_Sector_D1 <- sqldf("select name,sum(total_investment) Company_investment from D1 where main_sector='Cleantech / Semiconductors' group by name order by Company_investment")

#Top sector count wise for D2
df_company_investment_first_Sector_D2 <- sqldf("select name,sum(total_investment) Company_investment from D2 where main_sector='Cleantech / Semiconductors' group by name order by Company_investment")
#Second best sector wise for D2
df_company_investment_Sec_Sector_D2 <- sqldf("select name,sum(total_investment) Company_investment from D2 where main_sector='Others' group by name order by Company_investment")

#Top sector count wise for D3
df_company_investment_first_Sector_D3 <- sqldf("select name,sum(total_investment) Company_investment from D3 where main_sector='Others' group by name order by Company_investment")
#Second best sector wise for D3
df_company_investment_Sec_Sector_D3 <- sqldf("select name,sum(total_investment) Company_investment from D3 where main_sector='News, Search and Messaging' group by name order by Company_investment")



