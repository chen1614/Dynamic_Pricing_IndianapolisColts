################################################################################
# Pull Colts data from RStudio Server
################################################################################
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
#install.packages("odbc")
#install.packages("RMariaDB")
library(RMariaDB)
# Connect to a MariaDB version of a MySQL database
con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
                 , dbname="Colts2020"
                 , user="", password="")
# list of db tables
dbListTables(con)
# query tables
unsold_inv <- dbGetQuery(con, "select * from 2017_2019_Unsold_Inventory")
primary <- dbGetQuery(con, "select * from 2020_Purdue_Project_Primary")

getwd()
setwd("/home/chen1614/ColtsProject")

# We cleaned this dataset using Python, please refer to Python code
secondary <- read.csv('secondary_new.csv')

#Manually added Win rate for both team in Excel 
opp <- read.csv('opp.csv')

dbDisconnect(con)


################################################################################
library(plyr)
library(psych)
library(dplyr)
library(data.table)
library(sqldf)
#install.packages("plotly")
library(plotly)

################################################################################
# Preliminary EDA before data cleaning 
################################################################################
# secondary table 
describe(secondary)

count(secondary$sales_channel)
count(secondary$activity_name)
sapply(secondary, function(x) sum(is.na(x))/dim(secondary)[1]) # percent of missing value

# opponent table 
str(opp)
tapply(opp$ColtsWin, opp$Team, mean) # avg colts win prob vs different teams
tapply(opp$OppWin, opp$Team, mean)   # avg opponent win prob vs different teams

# priamry table
describe(primary)
str(primary)

# plot Top 10 Event in Revenue Ranking from 2012 - 2019
tempdf =aggregate(primary$TotalRevenue, by=list(EventCode=primary$EventCode), FUN=sum) # total revenue across games
tempdf = tempdf[order(tempdf$x,decreasing = TRUE),][10:1,]
rownames(tempdf)=NULL
tempdf$ann = c("$194,771","$196,981","$197,423","$198,011","$212,819","$214,555","$243,595","$258,328","$288,215","$540,260")
tempdf$EventCode <- factor(tempdf$EventCode, levels = tempdf[["EventCode"]])
tempdf$season = primary$Season[match(tempdf$EventCode, primary$EventCode)]
fig3 <- plot_ly(tempdf,y=~EventCode, x=~x, type = 'bar', orientation = 'h', text =~ann,
                textposition = 'inside',
                textfont = list(color = '#ffffff', size = 16, family = 'Arial'),
                marker = list(color = 'rgba(100, 50, 256, 0.6)',
                              line = list(color = 'rgba(100, 50, 256, 1.0)',
                                          width = 3))) %>%
  layout(xaxis = list(title = "Number of Ticket"),
         yaxis = list(title = "Event Name")
  ) %>% add_text(text =~season,
                 textposition = 'outside right',
                 textfont = list(color = '#000000', size = 14, family = 'Arial'))


aggregate(primary$TotalRevenue, by=list(EventCode=primary$EventCode), FUN=sum) # total revenue across games
aggregate(primary$TotalRevenue, by=list(SectionName=primary$SectionName), FUN=sum) # total revenue in different sections


# unsold table
str(unsold_inv)
summary(count(unsold_inv$event_name)) # games has the least, highest unsold tickets
summary(count(unsold_inv$section_name)) # sections has the least, highest unsold tickets


################################################################################
######################         Data Pre-Processing        ######################
################################################################################
# Formatting data in unsold_inv and primary
################################################################################

# Define Target Variable, used for forecasting demand
primary$target <- 1
unsold_inv$target <- 0 


# change column names
colnames(unsold_inv)[colnames(unsold_inv) == 'block_full_price'] <- 'final_price'
colnames(primary)[colnames(primary) == 'PurchasePrice'] <- 'final_price'
colnames(primary)[colnames(primary) == 'EventDate'] <- 'event_date'
colnames(primary)[colnames(primary) == 'EventCode'] <- 'event_name'
colnames(primary)[colnames(primary) == 'SectionName'] <- 'section_name'
colnames(primary)[colnames(primary) == 'RowName'] <- 'row_name'
colnames(primary)[colnames(primary) == 'FirstSeat'] <- 'first_seat'
colnames(unsold_inv)[colnames(unsold_inv) == 'seat_num'] <- 'first_seat'
colnames(primary)[colnames(primary) == 'LastSeat'] <- 'last_seat'
colnames(primary)[colnames(primary) == 'QtySeat'] <- 'num_seats'
colnames(primary)[colnames(primary) == 'EventDesc'] <- 'team'
colnames(primary)[colnames(primary) == 'Season'] <- 'season'
colnames(primary)[colnames(primary) == 'SaleDate'] <- 'sale_date'

# convert data type of primary table
primary$sale_date <- as.Date(primary$sale_date,format='%Y-%m-%d h:m:s')
primary$event_date <- as.Date(primary$event_date,format='%Y-%m-%d h:m:s')



# change 'EventDesc' column to disply only opponent team name --> Primary Key for merging
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in (1:nrow(primary)))  {
  a <- as.character(primary[i,'team'])
  if (substrRight(a,5) == 'Colts') {
    sub <- substr(a,1,nchar(a)-10)
    primary[i,'team'] <- sub
  } else if (substr(a,1,9) == 'Colts vs.') {
    sub <- substr(a,11,nchar(a))
    primary[i,'team'] <- sub
  } else if(substr(a,1,9) == 'Colts vs ') {
    sub <- substr(a, 10,nchar(a))
    primary[i,'team'] <- sub
  }
  b <- as.character(primary[i,'team'])
  if (substrRight(b,8) == 'Indianap') {
    primary[i,'team'] <- substr(b,1,nchar(b)-13)
  }
  c <- as.character(primary[i,'team'])
  if (substrRight(c,7) == 'Indianp') {
    primary[i,'team'] <-substr(c,1,nchar(c)-12)
  }
}


# unify team name in priamry table as in opp table. After the previous step, some 
# team names are still not correctly showing. So we wrote another loop to remedy 
# this issue 
for (i in (1:nrow(primary))) {
  team <- primary[i,'team']
  event_date = primary[i,'event_date']
  temp <- ''
  
  if (team == "Carolin") {
    team = "Carolina"
  }
  if (team == "Chief") {
    team = "Chiefs"
  }
  for (j in (1:nrow(opp))) {
    if (team == opp[j,'Opponent'] | team == opp[j,'Team']) {
      temp <- opp[j,'VisitingTeam']
      
      break
    }
    else {
      temp <- team
    }
  }
  primary[i,'team'] <- as.character(temp)
}
primary$team <- as.factor(primary$team)

rm(a,b,c,i,j,sub,team,temp,substrRight)

# EDA for Primary table
# plot line chart of average ticket price changed through 30 days before game from 2012 to 2019
b = primary
b$day_in_advance = difftime(as.character(primary$event_date),as.character(primary$sale_date),units = "days")
a = b %>% group_by(day_in_advance, season) %>% summarise(avg = mean(final_price, na.rm=T))
a = as.data.frame(a)
a$day_in_advance = as.numeric(a$day_in_advance)
a$season = as.factor(a$season)
a$avg = as.numeric(a$avg)
a = a[which(a$day_in_advance<= 30),]
colnames(a)[colnames(a) == 'avg'] <- 'Average Ticket Price(USD)'
colnames(a)[colnames(a) == 'day_in_advance'] <- 'Days Until Game'
str(a)
a = a[4:384,]
library(ggplot2)
a %>%
  ggplot( aes(x=`Days Until Game`, y=`Average Ticket Price(USD)`, group=season, color=season)) +
  geom_line()




################################################################################
# data clean for pre-processed secondary table
################################################################################

#Eliminate prices that are equal to 0
secondary <- secondary[secondary$final_price != 0,]
secondary <- secondary[secondary$orig_purchase_price != 0,]

# exclude record with orig_purchase_price and final_price difference greater than 1000
secondary <- secondary[secondary$final_price-secondary$orig_purchase_price<=1000,]

# drop insignificant columns and Playoff records 
secondary <- secondary[!secondary$event_name %in% c("13PO","14PO"),]
secondary$X <- NULL

# change column name, data type of secondary table
colnames(secondary)[colnames(secondary) == 'season_year'] <- 'season'
colnames(secondary)[colnames(secondary) == 'add_datetime'] <- 'sale_date'
secondary$event_name = as.character(secondary$event_name)
secondary$sale_date = as.Date(secondary$sale_date,format='%m/%d/%Y')
secondary$event_date = as.Date(secondary$event_date,format='%m/%d/%Y')
secondary$seat_num = as.numeric(secondary$seat_num)
secondary$season = as.factor(secondary$season)
secondary$target = as.factor(secondary$target)

#Event date in this table is delayed by 1 day, need to add 1 day to each row. 
#The date that are labeled as 8/1 and 8/2 does not match the date of its event name, so we changed them to the right date. 
secondary[,'event_date'] = secondary[,'event_date'] + 1 
secondary[secondary$event_date == '2019-08-01', ] = secondary[secondary$event_date == '2019-08-17', ]
secondary[secondary$event_date == '2019-08-02', ] = secondary[secondary$event_date == '2019-08-24', ]


table(secondary$event_date)
table(primary$event_date)

################################################################################
# Concatenate individual records of primary,secondary, and unsold_inv 
################################################################################

# remove insignificant columns
primary$index <- NULL
primary$TicketingAccountId <- NULL
primary$TotalRevenue <- NULL
unsold_inv$`Time of Game` <- NULL
unsold_inv$class_name <- NULL
unsold_inv$`Seat Type` <- NULL
unsold_inv$RowClass <- NULL
unsold_inv$price_code <- NULL
unsold_inv$`Price Code` <- NULL
unsold_inv$index <- NULL
str(unsold_inv)

# prepare for concatenate 
unsold_inv$sale_date <- NA
unsold_inv$event_date <- as.Date(unsold_inv$event_date)
unsold_inv$season <- format(as.Date(unsold_inv$event_date, format="%d/%m/%Y"),"%Y")

# price of unsold tickets are block price, need to divide the price by number of seats to obtain single price.
unsold_inv$final_price = unsold_inv$final_price / unsold_inv$num_seats

# concatenate Unsold_Inv and Primary so all the primary tickets are in one dataframe now
unsold_inv <- unsold_inv[c('sale_date','event_name','team','section_name','row_name','first_seat',
                           'last_seat','final_price','num_seats','event_date','season','target')]
primary_unsold <- rbind(primary,unsold_inv)
primary_unsold <- primary_unsold[c('event_name','team','event_date','num_seats','section_name',
                                   'row_name','first_seat','last_seat','final_price','sale_date',
                                   'season','target')]

# There is an additional space in Oakland Raiders, use gsub function to delete that extra space. 
primary_unsold$team = gsub('Oakland  Raiders', 'Oakland Raiders', primary_unsold$team)

# convert data type of primary_unsold table
primary_unsold$num_seats = as.numeric(primary_unsold$num_seats)
primary_unsold$section_name = as.factor(primary_unsold$section_name)
primary_unsold$first_seat = as.character(primary_unsold$first_seat)
primary_unsold$last_seat = as.character(primary_unsold$last_seat)
primary_unsold$final_price = as.numeric(primary_unsold$final_price)
primary_unsold$season = as.factor(primary_unsold$season)
primary_unsold$target = as.factor(primary_unsold$target)
primary_unsold$team = as.factor(primary_unsold$team)

# expand each transaction to display individual ticket information
primary_unsold <- setDT(primary_unsold)[ , list(event_name = event_name, team = team, event_date = event_date,
                                                num_seats = num_seats, section_name = section_name, row_name = row_name, 
                                                seat_num = seq(first_seat, last_seat, by = 1), final_price = final_price, 
                                                sale_date = sale_date, season = season, target = target), 
                                         by = 1:nrow(primary_unsold)]


# drop insignificant columns of Primary_Unsold
primary_unsold$num_seats <- NULL
primary_unsold$nrow <- NULL


# Primary Unsold EDA 
# plot Annual Primary Ticket Revenue from 2012 - 2019
str(primary_unsold)
primary_unsold$season = as.factor(primary_unsold$season)
tempdf = primary_unsold[primary_unsold$target==1,]
tempdf <- aggregate(tempdf$final_price, by=list(Season=tempdf$season), FUN=sum)
fig1 <- plot_ly(x = tempdf$season, y = tempdf$x,
                name = 'Primary Ticket Revenue', type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(205, 12, 24)', width = 4)
) %>% layout(title = "Annual Primary Ticket Revenue From Season 2012 - 2019",
             xaxis = list(title = "Season(year)",
                          categoryorder = "category ascending"),
             yaxis = list(title = "Annual Primary Ticket Revenue($)")
)
fig1


# plot Annual Ticket Sold and Unsold from 2012 - 2019
tempdf = as.data.frame(primary_unsold[primary_unsold$target==1,] %>% group_by(season) %>% tally())
tempdf = cbind(tempdf,c(NA,NA,NA,NA,NA,data.frame(primary_unsold[primary_unsold$target==0,] %>% group_by(season) %>% tally())[,"n"]))
tempdf$unsold = tempdf[,3]
tempdf[,3] = NULL
fig2 <- plot_ly(y=tempdf$season, x=tempdf$n, type = 'bar', orientation = 'h', name = 'sold',
                marker = list(color = 'rgba(246, 78, 139, 0.6)',
                              line = list(color = 'rgba(246, 78, 139, 1.0)',
                                          width = 3))) %>% add_trace(x = tempdf$unsold, name = 'unsold',
                                                                     marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                                                                   line = list(color = 'rgba(58, 71, 80, 1.0)',
                                                                                               width = 3))) %>% layout(barmode = 'stack',
                                                                                                                       xaxis = list(title = ""),
                                                                                                                       yaxis = list(title ="")
                                                                                               ) %>% layout(xaxis = list(title = "Number of Ticket"),
                                                                                                            yaxis = list(title = "Season(year)")
                                                                                               )
fig2



################################################################################
# Concatenate individual records of secondary and primary_unsold
################################################################################

# prepare for concatenate
primary_unsold$sales_channel = factor("colts.com")

# solve issue of duplicate CLT1214 eventname that existed in both 2014 and 2017, this will 
# create problem because the opponent team name might not be the correct ones. 
secondary_1214_14s <- secondary[(secondary$event_name == 'CLT1214' & secondary$season == 2014),]
secondary_1214_17s <- secondary[(secondary$event_name == 'CLT1214' & secondary$season == 2017),]
secondary_1214_17s$team <- 'Denver Broncos'
secondary_1214_14s$team <- 'Houston Texans'
secondary <- secondary[(secondary$event_name != 'CLT1214'),]
secondary$team = primary_unsold$team[match(secondary$event_name, primary_unsold$event_name)]
secondary <- rbind(secondary,secondary_1214_17s,secondary_1214_14s)

#Prepare to merge Secondary to Primary_Unsold. 
primary_unsold$orig_purchase_price = primary_unsold$final_price     # Need to differentiate it from Secondary data
primary_unsold = as.data.frame(primary_unsold)
primary_unsold = primary_unsold[c('target','event_name','section_name','row_name','seat_num',
                                  'team','season','event_date','sale_date','final_price',
                                  'orig_purchase_price','sales_channel')]


secondary = secondary[c('target','event_name','section_name','row_name','seat_num',
                        'team','season','event_date','sale_date','final_price',
                        'orig_purchase_price','sales_channel')]

# concatenate and name the table as "all"
all <- rbind(primary_unsold,secondary)

# drop duplicated record
all = all %>% distinct(event_name,season,section_name,row_name,seat_num,.keep_all = TRUE)
all$team <- as.factor(all$team)



################################################################################
# merge Opponent Table to all
################################################################################

# drop columns not needed
opp$index = NULL
opp$Opponent = NULL
opp$Team = NULL
opp$Division = NULL
opp$Top25Jersey.Ordinal. = NULL
opp$Conc = NULL

# convert data type
opp$Season = as.factor(opp$Season)
opp$FacebookFans = as.numeric(opp$FacebookFans)
opp$Distance = as.numeric(opp$Distance)
opp$Home.Opener = as.factor(opp$Home.Opener)
opp$LastVisitYears = as.numeric(opp$LastVisitYears)
opp$OppScoredLY = as.numeric(opp$OppScoredLY)
opp$OppDefGivenLY = as.numeric(opp$OppDefGivenLY)
opp$OppPlayoff.Prev.Bin. = as.factor(opp$OppPlayoff.Prev.Bin.)
opp$GAindyL10 = as.numeric(opp$GAindyL10)
opp$Rain.Snow = as.factor(opp$Rain.Snow)

# rename columns
colnames(opp)[colnames(opp) == 'VisitingTeam'] <- 'team'
colnames(opp)[colnames(opp) == 'Season'] <- 'season'
colnames(opp)[colnames(opp) == 'OppWin'] <- 'opp_win'
colnames(opp)[colnames(opp) == 'OppLSWin'] <- 'opp_LS_win'
colnames(opp)[colnames(opp) == 'ColtsWin'] <- 'Colts_win'
colnames(opp)[colnames(opp) == 'ColtsLSWin'] <- 'Colts_LS_win'
colnames(opp)[colnames(opp) == 'RoadAttendance'] <- 'road_attendance'
colnames(opp)[colnames(opp) == 'FacebookFans'] <- 'FB_fans'
colnames(opp)[colnames(opp) == 'Distance'] <- 'distance'
colnames(opp)[colnames(opp) == 'Temp.at.Kick'] <- 'temp_at_kick'
colnames(opp)[colnames(opp) == 'Rain.Snow'] <- 'rain_snow'
colnames(opp)[colnames(opp) == 'Colts.Out.of.Contention'] <- 'Colts_contention'
colnames(opp)[colnames(opp) == 'LastVisitYears'] <- 'last_visit_yrs'
colnames(opp)[colnames(opp) == 'OppScoredLY'] <- 'opp_scoredLY'
colnames(opp)[colnames(opp) == 'OppDefGivenLY'] <- 'opp_def_givenLY'
colnames(opp)[colnames(opp) == 'OppPlayoff.Prev.Bin.'] <- 'opp_playoff'
colnames(opp)[colnames(opp) == 'OffMVP.Interval.'] <- 'off_MVP'
colnames(opp)[colnames(opp) == 'DefMVP.Interval.'] <- 'def_MVP'
colnames(opp)[colnames(opp) == 'OddsFeb'] <- 'odds_f'
colnames(opp)[colnames(opp) == 'GAindyL10'] <- 'GA_indy_L10'
colnames(opp)[colnames(opp) == 'Home.Opener'] <- 'home_opener'


# This team in Opp table lacks a space, need to be consistent with the "all" table 
opp$team = gsub('St.Louis Rams', 'St. Louis Rams', opp$team)



#Merge opp to all to include team information for each ticket 
all_ticket = sqldf("select * from [all] left  outer join [opp] 
                    on [all].team = [opp].team AND
                   [all].season = [opp].season")

# drop insignificant columns
all_ticket$X = NULL 
all_ticket$season..14 = NULL 
all_ticket$team..15 = NULL



################################################################################
# Final Data Cleaning
################################################################################

library(tidyr)

# Remove preseason records. Opponent for preseason games doesn't show up in Opp, 
# so those games will have null values in team information
df <- all_ticket %>% drop_na(opp_win)


# Change data type
df$home_opener = as.factor(df$home_opener)
df$opp_playoff = as.factor(df$opp_playoff)
df$rain_snow = as.factor(df$rain_snow)
df$Colts_contention = as.factor(df$Colts_contention)
df$rain_snow = as.factor(df$rain_snow)
df$profit = df$final_price - df$orig_purchase_price
df$season = as.factor(df$season)
df$seat_num = as.factor(df$seat_num)
df$event_name = as.factor(df$event_name)
df$row_name = as.factor(df$row_name)
df$team = as.factor(df$team)

# Create new feature that shows how many days prior to the game the person bought the ticket
df$day_in_advance = difftime(as.character(df$event_date),as.character(df$sale_date),units = "days")
df$day_in_advance = as.numeric(df$day_in_advance)





#################################IMPUTE MISSING VALUES##########################################
# there are missing values for unsold tickets in the day_in_advance column, We try impute it with mice 
# This might take a long time to finish
library(mice)

#plot the missing values to check if that is the only column that has missing value 
library(VIM)
library(lattice)
df_miss = aggr(df, col=mdc(1:33), numbers=TRUE, sortVars=TRUE, 
               labels=names(df), cex.axis=.7, gap=3, 
               ylab=c("Proportion of missingness","Missingness Pattern"))


# Start impute using Mice, cart method is using classification/regression tree to impute 
tempData <- mice(data = df,m = 5, maxit=5,meth='cart',seed=123)

tempData$imp$day_in_advance

df = complete(tempData,1)

# Check distribution of imputed value and original value 
densityplot(tempData, main = "Density Plot of Imputed Values")


# There are records showing sale date after the event date, drop those records
df = df[-which(df$day_in_advance<0),]

rm(secondary_1214_14s,secondary_1214_17s,a,b,c,event_date,i,j,sub,team,temp,con,all,all_ticket,primary_unsold)



#getwd()
#setwd("/home/chen1614/ColtsProject")

#write to csv 
df = read.csv('df.csv')

# Need to reformat the data type again after impute
df$home_opener = as.factor(df$home_opener)
df$opp_playoff = as.factor(df$opp_playoff)
df$rain_snow = as.factor(df$rain_snow)
df$Colts_contention = as.factor(df$Colts_contention)
df$rain_snow = as.factor(df$rain_snow)
df$profit = df$final_price - df$orig_purchase_price
df$season = as.factor(df$season)
df$seat_num = as.factor(df$seat_num)
df$event_name = as.factor(df$event_name)
df$row_name = as.factor(df$row_name)
df$team = as.factor(df$team)

#Make a copy 
df1 <- df
df1$event_date <- as.Date(df1$event_date)
df1$wday <- weekdays(df1$event_date)
df1$wday <- as.factor(df1$wday)
df1$FB_fans = as.numeric(df1$FB_fans)
df1$distance = as.numeric(df1$distance)
df1$temp_at_kick = as.numeric(df1$temp_at_kick)
df1$last_visit_yrs = as.numeric(df1$last_visit_yrs)
df1$opp_def_givenLY = as.numeric(df1$last_visit_yrs)
df1$opp_scoredLY = as.numeric(df1$opp_scoredLY)
df1$def_MVP = as.numeric(df1$def_MVP)
df1$odds_f = as.numeric(df1$odds_f)
df1$GA_indy_L10 = as.numeric(df1$GA_indy_L10)
df1$event_date <- NULL
df1$sale_date <- NULL
df1$season <- NULL
df1$sales_channel <- NULL
df1$X <- NULL
df1$season..14 <- NULL
df1$team..15 <- NULL
df1$team <- NULL
df1$orig_purchase_price <- NULL
df1$target <- as.factor(df1$target)
str(df1)


################################################################################
######################         Model Training             ######################
################################################################################
#Use H2O environment to train predictive model 
library(h2o)
h2o.init()
h2o.clusterInfo()

# Select a game and pulled its record out from the main data frame 
tr <- df1[df1$event_name != 'CLT19DEN', ]
te <- df1[df1$event_name == 'CLT19DEN', ]

# Remove event name because opponent team name and season indicate the same thing. 
tr$event_name <- NULL
te$event_name <- NULL
df1$event_name <- NULL

# Using Random partition to observe the prediction accuracy. Need to convert 
# dataframe to H2O format
data <- as.h2o(tr)
y <- "target"
x <- setdiff(names(data),y)

# Do a 80% - 20% split
parts <- h2o.splitFrame(data, 0.8, seed=99)
test <- parts[[1]]
train <- parts[[2]]



#Model Training

#Gradient Boosting 
gbm <- h2o.gbm(x,y,train, nfolds = 3, seed = 999)
gbm
h2o.performance(gbm,test)

# XGBoosting 
xgb <- h2o.xgboost(x,y,train,nfolds = 3, seed = 999)
xgb
h2o.performance(xgb,test)

# Logistic Regression 
logit = h2o.glm(x,y,train,family = 'binomial', nfolds = 3)
logit
h2o.performance(logit,test)

# Random Forest
rf <- h2o.randomForest(x, y, train, nfolds = 3, seed = 999)
rf
h2o.performance(rf,test)

rm(gbm,xgb,logit,rf,test,train,data,x,y)



# Using specific game as testing set. tr indicate the training the data
# without that specific game. te indicate the records for that particular
# game 
data <- as.h2o(tr)
y <- "target"
x <- setdiff(names(data),y)

train <- as.h2o(tr)
test <- as.h2o(te)

#Model Training
gbm <- h2o.gbm(x,y,train, seed = 999, nfolds = 3)
gbm
h2o.performance(gbm,test)

xgb <- h2o.xgboost(x,y,train,nfolds = 3, seed = 999)
xgb
h2o.performance(xgb,test)

logit = h2o.glm(x,y,train,family = 'binomial', nfolds = 3)
logit
h2o.performance(logit,test)

rf <- h2o.randomForest(x, y, train, nfolds = 3, seed = 999)
rf
h2o.performance(rf,test)


#Saving the best model 
rf_model = h2o.saveModel(object = rf,path=getwd())
rf_load = h2o.loadModel(rf_model)
rf_load


################################################################################
######################        Optimization                ######################
################################################################################

#We want to choose a specific game to optimize 
demo <- df[which(df$event_name == 'CLT19DEN',df$sales_channel == 'colts.com'),]
str(demo)
demo$event_date <- as.Date(demo$event_date)
demo$wday <- weekdays(demo$event_date)
demo$wday <- as.factor(demo$wday)

demo$event_date <- NULL
demo$sale_date <- NULL
demo$season <- NULL
demo$sales_channel <- NULL
demo$X <- NULL
demo$season..14 <- NULL
demo$team..15 <- NULL
demo$team <- NULL
demo$orig_purchase_price <- NULL
demo$target <- NA
demo$day_in_advance <- 30
demo$final_price <- 0


#Optimize Function. We test each price level and calculate its revenue (price * Demand Prob.) until the
# the current revenue and previous revenue's difference is less than 0.4 (threshold assumption) 
GetOp <- function(r,df) {
  testing <- df[r,]
  testing$target <- NULL
  rev <- 0
  rev_pre <- 0
  diff <- 0
  a = 0 # The predicted probability of demand 
  p = 0 # Price 
  while(diff >= 0) {
    testing[,'final_price'] <- testing[,'final_price'] + 1
    testing <- as.h2o(testing)
    pred <-as.data.frame(h2o.predict(rf,testing))
    testing <- as.data.frame(testing)
    a <- pred$p1                      
    p <- testing[,'final_price']
    rev_pre <- as.numeric(rev)
    rev <- p * a
    diff <- rev - rev_pre
    if (diff < 0.4) {
      prob <<- a
      price <<-p
      break
    }
  }
}



# We chose 6 sections to optimize. 5 Upper levels and 1 lower level. 


# Run loop to get each ticket's probability of being sold 
# optimize Section 640 and save in dataframe
demo_640 <- demo[(which(demo$section_name == '640')),]
demo_640$target <- 0
demo_640$final_price <-0
for (i in 1:nrow(demo_640)){
  GetOp(i,demo_640)
  demo_640[i,'target'] <- prob
  demo_640[i,'final_price'] <- price
}

#optimize Section 609 and save in dataframe
demo_609 <- demo[(which(demo$section_name == '609')),]
demo_609$target <- 0
demo_609$final_price <-0
for (i in 1:nrow(demo_609)){
  GetOp(i,demo_609)
  demo_609[i,'target'] <- prob
  demo_609[i,'final_price'] <- price
}

#optimize Section 437 and save in dataframe
demo_437 <- demo[(which(demo$section_name == '437')),]
demo_437$target <- 0
demo_437$final_price <-0
for (i in 1:nrow(demo_437)){
  GetOp(i,demo_437)
  demo_437[i,'target'] <- prob
  demo_437[i,'final_price'] <- price
}

#optimize Section 625 and save in dataframe
demo_625 <- demo[(which(demo$section_name == '625')),]
demo_625$target <- 0
demo_625$final_price <-0
for (i in 1:nrow(demo_625)){
  GetOp(i,demo_625)
  demo_625[i,'target'] <- prob
  demo_625[i,'final_price'] <- price
}

#optimize Section 450 and save in dataframe
demo_450 <- demo[(which(demo$section_name == '450')),]
demo_450$target <- 0
demo_450$final_price <-0
for (i in 1:nrow(demo_450)){
  GetOp(i,demo_450)
  demo_450[i,'target'] <- prob
  demo_450[i,'final_price'] <- price
}

#optimize Section 117 and save in dataframe
demo_117 <- demo[(which(demo$section_name == '117')),]
demo_117$target <- 0
demo_117$final_price <-0
for (i in 1:nrow(demo_117)){
  GetOp(i,demo_117)
  demo_117[i,'target'] <- prob
  demo_117[i,'final_price'] <- price
}



# We assume that if porb is greater than 0.5, then the ticket is to be sold within 30 days 
demo_437_s30 <- demo_437[which(demo_437$target > 0.5),] #15 
demo_437_14 <- demo_437[which(demo_437$target <= 0.5),] #0     # the remaining records are used in optimizing the prices within 14 days
demo_437_14$day_in_advance <- 14

demo_609_s30 <- demo_609[which(demo_609$target > 0.5),] #215
demo_609_14 <- demo_609[which(demo_609$target <= 0.5),] #18
demo_609_14$day_in_advance <- 14

demo_625_s30 <- demo_625[which(demo_625$target > 0.5),] #161
demo_625_14 <- demo_625[which(demo_625$target <= 0.5),] #5
demo_625_14$day_in_advance <- 14

demo_640_s30 <- demo_640[which(demo_640$target > 0.5),] #125
demo_640_14 <- demo_640[which(demo_640$target <= 0.5),] #0
demo_640_14$day_in_advance <- 14

demo_450_s30 <- demo_450[which(demo_450$target > 0.5),] #34
demo_450_14 <- demo_450[which(demo_450$target <= 0.5),] #61
demo_450_14$day_in_advance <- 14

demo_117_s30 <- demo_117[which(demo_117$target > 0.5),] #39
demo_117_14 <- demo_117[which(demo_117$target <= 0.5),] #4
demo_117_14$day_in_advance <- 14


# Run loop to get prob and price of the tickets that are not sold within 30 days. 
for (i in 1:nrow(demo_640_14)){
  GetOp(i,demo_640_14)
  demo_640_14[i,'target'] <- prob
  demo_640_14[i,'final_price'] <- price
}


for (i in 1:nrow(demo_609_14)){
  GetOp(i,demo_609_14)
  demo_609_14[i,'target'] <- prob
  demo_609_14[i,'final_price'] <- price
}


for (i in 1:nrow(demo_437_14)){
  GetOp(i,demo_437_14)
  demo_437_14[i,'target'] <- prob
  demo_437_14[i,'final_price'] <- price
}


for (i in 1:nrow(demo_625_14)){
  GetOp(i,demo_625_14)
  demo_625_14[i,'target'] <- prob
  demo_625_14[i,'final_price'] <- price
}

for (i in 1:nrow(demo_450_14)){
  GetOp(i,demo_450_14)
  demo_450_14[i,'target'] <- prob
  demo_450_14[i,'final_price'] <- price
}

for (i in 1:nrow(demo_117_14)){
  GetOp(i,demo_117_14)
  demo_117_14[i,'target'] <- prob
  demo_117_14[i,'final_price'] <- price
}


#rm(demo_437,demo_452,demo_609,demo_625,demo_640)


# Subset number of tickets that are sold when price is updated at day 14 
demo_437_s14 <- demo_437_14[which(demo_437_14$target > 0.5),] #0
demo_437_3 <- demo_437_14[which(demo_437_14$target <= 0.5),] #0
demo_437_3$day_in_advance <- 3

demo_609_s14 <- demo_609_14[which(demo_609_14$target > 0.5),] #3
demo_609_3 <- demo_609_14[which(demo_609_14$target <= 0.5),] #15
demo_609_3$day_in_advance <- 3

demo_625_s14 <- demo_625_14[which(demo_625_14$target > 0.5),] #5
demo_625_3 <- demo_625_14[which(demo_625_14$target <= 0.5),] #0
demo_625_3$day_in_advance <- 3

demo_640_s14 <- demo_640_14[which(demo_640_14$target > 0.5),] #0
demo_640_3 <- demo_640_14[which(demo_640_14$target <= 0.5),] #0
demo_640_3$day_in_advance <- 3

demo_450_s14 <- demo_450_14[which(demo_450_14$target > 0.5),] #7
demo_450_3 <- demo_450_14[which(demo_450_14$target <= 0.5),] #54
demo_450_3$day_in_advance <- 3

demo_117_s14 <- demo_117_14[which(demo_117_14$target > 0.5),] #3
demo_117_3 <- demo_117_14[which(demo_117_14$target <= 0.5),] #1
demo_117_3$day_in_advance <- 3


# Run loop to get prob and price of the tickets that are not sold within 14 days. 
for (i in 1:nrow(demo_640_3)){
  GetOp(i,demo_640_3)
  demo_640_3[i,'target'] <- prob
  demo_640_3[i,'final_price'] <- price
}


for (i in 1:nrow(demo_609_3)){
  GetOp(i,demo_609_3)
  demo_609_3[i,'target'] <- prob
  demo_609_3[i,'final_price'] <- price
}


for (i in 1:nrow(demo_437_3)){
  GetOp(i,demo_437_3)
  demo_437_3[i,'target'] <- prob
  demo_437_3[i,'final_price'] <- price
}


for (i in 1:nrow(demo_640_3)){
  GetOp(i,demo_640_3)
  demo_640_3[i,'target'] <- prob
  demo_640_3[i,'final_price'] <- price
}

for (i in 1:nrow(demo_450_3)){
  GetOp(i,demo_450_3)
  demo_450_3[i,'target'] <- prob
  demo_450_3[i,'final_price'] <- price
}

for (i in 1:nrow(demo_117_3)){
  GetOp(i,demo_450_3)
  demo_450_3[i,'target'] <- prob
  demo_450_3[i,'final_price'] <- price
}


############################################################################################

#get unsold inventory 3 days before game
demo_609_s3 <- demo_609_3[which(demo_609_3$target > 0.5),] #4 
demo_609_3unsold <- demo_609_3[which(demo_609_3$target <= 0.5),] #11

demo_625_s3 <- demo_625_3[which(demo_625_3$target > 0.5),] #0
demo_625_3unsold <- demo_625_3[which(demo_625_3$target <= 0.5),] #0


demo_640_s3 <- demo_640_3[which(demo_640_3$target > 0.5),] #0
demo_640_3unsold <- demo_640_3[which(demo_640_3$target <= 0.5),] #0

demo_450_s3 <- demo_450_3[which(demo_450_3$target > 0.5),] #0
demo_450_3unsold <- demo_450_3[which(demo_450_3$target <= 0.5),] #0

demo_117_s3 <- demo_117_3[which(demo_117_3$target > 0.5),] #0
demo_117_3unsold <- demo_117_3[which(demo_117_3$target <= 0.5),] #0

demo_437_s3 <- demo_437_3[which(demo_437_3$target > 0.5),] #0
demo_437_3unsold <- demo_117_3[which(demo_437_3$target <= 0.5),] #0

#rm(demo_437_14,demo_452_14,demo_609_14,demo_625_14,demo_640_14)


#???Compare the original revenue and the optimized revenue 
ori_rev_640 <- sum(df[which(df$event_name == 'CLT19DEN' & df$section_name ==
                              '640' & df$sales_channel == 'colts.com' & df$target == '1'),]$final_price)
op_rev_640 <- sum(demo_640_s30$final_price) + sum(demo_640_s14$final_price) +sum(demo_640_s3$final_price)
paste0('Revenue Gain for section 640: $', op_rev_640 - ori_rev_640)
#1508

ori_rev_609 <- sum(df[which(df$event_name == 'CLT19DEN' & df$section_name ==
                              '609' & df$sales_channel == 'colts.com' & df$target == '1'),]$final_price)
op_rev_609 <- sum(demo_609_s30$final_price) + sum(demo_609_s14$final_price) +sum(demo_609_s3$final_price)
paste0('Revenue Gain for section 609: $', op_rev_609 - ori_rev_609)
#7172

ori_rev_625 <- sum(df[which(df$event_name == 'CLT19DEN' & df$section_name ==
                              '625' & df$sales_channel == 'colts.com' & df$target == '1'),]$final_price)
op_rev_625 <- sum(demo_625_s30$final_price) + sum(demo_625_s14$final_price) +sum(demo_625_s3$final_price)
paste0('Revenue Gain for section 625: $', op_rev_625 - ori_rev_625)
#4723

ori_rev_450 <- sum(df[which(df$event_name == 'CLT19DEN' & df$section_name ==
                              '450' & df$sales_channel == 'colts.com' & df$target == '1'),]$final_price)
op_rev_450 <- sum(demo_450_s30$final_price) + sum(demo_450_s14$final_price) +sum(demo_450_s3$final_price)
paste0('Revenue Gain for section 450: $', op_rev_450 - ori_rev_450)
#303

ori_rev_117 <- sum(df[which(df$event_name == 'CLT19DEN' & df$section_name ==
                              '117' & df$sales_channel == 'colts.com' & df$target == '1'),]$final_price)
op_rev_117 <- sum(demo_117_s30$final_price) + sum(demo_117_s14$final_price) +sum(demo_117_s3$final_price)
paste0('Revenue Gain for section 117: $', op_rev_117 - ori_rev_117)
#-453

ori_rev_437 <- sum(df[which(df$event_name == 'CLT19DEN' & df$section_name ==
                              '437' & df$sales_channel == 'colts.com' & df$target == '1'),]$final_price)
op_rev_437 <- sum(demo_437_s30$final_price) + sum(demo_437_s14$final_price) +sum(demo_437_s3$final_price)
paste0('Revenue Gain for section 437: $', op_rev_437 - ori_rev_437)
#506


resultList <- list(demo_117_s30 = demo_117_s30,
                   demo_117_s14 = demo_117_s14,
                   demo_117_s3 = demo_117_s3,
                   demo_117_3unsold = demo_117_3unsold,
                   demo_437_s30 = demo_437_s30,
                   demo_437_s14 = demo_437_s14,
                   demo_437_s3 = demo_437_s3,
                   demo_437_3unsold = demo_437_3unsold,
                   demo_450_s30 = demo_450_s30,
                   demo_450_s14 = demo_450_s14,
                   demo_450_s3 = demo_450_s3,
                   demo_450_3unsold = demo_450_3unsold,
                   demo_609_s30 = demo_609_s30,
                   demo_609_s14 = demo_609_s14,
                   demo_609_s3 = demo_609_s3,
                   demo_609_3unsold = demo_609_3unsold,
                   demo_625_s30 = demo_625_s30,
                   demo_625_s14 = demo_625_s14,
                   demo_625_s3 = demo_625_s3, 
                   demo_625_3unsold = demo_625_3unsold,
                   demo_640_s30 = demo_640_s30,
                   demo_640_s14 = demo_640_s14, 
                   demo_640_s3 = demo_640_s3,
                   demo_640_3unsold = demo_640_3unsold
)
for(i in names(resultList)){
  write.csv(resultList[[i]], paste0(i,".csv"))
}


resultList2 = list(demo_117 = demo_117,
                   demo_117_14 = demo_117_14,
                   demo_117_3 = demo_117_3,
                   demo_437 = demo_437,
                   demo_437_14 = demo_437_14, 
                   demo_437_3 = demo_437_3, 
                   demo_450 = demo_450,
                   demo_450_14 = demo_450_14, 
                   demo_450_3 = demo_450_3,
                   demo_609 = demo_609,
                   demo_609_14 = demo_609_14, 
                   demo_609_3 = demo_609_3, 
                   demo_625 = demo_625, 
                   demo_625_14 = demo_625_14, 
                   demo_625_3 = demo_625_3, 
                   demo_640 = demo_640,
                   demo_640_14 = demo_640_14, 
                   demo_640_3 = demo_640_3
)

for (i in names(resultList2)){
  write.csv(resultList2[[i]],paste0(i,".csv"))
}



#
FindRange <- function(data) {
  vec <- data$final_price
  lowerq = quantile(vec)[2]
  upperq = quantile(vec)[4]
  iqr = IQR(vec)
  upbound = (iqr*1.5) + upperq
  lowbound = lowerq - (iqr*1.5)
  temp <- vec[which(vec < upbound & vec > lowbound)]
  maxprice <<- as.numeric(max(vec))
  minprice <<- as.numeric(min(temp))
}

section <- c('117', '437', '450', '609', '625', '640')

d_117_30 <- read.csv('demo_117.csv')
FindRange(d_117_30)
# 32 61
d_437_30 <- read.csv('demo_437.csv')
FindRange(d_437_30)
# 35 62
d_450_30 <- read.csv('demo_450.csv')
FindRange(d_450_30)
#21 54
d_609_30 <- read.csv('demo_609.csv')
FindRange(d_609_30)
# 30 135
d_625_30 <- read.csv('demo_625.csv')
FindRange(d_625_30)
# 26 55
d_640_30 <- read.csv('demo_640.csv')
FindRange(d_640_30)
# 31 98
max_30 <- c(61,62,54,135,55,98)
min_30 <- c(32,35,21,30,26,31)
op_range_30 <- data.frame(section,min_30,max_30)
write.csv(op_range_30,'op_range_30.csv')

d_117_14 <- read.csv('demo_117_14.csv')
FindRange(d_117_14)
# 51 59
d_437_14 <- read.csv('demo_437_14.csv')
FindRange(d_437_14)
# sold
d_450_14 <- read.csv('demo_450_14.csv')
FindRange(d_450_14)
#33 61
d_609_14 <- read.csv('demo_609_14.csv')
FindRange(d_609_14)
#48 169
d_625_14 <- read.csv('demo_625_14.csv')
FindRange(d_625_14)
#37 50
d_640_14 <- read.csv('demo_640_14.csv')
FindRange(d_640_14)
#sold
min_14 <- c(51,'sold',33,48,37,'sold')
max_14 <- c(59,'sold',61,169,50,'sold')
op_range_14 <- data.frame(section,min_14,max_14)
write.csv(op_range_14,'op_range_14.csv')

d_117_3 <- read.csv('demo_117_3.csv')
FindRange(d_117_3)
# 51 51

# sold
d_450_3 <- read.csv('demo_450_3.csv')
FindRange(d_450_3)
#40 80
d_609_3 <- read.csv('demo_609_3.csv')
FindRange(d_609_3)
#51 120
d_625_3 <- read.csv('demo_625_3.csv')
FindRange(d_625_3)
#37 50
#sold
min_3 <- c(51,'sold',40,51,37,'sold')
max_3 <- c(51,'sold',80,120,50,'sold')
op_range_3 <- data.frame(section, min_3,max_3)
write.csv(op_range_3,'op_range_3.csv')

























#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#Train RF grid 
#=================================
#  Full Cartesian Grid Search
#=================================

# Set hyperparameter grid: 
library(dplyr)
hyper_grid.h2o <- list(ntrees = seq(10, 150, by = 10),
                       #mtries = seq(3, 5, by = 1),
                       max_depth = seq(1, 20, by = 5 ),
                       #min_rows = seq(1, 3, by = 1),
                       #nbins = seq(20, 30, by = 10),
                       sample_rate = c(0.5, 0.632, 0.75))

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 20,
                        seed = 1)

# The number of models is 90: 
sapply(hyper_grid.h2o, length) %>% prod()

#Train Models 
system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                       grid_id = "rf_grid1",
                                       x = x, 
                                       y = y, 
                                       seed = 999, 
                                       nfolds = 3, 
                                       training_frame = train,
                                       stopping_metric = "AUC", 
                                       hyper_params = hyper_grid.h2o,
                                       search_criteria = search_criteria)) #"Cartesian" covers the entire space of hyperparameter combinations

grid_perf <- h2o.getGrid(grid_id = "rf_grid1", 
                         sort_by = "auc", 
                         decreasing = TRUE)
grid_perf
# Best model chosen by validation error: 
best_model <- h2o.getModel(grid_perf@model_ids[[1]])
best_model

h2o.performance(best_model, test)






#Machine Learning with Caret
################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to create a dummy 0/1 variable for every level of a categorical 
# variable

library(caret)
library(dplyr)

#names(d)[1] <- "y"

#check for target balance

balance = d %>% group_by(y) %>% summarize(y_count = n())
balance
# y y_count
# 1 0  294800
# 2 1  204185
ggplot(data = d, aes(y)) + geom_bar()



# #Target encoding feature: section_names , row_names 
# str(d)
library(cattonum)
df1$target = as.numeric(df1$target) 
df1$target = df1[ ,1] - 1
df1 = catto_mean(train = df1, c(section_name,row_name), response = target)
df1$target = as.factor(df1$target)
str(df1)

#dummy encoding 
dummies <- dummyVars(y ~ home_opener + rain_snow + Colts_contention +opp_playoff+wday, data = d)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- cbind(d[c(1:12,14,17:19,21:26)], ex)                              # combine target var with Xs
names(d)[1] <- "y"                               # name target var 'y'
rm(dummies, ex)                                  # clean environment



#############################################################
tr <- df1[df1$event_name != 'CLT19DEN', ]
te <- df1[df1$event_name == 'CLT19DEN', ]


#event_name should be dropped 
tr$event_name = NULL
te$event_name = NULL 

library(h2o)
h2o.init(nthreads = 2, max_mem_size = '20g')
#h2o.shutdown()

h2o.clusterInfo()

data <- as.h2o(tr)

y <- "target"
x <- setdiff(names(data),y)


train <- as.h2o(tr)
test <- as.h2o(te)
#parts <- h2o.splitFrame(train, 0.7, seed=999) # randomly partition data into 80/20
#train = parts[[1]]
#valid = parts[[2]]

# train an logit Regression model on the H2O training data.frame
logitR <- h2o.glm(x = x, y = y, training_frame = train, 
                  #validation_frame = valid, 
                  seed = 12345, 
                  family = 'binomial', 
                  link = 'logit', 
                  #standardize = FALSE 
)
h2o.performance(logitR,test)

#SVM 
# svm = h2o.psvm(x,y,train,seed = 123, hyper_param = 0.8)
# h2o.performance(svm,te)

xgb <- h2o.xgboost(x = x,y = y,training_frame =  train,nfolds = 3, seed = 999,
                   ntrees = 100, 
                   max_depth = 10,
                   keep_cross_validation_predictions = TRUE 
)

h2o.performance(xgb,test)


#Random Forest Models 
rf = h2o.randomForest(x = x,y = y,training_frame =  train, seed = 999,
                      #ntrees = 50,
                      #max_depth = 20,
                      nfolds = 3,
                      keep_cross_validation_predictions = TRUE
)
rf
h2o.performance(rf,test)

#Ensemble Methods
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                #model_id = "my_ensemble_binomial",
                                base_models = list(xgb, rf))
perf <- h2o.performance(ensemble, newdata = test)



################################################################################################
# Generate a random grid of models and stack them together
################################################################################################
# GBM Hyperparamters
learn_rate_opt <- c(0.3, 1, 0.05, 0.01) #learning rate by which to shrink the feature weights. Shrinking feature weights after each boosting step makes the boosting process more conservative and prevents overfitting
max_depth_opt <- c(5, 6, 8,10) #Higher values will make the model more complex and can lead to overfitting
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0) #
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 10,
                     seed = 999,
                     nfolds = 3,
                     fold_assignment = "AUTO",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "ensemble_gbm_grid_binomial",
                                base_models = gbm_grid@model_ids)

h2o.performance(ensemble, newdata = test)




#############################################################################################
#============================================================================================
# Train XGboost Grid
hyper_params <- list(ntrees = seq(10, 200, 5),
                     learn_rate = seq(0.001, 0.1, 0.001),
                     max_depth = seq(2, 10, 1),
                     sample_rate = seq(0.5, 1.0, 0.001), #setting this value to 0.5 tells XGBoost to randomly collected half of the data instances to grow trees
                     col_sample_rate = seq(0.2, 1.0, 0.001)) #Specify the column sampling rate (y-axis) for each split in each level.
search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 30, 
                        seed = 1)
sapply(hyper_params, length) %>% prod()
# Train the grid
xgb_grid <- h2o.grid(algorithm = "xgboost",
                     x = x, y = y,
                     training_frame = train,
                     nfolds = 3,
                     seed = 1,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
# Sort the grid by CV AUC
grid <- h2o.getGrid(grid_id = xgb_grid@grid_id, sort_by = "AUC", decreasing = TRUE)
xgb_top <- h2o.getModel(grid@model_ids[[1]])
h2o.performance(xgb_top, test)
grid


#########################################################################################################
#Train RF grid 
#=================================
#  Full Cartesian Grid Search
#=================================

# Set hyperparameter grid: 

hyper_grid.h2o <- list(ntrees = seq(10, 100, by = 10),
                       #mtries = seq(3, 5, by = 1),
                       max_depth = seq(10, 30, by = 10),
                       #min_rows = seq(1, 3, by = 1),
                       #nbins = seq(20, 30, by = 10),
                       sample_rate = c(0.5, 0.632, 0.75))

# The number of models is 90: 
sapply(hyper_grid.h2o, length) %>% prod()

#Train Models 
system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                       grid_id = "rf_grid1",
                                       x = x, 
                                       y = y, 
                                       seed = 999, 
                                       nfolds = 3, 
                                       training_frame = train,
                                       stopping_metric = "AUC", 
                                       hyper_params = hyper_grid.h2o,
                                       search_criteria = search_criteria)) #"Cartesian" covers the entire space of hyperparameter combinations

grid_perf <- h2o.getGrid(grid_id = "rf_grid1", 
                         sort_by = "auc", 
                         decreasing = FALSE)

# Best model chosen by validation error: 
best_model <- h2o.getModel(grid_perf@model_ids[[1]])
best_model

# Use best model for making predictions: 
confusionMatrix(h2o.predict(best_model, test) %>% as.data.frame() %>% pull(predict), 
                test$target, 
                positive = 1
)
h2o.performance(best_model,test)


#Ensemble Methods
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                #model_id = "my_ensemble_binomial",
                                base_models = list(best_model, grid_top_model))
perf <- h2o.performance(ensemble, newdata = test)





#===============================================================================================
#===============================================================================================
#===============================================================================================
demo <- te
demo$target <- NA
demo$day_in_advance <- 30
demo$final_price <- 0


GetOp <- function(r) {
  testing <- demo[r,]
  testing$target <- NULL
  rev <- 0
  rev_pre <- 0
  diff <- 0
  a = 0
  p = 0
  while(diff >= 0) {
    testing[,'final_price'] <- testing[,'final_price'] + 1
    testing <- as.h2o(testing)
    pred <-as.data.frame(h2o.predict(rf,testing))
    testing <- as.data.frame(testing)
    a <- pred$p1
    p <- testing[,'final_price']
    rev_pre <- as.numeric(rev)
    rev <- p * a
    diff <- rev - rev_pre
    if (diff < 0.4) {
      prob <<- a
      price <<-p
      break
    }
  }
}


###############################################################################################

















################################################################################
# Remove features with limited variation
################################################################################
# remove features where the values they take on is limited
# here we make sure to keep the target variable and only those input
# features with enough variation
# nzv <- nearZeroVar(d, saveMetrics = TRUE)
# d <- d[, c(TRUE,!nzv$zeroVar[2:ncol(d)])]
# rm(nzv)
################################################################################
# Standardize (and/ normalize) your input features.
################################################################################
# Here we standardize the input features (Xs) using the preProcess() function 
# by performing a min-max normalization (aka "range" in caret).

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(df1[,c('FB_fans','distance','opp_scoredLY','opp_def_givenLY','odds_f')], method = c("range"))
df1 <- predict(preProcValues, df1)


################################################################################
# Get the target variable how we want it for modeling with caret
################################################################################

# make names for target if not already made
levels(d$y) <- make.names(levels(factor(d$y)))
levels(d$y)

# levels of a factor are re-ordered so that the level specified is first and 
# "X1" is what we are predicting. The X before the 1 has nothing to do with the
# X variables. It's just something weird with R. 'X1' is the same as 1 for the Y 
# variable and 'X0' is the same as 0 for the Y variable.
d$y <- relevel(d$y,"X1")





################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results
library(caret)
# identify records that will be used in the training set. Here we are doing a
# 70/30 train-test split. You might modify this.
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
tr <- d[d$event_name != 'CLT19DEN', ]
te <- d[d$event_name == 'CLT19DEN', ]

#event_name should be dropped 
tr$event_name = NULL
te$event_name = NULL 

#check for target balance
balance = tr %>% group_by(y) %>% summarize(y_count = n())

#in training set, number of 0s has more than 1s, maybe need to down sample
ggplot(data = tr, aes(y)) + geom_bar()

# down-sampled training set
#?downSample
dnTrain <- downSample(x=tr[,2:ncol(tr)], y=tr$y)
names(dnTrain)[32] <- "y"
ggplot(data = dnTrain, aes(y)) + geom_bar()

################################################################################
# Specify cross-validation design
################################################################################
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = T,  # if you want probabilities
                     summaryFunction = twoClassSummary, # for classification
                     #summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)


################################################################################
# Train different models
#
# Bookmark this site: https://topepo.github.io/caret/available-models.html
################################################################################
# train a logistic regession on down-sampled train set 
logit <- train(y ~ .,                 # model specification
               data = dnTrain,        # train set used to build model
               method = "glm",      # type of model you want to build
               trControl = ctrl,    # how you want to learn
               family = "binomial", # specify the type of glm
               metric = "ROC"       # performance measure
)
logit

# Generalized Linear Model 
# 
# 398248 samples
# 31 predictor
# 2 classes: 'X1', 'X0' 
# 
# No pre-processing
# Resampling: Cross-Validated (3 fold) 
# Summary of sample sizes: 265499, 265498, 265499 
# Resampling results:
#   
#   ROC        Sens       Spec     
# 0.7680863  0.7041592  0.6932715


# train a reguarlarized logistic regession model on down-sampled train set 
myGrid <-  expand.grid(cost = c(0.5, 1, 2)
                       , loss = c("L1","L2_dual","L2_primal")
                       , epsilon = c(0.001,0.010,0.10)) # create a tuning parameter grid
logitreg <- train(y ~ .,                  # model specification
                  data = dnTrain,         # train set used to build model
                  method = "regLogistic", # type of model you want to build
                  trControl = ctrl,       # how you want to learn
                  tuneGrid = myGrid,   # tuning parameter combos to try
                  metric = "ROC"          # performance measure
)
logitreg

# train an XG-boost model on down-sampled dataset
myGrid <-  expand.grid(nrounds = c(50,100,150)
                       , max_depth = c(1,2,3)
                       , eta = c(.3,.4)
                       , gamma = 0
                       , colsample_bytree = c(.6, .8)
                       , min_child_weight = 1
                       , subsample = c(.5,.75,1))
xgboost <- train(y ~ .,              # model specification
                 data = dnTrain,     # train set used to build model
                 method = "xgbTree",      # type of model you want to build
                 trControl = ctrl,   # how you want to learn
                 tuneGrid = myGrid,  # tuning parameter combos to try
                 metric = "ROC"      # performance measure
)
xgboost



