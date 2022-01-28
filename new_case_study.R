# set working directory
# the work was saved under file nae case_stud

# install tidyverse & readx1 pacakge
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(lubridate)

#importing Data 
# five datasets from kaggle was downloaded 

dailyActivity_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
dailyCalories_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
dailyIntensities_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
sleepDay_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightLogInfo_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# over viewing and Getting Familiar with datasets


daily_activity <- dailyActivity_merged
daily_calories <- dailyCalories_merged
daily_intensities <- dailyIntensities_merged
sleep_day <- sleepDay_merged
weight_log <- weightLogInfo_merged

head(daily_activity)
colnames(daily_activity)
glimpse(daily_activity)

head(daily_calories)
colnames(daily_calories)
glimpse(daily_calories)

head(sleep_day)
colnames(sleep_day)
glimpse(sleep_day)

head(weight_log)
colnames(weight_log)
glimpse(weight_log)

head(daily_intensities)
colnames(daily_intensities)
glimpse(daily_intensities)

#Data cleaning 
# A)Removing duplicates

# Id and activity date should be unique (0 observations removed)
daily_activity <- daily_activity %>%
  distinct(Id, ActivityDate, .keep_all = TRUE)

# Id and activity date should be unique (3 observations removed)
sleep_day <- sleep_day %>% 
  distinct(Id, SleepDay, .keep_all = TRUE)

# Id and Date should be unique (0 observations removed)
weight_log <- weight_log %>%
  distinct(Id, Date, .keep_all = TRUE)

# B)Checking for NA values and removing if any exist
#no NA value was found
which(!complete.cases(daily_activity))
which(!complete.cases(sleep_day))
which(!complete.cases(daily_intensities))
# in wight_Log there was 60 NA on FAT column 
which(!complete.cases(weight_log))

# For this case study I decided to use the merged 5 files,
#*daily_activity
#*weight_log
#*daily_intensities
#*sleep_day

# Analyze

# initial observations:

## * they all contain ID column
## * daily_activity, daily_intensities and daily_calories has same Rows Number(940)
## * daily_activity contain all daily_calories attribute and daily_ intensities


# A) daily activity Trends:

# checking User Activity 
#By grouping User into "Active" and "Non-Active" users. 
#Active users are those with average sum of high and medium daily activity over 50 minutes.

# will assume that user with total steps less than 700 the device was not used)
daily_activity_on <- daily_activity[daily_activity$TotalSteps > 700, ]
View(daily_activity_on)

# then we group by Id, and finding the mean for each activity 

daily_activity_users <- daily_activity_on %>% 
  group_by(Id) %>%
  View(daily_activity_users)
  summarise(VeryActiveMinutes_av = mean(VeryActiveMinutes), 
            FairlyActiveMinutes_av = mean(FairlyActiveMinutes),
            LightlyActiveMinutes_av = mean(LightlyActiveMinutes),
            SedentaryMinutes_av = mean(SedentaryMinutes),
            Calories_av = mean(Calories))

daily_activity_users <- daily_activity_users %>% 
  mutate(very_fairly_active_avg = VeryActiveMinutes_av + FairlyActiveMinutes_av)

## Creating a variable for active and non-active users
daily_activity_users <- daily_activity_users %>% 
  mutate(active = ifelse(very_fairly_active_avg > 60, "active", "non-active"))
View(daily_activity_users)

# Plotting a bar chart for the result 
ggplot(data = daily_activity_users)+
  geom_bar(mapping = aes(x = active,  fill = active))+ 
  labs(title = "Distribution of active and non-active users", x = "Activity level", y = "Number of users")+
  theme(title = element_text(size=20, face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=16))+ 
  guides(fill=guide_legend(title="Activity level"))
##Less than 25% of respondents are users, who exercise at least 50 minutes a day. 
##Non-active group has a majority people in it.
## we can assume that either an average user does not have an active lifestyle. Or does not wear a device during training.


# cross check daily_activity with daily_calories & daily_intensite by creating temp dataframe named it daily_activity_Temp

daily_activity_Temp1 <- daily_activity %>%
  select(Id, ActivityDate, Calories)
head(daily_activity_Temp1)
glimpse(daily_activity_Temp1)

# check if the two data frames of daily_activity_Temp1 and daily_calories are same
# first we install the sqldf package for ruining SQL statements on R data frames, optimized for convenience 

install.packages("sqldf")
library(sqldf)

sql_check1 <- sqldf('SELECT * FROM daily_activity_Temp1 INTERSECT SELECT * FROM daily_calories')
head(sql_check1)
glimpse(sql_check1)

# so it’s safe to assume that the values are the same between the data frames.
# the same is true for daily intensities, 
# so we can drop those two dataframes from analysis

# That leaves us with 3 data frames:
#* daily_activity
#* sleep_day
#* weight_log


## let check the Number of Id's in daily_activity and sleep_day
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log$Id)

## let check the number of observation in each 
nrow(daily_activity)
nrow(sleep_day)
nrow(weight_log)

# let make statistical Calculation summery  for each
# A) daily_activity
daily_activity %>%
  select(TotalSteps, TotalDistance, VeryActiveMinutes, SedentaryMinutes, Calories) %>%
  summary()
# B) sleep_day
sleep_day %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight_log
weight_log %>%
  select(WeightKg,BMI) %>%
  summary()

# few explorations

## Total steps VS sedentary minutes
ggplot(data = daily_activity) +
  geom_point(mapping = aes(x= TotalSteps, y= SedentaryMinutes, color= Calories))
## Its look like negative relationship between total steps taken and sedentary Minutes
## And calories generally trend positively with total steps taking
ggplot(data=daily_activity, aes(x=TotalSteps, y = Calories))+ geom_point() + stat_smooth(method=lm)

#*we can assume This shows that the data seem fairly accurate when it comes to recording steps and sedentary minutes. 
#*We could easily market this to consumers by telling them smart-devices could help them start their journey by measuring how much they’re already moving!
#*we could also market the devices as a way to let people know how sedentary they actually are.
#*We can also note that sedentary time is not necessarily related to calories burned.

#in general the people who took the most total steps tended to burn the most calories however, there’s a large spread there clustered towards the lower amounts.

calories.lm <- lm(Calories ~ TotalSteps, data = daily_activity)
calories.res <- resid(calories.lm)


plot(daily_activity$TotalSteps, calories.res, ylab="Residuals",xlab = "Total Steps", main = "Calories Burned")


## now lets plot the density of the residuals
plot(density(calories.res))

#Checking for normality 
qqnorm(calories.res)
qqline(calories.res)
##looks like the spread isn’t as far statistically as we though

#A potential strategy

#Here you could easily market that in order to burn calories, you don’t have to do high-intensity work outs, 
#you can just get out there and start walking!This would be such a relief as a consumer,
#because this proves to them that you can have results without starting a gym membership, or by starting a large workout routine. 
#You can burn calories, simply by walking.

#Sleep and Time in Bed
##1:1 trend from the amount of time slept and the total time someone spends in bed
ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()
#there are some outliers here! some data points that spent a lot of time in bed, but didn’t actually sleep, and then a small batch that slept a whole bunch and spent time in bed (I can relate)
## We could definitely market to consumers to monitor their time in bed with the watch against their sleep time.
##I wonder how this relates to the sedentary minutes data in the last dataset??

# Merging these two datasets together
combined_sleep_day_data <- merge(sleep_day, daily_activity, by="Id")
head(combined_sleep_day_data)
View(combined_sleep_day_data)
n_distinct(combined_sleep_day_data$Id)
## As we expected, there are only 24 unique id’s in the combined dataset, since only 24 users actively used the sleep data

#We could perform an outer join to include all of the fitbit users in the dataset, but theoretically, their sleep data would be empty (either null or N/A). Let’s try it!

combined_sleep_day_data2 <- merge(sleep_day, daily_activity, by="Id", all = TRUE)
head(combined_sleep_day_data2)

n_distinct(combined_sleep_day_data2$Id)

#Sedentary Time vs Time In Bed
##Let’s run a correlation to see what the correlation coefficient coefficient would be for a linear regression:

sedentary.lm <- lm(SedentaryMinutes ~ TotalTimeInBed, data = combined_sleep_day_data)
sedentary.lm

#And now a pearson correlation coefficient:
cor(combined_sleep_day_data$TotalTimeInBed,combined_sleep_day_data$SedentaryMinutes, method = "pearson")
## As time in bed goes up, sedentary minutes actually go down, but not to a statistically significant degree.

#A Few Extra Graphs

#There’s a strong positive correlation between very active minutes and calories burned.
ggplot(data = combined_sleep_day_data, aes(x=VeryActiveMinutes, y=Calories)) + 
  geom_point(color="#ca6708") + geom_smooth(method = 'loess', formula = y ~ x) + 
  labs(title="Very Active Minutes vs. Calories Burned", x="VeryActiveMinutes", y="Calories Burned")


lm(Calories ~ VeryActiveMinutes, data = combined_sleep_day_data)
#And it looks like a very small correlation for between total steps taken and calories burned.

ggplot(data = combined_sleep_day_data, aes(x=TotalSteps, y=Calories)) + 
  geom_point(color= "#ca6708") + labs(title = "Total steps VS Calories") +stat_smooth(method = lm)

lm(Calories ~ TotalSteps, data = combined_sleep_day_data)

#But a Moderate relationship for fairly active minutes.
ggplot(data = combined_sleep_day_data, aes(x=FairlyActiveMinutes, y=Calories)) + geom_point() + stat_smooth(method = lm)

lm(Calories ~ FairlyActiveMinutes, data = combined_sleep_day_data)

#some Thoughts
#We looked at this dataset of fitbit users pretty intensively to get an idea on what features are being used, and how we can market our items.

#Takeaway 1
#Fitbit does not collect hydration data, that puts Bellabeat way above the competition!

#Takeaway 2
#We showed that more people log their calories, steps taken, etc, and fewer users log their sleep data, and only a select few are logging their weight
#Takeaway 3
#To market this, we initially thought that simply being active and taking steps would help with people on their journey start to burn calories. While this may be true, we see that the correlation is beyond small, and maybe we shouldn’t market it that way.
#I would focus on the fat that simply collecting data will help you set goals. Take a strategy from a platform like noom, and make it our own here at Bellabeat!







