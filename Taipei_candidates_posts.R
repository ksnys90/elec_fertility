library(readxl)
library(tidyverse)
library(stringr)
library(openxlsx)
url <- "https://github.com/Jasper-Hewitt/elec_fertility/raw/main/data/Shanshan.xlsx"
Shanshan_df <- read.xlsx(url, sheet = 1)

#create a new column called fert_mention
Shanshan_df$fert_mention <- NA  

#fill na in Message column (to prevent errors when we use detect)
Shanshan_df$Message[is.na(Shanshan_df$Message)] <- 0
sum(Shanshan_df$Message == '0') #3 this happens when they did a live stream for example

#find posts related to 生育率. we can adjust the search terms later.
#we can also adopt another approach like sbert or chatgpt to see if the posts had 
#something to do with 生育率
#the sentence does or does not contain the word 'china'. I store the output in the variable 'chinamatch'
fertmatch <- str_detect(Shanshan_df$Message, "生育|生孩子|孕育|懷孕")

# if fertmatch for a specific row is TRUE (mentioned), assign 1 to that row in the fert_mention column 
Shanshan_df$fert_mention[fertmatch] <- 1
#if FALSE (not mentioned), assign 0
Shanshan_df$fert_mention[!fertmatch] <- 0

#count in how many tweets the word China appeared (count how many times 1 appears in the new column)
shanshan_fertcount <- sum(Shanshan_df$fert_mention == 1)
print(shanshan_fertcount) #11 posts about our search terms

#____________________________________________________________________________________________

#now let's do the same for 陳時中 and 蔣萬安
url <-'https://github.com/Jasper-Hewitt/elec_fertility/blob/main/data/wanan.xlsx?raw=true'
Wanan_df <- read.xlsx(url, sheet = 1)

#create a new column called fert_mention
Wanan_df$fert_mention <- NA  

#fill na in Message column (to prevent errors when we use detect)
Wanan_df$Message[is.na(Wanan_df$Message)] <- 0
sum(Wanan_df$Message == '0') #4 this happens when they did a live stream for example

#find posts related to 生育率. we can adjust the search terms later.
#we can also adopt another approach like sbert or chatgpt to see if the posts had 
#something to do with 生育率
#the sentence does or does not contain the word 'china'. I store the output in the variable 'chinamatch'
fertmatch <- str_detect(Wanan_df$Message, "生育|生孩子|孕育|懷孕")

# if fertmatch for a specific row is TRUE (mentioned), assign 1 to that row in the fert_mention column 
Wanan_df$fert_mention[fertmatch] <- 1
#if FALSE (not mentioned), assign 0
Wanan_df$fert_mention[!fertmatch] <- 0

#count in how many tweets the word China appeared (count how many times 1 appears in the new column)
Wanan_fertcount <- sum(Wanan_df$fert_mention == 1)
print(Wanan_fertcount) #19 posts about our search terms

#_____________________________________________________________________________________________
#陳時中
url <- 'https://github.com/Jasper-Hewitt/elec_fertility/blob/main/data/chenshihchung.xlsx?raw=true'

Shihchung_df <- read.xlsx(url, sheet = 1)

#create a new column called fert_mention
Shihchung_df$fert_mention <- NA  

#fill na in Message column (to prevent errors when we use detect)
Shihchung_df$Message[is.na(Shihchung_df$Message)] <- 0
sum(Shihchung_df$Message == '0') #18 this happens when they did a live stream for example

#find posts related to 生育率. we can adjust the search terms later.
#we can also adopt another approach like sbert or chatgpt to see if the posts had 
#something to do with 生育率
#the sentence does or does not contain the word 'china'. I store the output in the variable 'chinamatch'
fertmatch <- str_detect(Shihchung_df$Message, "生育|生孩子|孕育|懷孕")

# if fertmatch for a specific row is TRUE (mentioned), assign 1 to that row in the fert_mention column 
Shihchung_df$fert_mention[fertmatch] <- 1
#if FALSE (not mentioned), assign 0
Shihchung_df$fert_mention[!fertmatch] <- 0

#count in how many tweets the word China appeared (count how many times 1 appears in the new column)
Shihchung_fertcount <- sum(Shihchung_df$fert_mention == 9)
print(Shihchung_fertcount) #9 posts about our search terms


#__________________________________________________________________________________________________
#plot results in simple stack plot 

# the answer is already there, but I'm just exploring some of the possibilities in R.
# in the following lines I create a dataframe to plot the results.
total_mentions_taipei <- data.frame(
  candidate=c("HuangShan-shan","JiangWan-an","ChenShih-chung") ,
  total_mentions=c(shanshan_fertcount, Wanan_fertcount, Shihchung_fertcount)
)
# create a barplot with with ggplot.
# In order to display the discrepancy between the bars more clearly, I zoomed in on on
#the Y range 0 to 20 with ylim.
ggplot(total_mentions_taipei, aes(x=candidate, y=total_mentions)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim=c(0, 20))








