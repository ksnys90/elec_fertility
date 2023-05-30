library(readxl)
library(jiebaR)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(rvest)

#reading the files
zhang <- read.csv("taoyuan_zhangshanzheng.csv")
lai <- read.csv("taoyuan_laixiangling.csv")
lin <- read.csv("taoyuan_linzhijian.csv")
peng <- read.csv("taoyuan_zhengyunpeng.csv")

zhang2 <- zhang
lai2 <- lai
lin2 <- lin
peng2 <- peng

#checking the column names
colnames(zhang2)

#tagging posts that mention birth related terms as 1 and others as 0
zhang2$birthkey <- ifelse(grepl("生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢｜少子化｜ 生育率",zhang2$Message),1,0)
lai2$birthkey <- ifelse(grepl("生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢｜少子化｜ 生育率",lai2$Message),1,0)
lin2$birthkey <- ifelse(grepl("生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢｜少子化｜ 生育率",lin2$Message),1,0)
peng2$birthkey <- ifelse(grepl("生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢｜少子化｜ 生育率",peng2$Message),1,0)


#the whole facebook - what are they discussing?
  

#getting a quick summary numeber of the number of posts that mention birth
zhang2_birth <- zhang2 %>%
  filter(birthkey==1) %>%
  summarise(numberofpost=n())

lai2_birth <- lai2 %>%
  filter(birthkey==1) %>%
  summarise(numberofpost=n())

lin2_birth <- lin2 %>%
  filter(birthkey==1) %>%
  summarise(numberofpost=n())

peng2_birth <- peng2 %>%
  filter(birthkey==1) %>%
  summarise(numberofpost=n())


#filtering the rows that contain the post that mention birth

zhang2_birth1 <- zhang2 %>%
  filter(birthkey==1) 
  
lai2_birth1 <- lai2 %>%
  filter(birthkey==1) 
 
lin2_birth1 <- lin2 %>%
  filter(birthkey==1) 

peng2_birth1 <- peng2 %>%
  filter(birthkey==1) 




#filtering the rows that do contain the post that mention birth

zhang2_nobirth <- zhang2 %>%
  filter(birthkey==0) 

lai2_nobirth <- lai2 %>%
  filter(birthkey==0) 

lin2_nobirth <- lin2 %>%
  filter(birthkey==0) 

peng2_nobirth <- peng2 %>%
  filter(birthkey==0) 

            
