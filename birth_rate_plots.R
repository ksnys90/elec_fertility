setwd("/Users/jasperhewitt/Desktop/big data & social analysis/final_project/data/xlsx")
library(readxl)
library(tidyverse)

birth110_df <- read_xlsx("birthrates.xlsx", sheet=1)
birth109_df <- read_xlsx("birthrates.xlsx", sheet=2)
birth108_df <- read_xlsx("birthrates.xlsx", sheet=3)
birth107_df <- read_xlsx("birthrates.xlsx", sheet=4)
birth106_df <- read_xlsx("birthrates.xlsx", sheet=5)
birth105_df <- read_xlsx("birthrates.xlsx", sheet=6)
birth104_df <- read_xlsx("birthrates.xlsx", sheet=7)


#1. Data cleaning
#the government's xlsx sheet was made to be read from xlsx format. Some columns consist of 
#multiple rows, which is a nightmare to import. We therefore have to do some cleaning


# rename columns
colnames(birth110_df) <- c("Area", "Crude_Birth_Rate", 
                        "General_Fertility_Rate", "FRate age_group 15-19", 
                        "FRate age_group 20-24", " FRate age_group 25-29", 
                        "FRate age_group 30-34", "FRate age_group 35-39", 
                        "FRate age_group 40-44", "FRate age_group 45-49)", 
                        "Total_Fertility_Rate")

colnames(birth109_df) <- c("Area", "Crude_Birth_Rate", "General_Fertility_Rate", "FRate age_group 15-19", "FRate age_group 20-24", " FRate age_group 25-29", "FRate age_group 30-34", "FRate age_group 35-39", "FRate age_group 40-44", "FRate age_group 45-49)", "Total_Fertility_Rate")
colnames(birth108_df) <- c("Area", "Crude_Birth_Rate", "General_Fertility_Rate", "FRate age_group 15-19", "FRate age_group 20-24", " FRate age_group 25-29", "FRate age_group 30-34", "FRate age_group 35-39", "FRate age_group 40-44", "FRate age_group 45-49)", "Total_Fertility_Rate")
colnames(birth107_df) <- c("Area", "Crude_Birth_Rate", "General_Fertility_Rate", "FRate age_group 15-19", "FRate age_group 20-24", " FRate age_group 25-29", "FRate age_group 30-34", "FRate age_group 35-39", "FRate age_group 40-44", "FRate age_group 45-49)", "Total_Fertility_Rate")
colnames(birth106_df) <- c("Area", "Crude_Birth_Rate", "General_Fertility_Rate", "FRate age_group 15-19", "FRate age_group 20-24", " FRate age_group 25-29", "FRate age_group 30-34", "FRate age_group 35-39", "FRate age_group 40-44", "FRate age_group 45-49)", "Total_Fertility_Rate")
colnames(birth105_df) <- c("Area", "Crude_Birth_Rate", "General_Fertility_Rate", "FRate age_group 15-19", "FRate age_group 20-24", " FRate age_group 25-29", "FRate age_group 30-34", "FRate age_group 35-39", "FRate age_group 40-44", "FRate age_group 45-49)", "Total_Fertility_Rate")
colnames(birth104_df) <- c("Area", "Crude_Birth_Rate", "General_Fertility_Rate", "FRate age_group 15-19", "FRate age_group 20-24", " FRate age_group 25-29", "FRate age_group 30-34", "FRate age_group 35-39", "FRate age_group 40-44", "FRate age_group 45-49)", "Total_Fertility_Rate")

#drop rows 1:4 
birth110_df <- birth110_df[-(1:4),]
birth109_df <- birth109_df[-(1:4),]
birth108_df <- birth108_df[-(1:4),]
birth107_df <- birth107_df[-(1:4),]
birth106_df <- birth106_df[-(1:4),]
birth105_df <- birth105_df[-(1:4),]
birth104_df <- birth104_df[-(1:4),]

#drop rows 26:33 (the xlsx sheet also contains some information we don't need in the last few 
#rows. Like explainers)
birth110_df <- birth110_df[-(26:33),]
birth109_df <- birth109_df[-(26:33),]
birth108_df <- birth108_df[-(26:33),]
birth107_df <- birth107_df[-(26:33),]
birth106_df <- birth106_df[-(26:33),]
birth105_df <- birth105_df[-(26:33),]
birth104_df <- birth104_df[-(26:33),]

#insert a column with year for every df for later plots in the long format
birth110_df$year <- 110  
birth109_df$year <- 109
birth108_df$year <- 108
birth107_df$year <- 107
birth106_df$year <- 106
birth105_df$year <- 105
birth104_df$year <- 104



#create a new df with just the Area, TFR, and corresponding year 
birth110_small <- birth110_df%>%
  select(Area, year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

birth109_small <- birth109_df%>%
  select(Area, year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

birth108_small <- birth108_df%>%
  select(Area, year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

birth107_small <- birth107_df%>%
  select(Area, year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

birth106_small <- birth106_df%>%
  select(Area, year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

birth105_small <- birth105_df%>%
  select(Area,  year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

birth104_small <- birth104_df%>%
  select(Area,  year, Total_Fertility_Rate)%>%
  filter(Area %in% c("新北市", "臺北市", "臺南市", "高雄市", "臺中市", "桃園市"))

test <-rbind(birth104_small, birth105_small)


#rbind all of them into a df called yearly_TFR using for loop
#first put it in a list
df_list <- list(birth104_small, birth105_small, birth106_small, birth107_small, birth108_small, birth109_small, birth110_small)

df_list[[1]]

#create df called yearly_TFR, immediately assign birth104_small to it so it has a format and can be merged by Area
yearly_TFR <- df_list[[1]]
i=2
#for the rest of the sheets. rbind them to yearly_TFR
for (i in 2:7){
  yearly_TFR <- rbind(yearly_TFR, df_list[[i]])
  i<-i+1
}

#fix the Total_Fertility_Rate
yearly_TFR$Total_Fertility_Rate <- as.numeric(as.character(yearly_TFR$Total_Fertility_Rate))

#We still have to divide the number by 1000 in order to get the normal denotation of TFR
yearly_TFR$Fertility_Rate <- yearly_TFR$Total_Fertility_Rate/1000


library(showtext)
showtext_auto()

ggplot(yearly_TFR, aes(x=year, y=Fertility_Rate, group=Area, color=Area))+
  geom_line()






