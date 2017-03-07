library(readr)
library(ggplot2)
library(dplyr)
library(tibble)
library(nnet)
library(ROCR)
library(glmnet)
library(rworldmap) # install.packages(rworldmap)
library(maps)
FILE_PATH <- "Desktop/hiv.csv"
hiv_tibble <- read_csv(FILE_PATH)
hiv_tibble <- hiv_tibble[,-c(2:12)] # only 1990-2011.
hiv_tibble <- hiv_tibble[rowSums(is.na(hiv_tibble)) != ncol(hiv_tibble)-1,] # removes rows that have only NA.
colnames(hiv_tibble)[1] <- "Country" # renames column so join can happen.
hiv_tibble$non_na_count <- apply(hiv_tibble, 1, function(x) (sum(!is.na(x))-1))
hiv_tibble$summed <- rowSums( hiv_tibble[,2:23], na.rm=TRUE )
hiv_tibble <- hiv_tibble %>% mutate(mean_hiv_rate=summed/non_na_count)
hiv_tibble <- hiv_tibble[-24] # deletes non_na_count column
hiv_tibble <- hiv_tibble[-24] # deletes summed column
data("countryExData") # loads random data frame I found with package.
hiv_tibble <- left_join(hiv_tibble, countryExData, by="Country")
hiv_tibble <- arrange(hiv_tibble, EPI_regions)
ggplot(hiv_tibble, aes(x=log(GDP_capita.MRYA), y=mean_hiv_rate, size=Population2005, color=EPI_regions))+geom_point() # first plot probably for intro.

