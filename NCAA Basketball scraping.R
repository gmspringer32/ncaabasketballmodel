library(tidyverse)
library(rvest)
library(httr)
library(janitor)
library(plyr)
library(sjmisc)

read_data <- function(url, year){
  
  x <- GET(url, add_headers('user-agent' = 'Gov employment data scraper ([[your email]])'))
  
  df <- x %>% 
    read_html() %>%
    html_node("table") %>%
    html_table(header = FALSE) %>% 
    .[-c(1, 357),] %>% 
    row_to_names(1)
  
  df <- df %>% 
    dplyr::rename('SEED' = Rk, 'TEAM' = Team, 'W' = Rec) %>% 
    filter(TEAM != 'Team')
  
  length <- length(df[[1]])
  
  for (i in 1:length){
    df$POSTSEASON[i] <- str_split(df$TEAM, pattern = ",")[[i]][2]
    df$TEAM[i] <- str_split(df$TEAM, pattern = ",")[[i]][1]
    df$SEED[i] <- str_split(df$TEAM, pattern = "\\s{2,}")[[i]][2]
    df$TEAM[i] <- str_split(df$TEAM, pattern = "\\s{2,}")[[i]][1]
    df$SEED[i] <- str_split(df$SEED, pattern = "\\s")[[i]][1]
    df$W[i] <- str_split(df$W, pattern = "-")[[i]][1]
    
  }
  
  for(col in colnames(df)[6:22]){
    for (i in 1:length){
      if(str_contains(df[[col]][i], '.')){
        df[[col]][i] <- substr(df[[col]], 1,4)[[i]][1]
      }
      else{
        df[[col]][i] <- substr(df[[col]], 1,2)[[i]][1]
      }
    }
  }
  
  df$POSTSEASON <- mapvalues(df$POSTSEASON, 
                             from = c(" CHAMPS",' Finals',' Final Four', ' Elite Eight', ' Sweet Sixteen', ' R32', ' R64', ' R68'),
                             to = c('Champions', '2ND', 'F4', 'E8', 'S16', 'R32', 'R64', 'R68'))
  
  columns<- c(colnames(df[,c(1, 4:22)]))
  
  for(col in columns){
    df[[col]] <- parse_double(df[[col]])
  }
  
  names(df) <- toupper(names(df))
  
  df %>% 
    dplyr::rename('EFG_O' = `EFG%`, 'EFG_D' = `EFGD%`, '2P_O' = `2P%`, '2P_D' = `2P%D`,
                  '3P_O' = `3P%`, '3P_D' = `3P%D`, 'ADJ_T' = `ADJ T.`)
  
  df <- df %>% 
    mutate(YEAR = year)
  
  return(df)
}


ncaab08 <- read_data('https://barttorvik.com/trank.php?&begin=20071101&end=20080501&conlimit=All&year=2008&top=0&quad=5&rpi=#', 2008)
ncaab09 <- read_data('https://barttorvik.com/trank.php?year=2009&sort=&top=0&conlimit=All#', 2009)
ncaab10 <- read_data('https://barttorvik.com/trank.php?year=2010&sort=&top=0&conlimit=All#', 2010)
ncaab11 <- read_data('https://barttorvik.com/trank.php?year=2011&sort=&top=0&conlimit=All#', 2011)
ncaab12 <- read_data('https://barttorvik.com/trank.php?year=2012&sort=&top=0&conlimit=All#', 2012)
ncaab13 <- read_data('https://barttorvik.com/trank.php?year=2013&sort=&top=0&conlimit=All#', 2013)
ncaab14 <- read_data('https://barttorvik.com/trank.php?year=2014&sort=&top=0&conlimit=All#', 2014)
ncaab15 <- read_data('https://barttorvik.com/trank.php?year=2015&sort=&top=0&conlimit=All#', 2015)
ncaab16 <- read_data('https://barttorvik.com/trank.php?year=2016&sort=&top=0&conlimit=All#', 2016)
ncaab17 <- read_data('https://barttorvik.com/trank.php?year=2017&sort=&top=0&conlimit=All#', 2017)
ncaab18 <- read_data('https://barttorvik.com/trank.php?year=2018&sort=&top=0&conlimit=All#', 2018)
ncaab19 <- read_data('https://barttorvik.com/trank.php?year=2019&sort=&top=0&conlimit=All#', 2019)
ncaab20 <- read_data('https://barttorvik.com/trank.php?year=2020&sort=&top=0&conlimit=All#', 2020)
ncaab21 <- read_data('https://barttorvik.com/trank.php?year=2021&sort=&top=0&conlimit=All#', 2021)
ncaab22 <- read_data('https://barttorvik.com/trank.php?year=2022&sort=&top=0&conlimit=All#', 2022)


ncaab <- rbind(ncaab08, ncaab09, ncaab10, ncaab11, ncaab12, ncaab13, ncaab14, 
               ncaab15, ncaab16, ncaab17, ncaab18, ncaab19, ncaab20, ncaab21,
               ncaab22)
write_csv(ncaab, 'ncaab.csv')
