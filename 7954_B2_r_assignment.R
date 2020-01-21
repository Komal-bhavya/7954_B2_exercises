library(readr)
library(readxl)
library(dplyr)

excel_sheets("C:/Users/komal.sahay/Downloads/SaleData.xlsx")
sale_data <- read_excel("C:/Users/komal.sahay/Downloads/SaleData.xlsx", sheet = "Sales Data")
diamond<-read.csv("C:/Users/komal.sahay/Downloads/diamonds.csv")
meta_movie<-read.csv('C:/Users/komal.sahay/Downloads/movie_metadata.csv')
imdb<-read_delim('C:/Users/komal.sahay/Downloads/imdb.csv',delim = ",", escape_backslash = T, escape_double = F)


#1
q1<- function(df){
  least_sale<-df %>%group_by(Item) %>% summarise(min_sale_amt=min(Sale_amt))
  return(least_sale)
}


#2
q2<- function(df){
  sales_year_data <- df %>% group_by(format(as.Date(OrderDate,format="%Y-%m-%d"),"%Y"),Region) %>% summarise(total_sales = sum(Sale_amt))
  return (sales_year_data)
}

#3
q3<- function(df){
  df$days_diff <- Sys.Date()- as.Date(df$OrderDate,format="%Y-%m-%d")
  return(head(df))
}

#4
q4 <-function(df){
  manager_slsmen<-df %>% group_by(Manager) %>% summarise(list_of_salesman = paste(unique(SalesMan), collapse = ', '))
  return(manager_slsmen)
}

#5
q5<-function(df){
  agg_1 <- aggregate(data=df, SalesMan ~ Region, function(x) length(unique(x)))
  agg_2<-df %>% group_by(Region) %>% summarise(total_sale=sum(Units))
  result<-merge(agg_1, agg_2, by="Region", all=TRUE)
  return(result)
}


#6
q6<- function(df){
  data6 <- data.frame(Manager=df$Manager, Total_sale=df$Sale_amt)
  total_sale_amount= sum(df$Sale_amt)
  result <- data6 %>% group_by(Manager) %>% summarise(percent_sales= sum(Total_sale)*100/total_sale_amount)
  print(result)
}

#7
q7<- function(df){
  return(df[5,]$imdbRating)
}

#8
q8<- function(df){
  min_val<-min(df[,"duration"],na.rm=TRUE)
  max_val<-max(df[,"duration"],na.rm=TRUE)
  min_dur_movie<-filter(df,df$duration==min_val)
  max_dur_movie<-filter(df,df$duration==max_val)
  print("min duration movie:")
  print(min_dur_movie$title)
  print("max duration movie:")
  print(max_dur_movie$title)
  
}


#9
q9<- function(df){
  sorted_df<-df[order(df$year,-df$imdbRating),]
  return(sorted_df)
  
}


#10
q10<-function(df){
  filtered_data<-filter(df,gross>=2000000,budget<1000000,duration >= 30 & duration <=180)
  return(filtered_data)
}

#11
q11<- function(df){
  dup_row<-nrow(df)-nrow(unique(df))
  return(dup_row)
}

#12
q12<-function(df){
  df_updated <- df[-which(df$carat == ""), ]
  df_updated <- df[-which(df$cut == ""), ]
  return(df_updated)
}
q12(diamond)

#13
q13<- function(df){
  numeric_data<-select_if(df, is.numeric)
  return(numeric_data)
}


#14
q14<- function(df){
  df$z<-as.numeric(as.character(df$z))
  i<-1
  while(i<=53943)
  {
    if(df[i,5]>60){
      df[i,"volume"]<-df[i,8]*df[i,9]*df[i,10]
    } else{
      df[i,"volume"]<-8
    }
    i=i+1
  }
  return(df)
}

#15
q15<- function(df){
  df$price[is.na(df$price)] = mean(df$price, na.rm=TRUE)
  return(df)
}