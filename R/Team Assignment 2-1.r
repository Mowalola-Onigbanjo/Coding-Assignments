############################
#####Team 2 Code
############################

### install packages

install.packages("qdap")
install.packages('rJava')


###Supplementary work###
##Importing relevant libraries
library(ggplot2)
library(mlbench)
library(readxl)
library(rJava)
library(qdap)
library(magrittr)
library(stringr)
library(dplyr)
library(plotly)
library(tidyverse)

##Importing data
doubleclick_data <- read_excel("Desktop/Hult Academics/MBAN/1st Semester/R/Datasets/Air France Case Spreadsheet Supplement.xls", sheet = "DoubleClick")
kayak_data <- read_excel("Desktop/Hult Academics/MBAN/1st Semester/R/Datasets/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "Kayak", range = "B8:H9")

##Creating functions
#Summary statistics
my_stats <- function(x){
  my_min <- min(x,na.rm = T)
  my_mu <- mean(x,na.rm = T)
  my_sigma <- sd(x,na.rm = T)
  my_max <- max(x,na.rm = T)
  return(c(my_min, my_mu, my_sigma, my_max))
}#closing my_stats

#Normalization
my_normalize <- function(x){
  my_min <- min(x, na.rm = T)
  my_max <- max(x, na.rm = T)
  min_max <- (x - my_min)/(my_max - my_min)
  return(min_max)
}#closing my_normalize

#Header 
my_header <- function(x) {
  names(x) <- as.character(unlist(x[1,]))
  x[-1,]
}#closing my_header

###Massaging data###
##Converting data to a data frame
doubleclick_df <- as.data.frame(doubleclick_data)
kayak_df <- as.data.frame(kayak_data)

##Checking for blank items
sapply(doubleclick_df, function(x) sum(is.na(x)))
#Blank items are located on a variable not relevant to the analysis so it will not be touched

##Creating relevant data
#Avg ticket
doubleclick_df$`Avg Ticket` <- (doubleclick_df$Amount/doubleclick_df$`Total Volume of Bookings`)
#Net revenue  
doubleclick_df$`Net Revenue` <- (doubleclick_df$Amount - doubleclick_df$`Total Cost`)

#ROA/ROI for main data
doubleclick_df$ROI <- ((doubleclick_df$`Net Revenue`/doubleclick_df$`Total Cost`)*100)

#Checking for infinite variables due to ROI calculation
sapply(doubleclick_df, function(x) sum(is.infinite(x)))

#Replacing infinite variables with NA
is.na(doubleclick_df) <- sapply(doubleclick_df, is.infinite)

#Double checking for infinite variables
sapply(doubleclick_df, function(x) sum(is.infinite(x)))

#ROI for Kayak
kayak_df$ROI <- ((kayak_df$`Net Revenue`/kayak_df$`Media Cost`)*100)

##Normalizing relevant data
doubleclick_df$Clicks_norm <- my_normalize(doubleclick_data$Clicks)
doubleclick_df$Avg.Pos_norm <- my_normalize(doubleclick_data$`Avg. Pos.`)
doubleclick_df$Impressions_norm <- my_normalize(doubleclick_data$Impressions)
doubleclick_df$`Total Cost/Transaction_norm` <- my_normalize(doubleclick_data$`Total Cost/ Trans.`)
doubleclick_df$Amount_norm <- my_normalize(doubleclick_data$Amount)
doubleclick_df$`Total Cost_norm` <- my_normalize(doubleclick_data$`Total Cost`)
doubleclick_df$`Total Volume of Bookings_norm` <- my_normalize(doubleclick_data$`Total Volume of Bookings`)

###Analysis###
##Sampling data set for regression
train_index <- sample(1:nrow(doubleclick_df), size = 0.8*nrow(doubleclick_df))
train_data <- doubleclick_df[train_index, ]
test_data <- doubleclick_df[-train_index, ]

##Regression models
#Linear Regression - Total Volume of Bookings
doubleclick_logit <- lm(`Total Volume of Bookings` ~ Clicks+`Avg. Pos.`+Impressions, data = train_data)
 
#Plotting regression
plot(`Total Volume of Bookings` ~ Clicks+`Avg. Pos.`+Impressions, data = train_data)
abline(doubleclick_logit)

#Summary - Findings: Clicks & Impressions significant, Avg. Pos insignificant
booking_reg_sum <- summary(doubleclick_logit)
print(booking_reg_sum)


#Interpreting coefficients, use exp function
#exp(estimated coefficent) - 1
exp(1.312e-02) - 1 #clicks 
exp(-8.679e-02) - 1 #Avg. Pos
exp(-5.216e-05) - 1 #Impressions

#For every increase in clicks, 0.013 bookings are generated
#For every increase in Avg. Pos, -0.083 bookings are generated
#For every increase in impressions, -0.000005 bookings are generated

#Analyzing coefficients of clicks vs impressions - Clicks wins
doubleclick_logit_norm <- lm(`Total Volume of Bookings` ~ Clicks_norm+Avg.Pos_norm+Impressions_norm, data = train_data)
summary(doubleclick_logit_norm)
##Subsetting based on campaign
#doubleclick_df <- doubleclick_data[ which(doubleclick_df$`Publisher Name` == ),]

#Subsetting by different providers
sub_google_us <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "Google - US"),]
sub_google_global <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "Google - Global"),]
sub_msn_us <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "MSN - US"),]
sub_msn_global <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "MSN - Global"),]
sub_overture_us <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "Overture - US"),]
sub_overture_global <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "Overture - Global"),]
sub_yahoo_us <- doubleclick_df[ which(doubleclick_df$`Publisher Name` == "Yahoo - US"),]


##Creating summary table of relevant data
kayak_vec <- c(kayak_df$`Search Engine`,kayak_df$Clicks, kayak_df$`Media Cost`, kayak_df$`Total Bookings`, kayak_df$`Avg Ticket`, kayak_df$`Total Revenue`, kayak_df$`Net Revenue`, kayak_df$ROI)
google_us_vec <- c("Google US", sum(sub_google_us$Clicks), sum(sub_google_us$`Total Cost`), sum(sub_google_us$`Total Volume of Bookings`), mean(sub_google_us$`Avg Ticket`,na.rm = TRUE), sum(sub_google_us$Amount), sum(sub_google_us$`Net Revenue`), mean(sub_google_us$ROI,na.rm = TRUE))
google_global_vec <- c("Google Global", sum(sub_google_global$Clicks), sum(sub_google_global$`Total Cost`), sum(sub_google_global$`Total Volume of Bookings`), mean(sub_google_global$`Avg Ticket`,na.rm = TRUE), sum(sub_google_global$Amount), sum(sub_google_global$`Net Revenue`), mean(sub_google_global$ROI,na.rm = TRUE))
msn_us_vec <- c("MSN US", sum(sub_msn_us$Clicks), sum(sub_msn_us$`Total Cost`), sum(sub_msn_us$`Total Volume of Bookings`), mean(sub_msn_us$`Avg Ticket`,na.rm = TRUE), sum(sub_msn_us$Amount), sum(sub_msn_us$`Net Revenue`), mean(sub_msn_us$RO,na.rm = TRUE))
msn_global_vec <- c("MSN Global", sum(sub_msn_global$Clicks), sum(sub_msn_global$`Total Cost`), sum(sub_msn_global$`Total Volume of Bookings`), mean(sub_msn_global$`Avg Ticket`,na.rm = TRUE), sum(sub_msn_global$Amount), sum(sub_msn_global$`Net Revenue`), mean(sub_msn_global$ROI,na.rm = TRUE))
overture_us_vec <- c("Overture US", sum(sub_overture_us$Clicks), sum(sub_overture_us$`Total Cost`), sum(sub_overture_us$`Total Volume of Bookings`), mean(sub_overture_us$`Avg Ticket`,na.rm = TRUE), sum(sub_overture_us$Amount), sum(sub_overture_us$`Net Revenue`), mean(sub_overture_us$ROI,na.rm = TRUE))
overture_global_vec <- c("Overture Global", sum(sub_overture_global$Clicks), sum(sub_overture_global$`Total Cost`), sum(sub_overture_global$`Total Volume of Bookings`), mean(sub_overture_global$`Avg Ticket`,na.rm = TRUE), sum(sub_overture_global$Amount), sum(sub_overture_global$`Net Revenue`), mean(sub_overture_global$ROI,na.rm = TRUE))
yahoo_us_vec <- c("Yahoo US", sum(sub_yahoo_us$Clicks), sum(sub_yahoo_us$`Total Cost`), sum(sub_yahoo_us$`Total Volume of Bookings`), mean(sub_yahoo_us$`Avg Ticket`,na.rm = TRUE), sum(sub_yahoo_us$Amount), sum(sub_yahoo_us$`Net Revenue`), mean(sub_yahoo_us$ROI,na.rm = TRUE))

#Creating dataframe for relevant data
provider_summary<-data.frame(kayak_vec, google_us_vec, google_global_vec, msn_us_vec, msn_global_vec, overture_us_vec, overture_global_vec, yahoo_us_vec)

#Naming columns
provider_summary <- my_header(provider_summary)

#Naming rows
rownames(provider_summary) <- c("Clicks", "Media Cost", "Total Bookings", "Ave Ticket","Total Revenue", "Net Revenue", "Ave ROI")

#Changing data type from chr to numeric
provider_summary$Kayak <- as.numeric(provider_summary$Kayak)
provider_summary$`Google US` <- as.numeric(provider_summary$`Google US`)
provider_summary$`Google Global` <- as.numeric(provider_summary$`Google Global`)
provider_summary$`MSN US` <- as.numeric(provider_summary$`MSN US`)
provider_summary$`MSN Global` <- as.numeric(provider_summary$`MSN Global`)
provider_summary$`Overture US` <- as.numeric(provider_summary$`Overture US`)
provider_summary$`Overture Global` <- as.numeric(provider_summary$`Overture Global`)
provider_summary$`Yahoo US` <- as.numeric(provider_summary$`Yahoo US`)

#Kayak and MSN global have the highest ROI but it might be worth trying to do a weighted score to evaluate ROI, Net Revenue, Clicks, and etc. 
provider_summary <- data.frame(t(provider_summary))

## Weighted score analysis
provider_summary$weighted_score <- c()

rowsinprov <-nrow(provider_summary)

for (i in 1: rowsinprov) {
  provider_summary$weighted_score[i] <- (0.05*provider_summary$Clicks[i] + 
    0.9*provider_summary$Ave.ROI[i] + 
    0.025*provider_summary$Ave.Ticket[i] + 
    0.025*provider_summary$Net.Revenue[i])/1000
  
}#closing the for loop

#Cleaning decimals by making the data points integers
provider_summary$weighted_score <- lapply(provider_summary$weighted_score,as.integer)

##Subsetting provider's weighted scores
#Saving provider names for columns
col_provider_names = c("Kayak", "Google US", "Google Global", "MSN US", "MSN Global", "Overture US", "Overture Global", "Yahoo US")

provider_weighted <- as.data.frame(provider_summary$weighted_score, row.names = "Weighted Score", col.names = col_provider_names)

#####################################
####SHIT TO PLAY AROUND WITH
####################################

##### Bar chart to compare differents SEM on the data from the provider summary data frame
x <- c('Kayak', 'MSN-Global', 'MSN - US','Yahoo-US', 'Google-Global', 'Overture-Global', 'Google-US', 'Overture-US')
y <- c(64.5, 11.6, 2.18, 11.3, 5.69, 5.48, 2.22, 2.22)
data <- data.frame(x, y)

# The alphabetic default order so we need to code the following:
data$x <- factor(data$x, levels = data[["x"]])

p <- plot_ly(data, x= ~x, y= ~y, type = "bar", name = "Return on Advertising", color = I("blue"), alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

##### Creating weighted scores
new_germ$my_score <- c()

rowsingerman <-nrow(new_germ)

for (i in 1: rowsingerman) {
  new_germ$my_score[i] <- 0.5*new_germ$duration[i] + 
    0.1*new_germ$amount[i] + 
    0.1*new_germ$age[i] + 
    0.3*new_germ$installp[i]
  
}#closing the for loop

af_dataframe <- as.data.frame(Air_France_Case_Spreadsheet_Supplement)

colnames(af_dataframe) %<>% str_replace_all("\\s", "_") %<>% tolower()

AF_df <- as.numeric(nrow(af_dataframe))

for (i in 1:AF_df) {
  af_dataframe$score[i] <- 0.01*af_dataframe$clicks[i] +
    20*af_dataframe$avg._cost_per_click[i] +
    0.01*af_dataframe$total_cost[i]
  
}#closing loop

##### Create a new data frame for the analysis

AFdf <- AirFdf

Statistics <- c("Mean", "Median", "SD", "Min", "Max")
Amount <- round(c(mean(AFdf$Amount),median(AFdf$Amount), sd(AFdf$Amount),min(AFdf$Amount), max(AFdf$Amount)), 2)
Total_Cost <- round(c(mean(AFdf$`Total Cost`),median(AFdf$`Total Cost`),sd(AFdf$`Total Cost`),min(AFdf$`Total Cost`),max(AFdf$`Total Cost`)), 2)
Impressions <- round(c(mean(AFdf$Impressions),median(AFdf$Impressions),sd(AFdf$Impressions),min(AFdf$Impressions),max(AFdf$Impressions)), 2)
Clicks <- round(c(mean(AFdf$Clicks),median(AFdf$Clicks),sd(AFdf$Clicks),min(AFdf$Clicks),max(AFdf$Clicks)), 2)
Summary <- as.data.frame(cbind(Statistics, Amount, Total_Cost, Impressions, Clicks))

###### Create New Columns    


AFdf <- AFdf[-c(338),]

####### removing a Total Cost = 0 that generate an infinite value

AFdf$Revenue <- AFdf$Amount - AFdf$`Total Cost`
AFdf$ROA <- as.numeric(AFdf$Revenue / AFdf$`Total Cost`)
AFdf$Book_Prob <- ( AFdf$`Trans. Conv. %` * AFdf$`Engine Click Thru %`) /100
AFdf <- within(AFdf, Cost_Book <- `Total Cost`/ `Total Volume of Bookings`)
AFdf[AFdf==""] <- 0
AFdf[AFdf=="Inf"] <- 0
AFdf$Average_Revenue_Booking <- AFdf$Amount / AFdf$`Total Volume of Bookings`
summary(AFdf) 


###### Checking Data Frame 


summary(AFdf)
typeof(AFdf$`Match Type`)
head(AFdf)


######## Create Pivot Table in R to compare ROA and Avg cost per click

AFdf_pivot <- AFdf %>%
  group_by(`Publisher Name`) %>%
  summarize(avg_ROA = mean(ROA),
            avg_cpc = mean(`Avg. Cost per Click`))

summary(AFdf_pivot)

#######Create Pivot Table For  Bubble Chart to   compare different SEM 


AFdf_pivot2 <- AFdf %>% group_by(`Publisher Name`) %>% summarize(
  total_records = n(),
  total_amount = sum(`Total Cost`),
  avg_cpc = mean(`Avg. Cost per Click`),
  avg_prob = mean(`Book_Prob`),
  avg_ROA = mean(ROA),
)

summary(AFdf_pivot2)


######### Bar Plot 



# Bar chart to compare differents SEM
x <- c('Kayak', 'MSN-Global', 'MSN - US','Yahoo-US', 'Google-Global', 'Overture-Global', 'Google-US', 'Overture-US')
y <- c(64.5, 11.6, 2.18, 11.3, 5.69, 5.48, 2.22, 2.22)
data <- data.frame(x, y)

#The alphabetic default order so we need to code the following:
data$x <- factor(data$x, levels = data[["x"]])

p <- plot_ly(data, x= ~x, y= ~y, type = "bar", name = "Return on Advertising", color = I("blue"), alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

# Bar chart to compare the Top Two SEM

w <- c('Kayak', 'MSN-Global')
z <- c(64.5, 11.6)
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

p <- plot_ly(data2, x= ~w, y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

# Bar chart to compare the Top Two SEM

e <- c('Kayak Website', 'Other Online Travel-Sites')
t <- c(8, 0.8)
data2 <- data.frame(e, t)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["e"]])

p <- plot_ly(data2, x= ~e, y= ~t, type = "bar", mode = "lines+markers+text", name = "Click-Through Rate", color = I("red"), alpha = 0.5, width = 0.1) %>%
  layout(title = " Click-Through Rate (%) ",
         xaxis = list(title = ""),
         yaxis = list(title = "")
  )
p



########## Bubble Chart 


p <- plot_ly(AFdf_pivot2, x = ~avg_prob, y = ~avg_ROA,
             textposition = "auto",
             type = 'scatter', 
             mode = 'markers', 
             size = ~avg_cpc, 
             color = ~`Publisher Name`, 
             colors = 'Paired',
             marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Publisher Strategy',
         xaxis = list(title = "Probability of Booking", showgrid = TRUE),
         yaxis = list(title = "Average ROA", showgrid = TRUE),
         showlegend = TRUE)

p

summary(AFdf_pivot2)

p <- plot_ly(AFdf_pivot2, x = ~avg_prob, y = ~avg_cpc,
             textposition = "auto",
             type = 'scatter', 
             mode = 'markers', 
             size = ~avg_cpc,
             color = ~`Publisher Name`, 
             colors = 'Paired',
             marker = list(opacity = 0.8, sizemode = 'diameter',
                           hoverinfo = 'text')) %>%
  layout(title = 'Spending by Platform',
         xaxis = list(title = "Probability of Booking", showgrid = TRUE),
         yaxis = list(title = "Average cost per click", showgrid = TRUE),
         showlegend = TRUE)

p


# Create a new data frame for the analysis

kwdf <- AirFdf

View(kwdf)

# Filter the Data with ROA higher than 0

kwdf <- filter(kwdf, ROA > 0)

nrow(kwdf)

# divide data to do an analysis  by channel

# Google_us
Gusdf <- kwdf[which(kwdf$`Publisher Name` == "Google - US"),]

# Yahoo_us
Yusdf<-kwdf[which(kwdf$`Publisher Name` == "Yahoo - US"),]

# MSN_us
msn_usdf<-kwdf[which(kwdf$`Publisher Name` == "MSN - US"),]

# Overture_us_df
Ousdf<-kwdf[which(kwdf$`Publisher Name` == "Overture - US"),]

# MSN_global
msn_gldf<-kwdf[which(kwdf$`Publisher Name` == "MSN - Global"),]

# google_global
ggldf<-kwdf[which(kwdf$`Publisher Name` == "Google - Global"),]

# overture_global_df
Ogldf<-kwdf[which(kwdf$`Publisher Name` == "Overture - Global"),]


####### Keyword Analysis


# Isolate text from kwdf: Glob_kw stands for Global keywords
Glob_kw <- kwdf$Keyword
View(Glob_kw)

# Create a Barchart to visualize the most common keywords

# Create frequency for the 4510 obs 
frequency1 <- freq_terms(AFdf$Keyword,
                         top = 10,
                         at.least = 3, 
                         stopwords = c(tm::stopwords("en"))
)


# Make a frequency barchart
plot(frequency1)

# Pie Chart with Percentages (Booking by publisher)
require("RColorBrewer")
slices <- c(393,2071,99,98,553,661,635)
names <- c("Google - Global", "Google - US", "MSN - Global", "MSN - US", "Overture - Global", "Overture - US", "Yahoo - US")
pct <- round(slices/sum(slices)*100)
names <- paste(names, pct) # add percents to labels
names <- paste(names,"%",sep="") # ad % to labels
pie(slices,labels = names, col=rainbow(length(names)),
    main="Booking by Publisher")

pie(slices, labels = names, main="Booking by Publisher")

###############################################################################




