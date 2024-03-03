# 2020 PRESIDENTS POLL 

# In this data set there are two sheet(sheet 1 and sheet 2),both sheet have 17 variables.

## 1.Import library
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(stringr)
library(dplyr)

## 2.read 2020 presidential poll data file
sheet1_data <- read_excel("C://Users//swati//Downloads//presidential_polls_2020.xlsx", sheet = 1)
sheet2_data <- read_excel("C://Users//swati//Downloads//presidential_polls_2020.xlsx", sheet = 2)

## 3. Merged the data file(sheet1_data,sheet2_data)
df_polls<-rbind(sheet1_data,sheet2_data)

## 4. Formatting the data
I upgraded the dataset by switching numbers to dates and words to numbers.Converting numbers to dates allows for chronological analysis, helping understand when events occurred. Transforming words to numbers enables quantitative assessments, facilitating comparisons and statistical analysis across different variables. 

# 4.1 Modeldate(numeric to date)
origin_date<- as.Date("1900-01-01")
df_polls$modeldate<- as.Date(df_polls$modeldate,origin=origin_date)

# 4.2 End date(numeric to date)
df_polls$enddate<- as.Date(df_polls$enddate,origin=origin_date)

# 4.3 Start date(numeric to date)
df_polls$startdate<- as.Date(as.numeric(as.character(df_polls$startdate)),origin = origin_date)

# 4.4 Influence,weight,pct(character to numeric)
df_polls$influence<-as.numeric(df_polls$influence)
df_polls$pct<-as.numeric(df_polls$pct)
df_polls$weight<- as.numeric(df_polls$weight)
str(df_polls)

## 5. cleaning the data file

# 5.1 Missing value in Row-
I deleted rows with missing values because they were making my dataset less reliable for analysis. Removing these rows helps ensure that my data is accurate and unbiased, so I can trust the results of my analysis.

# Cleaned the data with specific rows
cleaned <-df_polls[-c(323:406, 462:544),]
#cleaned the data column wise -
I cleaned the cycle, candidate name, model date, track, poll Id and question Id because this data is 2020 presidential poll data where the column cycle, candidate name and model date represent only one things(2020, Joe biden and 11-05-2020), so that i removed this columns.
Also i removed the track column because its not showing any relevant information about the data. And poll Id and question Id is just a nominal data which provides specific identification number of the poll and question, which doesnot provide any useful information about poll and question this is the reason to remove this column.

df_polls_cleaned <- df_polls[, -c(1, 3, 4, 15, 16, 17)]
df_polls_cleaned<- na.omit(df_polls_cleaned)

# check the frequency of the cleaned data set
get_unique_counts <- function(data) {
  # Use dplyr for concise data manipulation
  library(dplyr)
  
  # Initialize an empty list to store unique counts for each column
  unique_counts <- list() 
  
  # Loop through each column
  for (i in 1:ncol(data)) {
    
    # Get unique values and their counts for the current column
    unique_values <- data[[i]] %>% 
      table() %>% 
      data.frame()
    # Store the data frame for the current column in the list
    unique_counts[[i]] <- unique_values
  }
  
  # Combine individual data frames into a single one
  combined_df <- bind_rows(unique_counts, .id = "column_name")
  
  # Return the combined data frame
  return(combined_df)
}

# Call the function with your data frame
unique_counts <- get_unique_counts(df_polls_cleaned)

# Print the data frame with unique values and counts
print(unique_counts)


# 6.spelling correction for the cleaned data set
# 6.1 Create a lookup table for state corrections
correction_state <- data.frame(incorrect = c("Pennsylvanian", "Pa", "North Carolina", "Nc", "Nati","Wyoming","wy","wisconsin","wi"),
                                correct = c("Pennsylvania", "Pennsylvania", "North Carolina", "North Carolina", "National","Wyoming","Wyoming","Wisconsin","Wisconsin"))

# 6.2 Apply corrections to the "state" column
df_polls_cleaned <- df_polls_cleaned %>%
  mutate(state = case_when(
    state %in% correction_state$incorrect ~ correction_state$correct[match(state, correction_state$incorrect)],
    TRUE ~ as.character(state)
  ))

## creating new column for to identify numbers of datesfor which polling was conducted
df_polls_cleaned$no_of_days<-df_polls_cleaned$enddate-df_polls_cleaned$startdate+1

# 7. plot
# 7.1 scatterplot
I am creating a scatter plot to visualize the relationship between the number of days and sample size. Additionally, I am using population column to represent different colors in the plot,
ggplot(df_polls_cleaned, aes(no_of_days,samplesize, colour=population))+
  geom_point()+
  scale_x_continuous(name = "number of days") +
  scale_y_continuous(name = "samplesize") +
  theme_gray() +
  labs(title = "scatterplot year 2020 Biden")

# 7.2 Box plot
# Here i identify the outliers in this box plot
box_plot_data1<-df_polls_cleaned%>%
  select(value=weight)%>%
  mutate(type="weight")
box_plot_data2<-df_polls_cleaned%>%
  select(value=influence)%>%
  mutate(type="influence")

box_plot_data<-bind_rows(mutate(box_plot_data1),
                         mutate(box_plot_data2))

ggplot(data = box_plot_data)+
  geom_boxplot(mapping = aes( x = reorder(type,value, FUN = median), y = value))+
  coord_flip()+
  labs(title = "Boxplot .",
       x = "type",
       y = "value") +
  scale_fill_manual(values = c("Low" = "blue", "Medium" = "green", "High" = "red")) +
  theme_gray()

# Less than one what impact in the type and value
box_plot_datanew<-box_plot_data%>%
  filter(value<1)

ggplot(data = box_plot_datanew)+
  geom_boxplot(mapping = aes( x = reorder(type,value, FUN = median), y = value))+
  coord_flip()+
  labs(title = "Boxplot .",
       x = "type",
       y = "value") +
  scale_fill_manual(values = c("Low" = "blue", "Medium" = "green", "High" = "red")) +
  theme_gray()

write.csv(df_polls_cleaned,file = "poll survey", row.names=TRUE)

  