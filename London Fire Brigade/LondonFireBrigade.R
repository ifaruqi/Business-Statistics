## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("Warwick_Business_School_logo.svg")


## ---- message = FALSE----------------------------------------------------------------------------------------------------------------------------------

# Install and load the necessary package and library

#install.packages("tidyverse")
#install.packages("grid")
#install.packages("gridExtra")
#install.packages("kableExtra")
#install.packages("Rmisc")
#install.packages("emmeans")
# install.packages("ggpubr")
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(kableExtra)
library(dplyr)
library(ggpubr)
library(emmeans)

options(width=100)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------

# read and load the fire data from the London Fire Brigade
firedata <- read_csv("London_Fire_data (2).csv")

# read and load data dictionary from the London Fire Brigade
datadict <- read_csv("DataDict.csv")



## ---- include = TRUE, results = "hide"-----------------------------------------------------------------------------------------------------------------

# check the summary of the data
summary(firedata)

# check the structure of the data
str(firedata)



## ---- include = TRUE, results = "hide"-----------------------------------------------------------------------------------------------------------------

# Changing Column Name for Convenience
colnames(firedata)[which(names(firedata) == "Notional Cost (£)")] <- "IncidentCost"

# Change DateofCall column into Date date type
firedata$DateOfCall <- as.Date(lubridate::dmy(firedata$DateOfCall))

# Create a new column to indicate the month when incident happens
firedata$MonthOfCall <- as.Date(cut(firedata$DateOfCall, breaks = "month"))

# Change data types for CalYear and IncidentGroup columns
# First generate a vector to keep the column names
columns <- c("CalYear", "IncidentGroup")

# Then, set the correct data types for the defined columns
firedata[columns] <- lapply(firedata[columns], factor)

# To check the new structure of the data after several changes above
str(firedata)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a subset of data to exclude Special Service data
firedata_without_specialservice <- filter(firedata, IncidentGroup != "Special Service")

# Create a summary of Fire and False Alarm Incident
firedata_without_specialservice_summary <- firedata_without_specialservice %>% 
  group_by(IncidentGroup) %>% 
  summarise( "TotalCost" = round(sum(IncidentCost/1000000, na.rm = TRUE),2), 
             "AverageCost" = round(mean(IncidentCost, na.rm = TRUE),2) )

# Add a new column to store a label for the graph
firedata_without_specialservice_summary$'Total Cost' <-
  paste(paste("£",as.character(round(firedata_without_specialservice_summary$TotalCost,1)),"M"))

firedata_without_specialservice_summary$'Average Cost' <-
  paste("£",as.character(round(firedata_without_specialservice_summary$AverageCost,1)))

# Print the requested information
print(firedata_without_specialservice_summary)



## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------
# Create subset of dataset to group the data based on year 
total_cost_per_year <- firedata_without_specialservice %>% 
  group_by(CalYear) %>% 
  summarise("Number of Incident" = n(), "Cost" = sum(IncidentCost/1000000, na.rm = TRUE) )

total_cost_per_year$TotalCost <- paste(paste("£",as.character(round(total_cost_per_year$Cost,1)),"M"))

# Create subset of dataset to group the data based on year and incident group
total_cost_group <- firedata_without_specialservice %>% 
  group_by(CalYear, IncidentGroup) %>% 
  summarise("Incident" = n(), "Cost" = sum(IncidentCost/1000000, na.rm = TRUE))

# Create a new column to save the label for the bar plot
total_cost_group$costlabel <- paste(paste("£",as.character(round(total_cost_group$Cost,1)),"M"))


# Plot a bar chart to visualise total cost of Fire and False Alarm incidents
ggplot(total_cost_group, aes(x = CalYear , y = Cost, fill = IncidentGroup, label = costlabel))+
  annotate("text", x = total_cost_per_year$CalYear, y = total_cost_per_year$Cost+2, 
           label = total_cost_per_year$TotalCost)+
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(size = 2.5, position = position_stack(vjust= 0.5))+

  ggtitle("Total Cost of Fire and False Alarm Incidents per Year") +
  labs(x = "Calendar Year", y = "Incident Cost", caption = "*until January 2022")+
  ylim(0,40)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(title = element_text(face="bold"))+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Incident Group")+
  theme(legend.key.size = unit(0.2, 'cm'))
  


## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------
# Create a subset of data for Monthly Average Cost  
total_cost_per_month <- firedata_without_specialservice %>% 
  group_by(MonthOfCall, IncidentGroup) %>% 
  summarise("Number of Incident" = n(), 
            "Cost" = sum(IncidentCost/1000000, na.rm = TRUE),
            "AverageCost" = mean(IncidentCost, na.rm = TRUE))

# Create variable for Average Respon Time of Fire and False Alarm Incident
mean_responsetime_fire <- round(mean(firedata_without_specialservice$IncidentCost[firedata_without_specialservice$IncidentGroup == "Fire"], na.rm = TRUE))

mean_responsetime_falsealarm <- round(mean(firedata_without_specialservice$IncidentCost[firedata_without_specialservice$IncidentGroup == "False Alarm"], na.rm = TRUE))

mean_responsetime <- c(mean_responsetime_fire, mean_responsetime_falsealarm)


# Creating subset of dataset to group the data based on Type of Incident
mean_cost_group <- firedata_without_specialservice %>% 
  group_by(IncidentGroup) %>% 
  summarise("Incident" = n(), 
            "Cost" = round(sum(IncidentCost/1000000, na.rm = TRUE)),
            "AverageCost" = round(mean(IncidentCost, na.rm = TRUE)))

# Creating a new column to save the label for the line plot
mean_cost_group$costlabel <- paste("mean = £",
                                         as.character(mean_cost_group$AverageCost))

mean_cost_group$datelabel <- as.Date(c("2021-10-01","2021-10-01"))

# Create a plot to illustrate monthly average cost of Fire and False Alarm Incident
ggplot(total_cost_per_month, aes(x = MonthOfCall, y = AverageCost, color = IncidentGroup))+
  geom_line(size = 1)+
  geom_hline(yintercept = mean_cost_group$AverageCost, size = 0.5, 
             linetype = "dashed", col = c("#F8766D","#00BA38"))+
  geom_text(data = mean_cost_group,aes(datelabel, AverageCost +c(125,200),label = costlabel, vjust = 1),
            size = 2.5, show.legend = FALSE)+
  ggtitle("Average Cost of Fire and False Alarm Incident per Month") +
  labs(x = "Month", y = "Average of Incident Cost (£)", color = "Incident Group")+
  ylim(0,1200)+
  theme_classic()+
  scale_x_date(date_labels = "%b-%y", breaks = "2 month")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(title = element_text(face="bold"))+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Incident Group")+
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(axis.text.x = element_text(size=10, angle=90))

# Create a plot to illustrate monthly total cost of Fire and False Alarm Incident
ggplot(total_cost_per_month, aes(x = MonthOfCall, y = Cost, color = IncidentGroup))+
  geom_line(size = 1)+
  geom_hline(yintercept = mean_cost_group$TotalCost, size = 0.5, 
             linetype = "dashed", col = c("#F8766D","#00BA38"))+
  ggtitle("Total Cost of Fire and False Alarm Incident per Month") +
  labs(x = "Month", y = "Total of Cost (M£)", color = "Incident Group")+
  ylim(0,2.5)+
  theme_classic()+
  scale_x_date(date_labels = "%b-%y", breaks = "2 month")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(title = element_text(face="bold"))+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Incident Group")+
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(axis.text.x = element_text(size=10, angle=90))
  


## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------
ggplot(firedata_without_specialservice, aes(IncidentCost))+
  geom_histogram()+
  labs(x = "Incident Cost (£)", y= "Frequency")+
  ggtitle("Distribution of Incident Cost")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(title = element_text(face="bold"))



## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------
ggplot(firedata_without_specialservice, aes(x = PumpHoursRoundUp, y = IncidentCost))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_grid(IncidentGroup ~., scales = "free")+
  labs(x = "Pump Hours (h)", y= "Incident Cost (£)")+
  ggtitle("Relationship between Pump Hours and Incident Cost")+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(title = element_text(face="bold"))


## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------

# Create label for the plot
ResponseTimeLabel <- 
  paste("Mean =", as.character(round(mean(firedata$FirstPumpArriving_AttendanceTime, na.rm=TRUE))),"Seconds")

# Save the mean of Response Time to a variable 
ResponseTimeMean <-
  round(mean(firedata$FirstPumpArriving_AttendanceTime, na.rm=TRUE))

# Plot the distribution of response time with frequency as y axis
responsetime_freqplot <- ggplot(data = firedata)+
  geom_histogram(aes(x=FirstPumpArriving_AttendanceTime,), binwidth = 30, color="black")+
  geom_density(aes(x=FirstPumpArriving_AttendanceTime), fill = "red", alpha = 0.2)+
  geom_vline(aes(xintercept = mean(FirstPumpArriving_AttendanceTime, na.rm = TRUE)), col = "green")+
  annotate("text", x = ResponseTimeMean + 200,label= ResponseTimeLabel, y=50000, col="green")+
  ggtitle("Distribution of Response Time for All Incidents")+
  labs(x = "Response Time (s)", y = "Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))

plot(responsetime_freqplot)

# Plot the distribution of response time with density as y axis
responsetime_densplot <- ggplot(data = firedata)+
  geom_histogram( mapping = (aes(x=FirstPumpArriving_AttendanceTime, y = ..density..)),binwidth = 30,color="black")+
  geom_density(aes(x=FirstPumpArriving_AttendanceTime), fill = "red", alpha = 0.2)+
  geom_vline(aes(xintercept = mean(FirstPumpArriving_AttendanceTime, na.rm = TRUE)), col = "green")+
  annotate("text", x = ResponseTimeMean + 200,label= ResponseTimeLabel, y=0.004, col="green")+
  ggtitle("Distribution of Overall Response Time")+
  labs(x = "Response Time (s)", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))

plot(responsetime_densplot)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a summary of Response Time data
mean_response_time <- firedata %>% 
  group_by(IncidentGroup) %>% 
  summarise("Incident" = n(), 
            "ResponseTime" = mean(FirstPumpArriving_AttendanceTime, na.rm = TRUE))

# Creating a new column to save the label for the vline
mean_response_time$'ResponseLabel' <- paste(as.character(round(mean_response_time$ResponseTime)),"Seconds")



# Print the requested information in a proper table format
mean_response_time[,c("IncidentGroup", "ResponseLabel")] %>%
  kbl(col.names = c("Incident Group", "Average Response Time"),
      caption = "Table 2. Summary of Average Response Time for Each Incident Group") %>%
  kable_styling()
  


## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------
# Frequency Plot
responsetime_group_freqplot <- ggplot(data = firedata)+
  geom_histogram(mapping = (aes(x=FirstPumpArriving_AttendanceTime, fill = IncidentGroup,)), color="black")+
  geom_vline(data = mean_response_time,aes(xintercept = ResponseTime), col = "red", size = 1)+
  geom_text(data = mean_response_time,aes(x=ResponseTime + 300, label=ResponseLabel,
                                          y=20000),colour="black")+
  #ggtitle("Distribution of Response Time for Each Incident Group")+
  labs(x = "Response Time (s)", y = "Frequency")+
  facet_grid(IncidentGroup ~.)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(legend.position = "none")

plot(responsetime_group_freqplot)

# Density Plot
responsetime_group_densplot <- ggplot(data = firedata)+
  geom_histogram(mapping = (aes(x=FirstPumpArriving_AttendanceTime, y = ..density..,  
                                fill = IncidentGroup,)), color="black")+
  geom_density(aes(x=FirstPumpArriving_AttendanceTime), fill = "red", alpha = 0)+
  geom_vline(data = mean_response_time, aes(xintercept = ResponseTime), col = "red", size = 1)+
  geom_text(data = mean_response_time, aes(x=ResponseTime + 300, label=ResponseLabel, 
                                           y=0.003), colour = "red")+
  #ggtitle("Distribution of Response Time for Each Incident Group")+
  labs(x = "Response Time (s)", y = "Density")+
  facet_grid(IncidentGroup ~.)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(legend.position = "none")

plot(responsetime_group_densplot)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating data frame for Special Service Incidents
special_service_data <- firedata %>% 
  filter(IncidentGroup == 'Special Service')



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate Average Response Time for Special Service Incident
ss_response_time_mean <-
  round(mean(special_service_data$FirstPumpArriving_AttendanceTime, na.rm = TRUE))
ss_response_time_label <-
  paste("Mean: ",paste(round(mean(special_service_data$FirstPumpArriving_AttendanceTime, na.rm = TRUE)), "s"))

# Calculate 10th Percentile of Response Time for Special Service Incident
ss_response_time_10th <-
  round(quantile(special_service_data$FirstPumpArriving_AttendanceTime, probs= 0.1, na.rm = TRUE))
ss_response_time_10_th_label <-
  paste("10th Percentile:",paste(round(quantile(special_service_data$FirstPumpArriving_AttendanceTime, probs= 0.1, na.rm = TRUE)), "s"))

# Calculate 90th Percentile of Response Time for Special Service Incident
ss_response_time_90th <-
  round(quantile(special_service_data$FirstPumpArriving_AttendanceTime, probs= 0.9, na.rm = TRUE))
ss_response_time_90_th_label <-
  paste("90th Percentile: ",paste(round(quantile(special_service_data$FirstPumpArriving_AttendanceTime, probs= 0.9, na.rm = TRUE)), "s"))

# Create a data frame to create multiple vertical line in histogram (as annotation)
ss_line_value <- c(ss_response_time_10th, ss_response_time_mean, ss_response_time_90th)
ss_line_color <- c("red", "green", "blue")
ss_line_label <-c(ss_response_time_10_th_label, ss_response_time_label, ss_response_time_90_th_label)
multi_vline <- data.frame(ss_line_value, ss_line_color, ss_line_label)




## ---- message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------
# Plot the distribution of Response Time for Special Service Incident
ggplot(data = special_service_data)+
  geom_density(aes(x=FirstPumpArriving_AttendanceTime), fill = "red", alpha = 0.2)+
  geom_vline(data = multi_vline, aes(xintercept = ss_line_value), col = ss_line_color)+
  geom_text(data = multi_vline, 
            aes(x=ss_line_value + c(-120,70,120), label=ss_line_label, y=0.005), 
            col = ss_line_color, size = 2.5)+
  ggtitle("Distribution of Response Time to Special Service Incidents")+
  labs(x = "Response Time (Second)", y = "Density")+
  scale_x_continuous(limits = c(0,1300),breaks=c(0, 174, 318, 480, 750, 1250))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))



## ------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(special_service_data, aes(FirstPumpArriving_AttendanceTime))+
  stat_ecdf(geom = "step", na.rm = TRUE, pad = FALSE)+
  geom_segment(aes(x = -Inf, y = 0.1, xend = 174, yend = 0.1), linetype = "dashed", col = "red")+
  geom_segment(aes(x = 174, y = -Inf, xend = 174, yend = 0.1), linetype = "dashed", col = "red")+
  geom_segment(aes(x = -Inf, y = 0.55, xend = 318, yend = 0.55), linetype = "dashed", col = "green")+
  geom_segment(aes(x = 318, y = -Inf, xend = 318, yend = 0.55), linetype = "dashed", col = "green")+
  geom_segment(aes(x = -Inf, y = 0.9, xend = 480, yend = 0.9), linetype = "dashed", col = "blue")+
  geom_segment(aes(x = 480, y = -Inf, xend = 480, yend = 0.9), linetype = "dashed", col = "blue")+
  annotate("text", x = c(30,12,30),y = c(0.08,0.53,0.88), 
           label = c("10th Percentile", "Mean", "90th Percentile"), 
           col = c("red", "green", "blue"),
           size = 2.5)+
  ggtitle("Empirical Cumulative Distribution Function of Response Time",
          subtitle = "Special Service Incident")+
  scale_x_continuous(limits = c(0,1300),breaks=c(0, 174, 318, 480, 750, 1250))+
  scale_y_continuous(limits = c(0,1),breaks=c(0, 0.1, 0.55, 0.9, 1))+
  labs(x = "Response Time (s)", y = "F (Response Time)")+
  theme_classic()+
  theme(title = element_text(face="bold"))



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a summary of mean, 10th percentile and 90th percentile of response time for each Special Service Type
special_service_summary <-special_service_data %>% 
  filter(is.na(SpecialServiceType)==FALSE) %>%
  group_by(SpecialServiceType) %>% 
  summarise("Frequency" = n(), 
            "MeanResponseTime" = round(mean(FirstPumpArriving_AttendanceTime, na.rm = TRUE)),
            "Percentile10_ResponseTime" = round(quantile(FirstPumpArriving_AttendanceTime, probs = 0.1, 
                                               na.rm = TRUE)),
            "Percentile90_ResponseTime" = round(quantile(FirstPumpArriving_AttendanceTime, probs = 0.9, 
                                         na.rm = TRUE)),
            "MaxResponseTime" = round(max(FirstPumpArriving_AttendanceTime, na.rm = TRUE),2),
            "MinResponseTime" = round(min(FirstPumpArriving_AttendanceTime, na.rm = TRUE),2)
            ) 

# Removing NA values
special_service_data <- special_service_data[!is.na(special_service_data$SpecialServiceType),]

# Arrange based on mean value
special_service_summary <- special_service_summary %>% 
  arrange(desc(MeanResponseTime))

# Print the requested information in a proper table format
print(special_service_summary)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(special_service_summary, aes(x=reorder(SpecialServiceType, -MeanResponseTime),
                                    y=MeanResponseTime)) + 
  geom_point() + 
  geom_crossbar(aes(ymin = Percentile10_ResponseTime, ymax = Percentile90_ResponseTime), width = 0.5)+
  geom_errorbar(aes(ymin = MinResponseTime, ymax = MaxResponseTime), width = 0.5)+
  ggtitle("Distribution of Response Time for Each Type of Special Service Incident")+
  labs(y="Response Time (s)", x="Special Service Type")+ 
  ylim(0,1200)+
  theme_classic()+
  theme(axis.text.x = element_text(size=10, angle=90))+
  theme(plot.title = element_text(hjust = 0))+
  theme(title = element_text(face="bold"))
 


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot distribution of the mean value of each Special Service Type
ggplot(special_service_summary, aes(MeanResponseTime))+
  geom_histogram(binwidth = 10)+
  labs(x = "Response Time (s)", y = "Frequency")+
  ggtitle("Distribution of Average Response Time for Each Special Service Incident")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Filtering Ealing & Greenwich data
EalingGreenwich <- firedata %>% 
  filter(ProperCase %in% c("Ealing", "Greenwich"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
EalingGreenwich_summary <- EalingGreenwich %>% 
 	group_by(ProperCase) %>% 
	summarise("Average of Response Time" = round(mean(FirstPumpArriving_AttendanceTime, na.rm = TRUE)), 
	          "Standard Deviation of Response Time" = sd(FirstPumpArriving_AttendanceTime, na.rm=TRUE), 
	          "Frequency" = n()) 

# Change the column name to be use as legend title
colnames(EalingGreenwich_summary)[which(names(EalingGreenwich_summary) == "ProperCase")] <- "Area"


# Print the requested information
print(EalingGreenwich_summary)



## ---- warning = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------
ggplot(EalingGreenwich, aes(FirstPumpArriving_AttendanceTime,..density.., fill=ProperCase))+
  geom_histogram(binwidth=30,position="identity", alpha=0.5)+ 
  labs(x="Response Time (s)", y="Density", fill="Area")+
  ggtitle("Distribution of Response Time for Ealing and Greenwich Area")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))
  


## ------------------------------------------------------------------------------------------------------------------------------------------------------
t.test(FirstPumpArriving_AttendanceTime ~ ProperCase, data = EalingGreenwich)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
ResponseTime_by_Location <- lm(FirstPumpArriving_AttendanceTime ~ ProperCase, data = EalingGreenwich)
(ResponseTime_by_Location_Emm <- emmeans(ResponseTime_by_Location, ~ProperCase))
(ResponseTime_by_Location_Contrast <- confint(pairs(ResponseTime_by_Location_Emm)))  



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot Estimated Average Response Time in Ealing and Greenwich
EalingGreenwich_Plot <- 
  ggplot(summary(ResponseTime_by_Location_Emm), aes(x=ProperCase, y=emmean, ymin=lower.CL, ymax=upper.CL)) +
  geom_point() + 
  geom_linerange() + 
  labs(y="Response Time (s)", x="Area", subtitle="Error bars are 95% CIs",
		     title="Response Time in Ealing and Greenwich")+
  scale_y_continuous(breaks = c(309, 311, 314, 317, 320))+
  theme(title = element_text(face="bold", size = 6))

# Plot Estimated Difference of Average Response Time between Ealing and Greenwich
EalingGreenwich_difference_Plot <-
  ggplot(ResponseTime_by_Location_Contrast, aes(x=contrast, y=estimate, ymin=lower.CL, ymax=upper.CL)) +
  geom_point() + 
  geom_linerange() + 
  labs(y="Difference of Response Time (s)", x="Area", subtitle="Error bars are 95% CIs",
       title="Difference of Response Time between Ealing and Greenwich") + 
  scale_y_continuous(breaks = c(0, 1.73, 5.5, 9.38))+
  geom_hline(yintercept=0, lty=2)+
  theme(title = element_text(face="bold", size = 6))

# Arrange both plot into a single plot
grid.arrange(EalingGreenwich_Plot, EalingGreenwich_difference_Plot, ncol = 2)


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
# Print the requested information in a proper table format
firedata_without_specialservice_summary[,c("IncidentGroup", "Total Cost", "Average Cost")] %>%
  kbl(col.names = c("Incident Group", "Total Cost", "Average Cost"),
      caption = "Table 1. Summary of Fire and False Alarm Cost for 2019-2022") %>%
  kable_styling()


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
plot(responsetime_densplot)


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
# Plot the distribution of response time for each incident group
grid.arrange(responsetime_group_densplot, responsetime_group_freqplot, ncol = 2, 
             top = text_grob("Distribution of Response Time for Each Incident Group", size = 15, face = "bold"))


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
special_service_summary[,c("SpecialServiceType", 
              "Frequency",
              "MeanResponseTime",
              "Percentile10_ResponseTime",
              "Percentile90_ResponseTime")]%>%
  kbl(col.names = c("Special Service Type", "Total Incident", "Average Response Time (s)",
                    "10th Percentile of Response Time (s)","90th Percentile of Response Time (s)"),
      caption = "Table 2. Response Time for Special Service Incident") %>%
  kable_styling() %>% 
  row_spec(which(special_service_summary$MeanResponseTime > 318), bold = T, color = "red", background = "white") %>% 
  row_spec(which(special_service_summary$MeanResponseTime < 250), bold = T, color = "black", background = "orange")


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
EalingGreenwich_summary %>%
  kbl(caption = "Table 3. Average of Response Time in Ealing and Greenwich") %>%
  kable_styling()


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
grid.arrange(EalingGreenwich_Plot, EalingGreenwich_difference_Plot, ncol = 2)

