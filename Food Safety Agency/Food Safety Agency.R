## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("Warwick_Business_School_logo.svg")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Install and load the necessary package and library

#install.packages("tidyverse")
#install.packages("grid")
#install.packages("gridExtra")
#install.packages("kableExtra")
#install.packages("Rmisc")
#install.packages("emmeans")
# install.packages("ggpubr")
library(tidyverse)
library(tidyr)
library(lubridate)
library(grid)
library(gridtext)
library(gridExtra)
library(kableExtra)
library(dplyr)
library(ggpubr)
library(emmeans)
library(Hmisc)
library(car)

options(width=100)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------
# read and load the fire data from the London Fire Brigade
ratingdata <- read_csv("2019-20-enforcement-data-food-hygiene.csv")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# check the summary of the data
summary(ratingdata)

# check the structure of the data
str(ratingdata)


## ------------------------------------------------------------------------------------------------------------------------------------------------------

# Change data types for Country and LAtype columns
# First generate a vector to keep the column names
columns_factor <- c("Country", "LAType")
columns_num <- c("Total%ofBroadlyCompliantestablishments-A", "Total%ofInterventionsachieved-premisesratedA")

# Then, set the correct data types for the defined columns
ratingdata[columns_factor] <- lapply(ratingdata[columns_factor], factor)

# To check the new structure of the data after several changes above
str(ratingdata)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Changing Column Name for Convenience
colnames(ratingdata)[which(names(ratingdata) == "Total%ofInterventionsachieved(premisesratedA-E)")] <- "OverallInterventionsAchieved"
colnames(ratingdata)[which(names(ratingdata) == "Total%ofInterventionsachieved-premisesratedA")] <- "ARatedInterventionsAchieved"
colnames(ratingdata)[which(names(ratingdata) == "Total%ofInterventionsachieved-premisesratedB")] <- "BRatedInterventionsAchieved"
colnames(ratingdata)[which(names(ratingdata) == "Total%ofInterventionsachieved-premisesratedC")] <- "CRatedInterventionsAchieved"
colnames(ratingdata)[which(names(ratingdata) == "Total%ofInterventionsachieved-premisesratedD")] <- "DRatedInterventionsAchieved"
colnames(ratingdata)[which(names(ratingdata) == "Total%ofInterventionsachieved-premisesratedE")] <- "ERatedInterventionsAchieved"
colnames(ratingdata)[which(names(ratingdata) == "ProfessionalFullTimeEquivalentPosts-occupied *")] <- "FTE"


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking outlier in successful intervention rate
boxplot(ratingdata$OverallInterventionsAchieved)

# Checking outlier in successful intervention rate
boxplot(ratingdata$FTE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create new data frame for further analysis & keep the original data
rtgdata <- ratingdata

# Change the "NR" and "NP" value to NA
rtgdata["ARatedInterventionsAchieved"][rtgdata["ARatedInterventionsAchieved"] == "NR"] <- NA
rtgdata["Total%ofBroadlyCompliantestablishments-A"][rtgdata["Total%ofBroadlyCompliantestablishments-A"] == "NP"] <- NA

# Convert from char to num data type
rtgdata$`Total%ofBroadlyCompliantestablishments-A` <- as.numeric(rtgdata$`Total%ofBroadlyCompliantestablishments-A`)
rtgdata$`ARatedInterventionsAchieved` <- as.numeric(rtgdata$`ARatedInterventionsAchieved`)

# create new column for analysis

# Number of rated establishment
rtgdata$ratedestablishment <- rtgdata$`Totalestablishments(includingnotyetrated&outside)`-rtgdata$Establishmentsnotyetratedforintervention - rtgdata$Establishmentsoutsidetheprogramme

rtgdata$FTEPerEstablishment <- 1000*rtgdata$FTE/rtgdata$ratedestablishment

rtgdata$ratedestablishmentin1000 <- rtgdata$ratedestablishment/1000

rtgdata$FTEAsProportion <- rtgdata$ratedestablishment*0.2


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Density Plot
dist_plot <- 
  ggplot(data = rtgdata)+
  geom_histogram(mapping = (aes(x=OverallInterventionsAchieved, y = ..density..)),binwidth = 5, color="black")+
  geom_density(aes(x=OverallInterventionsAchieved), fill = "red", alpha = 0.2)+
  geom_vline(aes(xintercept = mean(ratingdata$OverallInterventionsAchieved, na.rm = TRUE)), col = "red", size = 1)+
  geom_text(aes(x=mean(OverallInterventionsAchieved, na.rm = TRUE) - 10, 
                label=paste("Mean =", as.character(round(mean(OverallInterventionsAchieved, na.rm=TRUE))),"%"), 
                                           y=0.04), colour = "red")+
  ggtitle("Distribution of Successful Intervention Rate Accross Local Authorities")+
  labs(x = "% of Successful Interventions", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(legend.position = "none")


# Frequency Plot
ggplot(data = rtgdata)+
  geom_histogram(mapping = (aes(x=OverallInterventionsAchieved)),binwidth = 5, color="black")+
  geom_vline(aes(xintercept = mean(ratingdata$OverallInterventionsAchieved, na.rm = TRUE)), col = "red", size = 1)+
  geom_text(aes(x=mean(OverallInterventionsAchieved, na.rm = TRUE) - 10, 
                label=paste("Mean =", as.character(round(mean(OverallInterventionsAchieved, na.rm=TRUE))),"%"), 
                                           y=80), colour = "red")+
  ggtitle("Distribution of Successful Intervention Actions Rate Local Authorities")+
  labs(x = "% of Successful Interventions", y = "Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(legend.position = "none")



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert the dataset into long format

# Pick relevant columns
data_wide <- rtgdata[c("Country", "LAType", "LAName", "ARatedInterventionsAchieved","BRatedInterventionsAchieved","CRatedInterventionsAchieved","DRatedInterventionsAchieved","ERatedInterventionsAchieved", "FTE")]

data_wide$ARatedInterventionsAchieved <- as.numeric(data_wide$ARatedInterventionsAchieved)

# Convert the data from wide to long format
data_long <- data_wide %>% 
  pivot_longer(
    cols = `ARatedInterventionsAchieved`:`ERatedInterventionsAchieved`, 
    names_to = "rating",
    values_to = "interventionsuccessrate"
)

data_long$rating <- as.factor(data_long$rating)

str(data_wide)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a summary of Response Time data
mean_achieved <- data_long %>% 
  group_by(rating) %>% 
  summarise(successrate = mean(interventionsuccessrate, na.rm = TRUE))

# Create a new column to save the label for the vline
mean_achieved$'SuccessRateLabel' <- paste("Mean = ", as.character(round(mean_achieved$successrate)),"%")

# Create label for the plot
rating.labs <- c("ARatedInterventionsAchieved" = "Rating A" , "BRatedInterventionsAchieved" = "Rating B", "CRatedInterventionsAchieved" = "Rating C",
                 "DRatedInterventionsAchieved" = "Rating D", "ERatedInterventionsAchieved" = "Rating E")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Frequency Plot
rating_freqplot<- 
  ggplot(data = data_long)+
  geom_histogram(mapping = (aes(x=interventionsuccessrate, fill = rating)), color="black")+
  geom_vline(data = mean_achieved,aes(xintercept = successrate), col = "red", size = 1)+
  geom_text(data = mean_achieved,aes(x=25, label=SuccessRateLabel,
                                          y=c(150, 80, 80, 70, 40)),colour="black")+
  #ggtitle("Distribution of Successful Enforcement Actions for Each Rating")+
  labs(x = "% of Successful Interventions", y = "Frequency")+
  facet_grid(rating ~., labeller = labeller(rating = rating.labs), scales = "free")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(legend.position = "none")

# Density Plot
rating_densplot <- 
  ggplot(data = data_long)+
  geom_histogram(mapping = (aes(x=interventionsuccessrate, y= ..density.., fill = rating)), color="black")+
  geom_density(aes(x=interventionsuccessrate), fill = "black", alpha = 0.3)+
  geom_vline(data = mean_achieved,aes(xintercept = successrate), col = "red", size = 1)+
  geom_text(data = mean_achieved,aes(x=25, label=SuccessRateLabel,
                                          y=c(0.15, 0.1, 0.075, 0.06, 0.03)),colour="black")+
  #ggtitle("Distribution of Successful Enforcement Actions for Each Rating")+
  labs(x = "% of Successful Interventions", y = "Density")+
  facet_grid(rating ~., labeller = labeller(rating = rating.labs), scales = "free")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(legend.position = "none")



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Shapiro Test to Test Normality of The Data
shapiro.test(rtgdata$OverallInterventionsAchieved)
shapiro.test(rtgdata$FTE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking the correlation between FTE of FSA and Percentage of Intervention Achieved
rcorr(as.matrix(select(rtgdata, OverallInterventionsAchieved, FTE, ratedestablishment)), type = "spearman")


## ------------------------------------------------------------------------------------------------------------------------------------------------------

# FTE vs Overall Intervention Achieved
( allcountry <- 
  ggplot(rtgdata, aes(y=OverallInterventionsAchieved, x=FTE)) + 
  geom_point() + 
  labs(x="Number of FTE", y="% of Successful Intervention", subtitle = "All Countries") + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  geom_smooth(method=lm)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0)))



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# FTE vs Overall Intervention Achieved in Each Country
( eachcountry <- 
  ggplot(rtgdata, aes(y=OverallInterventionsAchieved, x=FTE, colour = Country)) + 
  geom_point() + 
  scale_y_continuous(limits =c(0,125), breaks =c(0,25,50,75,100))+
  facet_grid(Country ~.)+ 
  geom_smooth(method=lm)+
  labs(x = "Number of FTE", y = "% of Successful Interventions", subtitle = "Each Country")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0))+
  theme(legend.position="none")
)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove axis titles from all plots
p = list(allcountry,eachcountry) %>% map(~.x + labs(x=NULL, y=NULL))

# Arrange the plot
grid.arrange(grobs=p, ncol = 2, top = text_grob("Number of FTE vs Successful Intervention", size = 12, face = "bold"), left = text_grob("% of Successful Interventions", rot = 90, face ="bold"), bottom = text_grob("Number of FTE", face = "bold"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a linear model to predict success rate based on the number of FTE
successrate_by_FTE <- lm(OverallInterventionsAchieved ~ FTE, data = rtgdata)
summary(successrate_by_FTE)
cbind(coefficient=coef(successrate_by_FTE), confint(successrate_by_FTE))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create variable to store data for each country
englanddata <- rtgdata %>% 
  filter (Country == "England")

northernirelanddata <- rtgdata %>% 
  filter (Country == "Northern Ireland")

walesdata <- rtgdata %>% 
  filter (Country == "Wales")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a linear model to predict success rate based on the number of FTE for each country

# England
successrate_by_FTE_England <- lm(OverallInterventionsAchieved ~ FTE, data = englanddata)
summary(successrate_by_FTE_England)
cbind(coefficient=coef(successrate_by_FTE_England), confint(successrate_by_FTE_England))

# Northern Ireland
successrate_by_FTE_NorthernIreland <- lm(OverallInterventionsAchieved ~ FTE, data = northernirelanddata)
summary(successrate_by_FTE_NorthernIreland)
cbind(coefficient=coef(successrate_by_FTE_NorthernIreland), confint(successrate_by_FTE_NorthernIreland))

# Wales
successrate_by_FTE_Wales <- lm(OverallInterventionsAchieved ~ FTE, data = walesdata)
summary(successrate_by_FTE_Wales)
cbind(coefficient=coef(successrate_by_FTE_Wales), confint(successrate_by_FTE_Wales))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Shapiro-Wilk Test to test normality
shapiro.test(rtgdata$FTEPerEstablishment)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking the correlation between FTE of FSA and Percentage of Intervention Achieved
rcorr(as.matrix(select(rtgdata, OverallInterventionsAchieved, FTEPerEstablishment)), type = "spearman")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a plot showing relationship between number of FTE per Total Rated Establishments and Overall Intervention Achieved
FTE_Per_Est_Plot <- 
  ggplot(rtgdata, aes(x = FTEPerEstablishment, y = OverallInterventionsAchieved)) +
  geom_point() +
   labs(x="FTE per Rated Establishment", y="% of Successful Intervention", 
       subtitle="Each point is a Local Authority \nThe shaded area shows the 95% CI for the best-fitting regression line") + 
  scale_y_continuous(limits =c(0,125), breaks = c(0, 25, 50, 75, 100))+
  geom_smooth(method=lm)+
  ggtitle("FTE Per Total Rated Establishment vs. Succesful Intervention")+
  labs(x = "FTE per Total Rated Establishment (x1,000)", y = "% of Successful Interventions")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0))+
  theme(title = element_text(face="bold"))



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a linear model to predict success rate based on the number of FTE
successrate_by_FTE_per_RatedEstablishment <- lm(OverallInterventionsAchieved ~ FTEPerEstablishment, data = rtgdata)
summary(successrate_by_FTE_per_RatedEstablishment)
cbind(coefficient=coef(successrate_by_FTE_per_RatedEstablishment), confint(successrate_by_FTE_per_RatedEstablishment))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a linear model with FTE and Total Number of Rated Establishment as Predictors
successrate_by_FTE_and_RatedEstablishment <- lm(OverallInterventionsAchieved ~ FTE + ratedestablishmentin1000, data = rtgdata)
summary(successrate_by_FTE_and_RatedEstablishment)
cbind(coefficient=coef(successrate_by_FTE_and_RatedEstablishment), confint(successrate_by_FTE_and_RatedEstablishment))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
anova(successrate_by_FTE, successrate_by_FTE_and_RatedEstablishment)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
vif(successrate_by_FTE_and_RatedEstablishment)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a dataframe as the input to the model
predict_data <- expand.grid(FTE = seq(1, 20, 0.5),
                            ratedestablishmentin1000 =seq(3,10,0.5))

# Predict the intervention success rate using FTE and Number of Rated Establishment
predict_data <- mutate(predict_data,
                         OverallInterventionsAchieved = predict(successrate_by_FTE_and_RatedEstablishment, predict_data))

# Create dataframe for example
example_data <- expand.grid(FTE = c(7, 13),
                            ratedestablishmentin1000 = c(5,8))

example_data <- example_data[-c(4), ]

example_data$State <- c("Initial", "Scenario 1", "Scenario 2")

example_data[nrow(example_data) +1,] = c(20, 3, "Optimal")

example_data$FTE <- as.numeric(example_data$FTE)

example_data$ratedestablishmentin1000 <- as.numeric(example_data$ratedestablishmentin1000)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a plot showing the relationship between number of FTE and number of rated establishment as predictors for successful intervention  rate
cont_plot <-
ggplot()+
scale_color_viridis_c(option = "viridis")+
geom_point(data = predict_data, aes(x=FTE, y = ratedestablishmentin1000, color = OverallInterventionsAchieved), size = 15, shape = 15)+
ggtitle("FTE and Number of Rated Establishment vs Successful Intervention Rate")+
labs(x = "FTE", y = "Total Number of Rated Establishment (x1,000)", color = "% Successful Intervention")+
scale_x_continuous(limits = c(1,20), breaks = seq(2,20,2))+
scale_y_continuous(limits = c(3,10), breaks = seq(3,10,1))+
theme_classic()+
theme(plot.title = element_text(hjust = 0.5))+
theme(title = element_text(face="bold", size = 8))+
geom_point(data = example_data, aes(FTE, ratedestablishmentin1000, shape = State), size = 3)


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
plot(dist_plot)


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
# Plot the distribution of response time for each incident group
grid.arrange(rating_densplot, rating_freqplot, ncol = 2, 
             top = text_grob("Distribution of Successful Intervention Rate for Each Establishment Rating", size = 10, face = "bold"))


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
grid.arrange(grobs=p, ncol = 2, top = text_grob("Number of FTE vs Successful Intervention Rate", size = 12, face = "bold"), left = text_grob("% of Successful Interventions", rot = 90, face ="bold"), bottom = text_grob("Number of FTE", face = "bold"))


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
plot(FTE_Per_Est_Plot)


## ----echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'--------------------------------------------------------------------------------
plot(cont_plot)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Read the dataset
salesdata <- read.csv("publisher_sales.csv")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the summary and the structure of the data
summary(salesdata)
str(salesdata)


## ------------------------------------------------------------------------------------------------------------------------------------------------------

# Change data types for Country and LAtype columns
# First generate a vector to keep the column names
columns <- c("sold.by", "publisher.type", "genre")

# Then, set the correct data types for the defined columns
salesdata[columns] <- lapply(salesdata[columns], factor)

# To check the new structure of the data after several changes above
str(salesdata)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the correlation between variables
rcorr(as.matrix(select(salesdata, avg.review, daily.sales, total.reviews, sale.price)))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking outlier in avg.review
boxplot(salesdata$avg.review)
salesdata %>% filter (avg.review < 3)
# Result: Data seems normal, the value are possible to be the real value (not input error). Conclusion: Keep all the data.

# Checking outlier in daily.sales
boxplot(salesdata$daily.sales)
salesdata %>% filter (daily.sales > 175)
# Result: Data seems normal, the value are possible to be the real value (not input error). Conclusion: Keep all the data.

# Checking outlier in total.reviews
boxplot(salesdata$total.reviews)
salesdata %>% filter (total.reviews == 0)
# Result: Data seems normal, the value are possible to be the real value (not input error). Conclusion: Keep all the data.

# Checking outlier in sale.price
boxplot(salesdata$sale.price)
salesdata %>% filter (sale.price > 15)
# Result: Data seems normal, the value are possible to be the real value (not input error). Conclusion: Keep all the data.



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot to see the distribution of average daily sales for each genre
ggplot(salesdata, aes(daily.sales, ..density.., fill=genre))+
  geom_histogram(binwidth=10,position="identity", alpha=0.5)+ 
  labs(x="Average of Daily Sales", y="Density", fill="Genre")+
  ggtitle("Distribution of Average Daily Sales for Each Genre")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Change the level in genre column to get positive result in mean difference (to be more intuitive)
salesdata$genre <- factor(salesdata$genre, levels = c("fiction", "non_fiction", "childrens"), labels = c("Fiction","Non Fiction", "Children"))

# Create subset data for t-test approach
fiction_childerns <- salesdata %>% 
  filter(genre %in% c("Fiction", "Children"))

fiction_nonfiction <- salesdata %>% 
  filter(genre %in% c("Fiction", "Non Fiction"))

childrens_nonfiction <- salesdata %>% 
  filter(genre %in% c("Children", "Non Fiction"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate simple statistical summary
salesdata_summary <- salesdata %>% 
 	group_by(genre) %>% 
	dplyr::summarise("Average of Daily Sales" = round(mean(daily.sales, na.rm = TRUE),1), 
	          "Standard Deviation of Average Daily Sales" = round(sd(daily.sales, na.rm=TRUE),1), 
	          "Frequency" = n()) 

# Change column name
colnames(salesdata_summary)[which(names(salesdata_summary) == "genre")] <- "Genre"

salesdata_summary



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Two Sample t-test
t.test(daily.sales ~ genre, data = fiction_childerns)
t.test(daily.sales ~ genre, data = fiction_nonfiction)
t.test(daily.sales ~ genre, data = childrens_nonfiction)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a model to predict daily sales by genre
sales_by_genre <- lm(daily.sales ~ genre, data = salesdata)
summary(sales_by_genre)

# Estimation Approach
(sales_by_genre_emm <- emmeans(sales_by_genre, ~genre))
(sales_by_genre_con <- confint(pairs(sales_by_genre_emm))) 


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Estimation of Average Daily Sales for Each Book Genre
estplot <- 
  ggplot(summary(sales_by_genre_emm), aes(x=genre, y=emmean, ymin=lower.CL, ymax=upper.CL)) +
  geom_point() + 
  geom_linerange() + 
  labs(y="Average of Daily Sales", x="Genre", subtitle="Error bars are 95% CIs",
		     title="Average Daily Sales for Each Book Genre")+
  scale_y_continuous(breaks = c(105.9, 75.9, 55.6))+
  theme(title = element_text(face="bold", size = 7))

# Difference of Average Daily Sales for Each Book Genre
conplot <-
  ggplot(sales_by_genre_con, aes(x=contrast, y=estimate, ymin=lower.CL, ymax=upper.CL)) +
  geom_point() + 
  geom_linerange() + 
  labs(y="Difference of Average Daily Sales", x="Genre", subtitle="Error bars are 95% CIs",
       title="Difference of Average Daily Sales for Each Book Genre") + 
  scale_y_continuous(breaks = c(50.3, 30, 20.3, 0))+
  geom_hline(yintercept=0, lty=2)+
  theme(title = element_text(face="bold", size = 7)) + 
  scale_x_discrete(labels = c("Fiction - \nChildren", "Fiction -\nNon Fiction", "Non Fiction - \nChildren"))

# Arrange both plot into a single plot
grid.arrange(estplot, conplot, ncol = 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a model to predict daily sales by average review score
sales_by_score <- lm(daily.sales ~ avg.review, salesdata)
summary(sales_by_score)
cbind(coefficient=coef(sales_by_score), confint(sales_by_score))
anova(sales_by_score)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a model to predict daily sales by total number of review
sales_by_totalreview <- lm(daily.sales ~ total.reviews, salesdata)
summary(sales_by_totalreview)
cbind(coefficient=coef(sales_by_totalreview), confint(sales_by_totalreview))
anova(sales_by_totalreview)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict total sales by average review scores and total number of review
sales_by_score_totalreview <- lm(daily.sales ~ avg.review + total.reviews, salesdata)
summary(sales_by_score_totalreview)
cbind(coefficient=coef(sales_by_score_totalreview), confint(sales_by_score_totalreview))
anova(sales_by_score_totalreview)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Check multicolinearity between the predictors
vif(sales_by_score_totalreview)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Examine model with interaction terms
sales_by_score_totalreview_int <- lm(daily.sales ~total.reviews * avg.review, salesdata)
summary(sales_by_score_totalreview_int)
cbind(coefficient=coef(sales_by_score_totalreview_int), confint(sales_by_score_totalreview_int))
anova(sales_by_score_totalreview_int)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Compare model with only main effect and the model with interaction terms
anova(sales_by_score_totalreview, sales_by_score_totalreview_int)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Visualisation of base model and model with interactions
surf_data <- tibble(total.reviews = unlist(expand.grid(seq(100, 250 , 25), seq(4, 5, 0.1))[1]),
                         avg.review = unlist(expand.grid(seq(100, 250, 25), seq(4, 5, 0.1))[2]))

surf_data <- mutate(surf_data,
                         main_hat = predict(sales_by_score_totalreview, surf_data),
                         intr_hat = predict(sales_by_score_totalreview_int, surf_data))

surf_main <- ggplot(surf_data, aes(total.reviews, avg.review)) + geom_contour_filled(aes(z = main_hat)) + labs(subtitle = "Main Effects")  + guides(fill=guide_legend(title="Average of Daily Sales"))
surf_intr <- ggplot(surf_data, aes(total.reviews, avg.review)) + geom_contour_filled(aes(z = intr_hat)) + labs(subtitle = "Interaction Effects")   + guides(fill=guide_legend(title="Average of Daily Sales"))
grid.arrange(surf_main, surf_intr, nrow = 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a plot to visualize the effect of interaction terms

score_review <- filter(surf_data, total.reviews %in% c(100, 125, 150, 175, 200, 200, 250)) %>%
  mutate(total.reviews = factor(total.reviews))
(
  predplot<-
  ggplot(score_review) + 
  geom_line(aes(avg.review, main_hat, colour = total.reviews), size = 1) +
  geom_line(aes(avg.review, intr_hat, colour = total.reviews), linetype = "dashed", size = 1) +
  labs(x = "Average of Review Score", y = "Prediction of Average Daily Sales")+
  ggtitle("Prediction of Average Daily Sales")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(face="bold"))+
  theme(title = element_text(face="bold", size = 10)) +
  guides(colour=guide_legend(title="Total Number of Review"))
)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a model to predict average daily sales by average sale price
sales_by_price <- lm(daily.sales~sale.price, salesdata)
summary(sales_by_price)
cbind(coefficient=coef(sales_by_price), confint(sales_by_price))
anova(sales_by_price)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Estimation Approach
( sales_by_price_emm <- emmeans(sales_by_price, ~sale.price, at=list(sale.price=c(1, 5, 9, 13, 17))))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a model to predict average daily sales by genre and sale price (with only main effect)
sales_by_price_genre <- lm(daily.sales~sale.price+genre, salesdata)
summary(sales_by_price_genre)
cbind(coefficient=coef(sales_by_price_genre), confint(sales_by_price_genre))
anova(sales_by_price_genre)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a model to predict average daily sales by genre and sale price (with interaction terms)
sales_by_price_genre_int <- lm(daily.sales~sale.price*genre, salesdata)
summary(sales_by_price_genre_int)
cbind(coefficient=coef(sales_by_price_genre_int), confint(sales_by_price_genre_int))
anova(sales_by_price_genre_int)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Compare the model with only main effect and with the model with interaction terms
anova(sales_by_price, sales_by_price_genre_int)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Estimate the value of average daily sales
( sales_by_price_genre_int_emm <- emmeans(sales_by_price_genre_int, ~sale.price+genre, at=list(sale.price=c(1, 5, 9, 13, 17))))



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Combine model with only sale price with the model with sale price and genre
both_models_emms <- bind_rows(list(data.frame(sales_by_price_emm, Model="Only Sale Price"), data.frame(sales_by_price_genre_int_emm, Model="Sale Price and Genre")))

both_models_emms$genre <- as.character(both_models_emms$genre)

both_models_emms[is.na(both_models_emms)] = "Overall"

both_models_emms$genre <- as.factor(both_models_emms$genre)

both_models_emms$genre


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a plot to visualize the effect of sale price to average daily sale in each genre
bothplot <- 
  ggplot(both_models_emms, aes(x=sale.price, y=emmean, ymin=lower.CL, ymax=upper.CL, col=genre)) + 
  geom_line(aes(x=sale.price, y= emmean, lty = Model)) + 
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, col = genre), alpha = 0.1)+
  labs(x="Sale Price (£)", y="Average of Daily Sales", col="Genre", subtitle="Error bars are 95% CIs")+
  ggtitle("Effect of Sale Price and Genre to Average Daily Sales")+
  scale_x_continuous(breaks = c(1, 5, 9, 13, 17))+
  scale_y_continuous(breaks = c(30, 60, 90, 120))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0))+
  theme(title = element_text(face="bold"))+
  theme(title = element_text(face="bold", size = 10)) 



## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------
salesdata_summary %>%
  kbl(caption = "Table 1. Average of Daily Sales for Each Book Genre") %>%
  kable_styling()


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
grid.arrange(estplot, conplot, ncol = 2)


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
plot(predplot)


## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.align='center'-------------------------------------------------------------------------------
plot(bothplot)

