library(tidyverse)
library(readxl)

# load csv files
sales <- read_csv("Sales.csv")
nyop <- read_csv("NYOP.csv")

sales
nyop


##################################### Flat Rate Pricing #####################################
# a. What are appropriate null and alternative hypothesis to compare population proportions?

# H0: There is no difference between the proportion of purchases in FR and FR Charity.
# Ha: There is a difference between the proportion of purchases in FR and FR Charity.



# b. The appropriate test statistic for the difference between two population proportions is given by p¯1−p¯2p¯1(1−p¯1)n1+p¯2(1−p¯2)n2√, 
# and with the large number of samples you can go ahead and assume the sampling distribution is normally distributed. 
# Is the difference significant at the 5% level?

sales %>% 
  dplyr::filter(Condition %in% c("FR", "FR Charity")) %>% 
  dplyr::group_by(Condition) %>% 
  dplyr::summarize(total_riders = sum(Riders),
                   total_sold = sum(NumberSold),
                   proportion = total_sold / total_riders) -> fr_summary

# Calculate the test statistic
p1_fr <- fr_summary[fr_summary$Condition == "FR",]$proportion
p2_fr <- fr_summary[fr_summary$Condition == "FR Charity",]$proportion
n1_fr <- fr_summary[fr_summary$Condition == "FR",]$total_riders
n2_fr <- fr_summary[fr_summary$Condition == "FR Charity",]$total_riders

z_stat_fr <- (p1_fr - p2_fr) / sqrt((p1_fr * (1 - p1_fr) / n1_fr) + (p2_fr * (1 - p2_fr) / n2_fr)) 

# Calculate the p-value
p_value_fr <- 2 * (1 - pnorm(abs(z_stat_fr)))

# Check if significant at 5% level
significant_fr <- p_value_fr < 0.05

# Add these to the result data frame
# Create a named vector to store test results
test_results_fr <- c(z_stat_fr = z_stat_fr, p_value_fr = p_value_fr, significant_fr = significant_fr)

# Return results
list(fr_summary = fr_summary, test_results_fr = test_results_fr)


# With a p-value of 0.12, you fail to reject the null hypothesis at the 5% significance level. 
# This indicates that there isn't sufficient evidence to claim a significant difference between the proportions 
# of purchases in the FR and FR Charity conditions.





### c. What is the p-value associated with the test statistic?  0.12
### d. Interpret what this p-value means.
# The p-value of 0.12 indicates that if the null hypothesis were true 
# (i.e., there's no difference between the proportions of purchases in the FR and FR Charity conditions), 
# there's a 12% chance of observing a test statistic as extreme as the one you got. 
# Because this p-value is greater than the commonly used alpha level of 0.05, you fail to reject the null hypothesis. 
# In practical terms, this means that the data doesn't provide strong evidence to conclude that the charitable component (FR Charity) 
# had a significant impact on the proportion of purchases compared to the standard flat-rate (FR).




##################################### NYOP Pricing #####################################

# When considering the two NYOP pricing conditions, NYOP and NYOP Charity, is there a difference between the proportion of purchases? 
# Repeat steps (a)-(d) as above.

# a. What are appropriate null and alternative hypothesis to compare population proportions?

# H0: There is no difference between the proportion of purchases in FR and FR Charity.
# Ha: There is a difference between the proportion of purchases in FR and FR Charity.

# b. The appropriate test statistic for the difference between two population proportions is given by p¯1−p¯2p¯1(1−p¯1)n1+p¯2(1−p¯2)n2√, 
# and with the large number of samples you can go ahead and assume the sampling distribution is normally distributed. 
# Is the difference significant at the 5% level?

sales %>% 
  dplyr::filter(Condition %in% c("NYOP", "NYOP Charity")) %>% 
  dplyr::group_by(Condition) %>% 
  dplyr::summarize(total_riders = sum(Riders),
                   total_sold = sum(NumberSold),
                   proportion = total_sold / total_riders) -> nyop_summary


# Extract the proportions and total riders for NYOP and NYOP Charity
p1_nyop <- nyop_summary[nyop_summary$Condition == "NYOP",]$proportion
p2_nyop <- nyop_summary[nyop_summary$Condition == "NYOP Charity",]$proportion
n1_nyop <- nyop_summary[nyop_summary$Condition == "NYOP",]$total_riders
n2_nyop <- nyop_summary[nyop_summary$Condition == "NYOP Charity",]$total_riders

# Calculate the test statistic
z_stat_nyop <- (p1_nyop - p2_nyop) / sqrt((p1_nyop * (1 - p1_nyop) / n1_nyop) + (p2_nyop * (1 - p2_nyop) / n2_nyop)) 

# Calculate the p-value
p_value_nyop <- 2 * (1 - pnorm(abs(z_stat_nyop)))

# Check if significant at the 5% level
significant_nyop <- p_value_nyop < 0.05

list(z_stat_nyop = z_stat_nyop, p_value_nyop = p_value_nyop, significant_nyop = significant_nyop)


# c. What is the p-value associated with the test statistic? 0
# d. Interpret what this p-value means.
# A p-value of 0 is a strong indicator to reject the null hypothesis. 
# In the context of this experiment, it means there's a statistically significant difference between the proportions of purchases 
# in the "NYOP" and "NYOP Charity" conditions. Essentially, the addition of charity in the pricing model has a significant impact on 
# purchase behavior.

