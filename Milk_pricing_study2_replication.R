#######################################################################################
### STUDY 2 - R replication code for                                                ###
### Dynamic pricing to reduce retail dairy shrink:                                  ###
### Evidence from lab and grocery store experiments                                 ###
#######################################################################################

# DATE: DECEMBER 2025
# Editor: Aaron Adalja

rm(list = ls())
objects()
options(error=recover, scipen=999, max.print = 9999)

# Any package that is required by the script below is given here:----
# Check to see if packages are installed, if not install.
inst_pkgs = load_pkgs =  c("readxl", "tidyverse", "ggplot2", "dplyr", 
                           "emmeans", "ggsignif", "gridExtra",  "textreg", 
                           "chisq.posthoc.test", "stargazer", "conflicted", "tibble", 
                           "flextable", "gtsummary", "knitr",  "lubridate")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages---
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

raw_2022 <- read_excel("rawdata_study2.xlsx",sheet = "2022 data")
raw_2023 <- read_excel("rawdata_study2.xlsx",sheet = "2023 data")
raw_combined <- read_excel("rawdata_study2.xlsx",sheet = "combined")


### 2023 ANALYSIS ###-----


# Categorize the data into different milk types
data<- raw_2023 %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))


# Convert the Date column to Date type
data$Date <- as.Date(data$Date)

# Assign weeks
data <- data %>%
  mutate(Week = case_when(
    Date < as.Date("2023-09-19") ~ "Week 1",
    Date >= as.Date("2023-09-19") ~ "Week 2"
  ))



data <- data %>%
  mutate(Shelf.Life = case_when(
    Description %in% c('CD SKM 8-21', 'CD 2% MLK 8-21', 'CD WHL MLK 8-21') ~ 'High',
    Description %in% c('CD SKM HLF GAL', 'CD 2% MLK HLH GAL', 'CD WHL MLK HLF GAL') ~ 'Medium',
    Description %in% c('CD SKM 0-3', 'CD 2% MLK 0-3', 'CD WHL 0-3') ~ 'Low',
    TRUE ~ NA_character_
  ))


data1<- data%>%
  group_by(Date,Week) %>%
  summarize(total_sales = sum(sales))

lm1<-lm(total_sales ~ Week, data=data1)
summary(lm1)
acf((resid(lm1)))
data$Milk.Type <- factor(data$Milk.Type)
data$Week <- factor(data$Week)
data$Shelf.Life <- factor(data$Shelf.Life)
data$Day <-factor(data$Day)
data$ppu<-factor(data$ppu)

#############Graphing###############

#Figure 3a
means_units <- data %>%
  group_by(Milk.Type, Week) %>%
  summarize(Mean_Units = mean(sales, na.rm = TRUE))

data <- left_join(data, means_units)

# Generate the plot with unit data
view(means_units)
p_units <- ggplot(data, aes(x = Date, y = units, color = Milk.Type)) +
  geom_line() + # Add lines for units data
  geom_point() + # Add points for units data
  scale_x_date(breaks = as.Date(c("2023-09-12", "2023-09-13", "2023-09-14", "2023-09-15", "2023-09-16", "2023-09-17", "2023-09-18",
                                  "2023-09-19", "2023-09-20", "2023-09-21", "2023-09-22", "2023-09-23", "2023-09-24", "2023-09-25")),
               labels = c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", 
                          "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon"),
               date_labels = "%a") +
  geom_hline(data = means_units, aes(yintercept = Mean_Units, linetype = Milk.Type), show.legend = TRUE) + # Add mean lines with legend
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x", drop = TRUE) + # Faceting
  scale_color_manual(values = c("Whole Milk" = "red", "2% Milk" = "blue", "Skim Milk" = "green")) + # Custom colors for Milk Types
  scale_linetype_manual(values = c("Whole Milk" = "dashed", "2% Milk" = "dotted", "Skim Milk" = "dotdash")) + # Custom linetypes for legends
  labs(title = "Total Units Sold by Date for Different Milk Types and Shelf Life Levels",
       x = "Date",
       y = "Total Units Sold",
       color = "Milk Type",
       linetype = "Means") +
  theme_minimal() + # Minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p_units)


######################################################
#Figure 4a 

mean_sales <- data %>%
  group_by(Shelf.Life, Week) %>%
  summarize(Mean_Sales = mean(sales, na.rm = TRUE))

# Merge the aggregated data back to the main dataset
data <- left_join(data, means_sales)

view(data)

# Generate the plot with sale data
p_sales <- ggplot(data, aes(x = Date, y = sales, color = Milk.Type)) +
  geom_line() + # Add lines for sales data
  geom_point() + # Add points for sales data
  scale_x_date(breaks = as.Date(c("2023-09-12", "2023-09-13", "2023-09-14", "2023-09-15", "2023-09-16", "2023-09-17", "2023-09-18",
                                  "2023-09-19", "2023-09-20", "2023-09-21", "2023-09-22", "2023-09-23", "2023-09-24", "2023-09-25")),
               labels = c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", 
                          "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon"),
               date_labels = "%a") +
  geom_hline(data = means_sales, aes(yintercept = Mean, linetype = Milk.Type), show.legend = TRUE) + # Add mean lines with legend
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x", drop = TRUE) + # Faceting
  scale_color_manual(values = c("Whole Milk" = "red", "2% Milk" = "blue", "Skim Milk" = "green")) + # Custom colors for Milk Types
  scale_linetype_manual(values = c("Whole Milk" = "dashed", "2% Milk" = "dotted", "Skim Milk" = "dotdash")) + # Custom linetypes for legends
  labs(title = "Total Sales by Date for Different Milk Types and Shelf Life Levels",
       x = "Date",
       y = "Total Sales",
       color = "Milk Type",
       linetype = "Means") +
  theme_minimal() + # Minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p_sales)


#####################################################################################3

# Figure F3b 

# Categorize the data into different milk types
data <- data %>%
  mutate(
    Milk.Type = case_when(
      grepl("WHL", Description) ~ "Whole Milk",
      grepl("2%", Description) ~ "2% Milk",
      grepl("SKIM|SKM", Description) ~ "Skim Milk",
      TRUE ~ NA_character_
    ),
    Milk.Type = factor(Milk.Type),
    Interaction = factor(Milk.Type, levels = c("Skim Milk", "2% Milk", "Whole Milk"))
  )

# Calculate the aggregated total units for each day
total_units_per_day <- data %>%
  group_by(Date) %>%
  summarize(Total_Units = sum(units, na.rm = TRUE))

# Merge the aggregated data back to the main dataset
data <- left_join(data, total_units_per_day, by = "Date")

# Calculate the aggregated total units for each shelf life
Total_Units_Shelf <- data %>%
  group_by(Date, Shelf.Life) %>%
  summarize(Total_Units_Shelf = sum(units, na.rm = TRUE))

# Correct the join to include 'Shelf.Life' in the by clause
data <- left_join(data, Total_Units_Shelf, by = c("Date", "Shelf.Life"))


view(means_units)
view(total_units_per_shelf)
# Calculate means for each Milk Type and Shelf Life
mean_units_per_type_shelf <- total_units_per_shelf %>%
  group_by(Shelf.Life, Week) %>%
  summarize(Mean_Units = mean(Total_Units_Shelf, na.rm = TRUE))

# Calculate the aggregated total units for each shelf life
total_units_per_shelf <- data %>%
  group_by(Milk.Type, Week) %>%
  summarize(Total_Units_Shelf = sum(units, na.rm = TRUE), .groups = 'drop')


# Ensure `total_units_per_shelf` is correctly created before viewing it
# view(total_units_per_shelf)

# Calculate means for each Shelf Life and Week
mean_units_per_type_shelf <- total_units_per_shelf %>%
  group_by(Milk.Type, Week) %>%
  summarize(Mean_Units = mean(Total_Units_Shelf, na.rm = TRUE), .groups = 'keep')


mean_units_per_type_shelf <- total_units_per_shelf %>%
  group_by(Milk.Type, Week) %>%
  summarize(Mean_Units = mean(Total_Units_Shelf, na.rm = TRUE), .groups = 'keep')

# Calculate the aggregated total units for each shelf life
total_sales_per_shelf <- data %>%
  group_by(Milk.Type, Week) %>%
  summarize(Total_sales_Shelf = sum(sales, na.rm = TRUE), .groups = 'keep')


# Ensure `total_units_per_shelf` is correctly created before viewing it
# view(total_units_per_shelf)

# Calculate means for each Shelf Life and Week
mean_sales_per_type_shelf <- total_sales_per_shelf %>%
  group_by(Milk.Type, Week) %>%
  summarize(Mean_Units = Total_sales_Shelf/7, na.rm = TRUE, groups = 'keep')


mean_units_per_type_shelf <- total_units_per_shelf %>%
  group_by(Milk.Type, Week) %>%
  summarize(Mean_Units = mean(Total_Units_Shelf, na.rm = TRUE), .groups = 'keep')


mean_units_per_type_shelf
#Merge the additional aggregated data back to the main dataset
data <- left_join(data, total_units_per_shelf, by = c("Milk.Type", "Week"))
data <- left_join(data, mean_units_per_type_shelf, by = c("Week", "Milk.Type"))


data$Shelf.Life <- factor(data$Shelf.Life, levels = c("High", "Medium", "Low"))



# Calculate the percentage composition for each type of milk for each day
data <- data %>%
  mutate(Percentage = (units / Total_Units_Shelf) * 100)
data$Interaction <- factor(data$Interaction, levels = c("Skim Milk", "2% Milk", "Whole Milk")) #reordering 


data <- data %>% 
  group_by(Shelf.Life, Week) %>% 
  mutate(avg_units_week = mean(Total_Units))

p_units_bar <- ggplot(data, aes(x = Date, y = Percentage, fill = Interaction)) +
  geom_bar(stat = "identity", position = "stack",width=0.9) +
  geom_line(aes(y = Total_Units_Shelf * 100 / max(Total_Units_Shelf), group = Shelf.Life, color = "Total Units"), size = 1) +
  # geom_line(aes(y = Mean_Units* 100 / max(Total_Units_Shelf), group = interaction(Milk.Type, Shelf.Life), color = "Mean Units"), linetype = "dotted", size = 1) +
  geom_hline(aes(yintercept = mean(Total_Units_Shelf), group = Shelf.Life))+
  scale_fill_manual(values = c("Whole Milk" = "tomato1", "2% Milk" = "lightskyblue", "Skim Milk" = "olivedrab2"),guide = FALSE) +
  scale_color_manual(values = c("Total Units" = "black", "Mean Units" = "black"),guide = TRUE) +
  scale_y_continuous(name = "Percentage Composition", sec.axis = sec_axis(~ . * max(data$Total_Units) / 100, name = "Units Sold")) +
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x") +
  scale_x_date(date_labels = " ", date_breaks = "1 day", expand = c(0, 0)) +
  labs(
    x = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))



####################################################################

# Figure F4b 

# Categorize the data into different milk types
data <- data %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))


# Calculate the aggregated total units for each day
total_sales_per_day <- data %>%
  group_by(Date) %>%
  summarize(Total_Sales = sum(sales, na.rm = TRUE))

# Merge the aggregated data back to the main dataset
data <- left_join(data, total_sales_per_day, by = "Date")

# Calculate the aggregated total units for each shelf life
total_sales_per_shelf <- data %>%
  group_by(Date, Shelf.Life,Week) %>%
  summarize(Total_Sales_Shelf = sum(sales, na.rm = TRUE))


# Calculate means for each Milk Type and Shelf Life
mean_sales_per_type_shelf <- total_sales_per_shelf %>%
  group_by(Week, Shelf.Life) %>%
  summarize(Mean_Sales = mean(Total_Sales_Shelf, na.rm = TRUE))

# Merge the additional aggregated data back to the main dataset
data <- left_join(data, total_sales_per_shelf, by = c("Date", "Shelf.Life", "Week"))
data <- left_join(data, mean_sales_per_type_shelf, by = c("Week", "Shelf.Life"))


data$Shelf.Life <- factor(data$Shelf.Life, levels = c("High", "Medium", "Low"))


# Calculate the percentage composition for each type of milk for each day
data <- data %>%
  mutate(Percentage.Sales = (sales / Total_Sales_Shelf) * 100)


data$Interaction <- factor(data$Milk.Type)
data$Interaction <- factor(data$Interaction, levels = c("Skim Milk", "2% Milk", "Whole Milk"))


p_sales_bar <- ggplot(data, aes(x = Date, y = Percentage.Sales, fill = Interaction)) +
  geom_bar(stat = "identity", position = "stack", width=0.9) +
  geom_line(aes(y = Total_Sales_Shelf * 100 / max(Total_Sales_Shelf), group = Shelf.Life, color = "Total Sales"), size = 1) +
  geom_line(aes(y = Mean_Sales* 100 / max(Total_Sales_Shelf), group = interaction(Milk.Type, Shelf.Life), color = "Mean Sales"), linetype = "dotted", size = 1) +
  scale_fill_manual(values = c("Whole Milk" = "tomato1", "2% Milk" = "lightskyblue", "Skim Milk" = "olivedrab2")) +
  scale_color_manual(values = c("Total Sales" = "black", "Mean Sales" = "black")) +
  scale_y_continuous(name = "Percentage Composition", sec.axis = sec_axis(~ . * max(data$Total_Sales) / 100, name = "Sales ($)")) +
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x") +
  scale_x_date(date_labels = "%a", date_breaks = "1 day", expand = c(0, 0)) +
  labs(
    x = "Day of the week",
    fill = "Milk Type",
    color = "Total Sales and Mean Sales") +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_blank()) # Removes the facet labels (Week 1, Week 2)

p_units_bar <- p_units_bar + theme(plot.margin = margin(t = 1, r = 5, b =-7, l = 5, unit = "mm"))
p_sales_bar <- p_sales_bar + theme(plot.margin = margin(t = 1, r = 5, b = 2, l = 5, unit = "mm"))



# Combine the two plots one on top of the other
grid.arrange(p_units_bar, p_sales_bar, nrow = 2)

grid.arrange(p_units_bar, p_sales_bar, nrow = 2, heights = c(1.2, 1.2))


############################################################


# Step 2: Aggregate data by Milk Type and Shelf Life for both units and sales
data_aggregated <- data %>%
  group_by(Milk.Type, Shelf.Life) %>%
  summarize(Total_Units = sum(units, na.rm = TRUE),
            Total_Sales = sum(sales, na.rm = TRUE),
            .groups = 'drop')

# Step 3: Calculate percentage composition
data_aggregated <- data_aggregated %>%
  group_by(Milk.Type) %>%
  mutate(Percentage_Units = (Total_Units / sum(Total_Units)) * 100,
         Percentage_Sales = (Total_Sales / sum(Total_Sales)) * 100) %>%
  ungroup()

# Step 4: Adjust plotting code for units
p_units_shelf_life <- ggplot(data_aggregated, aes(x = Milk.Type, y = Percentage_Units, fill = Shelf.Life)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("High" = "gold", "Medium" = "darkorange", "Low" = "red")) +
  labs(y = "Percentage Composition", x = "Milk Type", fill = "Shelf Life") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Adjust plotting code for sales
p_sales_shelf_life <- ggplot(data_aggregated, aes(x = Milk.Type, y = Percentage_Sales, fill = Shelf.Life)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("High" = "gold", "Medium" = "darkorange", "Low" = "red")) +
  labs(y = "Percentage Composition", x = "Milk Type", fill = "Shelf Life") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plots
print(p_units_shelf_life)
print(p_sales_shelf_life)

# Step 1: Aggregate data
data_aggregated <- data %>%
  group_by(Date, Week, Milk.Type, Shelf.Life) %>%
  summarize(Total_Units = sum(units, na.rm = TRUE),
            .groups = 'drop')

# Step 2: Calculate percentage composition for each Shelf.Life within each Date, Week, and Milk.Type
data_percentage <- data_aggregated %>%
  group_by(Date, Week, Milk.Type) %>%
  mutate(Percentage_Units = (Total_Units / sum(Total_Units)) * 100) %>%
  ungroup()

# Step 3: Plotting
# Note: Adjusting to use days on the x-axis and milk type comparisons, with shelf life compositions
p <- ggplot(data_percentage, aes(x = Date, y = Percentage_Units, fill = Shelf.Life)) +
  geom_bar(stat = "identity", position = "fill") + # Use position="fill" for percentage stack
  scale_fill_manual(values = c("High" = "gold", "Medium" = "darkorange", "Low" = "red")) +
  facet_grid(Milk.Type ~ Week, scales = "free_x") + # Facet by Milk.Type and Week
  scale_y_continuous(name = "Percentage Composition", labels = scales::percent, 
                     sec.axis = sec_axis(~., name = "Milk Type", labels = function(b) NULL)) + # Secondary axis for Milk Type (indirect representation)
  scale_x_date(date_labels = "%a", date_breaks = "1 day") +
  labs(x = "Day of the Week", fill = "Shelf Life") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(angle = 0))

# Print the plot
print(p)



########################ANOVAS#################

# Categorize the data into different milk types
data<- raw_2023 %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))



# Convert the Date column to Date type
data$Date <- as.Date(data$Date)

# Assign weeks
data <- data %>%
  mutate(Week = case_when(
    Date < as.Date("2023-09-19") ~ "Week 1",
    Date >= as.Date("2023-09-19") ~ "Week 2"
  ))


data <- data %>%
  mutate(Shelf.Life = case_when(
    Description %in% c('CD SKM 8-21', 'CD 2% MLK 8-21', 'CD WHL MLK 8-21') ~ 'High',
    Description %in% c('CD SKM HLF GAL', 'CD 2% MLK HLH GAL', 'CD WHL MLK HLF GAL') ~ 'Medium',
    Description %in% c('CD SKM 0-3', 'CD 2% MLK 0-3', 'CD WHL 0-3') ~ 'Low',
    TRUE ~ NA_character_
  ))
str(data)

data$Milk.Type <- factor(data$Milk.Type)
data$Week <- factor(data$Week)
data$Shelf.Life <- factor(data$Shelf.Life)
data$Day <- factor(data$Day)
data$ppu<-factor(data$ppu)


Logsales = log(sales, data=data)

data$log_sales <- log(data$sales)
data$log_units <- log(data$units)


lm_units<- lm(units~ Day + Week + Shelf.Life + Milk.Type + Week*Shelf.Life, data = data)
summary(lm_units)

lm_sales <- lm(sales~ Day + Week + Shelf.Life + Milk.Type + Week*Shelf.Life, data = data)
summary(lm_sales)

lm_units3<-lm(units ~ Week*Shelf.Life + Week*Milk.Type + Shelf.Life*Milk.Type,data= data)
summary(lm_units3)

stargazer(lm_units, lm_sales, type = "latex")  # 'type = "text"' for plain text output, you can also use "html" or "latex"

aov_units<- aov(sales~ Day + Week + Shelf.Life + Milk.Type + Shelf.Life*Milk.Type, data = data)
summary(aov_units)


emmeans(anova_result, pairwise~Week|Shelf.Life)
emmeans(anova_result, pairwise~Week|Shelf.Life*ppu)

emmeans(aov_units, pairwise~Shelf.Life*Milk.Type)

joint_tests(aov_units,by="Week")

emm_df <- summary(anova_result)$emmeans

# For Week and Shelf.Life interaction
emm_interaction <- emmeans(anova_result, specs = pairwise ~ Week | Shelf.Life)
emm_df<-as.data.frame(emm_interaction$emmeans)
# For Week, Shelf.Life, and Day interaction
emm_interaction_day <- emmeans(anova_result, specs = pairwise ~ Week | Shelf.Life * Day)

# Convert EMMs to a dataframe
emm_df <- summary(emm_interaction)$emmeans
emm_df_day <- summary(emm_interaction_day)$emmean
emm_df


ggplot(emm_df, aes(x = Shelf.Life, y = emmean, group = Week, fill = Week)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, position = position_dodge(0.9)) +
  labs(x = "Shelf life", y = "Estimated marginal means of sales ($)") + theme_minimal()


aov_units<- lm(units~ Day + Week + Shelf.Life + Milk.Type + Week*Shelf.Life*Milk.Type, data = data)
summary(aov_units)


total_units<-data%>%
  filter(Week=="Week 2")%>%
  group_by(Shelf.Life, Milk.Type)%>%
  summarize(total_units = sum(units))%>%
  ungroup()


total_units_wide<-pivot_wider(total_units, names_from = Shelf.Life, values_from = total_units)

total_units_wide<-column_to_rownames(total_units_wide, "Milk.Type")

chitest<-chisq.test(total_units_wide)

chisq.posthoc.test(total_units_wide, method = "holm")

results_df <- data.frame(
  Statistic = chitest$statistic,
  df = chitest$parameter,
  p.Value = chitest$p.value
)



### 2023 vs. 2022 ANALYSIS ###----

fresh <- raw_combined
# Create the 'day' column
day_column <- rep(1:84, each=1)

# Add the 'day' column to the beginning of the existing dataset
data <- data.frame(sno= day_column, fresh)

#graph-data setting 
data$week <- cut(data$sno, breaks=c(0,21,42,63,84), labels=c("Week -2", "Week -1", "Week 1", "Week 2"))
weeklymeans23 <- aggregate(unit23 ~ week + Description, data, mean)
weeklymeans22 <- aggregate(unit22 ~ week + Description, data, mean)
dataf <- data %>% filter(Description %in% c("CD 2% MLK HLH GAL", "CD SKIM HLF GAL", "CD WHL MLK HLF GAL"))
dataf <- data %>% filter(Description %in% c("CD 2% MLK HLH GAL", "CD SKIM HLF GAL", "CD WHL MLK HLF GAL"))
filtered_data <- subset(data, week %in% c("Week 1", "Week 2"))
view(filtered_data)
view(dataf)
view(weeklymeans22)
view(weeklymeans23)
color_mapping <- c("Week 1"="black", "CD 2% MLK HLH GAL" = "lightskyblue", "CD SKIM HLF GAL" = "olivedrab2", "CD WHL MLK HLF GAL" = "tomato1")
linetype_mapping <- c("CD 2% MLK HLH GAL" = "solid", "CD SKIM HLF GAL" = "solid", "CD WHL MLK HLF GAL" = "solid", "mean" = "dotted")


description_mapping <- c("CD 2% MLK HLH GAL" = "Reduced 2% Milk",
                         "CD SKIM HLF GAL" = "Fat Free Milk",
                         "CD WHL MLK HLF GAL" = "Whole Milk")
day_mapping <- c("tue" = "Tue",
                 "wed" = "Wed",
                 "thu" = "Thu","fri" = "Fri", "sat" = "Sat", "sun" = "Sun")

custom_order<-c("tue", "wed", "thu","fri", "sat", "sun", "mon" )
---------------------------------------------------------------------------------------------
  #Graph codes F1a
  
  p <- ggplot(data, aes(x=factor(Day, levels=custom_order), y=unit22, color=Description)) +
  geom_line(aes(group=Description)) +
  geom_point() +
  scale_color_manual(values=color_mapping, labels=description_mapping) +
  geom_hline(data=weeklymeans22, aes(yintercept=unit22, color=Description, linetype=Description)) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dotted","CD SKIM HLF GAL"="dotted" ),labels=c("CD WHL MLK HLF GAL"="Mean Whole Milk", 
                                                                                                                                   "CD 2% MLK HLH GAL"="Mean Reduced 2% Milk",
                                                                                                                                   "CD SKIM HLF GAL"="Mean Fat Free Milk")) +
  labs(title="Day of the week vs number of units sold in 2022", x="Time in days", y="Units sold in 2022") + facet_wrap(vars(week), nrow = 1)

print(p)
#Graph code F1b

p <- ggplot(filtered_data, aes(x=factor(Day, levels=custom_order), y=sale23, color=Description)) +
  geom_line(aes(group=Description)) +
  geom_point() +
  scale_color_manual(values=c(color_mapping, labels=description_mapping), guide = FALSE) +
  geom_hline(data=weeklymeans_sales23, aes(yintercept=sale23, color=Description, linetype=Description)) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dotted","CD SKIM HLF GAL"="dotted"), guide =FALSE) +
  labs(x="Day of the week", y="Sales ($)") + facet_wrap(vars(week), nrow = 1) +  scale_x_discrete(labels = function(x) sapply(x, function(day) paste(toupper(substring(day, 1, 1)), substring(day, 2), sep=""))) +
  # Weekly facet
  facet_wrap(vars(week), nrow = 1, scales = "free_x") + theme_minimal()

print(p)
####################################################

# Plot for F1c
# Calculate the total units sold per day
daily_totals <- merged_data_2023 %>%
  group_by(Day, week) %>%
  summarise(TotalUnitsDay = sum(unit23)) %>%
  ungroup()



# Merge the daily totals back to the main data
merged_data_2023_updated <- merge(merged_data_2023, daily_totals, by = c("Day", "week"))

# Calculate percentage for each milk type using the daily totals
merged_data_2023_updated <- merged_data_2023_updated %>%
  mutate(Percentage = (unit23 / TotalUnitsDay) * 100)

#Plot F1C 
p_f1c_corrected <- ggplot(merged_data_2023_updated, aes(x=factor(Day, levels=custom_order))) +
  
  # Bar plot for percentage of each milk type
  geom_bar(aes(y=Percentage, fill=Description), stat="identity", position="stack") +
  
  # Line plot for total units sold each day with dummy aesthetic for legend
  geom_line(aes(y=TotalUnitsDay, group=1, color="Total Units Sold"), size=1) +
  
  # Points for total units sold each day with dummy aesthetic for legend
  geom_point(aes(y=TotalUnitsDay, color="Total Units Sold"), size=1) +
  
  # Line plot for weekly mean units sold
  geom_hline(data=weeklymeans_units_2023, aes(yintercept=unit23, linetype=Description), color="black") +
  
  # Scales and legends
  scale_fill_manual(values=color_mapping, labels=description_mapping) +
  scale_color_manual(values=c("Total Units Sold"="black"), labels=c("Total Units Sold"="Total Units Sold")) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dashed", "CD SKIM HLF GAL"="dotdash"),
                        labels=c("CD WHL MLK HLF GAL"="Mean Whole Milk", 
                                 "CD 2% MLK HLH GAL"="Mean Reduced 2% Milk",
                                 "CD SKIM HLF GAL"="Mean Fat Free Milk")) +
  
  # Dual Y-axis
  scale_y_continuous(name="Percentage (%)", sec.axis=sec_axis(~ ., name="Total Units Sold in 2023")) +
  
  # Weekly facet
  facet_wrap(vars(week), nrow = 1, scales = "free_x") +
  
  # Plot details
  labs(title="Day of the week vs. Units and Percentage sold in 2023", x="Time in days") +
  
  labs(title="Day of the week vs. Units and Percentage sold in 2023", 
       x="Time in days", 
       color="Total",     # Custom title for color legend
       linetype="Means",  # Custom title for linetype legend
       fill="Milk Type") +  # Custom title for fill legend
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(p_f1c_corrected)

############################################################## 
#Figure F1d

# Calculate the total units sold per day for 2022
daily_totals_2022 <- merged_data_2023_updated %>%
  group_by(Day, week) %>%
  summarise(TotalUnitsDay22 = sum(unit22)) %>%
  ungroup()

# Merge the daily totals for 2022 back to the main data
merged_data_2022_updated <- merge(merged_data_2023_updated, daily_totals_2022, by = c("Day", "week"))

# Calculate percentage for each milk type for 2022 using the daily totals
merged_data_2022_updated <- merged_data_2022_updated %>%
  mutate(Percentage22 = (unit22 / TotalUnitsDay22) * 100)

# Weekly mean calculation for units sold in 2022
weeklymeans_units_2022 <- aggregate(unit22 ~ week + Description, merged_data_2022_updated, mean)

# Plot for F1d using 2022 data
p_f1d <- ggplot(merged_data_2022_updated, aes(x=factor(Day, levels=custom_order))) +
  
  # Bar plot for percentage of each milk type for 2022
  geom_bar(aes(y=Percentage22, fill=Description), stat="identity", position="stack") +
  
  # Line plot for total units sold each day for 2022
  geom_line(aes(y=TotalUnitsDay22, group=1), color="black") +
  
  # Points for total units sold each day for 2022
  geom_point(aes(y=TotalUnitsDay22), color="black") +
  
  # Line plot for weekly mean units sold for 2022
  geom_hline(data=weeklymeans_units_2022, aes(yintercept=unit22, linetype=Description), color="black") +
  
  # Scales and legends
  scale_color_manual(values=c("CD WHL MLK HLF GAL"="red", "CD 2% MLK HLH GAL"="blue", "CD SKIM HLF GAL"="green")) +
  scale_fill_manual(values=color_mapping, labels=description_mapping) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dashed","CD SKIM HLF GAL"="dotdash"),
                        labels=c("CD WHL MLK HLF GAL"="Mean Whole Milk", 
                                 "CD 2% MLK HLH GAL"="Mean Reduced 2% Milk",
                                 "CD SKIM HLF GAL"="Mean Fat Free Milk")) +
  
  # Dual Y-axis
  scale_y_continuous(name="Percentage (%)", sec.axis=sec_axis(~ ., name="Total Units Sold in 2022")) +
  
  # Weekly facet
  facet_wrap(vars(week), nrow = 1, scales = "free_x") +
  
  # Plot details for 2022
  labs(title="Day of the week vs. Units and Percentage sold in 2022", x="Time in days") +
  theme_minimal()

print(p_f1d)

#####################################################

# Figure F2a
merged_data_2023 <- subset(merged_data_2023, week %in% c("Week 1", "Week 2"))

# Calculate $ per unit sold for each day in 2023
merged_data_2023 <- merged_data_2023 %>%
  mutate(dollar_per_unit = sale23 / unit23)
merged_data_2023 <- merged_data_2023 %>%
  mutate(description_week = ifelse(week == "Week 1", "Week 1", as.character(Description)))
# Weekly mean calculation for $ per unit sold in 2023
weeklymeans_dollar_per_unit <- aggregate(dollar_per_unit ~ week + Description, merged_data_2023, mean)

view(custom_order)

# Plot for $ per unit sold in 2023
p_dollar_per_unit_2023 <- ggplot(merged_data_2023, aes(x=factor(Day, levels=custom_order), y=dollar_per_unit, color=Description)) +
  
  # Line plot for daily $ per unit values
  geom_line(aes(group=Description)) +
  geom_line(aes(group=Description, color=description_week)) +
  # Point plot for daily $ per unit values
  
  # Line plot for weekly mean values
  geom_hline(data=weeklymeans_dollar_per_unit, aes(yintercept=dollar_per_unit, color=Description, linetype=Description)) +
  
  # Scales and legends
  scale_color_manual(values=color_mapping, labels=description_mapping, guide=FALSE) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dotted","CD SKIM HLF GAL"="dotted"),
                        labels=c("CD WHL MLK HLF GAL"="Mean Whole Milk", 
                                 "CD 2% MLK HLH GAL"="Mean Reduced 2% Milk",
                                 "CD SKIM HLF GAL"="Mean Fat Free Milk"), guide =FALSE) +
  scale_x_discrete(labels = function(x) sapply(x, function(day) paste(toupper(substring(day, 1, 1)), substring(day, 2), sep=""))) +
  # Weekly facet
  facet_wrap(vars(week), nrow = 1, scales = "free_x") +
  
  # Plot details for 2023
  labs(x="Day of the week ", y="$ per unit sold") + 
  
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))



print(p_dollar_per_unit_2023)


##################################################

# Figure f2b

# Weekly mean calculation for total $ sold in 2022
weeklymeans_dollar_2022 <- aggregate(sale22 ~ week + Description, merged_data_2022_updated, mean)

# Plot for total $ sold in 2023
p_dollar_2022 <- ggplot(merged_data_2022_updated, aes(x=factor(Day, levels=custom_order), y=sale22, color=Description)) +
  
  # Line plot for daily total $ values
  geom_line(aes(group=Description)) +
  
  # Point plot for daily total $ values
  geom_point() +
  
  # Line plot for weekly mean total $ values
  geom_hline(data=weeklymeans_dollar_2023, aes(yintercept=sale23, color=Description, linetype=Description)) +
  
  # Scales and legends
  scale_color_manual(values=color_mapping, labels=description_mapping) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dotted","CD SKIM HLF GAL"="dotted"),
                        labels=c("CD WHL MLK HLF GAL"="Mean Whole Milk", 
                                 "CD 2% MLK HLH GAL"="Mean Reduced 2% Milk",
                                 "CD SKIM HLF GAL"="Mean Fat Free Milk")) +
  
  # Weekly facet
  facet_wrap(vars(week), nrow = 1, scales = "free_x") +
  
  # Plot details for 2023
  labs(title="Day of the week vs Total Sales ($) in 2022", x="Time in days", y="Total Sales ($)") +
  theme_minimal()

print(p_dollar_2022)

#################################################################

#f2c

# Calculate the total dollars earned per day for 2023
daily_dollar_totals_2023 <- merged_data_2023_updated %>%
  group_by(Day, week) %>%
  summarise(TotalDollarDay = sum(sale23)) %>%
  ungroup()

view(merged_data_2023_updated)

# Merge the daily dollar totals for 2023 back to the main data
merged_data_dollar_2023 <- merge(merged_data_2022_updated, daily_dollar_totals_2023, by = c("Day", "week"))

# Calculate mean of total dollars earned per day
mean_total_dollars <- mean(merged_data_dollar_2023$TotalDollarDay, na.rm = TRUE)

# Add MeanTotalDollars to the dataset
merged_data_dollar_2023$MeanTotalDollars <- mean_total_dollars

# Calculate percentage for each milk type for 2023 using the daily dollar totals
merged_data_dollar_2023 <- merged_data_dollar_2023 %>%
  mutate(PercentageDollar = (sale23 / TotalDollarDay) * 100)

# Weekly mean calculation for dollars earned in 2023
weeklymeans_dollar_2023 <- aggregate(sale23 ~ week + Description, merged_data_dollar_2023, mean)

# Plot for F2c using 2023 data
p_f2c_2023 <- ggplot(merged_data_dollar_2023, aes(x=factor(Day, levels=custom_order))) +
  # Bar plot for percentage of dollars earned from each milk type for 2023
  geom_bar(aes(y=PercentageDollar, fill=Description), stat="identity", position="stack") +
  
  # Line plot for total dollars earned each day for 2023
  geom_line(aes(y=TotalDollarDay, group=1), color="black") +
  
  # Points for total dollars earned each day for 2023
  geom_point(aes(y=TotalDollarDay), color="black") +
  
  # Line plot for weekly mean dollars earned for 2023
  geom_hline(data=weeklymeans_dollar_2023, aes(yintercept=sale23, linetype=Description), color="black") +
  
  # Horizontal line for mean total dollars
  geom_hline(yintercept = mean_total_dollars, color = "yellow", linetype = "dashed") +
  
  # Dummy line for the legend
  geom_line(aes(y = Inf, color = "Mean Total Dollars"), linetype = "dashed", show.legend = TRUE) +
  
  # Scales and legends
  scale_fill_manual(values=color_mapping, labels=description_mapping) +
  scale_color_manual(values = c("Mean Total Dollars" = "yellow")) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dotted", "CD 2% MLK HLH GAL"="dashed","CD SKIM HLF GAL"="dotdash"),
                        labels=c("CD WHL MLK HLF GAL"="Mean Whole Milk", 
                                 "CD 2% MLK HLH GAL"="Mean Reduced 2% Milk",
                                 "CD SKIM HLF GAL"="Mean Fat Free Milk")) +
  
  # Dual Y-axis
  scale_y_continuous(name="Percentage (%)", sec.axis=sec_axis(~ ., name="Total Sales ($) in 2023")) +
  
  # Weekly facet
  facet_wrap(vars(week), nrow = 1, scales = "free_x") +
  
  # Plot details for 2023
  labs(title="Compositional analysis for type of the milk sold and sales in 2023", 
       x="Time in days", 
       fill="Milk Type", 
       color="Total", 
       linetype="Means") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjusting text angle and position
# Print the plot
print(p_f2c_2023)


#####################################################

#Figure 2d 

# Calculate the total dollars earned per day for 2023
daily_dollar_totals_2023 <- merged_data_2023_updated %>%
  group_by(Day, week) %>%
  summarise(TotalDollarDay = sum(sale22)) %>%
  ungroup()

# Merge the daily dollar totals for 2022 back to the main data
merged_data_dollar_2023 <- merge(merged_data_2023_updated, daily_dollar_totals_2023, by = c("Day", "week"))

# Calculate mean of total dollars earned per day for 2022
mean_total_dollars_2023 <- mean(merged_data_dollar_2023$TotalDollarDay, na.rm = TRUE)

# Add MeanTotalDollars to the dataset for 2022
merged_data_dollar_2023$MeanTotalDollars <- mean_total_dollars_2023

# Calculate percentage for each milk type for 2022 using the daily dollar totals
merged_data_dollar_2023 <- merged_data_dollar_2023 %>%
  mutate(PercentageDollar = (sale22 / TotalDollarDay) * 100)


# Weekly mean calculation for dollars earned in 2022
weeklymeans_dollar_2023 <- aggregate(sale22 ~ week + Description, merged_data_dollar_2023, mean)

# Assuming 'Description' column has the milk types (whole, 2%, skim)
linetype_mapping_updated <- c("whole" = "dashed", "2%" = "dotted", "skim" = "dotdash")

# Weekly mean calculation for dollars earned in 2023
weeklymeans_dollar_2023 <- aggregate(sale22 ~ week + Description, merged_data_dollar_2023, mean)


# Assuming 'Description' contains values like "CD 2% MLK HLH GAL", "CD SKIM HLF GAL", "CD WHL MLK HLF GAL"
linetype_mapping_for_2d <- c("CD 2% MLK HLH GAL" = "dotted", 
                             "CD SKIM HLF GAL" = "dashed", 
                             "CD WHL MLK HLF GAL" = "dotdash")


p_f2d_2023 <- ggplot(merged_data_dollar_2023, aes(x=factor(Day, levels=custom_order))) +
  # Bar plot for percentage of dollars earned
  geom_bar(aes(y=PercentageDollar, fill=Description), stat="identity", position="stack") +
  
  # Line plot for total dollars earned each day
  geom_line(aes(y=TotalDollarDay, group=1, color="Total Dollars")) +
  geom_point(aes(y=TotalDollarDay, color="Total Dollars"), size=1) +
  
  # Line plot for weekly mean dollars earned (black in color)
  geom_hline(data=weeklymeans_dollar_2023, aes(yintercept=sale22, linetype=Description, color="Weekly Mean")) +
  
  # Horizontal line for mean total dollars (yellow color)
  geom_hline(aes(yintercept=MeanTotalDollars, linetype="Mean Total Dollars", color="Mean Total Dollars")) +
  
  # Scales and legends
  scale_fill_manual(values=color_mapping, labels=description_mapping) +
  scale_color_manual(values = c("Total Dollars" = "black", "Weekly Mean" = "black", "Mean Total Dollars" = "yellow")) +
  scale_linetype_manual(values=c("CD WHL MLK HLF GAL"="dashed", 
                                 "CD 2% MLK HLH GAL"="dotted",
                                 "CD SKIM HLF GAL"="dotdash",
                                 "Mean Total Dollars"="dotted"),
                        labels=c("CD WHL MLK HLF GAL"="Mean of Whole Milk", 
                                 "CD 2% MLK HLH GAL"="Mean of Reduced 2% Milk",
                                 "CD SKIM HLF GAL"="Mean of Fat Free Milk",
                                 "Mean Total Dollars"="Mean Total Dollars")) +
  scale_y_continuous(name="Percentage (%)", sec.axis=sec_axis(~ ., name="Total Sales ($) in 2022")) +
  facet_wrap(vars(week), nrow = 1, scales = "free_x") +
  labs(title="Compositional analysis for the type of milk sold and sales in 2022 ", 
       x="Time in days", 
       fill="Milk Type", 
       color="Line Type", 
       linetype="Line Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjusting text angle and position

print(p_f2d_2023) # Display the plot



########################################



data$Date <- as.Date(data$Date)

# Categorize the data into different milk types
data <- data %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))

# Assign weeks

view(data)
# Calculate the mean for each type of milk per shelf life category for each week
means <- data %>%
  group_by(Shelf.Life, Milk.Type, Week) %>%
  summarize(Mean = mean(units, na.rm = TRUE))

# Generate the plot split by week and with a common y-axis scale for all shelf life levels
p <- ggplot(data, aes(x = Date, y = units, color = Milk.Type, group = Description)) +
  geom_line(aes(linetype = "solid"), size = 1) +
  geom_point(size = 3) +
  geom_hline(data = means, aes(yintercept = Mean, color = Milk.Type, linetype = paste("Mean", Milk.Type)), size = 0.9) +
  scale_color_manual(values = c("Whole Milk" = "red", "2% Milk" = "blue", "Skim Milk" = "green")) +
  scale_linetype_manual(values = c("solid" = "solid", 
                                   "Mean Whole Milk" = "dotted", "Mean 2% Milk" = "dotted", "Mean Skim Milk" = "dotted")) +
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x")
scale_x_date(date_labels = "%a", date_breaks = "1 day", expand = c(0, 0)) +
  labs(title = "Units Sold by Date for Different Milk Types and Shelf Life Levels",
       x = "Date",
       y = "Units Sold",
       color = "Milk Type",
       linetype = "Mean Line") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Read the dataset (adjust the path if needed)

# Convert 'Date' to a Date object and ensure 'Units' is numeric
data$Date <- as.Date(data$Date)
data$units <- as.numeric(as.character(data$units))

# Create a factor for Day with levels ordered Tuesday through Monday
data$Day <- factor(data$Day, levels = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday"))

# Calculate means for each week and shelf life
mean_values <- data %>%
  group_by(Week, Shelf.Life, Day) %>%
  summarize(Mean = mean(Units))

# Plot
ggplot(data, aes(x = Day, y = Units, fill = Shelf.Life)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_line(data = mean_values, aes(x = Day, y = Mean, group = interaction(Week, Shelf.Life), color = Shelf.Life), linetype = "dotted") +
  labs(title = "Units Sold in 2023 by Shelf Life and Day",
       x = "Day",
       y = "Units Sold in 2023") +
  theme_minimal() +
  facet_wrap(~ Week, ncol = 1, scales = "free_x")


#############################################################################
# Generate the plot for total sales during the 2 weeks of the 2023 study
# Calculate the mean for each type of milk per shelf life category for each week

data <- data %>%
  mutate(Shelf.Life = case_when(
    Description %in% c('CD SKM 8-21', 'CD 2% MLK 8-21', 'CD WHL MLK 8-21') ~ 'High',
    Description %in% c('CD SKIM HLF GAL', 'CD 2% MLK HLH GAL', 'CD WHL MLK HLF GAL') ~ 'Medium',
    Description %in% c('CD SKIM 0-3', 'CD 2% MLK 0-3', 'CD WHL 0-3') ~ 'Low',
    TRUE ~ NA_character_
  ))


means_units <- data %>%
  group_by(Shelf.Life, Milk.Type, Week) %>%
  summarize(Mean = mean(units, na.rm = TRUE))

# Generate the plot with sale data
p_sales <- ggplot(data, aes(x = Date, y = units, color = Milk.Type, group = Description)) +
  geom_line(aes(linetype = "solid"), size = 1) +
  geom_point(size = 3) +
  geom_hline(data = means_units, aes(yintercept = Mean, color = Milk.Type, linetype = paste("Mean", Milk.Type)), size = 0.9) +
  scale_color_manual(values = c("Whole Milk" = "red", "2% Milk" = "blue", "Skim Milk" = "green")) +
  scale_linetype_manual(values = c("solid" = "solid", 
                                   "Mean Whole Milk" = "dotted", "Mean 2% Milk" = "dotted", "Mean Skim Milk" = "dotted")) +
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x") +
  scale_x_date(date_labels = "%a", date_breaks = "1 day", expand = c(0, 0)) +
  labs(title = "Total units sold by Date for Different Milk Types and Shelf Life Levels",
       x = "Date",
       y = "Total units",
       color = "Milk Type",
       linetype = "Mean Line") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p_sales)

# Categorize the data into different milk types
data <- data %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))

# Assign weeks
data <- data %>%
  mutate(Week = case_when(
    Date < as.Date("2023-09-11") ~ "Week 1",
    Date >= as.Date("2023-09-11") ~ "Week 2"
  ))

# Calculate the aggregated total units for each day
total_units_per_day <- data %>%
  group_by(Date) %>%
  summarize(Total_Units = sum(units, na.rm = TRUE))

# Merge the aggregated data back to the main dataset
data <- left_join(data, total_units_per_day, by = "Date")

# Calculate the percentage composition for each type of milk for each day
data <- data %>%
  mutate(Percentage = (units / Total_Units) * 100)

# Create the bar graph
p_units_bar <- ggplot(data, aes(x = Date, y = Percentage, fill = interaction(Milk.Type, Shelf.Life), group = Description)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(aes(y = Total_Units * 100 / max(Total_Units), group = 1, color = "Total"), size = 1) +  # Adjusted for percentage scale
  scale_fill_manual(values = c("Whole Milk.High" = "red", "2% Milk.High" = "blue", "Skim Milk.High" = "green",
                               "Whole Milk.Medium" = "red", "2% Milk.Medium" = "blue", "Skim Milk.Medium" = "green",
                               "Whole Milk.Low" = "red", "2% Milk.Low" = "blue", "Skim Milk.Low" = "green")) +
  scale_color_manual(values = c("Total" = "black")) +
  scale_y_continuous(name = "Percentage Composition", sec.axis = sec_axis(~ . * max(data$Total_Units) / 100, name = "Total Units Sold")) +
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x") +
  scale_x_date(date_labels = "%a", date_breaks = "1 day", expand = c(0, 0)) +
  labs(title = "Total Units by Date for Different Milk Types and Shelf Life Levels",
       x = "Date",
       fill = "Milk Type and Shelf Life",
       color = "Total Units") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_units_bar)

####################################################################

# Convert the Date column to Date type
data$Date <- as.Date(data$Date)

# Categorize the data into different milk types
data <- data %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))


# Assign weeks
data <- data %>%
  mutate(Week = case_when(
    Date < as.Date("2023-09-11") ~ "Week 1",
    Date >= as.Date("2023-09-11") ~ "Week 2"
  ))

# Calculate the aggregated total sales for each day
total_sales_per_day <- data %>%
  group_by(Date) %>%
  summarize(Total_Sales = sum(sales, na.rm = TRUE))

# Merge the aggregated data back to the main dataset
data <- left_join(data, total_sales_per_day, by = "Date")
view(data)
# Calculate the percentage composition for each type of milk for each day
data <- data %>%
  mutate(Percentage = (sales / Total_Sales) * 100)

# Create the bar graph
p_sales_bar <- ggplot(data, aes(x = Date, y = Percentage, fill = interaction(Milk.Type, Shelf.Life), group = Description)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(aes(y = Total_Sales * 100 / max(Total_Sales), group = 1, color = "Total"), size = 1) +  # Adjusted for percentage scale
  scale_fill_manual(values = c("Whole Milk.High" = "red", "2% Milk.High" = "blue", "Skim Milk.High" = "green",
                               "Whole Milk.Medium" = "red", "2% Milk.Medium" = "blue", "Skim Milk.Medium" = "green",
                               "Whole Milk.Low" = "red", "2% Milk.Low" = "blue", "Skim Milk.Low" = "green")) +
  scale_color_manual(values = c("Total" = "black")) +
  scale_y_continuous(name = "Percentage Composition", sec.axis = sec_axis(~ . * max(data$Total_Sales) / 100, name = "Total Sales in Dollars")) +
  facet_grid(Shelf.Life ~ Week, scales = "free_x", space = "free_x") +
  scale_x_date(date_labels = "%a", date_breaks = "1 day", expand = c(0, 0)) +
  labs(title = "Total Sales in Dollars by Date for Different Milk Types and Shelf Life Levels",
       x = "Date",
       fill = "Milk Type and Shelf Life",
       color = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_sales_bar)


#############################################################################3


# Convert the Date column to Date type
data$Date <- as.Date(data$Date)

view(data)

# Assign weeks
data <- data %>%
  mutate(Week = case_when(
    Date <= as.Date("2023-09-19") ~ "Week 1",
    Date > as.Date("2023-09-19") ~ "Week 2"
  ))

# Filter the data to keep only the records from the two weeks of the 2023 study
data_2023 <- data %>%
  filter(Week %in% c("Week 1", "Week 2"))

# Convert the Date to factor for 'Day of the week'
data_2023$DayOfWeek <- weekdays(data_2023$Date)

# Execute ANOVA
anova_result <- aov(unit22 ~ Milk.Type + DayOfWeek + week, data = data_2023)
summary(anova_result)


# Check the structure of the dataset
str(data_2023)

# Ensure all variables are present and correctly formatted
head(data_2023[, c("unit23", "Milk.Type", "sale23", "DayOfWeek", "week")])

cat("Milk.Type levels:", levels(data$Milk.Type), "\n")
cat("DayOfWeek levels:", levels(data_2023$DayOfWeek), "\n")
cat("week levels:", levels(data_2023$week), "\n")

data_2023$Milk.Type <- factor(data_2023$Milk.Type)
data_2023$week <- factor(data_2023$week)


##############################################

# Categorize the data into different milk types
data<- X2023data %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))


# Convert the Date column to Date type
data$Date <- as.Date(data$Date)

# Assign weeks
data <- data %>%
  mutate(Week = case_when(
    Date < as.Date("2023-09-19") ~ "Week 1",
    Date >= as.Date("2023-09-19") ~ "Week 2"
  ))


data <- data %>%
  mutate(Shelf.Life = case_when(
    Description %in% c('CD SKM 8-21', 'CD 2% MLK 8-21', 'CD WHL MLK 8-21') ~ 'High',
    Description %in% c('CD SKM HLF GAL', 'CD 2% MLK HLH GAL', 'CD WHL MLK HLF GAL') ~ 'Medium',
    Description %in% c('CD SKM 0-3', 'CD 2% MLK 0-3', 'CD WHL 0-3') ~ 'Low',
    TRUE ~ NA_character_
  ))
str(data)

data$Milk.Type <- factor(data$Milk.Type)
data$Week <- factor(data$Week)
data$Shelf.Life <- factor(data$Shelf.Life)
data$Day <- factor(data$Day)
data$ppu<-factor(data$ppu)


data <- # your data frame
  aov.models <- list()
cols <- names(data)[4:ncol(data)]  # Assuming you want to start from the 4th column

for (col in cols) {
  formula <- reformulate(termlabels = c("Milk.Type", "Week", "Shelf.Life", "Day"), response = col)
  aov.models[[col]] <- aov(formula, data = data)
}


anova_result <- aov(sales ~ Milk.Type + Day + Week +  Shelf.Life, data = data)
summary(anova_result)
summary_results <- summary(anova_result)
anova_table <- as.data.frame(summary_results[[1]])
kable(anova_table, caption = "ANOVA Results", format = "html", digits = 3, align = 'c')


anova_table <- as.data.frame(summary_results[[1]])

# Convert the ANOVA table to a graphical object
table_grob <- tableGrob(anova_table)

# Save the table as a JPG
jpeg("anova_results.jpg", width = 800, height = 600)
grid.draw(table_grob)
dev.off()



ggplot(data, aes(x = Day, y = units)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Units Sold per Day of the Week",
       x = "Day of the Week",
       y = "Units Sold")



tbl_summary(data, by=Week)

data %>%
  tbl_strata ( strata=Week, ~.x %>%
                 tbl_summary(by = Description))

anova_result <- aov(sales~ Day + Week + Shelf.Life + Milk.Type + Week*Shelf.Life, data = data)
summary(anova_result)


emmeans(anova_result, pairwise~Week|Shelf.Life)
emmeans(anova_result, pairwise~Week|Shelf.Life*ppu)

joint_tests(anova_result,by="Shelf.Life")

emm_df <- summary(anova_result)$emmeans

# For Week and Shelf.Life interaction
emm_interaction <- emmeans(anova_result, specs = pairwise ~ Week | Shelf.Life)
emm_df<-as.data.frame(emm_interaction$emmeans)
# For Week, Shelf.Life, and Day interaction
emm_interaction_day <- emmeans(anova_result, specs = pairwise ~ Week | Shelf.Life * Day)

# Convert EMMs to a dataframe
emm_df <- summary(emm_interaction)$emmeans
emm_df_day <- summary(emm_interaction_day)$emmean
emm_df


ggplot(emm_df, aes(x = Shelf.Life, y = emmean, group = Week, fill = Week)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, position = position_dodge(0.9)) +
  labs(x = "Shelf life", y = "Estimated marginal means of units sold") + theme_minimal()



### ADDITIONAL ANALYSIS 2023 VS. 2022 ###----

# First, we'll reshape the data for the unit columns
dataf <- raw_combined

unit_2022 <- dataf %>%
  select(Day, Description,unit22,week)%>%
  rename(Unit = unit22 )%>%
  mutate(Year = "2022")

unit_2023 <- dataf %>%
  select(Day, Description,unit23, week) %>%
  rename(Unit = unit23) %>%
  mutate(Year = "2023")


# Combine the reshaped unit data
combined_units <- rbind(unit_2022,unit_2023)

# Now, we'll reshape the data for the sale columns
sale_2022 <- dataf %>%
  select(Day, Description,sale22,week) %>%
  rename(Sale = sale22) %>%
  mutate(Year = "2022")

sale_2023 <- dataf %>%
  select(Day, Description,sale23,week) %>%
  rename(Sale = sale23) %>%
  mutate(Year = "2023")

# Combine the reshaped sale data
combined_sales <- rbind(sale_2022, sale_2023)

# Now, we merge the combined units and sales data on the common columns
final_data <- left_join(combined_units, combined_sales, by=c("Day","Description", "week", "Year"))

# View the final reshaped data

data_2023 <- final_data %>%
  filter(Year == "2023")


data_2023$int<-ifelse(data_2023$week %in% c("Week -2", "Week -1"), "before", "after")


data_2023 <- data_2023 %>%
  mutate(Milk.Type = case_when(
    grepl("WHL", Description) ~ "Whole Milk",
    grepl("2%", Description) ~ "2% Milk",
    grepl("SKIM|SKM", Description) ~ "Skim Milk",
    TRUE ~ NA_character_
  ))


daily_totals <- final_data %>%
  group_by(Day, week, Year) %>%
  summarize(total = sum(Sale)) %>%
  ungroup()

final_data %>%
  tbl_strata ( strata=Year, ~.x %>%
                 tbl_summary(by = week))
final_data %>%
  tbl_summary(by = week) %>%
  add_p()


totals22 <- daily_totals$total[daily_totals$Year == "2022"]
totals23 <- daily_totals$total[daily_totals$Year == "2023"]

test_totals_22v23 <- t.test(totals22, totals23)


########################ANOVAS##################


final_data$Milk.Type <- factor(final_data$Milk.Type)
final_data$int <- factor(final_data$int)
final_data$Year <- factor(final_data$Year)
final_data$Day <- factor(final_data$Day)

anova_result <- aov( Unit ~ Milk.Type + Day + Year, data = final_data)
summary(anova_result)

totalweek1<-data_2023$sales[data_2023$Week == "Week 1"]
totalweek2<-data_2023$sales[data_2023$Week == "Week 2"]


test_totals_week1vweek2 <- t.test(totalweek1, totalweek2)
test_totals_week1vweek2


data_2023$Milk.Type <- factor(data_2023$Milk.Type)
data_2023$Shelf.Life <- factor(data_2023$Shelf.Life)
data_2023$Week <- factor(data_2023$Week)
data_2023$Year <- factor(data_2023$Year)
data_2023$DayOfWeek <- factor(data_2023$DayOfWeek)

anova_result <- aov( sales ~ Milk.Type + DayOfWeek + Week+ Shelf.Life, data = data_2023)
summary(anova_result)

ggplot(final_data, aes(x = Day, y = Unit)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Units Sold per Day of the Week",
       x = "Day of the Week",
       y = "Units Sold")



######################################

final_data <- final_data %>% select(-Description)

# Grouping by Milk Type, Week, and Year, and summarizing average units sold
average_units <- final_data %>%
  group_by(Milk.Type, week, Year) %>%
  summarise(Average_Units = mean(Unit, na.rm = TRUE)) %>%
  ungroup()

# Prepare data for ANOVA
anova_data <- average_units %>% 
  mutate(Year = as.factor(Year))

# Performing ANOVA
anova_table <- anova_data %>%
  tbl_uvregression(
    method = lm,
    y = Average_Units,
    include = -c(week, Milk.Type)
  )

# Print the ANOVA table
print(anova_table)
