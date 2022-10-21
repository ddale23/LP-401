#load in packages
library(tidyverse)
library(gghighlight)
library(gridExtra)
library(gt)
library(scales)
library(gtExtras)

#read in data saved to local computer (source via Community Solutions - Referenced in paper)
base_data = read.csv("C:/Users/drez_/Desktop/lp_401_data_use.csv")

#graph on page 3 of paper
base_data %>%
  ggplot(aes(MedianHouseholdIncome, ChildPovertyRate)) + geom_point(size = 7.5, alpha = 1, 
                                                                    color = "black", fill = "black") + 
  gghighlight(Geography == 'St.Clair-Superior', label_key = Geography, 
              unhighlighted_params = list(color = alpha("lightblue", 0.75))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 11, hjust = 0.5), 
        axis.text = element_text(size = 10, face = 'bold'),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Comparing Median Household Income & Child Poverty Rate in Cleveland",
       subtitle = 'St. Clair-Superior Highlighted in Black',
       caption = 'By: Drezdan Dale | Data via CommunitySolutions.com',
       y = "Child Poverty Rate", 
       x = "Median Household Income (in $)")

#mutated data from Community Solutions - applied rank function in excel to data
st_clair = read_csv("C:/Users/drez_/Desktop/lp_401_stclair.csv")

#Data table from page 2 of paper
st_clair %>%
  mutate(Value = case_when( Value > 100 ~ label_dollar() (Value),
        Value < 100 ~ label_percent()(Value))) %>%
  gt(rowname_col = 'Metric') %>%
  gt_theme_espn() %>%
  tab_header(
    title = 'St.Clair - Superior Neighborhood',
    subtitle = 'Key Metrics') %>%
  cols_width (
    Value ~ 100,
    Rank ~ 100
  ) %>%
  cols_label(Value = 'Value',
             Rank = 'CLE Rank') %>%
  tab_source_note(
    source_note = "CLE Rank out of 34 qualifying Neighbohoods"
  ) %>%
  tab_source_note(
    source_note = "Data Source: CommunitySolutions.com"
  ) 


















