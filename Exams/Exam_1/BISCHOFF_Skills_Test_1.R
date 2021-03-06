library(tidyverse)
library(dplyr)


# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)

df <- read.csv("C:\\Users\\pbisc\\Desktop\\Data_Course_Bischoff\\Exams\\Exam_1\\cleaned_covid_data.csv")

df

#II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)
      #Use the tidyverse suite of packages
      #Selecting rows where the state starts with “A” is tricky (you can use the grepl() function or just a vector of those states if you prefer)
starts_w_a <- "^A"
A_states <- subset(df, grepl(starts_w_a, df$Province_State))
view(A_states)

#III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)
  #Create a scatterplot
  #Add loess curves WITHOUT standard error shading
  #Keep scales “free” in each facet
ggplot(A_states,aes(x=Last_Update,y=Deaths)) +
  geom_point(aes(color=Province_State)) +
  facet_wrap(~Province_State,scales = "free") +
  geom_smooth(method="loess",aes(group=Province_State),color="black",se=FALSE)

#IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as 
# a new data frame object called state_max_fatality_rate. (20 pts)
  #I’m looking for a new data frame with 2 columns:
      #  “Province_State”
      #“Maximum_Fatality_Ratio”
  #Arrange the new data frame in descending order by Maximum_Fatality_Ratio
  #This might take a few steps. Be careful about how you deal with missing values!
state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(-Maximum_Fatality_Ratio) 
state_max_fatality_rate
  
#  V. Use that new data frame from task IV to create another plot. (20 pts)

#X-axis is Province_State
#Y-axis is Maximum_Fatality_Ratio
#bar plot
#x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
#X-axis labels turned to 90 deg to be readable
#Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.
ggplot(state_max_fatality_rate,aes(x = reorder(Province_State, -Maximum_Fatality_Ratio),y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

#VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time

#You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.


    # This kicked my butt, so I am not answering the extra credit, haha