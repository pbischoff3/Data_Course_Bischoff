library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)


dat <- read_csv("../Data_Course_Bischoff/Data/BioLog_Plate_Data.csv")


# 1: Cleans this data into tidy (long) form

dat2 <- dat %>% 
  pivot_longer(starts_with("Hr_"),
               names_to = "Time", 
               values_to = "Absorbance",
               names_prefix = "Hr_") %>% 
  mutate( Time = as.numeric(Time)) %>% 
  
  # 2: Creates a new column specifying whether a sample is from soil or water
  
  mutate(SampleType = case_when(`Sample ID` == "Clear_Creek"~ "Water",
                                `Sample ID` == "Waste_Water" ~ "Water",
                                TRUE ~ "Soil")) 

# 3: Generates a plot that matches this one (note just plotting dilution == 0.1):

dat2 %>% 
  filter(Dilution == .1) %>% 
  arrange("-Substrate") %>% 
  ggplot(aes(x=Time, y= Absorbance,color=SampleType))+
  geom_smooth(se = FALSE) +
  facet_wrap(~Substrate)+
  theme_minimal()+
  theme(
    text = element_text(size = 8))
  

#4: Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group):
#This plot is just showing values for the substrate “Itaconic Acid”


### CLEAR CREEK
    Clear_creek_24_.1 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 24, Dilution ==.100) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_creek_24_.01 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 24, Dilution ==.010) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_creek_24_.001 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 24, Dilution ==.001) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_Creek_24_list <- #### first round, 24 hours
  list(Clear_creek_24_.1,Clear_creek_24_.01,Clear_creek_24_.001) %>% 
  reduce(full_join)


    Clear_creek_48_.1 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 48, Dilution ==.100) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_creek_48_.01 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 48, Dilution ==.010) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_creek_48_.001 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 48, Dilution ==.001) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_Creek_48_list <- #### 48 hours
  list(Clear_creek_48_.1,Clear_creek_48_.01,Clear_creek_48_.001) %>% 
  reduce(full_join)

  
    Clear_creek_144_.1 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 144, Dilution ==.100) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_creek_144_.01 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 144, Dilution ==.010) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_creek_144_.001 <- dat2 %>% 
  filter(Substrate == "Itaconic Acid",`Sample ID` == "Clear_Creek", Time == 144, Dilution ==.001) %>% 
  mutate(Mean_absorbance = mean(Absorbance))

    Clear_Creek_144_list <- #### 144 hours
  list(Clear_creek_144_.1,Clear_creek_144_.01,Clear_creek_144_.001) %>% 
  reduce(full_join)

    Clear_Creek_TOTAL <- #### TOTAL
  list(Clear_Creek_144_list,Clear_Creek_48_list,Clear_Creek_24_list) %>% 
  reduce(full_join)
### END OF CLEAR CREEK
    
    

### SOIL 1
    Soil_1_24_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 24, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_24_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 24, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_24_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 24, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_24_list <- #### first round, 24 hours
      list(Soil_1_24_.1,Soil_1_24_.01,Soil_1_24_.001) %>% 
      reduce(full_join)
    
    
    Soil_1_48_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 48, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_48_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 48, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_48_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 48, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_48_list <- #### 48 hours
      list(Soil_1_48_.1,Soil_1_48_.01,Soil_1_48_.001) %>% 
      reduce(full_join)
    
    
    Soil_1_144_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 144, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_144_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 144, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_144_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_1", Time == 144, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_1_144_list <- #### 144 hours
      list(Soil_1_144_.1,Soil_1_144_.01,Soil_1_144_.001) %>% 
      reduce(full_join)
    
    Soil_1_TOTAL <- #### TOTAL
      list(Soil_1_144_list,Soil_1_48_list,Soil_1_24_list) %>% 
      reduce(full_join)
### END OF SOIL_1   
    
    

### SOIL 2
    Soil_2_24_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 24, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_24_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 24, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_24_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 24, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_24_list <- #### first round, 24 hours
      list(Soil_2_24_.1,Soil_2_24_.01,Soil_2_24_.001) %>% 
      reduce(full_join)
    
    
    Soil_2_48_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 48, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_48_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 48, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_48_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 48, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_48_list <- #### 48 hours
      list(Soil_2_48_.1,Soil_2_48_.01,Soil_2_48_.001) %>% 
      reduce(full_join)
    
    
    Soil_2_144_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 144, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_144_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 144, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_144_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Soil_2", Time == 144, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Soil_2_144_list <- #### 144 hours
      list(Soil_2_144_.1,Soil_2_144_.01,Soil_2_144_.001) %>% 
      reduce(full_join)
    
    Soil_2_TOTAL <- #### TOTAL
      list(Soil_2_144_list,Soil_2_48_list,Soil_2_24_list) %>% 
      reduce(full_join)
    ### END OF SOIL_1          
    
    
    
### Waste Water
    Waste_Water_24_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 24, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_24_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 24, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_24_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 24, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_24_list <- #### first round, 24 hours
      list(Waste_Water_24_.1,Waste_Water_24_.01,Waste_Water_24_.001) %>% 
      reduce(full_join)
    
    
    Waste_Water_48_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 48, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_48_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 48, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_48_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 48, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_48_list <- #### 48 hours
      list(Waste_Water_48_.1,Waste_Water_48_.01,Waste_Water_48_.001) %>% 
      reduce(full_join)
    
    
    Waste_Water_144_.1 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 144, Dilution ==.100) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_144_.01 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 144, Dilution ==.010) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_144_.001 <- dat2 %>% 
      filter(Substrate == "Itaconic Acid",`Sample ID` == "Waste_Water", Time == 144, Dilution ==.001) %>% 
      mutate(Mean_absorbance = mean(Absorbance))
    
    Waste_Water_144_list <- #### 144 hours
      list(Waste_Water_144_.1,Waste_Water_144_.01,Waste_Water_144_.001) %>% 
      reduce(full_join)
    
    Waste_Water_TOTAL <- #### TOTAL
      list(Waste_Water_144_list,Waste_Water_48_list,Waste_Water_24_list) %>% 
      reduce(full_join)
    ### WASTE WATER  
    
    
### Combine all substrates
I_Acid_TOTAL <- #### TOTAL
      list(Waste_Water_TOTAL,Soil_2_TOTAL,Soil_1_TOTAL,Clear_Creek_TOTAL) %>% 
      reduce(full_join)
    

Question_4 <- I_Acid_TOTAL %>% 
  ggplot(aes(x=Time, y=Mean_absorbance,color=`Sample ID`))+
  theme_minimal()+
  facet_wrap(~Dilution)+
  geom_line()

anim <- Question_4+
  transition_reveal(Time)

anim
