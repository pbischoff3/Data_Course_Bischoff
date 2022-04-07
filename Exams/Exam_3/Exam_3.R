# Load Packages ####
library(tidyverse)
library(janitor)
library(modelr)
library(easystats)
library(broom)
# Load and Clean Faculty Salaries####
faculty <- read_csv("./FacultySalaries_1995.csv")
glimpse(faculty)

faculty <- janitor::clean_names(faculty)

faculty <- faculty %>% 
  select(-ends_with("_comp")) %>% 
  select(-starts_with("num_")) %>% 
  select(-ends_with("_comp_all")) %>% 
  select(-ends_with("_salary_all")) %>% 
  filter(tier != "VIIB")

faculty <- faculty %>% 
  rename(
    full = avg_full_prof_salary,
    assist = avg_assist_prof_salary,
    assoc = avg_assoc_prof_salary
  )

faculty <- faculty %>% 
  pivot_longer(cols = c(full,assoc,assist),
               names_to = "rank",
               values_to = "salary")
glimpse(faculty)

# Create Box-and-Whisker####
faculty %>% 
  ggplot(aes(x=rank,y=salary,fill=rank))+
  geom_boxplot()+
  facet_grid(~tier)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60),
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        strip.text.x = element_text(size=14))

# ANOVA test according to State Tier and Rank on Salary ####
anova <- aov(salary ~ state + tier + rank, data = faculty)
summary(anova)

# Load juniper_oils.csv and clean ####
oil <- read_csv("./Juniper_Oils.csv")
glimpse(oil)

oil <- oil %>% 
  select(c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene",
           "cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene",
           "widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol",
           "cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal","YearsSinceBurn")) %>% 
  janitor::clean_names()

oil <- oil %>% 
  pivot_longer(cols = !years_since_burn,
               names_to = "chemical_id",
               values_to = "concentration")
glimpse(oil)
# Graph with a facet ~chemical_id ####
oil %>% 
  ggplot(aes(x=years_since_burn,y=concentration))+
  geom_smooth()+
  facet_wrap(~chemical_id, scales = "free_y")+
  theme_minimal()

# GLM to find significant chemicals ####
x <- oil %>% 
  glm(formula = concentration ~ years_since_burn + chemical_id,
      family = "gaussian")
summary(x)

x_adj <- broom::tidy(x) %>% 
  mutate(term = term %>% str_remove_all("chemical_id"))

x_adj <- x_adj %>% 
  filter(p.value<=.05)

print(x_adj)
