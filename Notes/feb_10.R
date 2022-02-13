library(tidyverse)

#filter = choses rows based on expressions 
#group_by() / summarize()
#mutate() = create new columns based on existing ones
#arrange() = not very useful, sets order of rows
  #desc() for descending order
# select() = like filter but works on columns instead of row. based on name
  # starts_width()
  # ends_width()
  # contains()

iris %>% 
  select(contains("tal"))

iris %>% 
  mutate(Sepal.Area = Sepal.Length * Sepal.Width) %>% 
  filter(Species != "setosa") %>% #!= means does not equal
  filter(Sepal.Length < 6.5 | Sepal.Width < 2.75) %>% # | means "or"
  ggplot(aes(x=Sepal.Length,y=Sepal.Area,color=Species))+
  geom_point()



df <- read_csv("./Data/wide_income_rent.csv")
df %>% 
  ggplot(aes(x=income,y=rent,color=state))+
  geom_point()
# this fails, as income is on row, not column
  # need a "rent, income, and state" column
names(df)
glimpse(df)


pivot_longer(df,!variable,names_to = "State",values_to = "USD") %>%   #variable is name of column we did not want to pivot. can use a minus here too
  ggplot(aes(color=variable,x=State,y=USD)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle=90,hjust=1))


df %>% 
  select(starts_with("Pue"))


utah <- read.csv("./Data/Utah_Religions_by_County.csv")
view(utah)

# when pivoting, single variable over multiple columns

utah_long <- utah %>% 
  pivot_longer(-c("County","Pop_2010","Religious"),
               names_to = "Religion",
               values_to = "Proportion")

evang_order <- utah_long %>% 
  filter(Religion == "Muslim") %>% 
  arrange(desc(Proportion))


utah_long %>% 
  mutate(County = factor(County,levels = evang_order$County)) %>% 
  ggplot(aes(x=County,y=Proportion,fill=Religion))+
  geom_col()

evang_order

