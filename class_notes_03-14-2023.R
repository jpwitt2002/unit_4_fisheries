# 2023-03-14
# JPW
# data joins yippee 
# mutating joins for adding new variables to one table from matching observations in another table
# filtering joins for reducing first frame according to what is in the the second table
# anti join to show things with no match

library(tidyverse)

data1 = data.frame(ID = c(1,2), 
                   X1 = c("a1", "a2"))
data2 = data.frame(ID = c(2,3), 
                   X2 = c("b1", "b2"))

# left join
data12 = left_join(data1, data2, by="ID")
# you can also code like this
data12 = data1 %>%
  left_join(data2, by="ID")
# right join would preserve data2 instead

# inner join only preserves things in common between both datasets
data12 = data1 %>%
  inner_join(data2)

# full join preserves everything
data12 = data1 %>%
  full_join(data2)

# filtering joins

data12 = data1 %>% 
  semi_join(data2)
# semi to only keep columns from the first set that have matching id in the second

data12 = data1 %>%
  anti_join(data2)
# anti to only give data from the first that is missing in the second


# long and wide dataframes
# used to be gather and spread but no longer 
# now its pivot longer and pivot wider bc make it complicated ig

survey = data.frame(quadrat_id = c(101, 102, 103, 104), 
                    barnacle_n = c(2, 11, 8, 27), 
                    chiton_n = c(1, 0, 0, 2), 
                    mussel_n = c(0, 1, 1, 4))

long = survey %>%
  pivot_longer(cols = c("barnacle_n", "chiton_n", "mussel_n"), 
               names_to="taxon", 
               values_to="counts")


wide = long %>%
  pivot_wider(names_from = taxon, 
              values_from = counts)
# Exercise 1.2

ggplot(data=wide) +
  geom_point(aes(x=quadrat_id, y=barnacle_n, color="red")) + 
  geom_point(aes(x=quadrat_id, y=chiton_n, color="green")) +
  geom_point(aes(x=quadrat_id, y=mussel_n, color="blue"))
# legend and y-lab are messed up

ggplot(data=long) +
  geom_point(aes(x=quadrat_id, y=counts, color=taxon))
# ggplot better in long format

# wide can be helpful when modeling 


  



