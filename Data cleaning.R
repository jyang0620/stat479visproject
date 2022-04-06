library(readr)
library(tidyverse)
library(ggplot2)

# Read in data
ds_sal = read_csv("https://uwmadison.box.com/shared/static/g60evvocvsmpqdy6v2i7nkqho4ff7mac.csv")
costoliv = read_csv("https://uwmadison.box.com/shared/static/6xphn35fqqq8svwr3y9iis1cpxjqfo8b.csv")

# Sneak Preview
head(ds_sal)
names(ds_sal)
head(costoliv)
names(costoliv)

# Separating Location into City and State
ds_salfix = ds_sal %>%
  separate(Location, c('City','State'),sep = ', ')
ds_salfix[127,]$City = 'Los Angeles'
ds_salfix[127,]$State = 'CA'

# Filtering columns to our desired variables
ds_salfix = ds_salfix[,c(1:3,5:9,14:15,20:22)]
ds_salfix$cost.of.living = NA #empty column

for (i in seq(length(ds_salfix$`Metro Area`))) {
  # print(i)
  j = which(costoliv$`Metro Area` == ds_salfix[i,]$`Metro Area`)
  ds_salfix[i,]$cost.of.living = costoliv[j,]$`Cost of Living Index`
}

# Initial Overview plot looking at average salary vs cost of living
ds_salfix %>%
  drop_na(cost.of.living) %>%
  ggplot() +
  geom_point(aes(`Avg Salary(K)`, cost.of.living, col = State))
