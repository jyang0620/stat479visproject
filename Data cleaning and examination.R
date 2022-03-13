library(readr)
library(tidyverse)
library(ggplot2)

# Read in data
ds_sal = read_csv("https://uwmadison.box.com/shared/static/l2n9u9d97yxzibvd71y7a370kt0pj1pu.csv")
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
ds_salfix$cost.of.living = NA #empty column

# Filtering columns to our desired variables
ds_salfix = ds_salfix[,c(1:3,5:8,13:14,19:21)]

# Appending Cost of Living onto dataframe
for(i in seq(length(ds_salfix$City))){
  if (ds_salfix[i,]$City %in% costoliv$City) {
    ix = which(costoliv$City == ds_salfix[i,]$City)
    for (j in ix){
      if(ds_salfix[i,]$State == costoliv[j,]$State){
        print(i)
        print(ds_salfix[i,]$City)
        print(j)
        print(costoliv[j,]$State)
        ds_salfix[i,]$cost.of.living = costoliv[j,]$`Cost of Living Index`
      }
    }
  }
}

# Initial Overview plot looking at average salary vs cost of living
ds_salfix %>%
  drop_na(cost.of.living) %>%
  ggplot() +
  geom_point(aes(`Avg Salary(K)`, cost.of.living, col = State))

# Searching for missing cities in our data frame
missingcities = c()
`%!in%` <- Negate(`%in%`)
for(i in seq(length(ds_salfix$City))){
  if (ds_salfix[i,]$City %!in% costoliv$City) {
    missingcities = c(missingcities, ds_sal[i,]$Location)
  }
}
missingcities %>%
  unique() 

# A lot of suburbs of metropolitan areas, with the large ones being from the SF
# Bay Area, as well as DC, New York, Philadelphia, Boston and Seattle, or areas
# involving big tech. Need way to account for these.

