library(tidyverse)
df <- read.csv("AmesHousing.csv")

df <- df %>%
    select(Overall.Qual,Gr.Liv.Area,Total.Bsmt.SF,Garage.Cars,Year.Built,Full.Bath, Bedroom.AbvGr
           ,TotRms.AbvGrd,Garage.Area, Neighborhood, SalePrice) %>%
    filter(if_all(everything(), ~!is.na(.))) %>%
    mutate(YearsAgo.Built = 2026 - Year.Built) %>%
    mutate(Neighborhood = case_when(
      Neighborhood == "NAmes"     ~ 0,
      Neighborhood == "Gilbert"   ~ 1,
      Neighborhood == "StoneBr"   ~ 2,
      Neighborhood == "NWAmes"    ~ 3,
      Neighborhood == "Somerst"   ~ 4,
      Neighborhood == "BrDale"    ~ 5,
      Neighborhood == "NPkVill"   ~ 6,
      Neighborhood == "NridgHt"   ~ 7,
      Neighborhood == "Blmngtn"   ~ 8,
      Neighborhood == "NoRidge"   ~ 9,
      Neighborhood == "SawyerW"   ~ 10,
      Neighborhood == "Sawyer"    ~ 11,
      Neighborhood == "Greens"    ~ 12,
      Neighborhood == "BrkSide"   ~ 13,
      Neighborhood == "OldTown"   ~ 14,
      Neighborhood == "IDOTRR"    ~ 15,
      Neighborhood == "ClearCr"   ~ 16,
      Neighborhood == "SWISU"     ~ 17,
      Neighborhood == "Edwards"   ~ 18,
      Neighborhood == "CollgCr"   ~ 19,
      Neighborhood == "Crawfor"   ~ 20,
      Neighborhood == "Blueste"   ~ 21,
      Neighborhood == "Mitchel"   ~ 22,
      Neighborhood == "Timber"    ~ 23,
      Neighborhood == "MeadowV"   ~ 24,
      Neighborhood == "Veenker"   ~ 25,
      Neighborhood == "GrnHill"   ~ 26,
      Neighborhood == "Landmrk"   ~ 27
    )) %>%
    mutate(SalePrice = log(SalePrice))

help(across)

statistics <- df %>%
  summarize(
    across(everything(), list(
      Mean = ~ mean(.),
      Median = ~ median(.),
      StandardDeviation = ~ sd(.),
      Minimum = ~ min(.),
      Maximum = ~ max(.)))
  )

statistics

df %>%
  ggplot(mapping = aes(x = SalePrice)) + geom_histogram()

df %>%
  ggplot(mapping = aes(x=Gr.Liv.Area, y=SalePrice)) + geom_point()

df %>% 
  ggplot(mapping = aes(x=Overall.Qual, y = SalePrice)) + geom_point()

correlationDf = df[c('YearsAgo.Built','SalePrice')]

cor(correlationDf, use='complete.obs')