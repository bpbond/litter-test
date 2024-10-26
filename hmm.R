# Hmm

library(readr)
litter <- read_csv("TEMPEST litter data - Sheet1.csv", skip = 1)
library(lubridate)
library(dplyr)
litter %>% 
  mutate(Date = mdy(litter$`Date collected`)) %>% 
  filter(year(Date) == 2020) %>% 
  group_by(Plot, Trap) %>% 
  summarise(ACRU = sum(`Leaf-ACRU`), FAGR = sum(`Leaf-FAGR`), 
            LITU = sum(`Leaf-LITU`), Other = sum(`Leaf-Other`)) ->
  litter

litter$Total = rowSums(litter[c("ACRU", "FAGR", "LITU", "Other")])

message("Mean and variability by plot and species:")
library(tidyr)
litter %>% 
  pivot_longer(c(-Plot, -Trap)) %>% 
  group_by(Plot, name) %>% 
  summarise(avg = round(mean(value), 1), stdev = round(sd(value), 1),
            .groups = "drop") %>% 
  mutate(string = paste(format(avg, nsmall=1), format(stdev, nsmall=1), sep = "±")) %>% 
  select(-avg, -stdev) %>% 
  pivot_wider(values_from = string) %>% 
  print()

message("Chi-square test for variability between traps, by plot:")
# Is there variability between traps?
plots <- unique(litter$Plot)
result <- data.frame(Plot = plots)
for(pnum in seq_along(plots)) {
  plot <- plots[pnum]
  for(type in c("ACRU", "FAGR", "LITU", "Other", "Total")) {
    x <- litter[litter$Plot == plot,]
    y <- unlist(x[,type])
    names(y) <- x$Trap
    result[pnum, type] <- format(round(chisq.test(y)$p.value, 3), nsmall = 3)
  }
}
print(result)

