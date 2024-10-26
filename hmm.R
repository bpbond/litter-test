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

# Trees
trees <- read_csv("inventory.csv")
trees %>% 
  select(Plot, Grid, Species_code, DBH_2020) %>% 
  mutate(Species = case_when(Species_code == "ACRU" ~ "ACRU",
                             Species_code == "LITU" ~ "LITU",
                             Species_code == "FAGR" ~ "FAGR",
                             .default = "Other"),
         Plot = substr(Plot, 1, 1)) %>% 
  group_by(Plot, Grid, Species) %>% 
  summarise(Leaf = sum(DBH_2020 ^ 2, na.rm = TRUE), .groups = "drop") ->
  trees

# Compute totals
trees %>%
  group_by(Plot, Grid) %>% 
  summarise(Leaf = sum(Leaf)) %>% 
  mutate(Species = "Total") %>%
  bind_rows(trees) ->
  trees


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

# Temporary - fake grid locations
grid_locations <- expand_grid(Plot = c("C", "F", "S"), Trap = LETTERS[1:12])
grid_locations$Grid <- sample(unique(trees$Grid), size = nrow(grid_locations))

litter %>% 
  left_join(grid_locations, by = c("Plot", "Trap")) %>% 
  pivot_longer(c(ACRU, FAGR, LITU, Other, Total), 
               names_to = "Species", values_to = "Litter") ->
  litter_long

# Exact grid square matching
litter_long %>% 
  left_join(trees, by = c("Plot", "Grid", "Species")) %>% 
  replace_na(list(Leaf = 0))

# Fuzzy match - any tree within a 3x3 centered around the trap is included
