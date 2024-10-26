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
            LITU = sum(`Leaf-LITU`), Other = sum(`Leaf-Other`),
            .groups = "drop") ->
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
grid_locations$Grid <- sample(unique(na.omit(trees$Grid)), size = nrow(grid_locations))

litter %>% 
  left_join(grid_locations, by = c("Plot", "Trap")) %>% 
  pivot_longer(c(ACRU, FAGR, LITU, Other, Total), 
               names_to = "Species", values_to = "Litter") ->
  litter_long

# Exact grid square matching
litter_long %>% 
  left_join(trees, by = c("Plot", "Grid", "Species")) %>% 
  replace_na(list(Leaf = 0)) ->
  combined_exact

library(ggplot2)
p <- ggplot(combined_exact, aes(Litter, Leaf, color = Species)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(.~Plot) + 
  ggtitle("Exact grid square matching")
print(p)
ggsave("exact-matching.png", width = 8, height = 4)

# Fuzzy match - any tree within a 3x3 centered around the trap is included

# Helper function to compute the 3x3 grid around a litter trap
# For example, for C2 we want {B1, B2, B3, C1, C2, C3, D1, D2, D3}
makebox <- function(loc) {
  if(!grepl("^[A-J][1-8]$", loc)) return(NULL) # make sure data is in expected form
  
  x <- which(LETTERS == substr(loc, 1, 1)) 
  xseq <- seq(x - 1, x + 1)
  xseq <- xseq[xseq > 0 & xseq < 11] 
  y <- as.integer(substr(loc, 2, 2))
  yseq <- seq(y - 1, y + 1)
  yseq <- yseq[yseq > 0 & yseq < 9]
  df <- expand.grid(LETTERS[xseq], yseq)
  
  return(paste0(df[,1], df[,2]))
}

# For each litter_long plot and grid, get the trees in surrounding box
ll_split <- split(litter_long, paste(litter_long$Plot, litter_long$Grid))
results <- list()
for(i in seq_along(ll_split)) {
  x <- ll_split[[i]]
  # Find the nearby grid squares and trees within them
  nearby <- makebox(x$Grid[1])
  y <- trees[trees$Plot == x$Plot[1] & trees$Grid %in% nearby,]
  # Add them up, join with trees, and store
  y_total <- y %>% group_by(Species) %>% summarise(Leaf = sum(Leaf))
  x %>% left_join(y_total, by = "Species") -> results[[i]]
}

results %>% 
  bind_rows() %>% 
  replace_na(list(Leaf = 0)) ->
  combined_fuzzy

p_fuzzy <- p %+% combined_fuzzy
p_fuzzy <- p_fuzzy + ggtitle("Fuzzy (3x3) grid square matching")
print(p_fuzzy)
ggsave("fuzzy-matching.png", width = 8, height = 4)


