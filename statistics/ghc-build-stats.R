library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

################################ Preparation ###################################

timingsHeader <- c("Package", "Phase", "Module", "Time", "Alloc", "GHC")

load <- function(file, ghc) {
  df <- read.csv(file=file, header = FALSE) %>% mutate(GHC = as.factor(ghc))
  colnames(df) <- timingsHeader
  df
}

ghc901        <- load("9.0.1-data.csv", "ghc901")
ghc901intmap  <- load("9.0.1-intmap-data.csv", "ghc901intmap")


df <- rbind(ghc901,ghc901intmap) %>%
  arrange(desc(Alloc)) %>%
  filter(Package != "wai"
       , Package != "wai-extra-3.1.6"
       , Package != "persistent-2.13.1.1")

############################### Exploration #####################################

### Descriptive Stats
summary <- df %>%
  group_by(Package,GHC,Phase) %>%
  summarise(AvgAlloc = mean(Alloc)
          , AvgTime  = mean(Time)
          , MedAlloc = median(Alloc)
          , MedTime  = median(Time)) %>%
  pivot_wider(names_from = GHC, values_from = c(AvgAlloc,AvgTime,MedAlloc,MedTime))

summaryModules <- df %>%
  group_by(Package, GHC,Module) %>%
  summarise(AvgAlloc = mean(Alloc)
          , AvgTime = mean(Time)
          , MedAlloc = median(Alloc)
          , MedTime = median(Time)) %>%
  pivot_wider(names_from = GHC, values_from = c(AvgAlloc,AvgTime,MedAlloc,MedTime))

### Global difference
slowdown <- summaryModules %>%
  group_by(Package,Module) %>%
  summarise(slowdown = ((AvgAlloc_ghc901 - AvgAlloc_ghc901intmap) / AvgAlloc_ghc901) * 100)


## sanity check
observations <- df %>%
  group_by(GHC,Package) %>%
  count() %>%
  pivot_wider(names_from = GHC, values_from = n)

################################ Utilities #####################################
df %>% .[1:20, ] -> text_size



################################ Plotting ######################################

p <- ggplot(df) +
  geom_point(aes(x=Alloc, y=Time, color = Package)) +
  geom_text(data = text_size, aes(x=Alloc,y=Time,label = Module, color = Package),
            position=position_jitter(width = 1, height = 1),
            show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(. ~ GHC)


## clevelan dot plot showing the comparison per phase
colors <- c("ghc901intmap" = "blue", "ghc901" = "red", "segment" = "grey")

p2 <- ggplot(summary) +
  geom_segment(aes(x=Phase, xend=Phase, y=AvgAlloc_ghc901intmap, yend=AvgAlloc_ghc901, color="segment")) +
  geom_point(aes(x=Phase, y=AvgAlloc_ghc901intmap, color = "ghc901intmap"), size = 3) +
  geom_point(aes(x=Phase, y=AvgAlloc_ghc901, color = "ghc901"), size = 3) +
  coord_flip() +
  labs(x = "Package"
     , y = "Allocations"
     , color = "Legend") +
  scale_color_manual(values = colors) +
  facet_grid(Package ~ .)
