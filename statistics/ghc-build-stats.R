library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)
options("scipen"=999, "digits"=4)
################################ Preparation ###################################

timingsHeader <- c("Package", "Phase", "Module", "Time", "Alloc", "GHC")

load <- function(file, ghc) {
  df <- read.csv(file=file, header = FALSE) %>% mutate(GHC = as.factor(ghc))
  colnames(df) <- timingsHeader
  df %>% mutate(Package = as.factor(Package), Module = as.factor(Module))
}

ghcOld  <- load("../9.3-master.csv", "ghc_old")
## ghcNew  <- load("../9.3-occanal-unfolded.csv", "ghc_new")
ghcNew  <- load("../9.3-master2.csv", "ghc_new")


df <- rbind(ghcOld,ghcNew) %>%
  arrange(desc(Alloc)) %>%
  filter(Package != "wai"
       , Package != "wai-extra-3.1.6"
       , Package != "persistent-2.13.1.1"
       , Package != "random-1.2.0")

############################### Exploration #####################################

### Descriptive Stats
summary <- df %>%
  group_by(Package,GHC,Phase) %>%
  summarise(AvgAlloc = mean(Alloc)
          , AvgTime  = mean(Time)
          , MedAlloc = median(Alloc)
          , MedTime  = median(Time)) %>%
  pivot_wider(names_from = GHC, values_from = c(AvgAlloc,AvgTime,MedAlloc,MedTime)) %>% na.omit()

summaryModules <- df %>%
  group_by(GHC, Package, Module) %>%
  summarise(AvgAlloc = mean(Alloc)
          , AvgTime = mean(Time)
          , MedAlloc = median(Alloc)
          , MedTime = median(Time)) %>%
  pivot_wider(names_from = GHC, values_from = c(AvgAlloc,AvgTime,MedAlloc,MedTime)) %>% na.omit()

### Global difference
slowdown <- summaryModules %>%
  group_by(Package,Module) %>%
  summarise(slowdown = ((AvgAlloc_ghc_new - AvgAlloc_ghc_old) / AvgAlloc_ghc_old) * 100)

slowdownPhase <- summary %>%
  group_by(Package, Phase) %>%
  summarise(slowdown = ((AvgAlloc_ghc_new - AvgAlloc_ghc_old) / AvgAlloc_ghc_old) * 100) %>%
  arrange(desc(slowdown))


## sanity check
observations <- df %>%
  group_by(GHC,Package) %>%
  count() %>%
  pivot_wider(names_from = GHC, values_from = n)

################################ Utilities #####################################
df %>% .[1:20, ] -> text_size



################################ Plotting ######################################

p <- ggplot(df) +
  geom_point(aes(x=Alloc, y=Time)) +
  geom_text(data = text_size, aes(x=Alloc,y=Time, label = Module),
            position=position_jitter(),
            show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(GHC ~ ., scales = "free")


## clevelan dot plot showing the comparison per phase
colors <- c("ghc_new" = "blue", "ghc_old" = "red", "segment" = "grey")

p2 <- ggplot(summary) +
  geom_segment(aes(x=Phase, xend=Phase, y=AvgAlloc_ghc_new, yend=AvgAlloc_ghc_old)) +
  geom_point(aes(x=Phase, y=AvgAlloc_ghc_new), color = "blue", size = 2) +
  geom_point(aes(x=Phase, y=AvgAlloc_ghc_old), color = "red", size = 2) +
  coord_flip() +
  labs(x = "Package"
     , y = "Allocations"
     , color = "Legend") +
  scale_color_manual(values = colors) +
  facet_grid(Package ~ .)

p3 <- ggplot(summary) +
  geom_segment(aes(x=Phase, xend=Phase, y=AvgTime_ghc_new, yend=AvgTime_ghc_old)) +
  geom_point(aes(x=Phase, y=AvgTime_ghc_new), color = "blue", size = 2) +
  geom_point(aes(x=Phase, y=AvgTime_ghc_old), color = "red", size = 2) +
  coord_flip() +
  labs(x = "Package"
     , y = "Time"
     , color = "Legend") +
  scale_color_manual(values = colors) +
  facet_grid(Package ~ .)


## ggsave(filename = "timevsallocs.svg", plot = p, height=10, width= 8)
## ggsave(filename = "hashedghc.svg", plot = p2, height=12, width= 7)
write.table(slowdownPhase, "PhaseSlowdown",sep= "\t",row.names=FALSE)
write.table(slowdown, "ModuleSlowdown",sep= "\t",row.names=FALSE)
