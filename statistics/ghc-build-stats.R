library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)
library(gdata)
options("scipen"=999, "digits"=4)
################################ Preparation ###################################

timingsHeader <- c("Package", "Phase", "Module", "Time", "Alloc", "GHC")

load <- function(file, ghc) {
  df <- read.csv(file=file, header = FALSE) %>% mutate(GHC = as.factor(ghc))
  colnames(df) <- timingsHeader
  df %>% mutate(Package = as.factor(Package), Module = as.factor(Module))
}

ghcOld  <- load("../9.3-master.csv", "ghc_old")
ghcNew  <- load("../9.3-occanal-unfolded.csv", "ghc_new")
## ghcNew  <- load("../9.3-master2.csv", "ghc_new")


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
allocChanges <- summaryModules %>%
  group_by(Package,Module) %>%
  summarise(PctChange = ((AvgAlloc_ghc_new - AvgAlloc_ghc_old) / AvgAlloc_ghc_old) * 100)

allocChangesByPhasePackage <- summary %>%
  group_by(Package, Phase) %>%
  mutate(OldminusNew = AvgAlloc_ghc_old - AvgAlloc_ghc_new,
         PctChange = abs((OldminusNew / AvgAlloc_ghc_old) * 100)) %>%
  select(Package,Phase, AvgAlloc_ghc_old, AvgAlloc_ghc_new, OldminusNew, PctChange) %>%
  arrange(desc(PctChange))

timeChanges <- summaryModules %>%
  group_by(Package,Module) %>%
  summarise(slowdown = ((AvgTime_ghc_new - AvgTime_ghc_old) / AvgTime_ghc_old) * 100)

timeChangesByPhasePackage <- summary %>%
  group_by(Package, Phase) %>%
  mutate(OldminusNew = AvgTime_ghc_old - AvgTime_ghc_new,
         PctChange = abs((OldminusNew / AvgTime_ghc_old) * 100)) %>%
  select(Package,Phase, AvgTime_ghc_old, AvgTime_ghc_new, OldminusNew, PctChange) %>%
  arrange(desc(PctChange))

## sanity check
observations <- df %>%
  group_by(GHC,Package) %>%
  count() %>%
  pivot_wider(names_from = GHC, values_from = n)

################################ Utilities #####################################
df %>% .[1:20, ] -> text_size



################################ Plotting ######################################

  ## geom_text(data = text_size, aes(x=Alloc,y=Time, label = Module),
  ##           position=position_jitter(),
  ##           show.legend = FALSE) +
  ## theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ## geom_boxplot(aes(x=Alloc, y=Time, color=GHC)) +

p <- ggplot(df) +
  geom_density(aes(x=Time)) +
  facet_grid(. ~ Phase, scales = "free")


## clevelan dot plot showing the comparison per phase
colors <- c("ghc_new" = "blue", "ghc_old" = "red", "segment" = "grey")

p2 <- ggplot(summary) +
  geom_segment(aes(x=Phase, xend=Phase, y=AvgAlloc_ghc_new, yend=AvgAlloc_ghc_old)) +
  geom_point(aes(x=Phase, y=AvgAlloc_ghc_new), color = "blue", size = 2) +
  geom_point(aes(x=Phase, y=AvgAlloc_ghc_old), color = "red", size = 2) +
  coord_flip() +
  labs(x = "Package"
     , y = "Allocations [MB]"
     , color = "Legend") +
  scale_color_manual(values = colors) +
  facet_grid(Package ~ .)

p3 <- ggplot(summary) +
  geom_segment(aes(x=Phase, xend=Phase, y=AvgTime_ghc_new, yend=AvgTime_ghc_old)) +
  geom_point(aes(x=Phase, y=AvgTime_ghc_new), color = "blue", size = 2) +
  geom_point(aes(x=Phase, y=AvgTime_ghc_old), color = "red", size = 2) +
  coord_flip() +
  labs(x = "Package"
     , y = "Time [ms]"
     , color = "Legend") +
  scale_color_manual(values = colors) +
  facet_grid(Package ~ .)


## ggsave(filename = "timevsallocs.svg", plot = p, height=10, width= 8)
## ggsave(filename = "hashedghc.svg", plot = p2, height=12, width= 7)


tableOutput <- function(prelude,fout,df){
  separator <- "\n================================================================================\n"
  header    <- paste(prelude,separator,sep = "")

  ## rebind the colnames to a row before conversion so they are treated properly
  ## in fixed width format (fwf)
  fHandle <- file(fout)
  writeLines(header, fHandle)
  close(fHandle)

  gdata::write.fwf(as.data.frame(df)
                 , file     = fout
                 , append   = TRUE
                 , quote    = FALSE
                 , colnames = TRUE  
                 ## , width    = 35
                 , sep = "\t\t")
}

tableOutput("Allocation [MB] by Package and Phase", "out.table", allocChangesByPhasePackage)
