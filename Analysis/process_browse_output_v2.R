#process browse

library(tidyverse)
library(lemon)
library(cowplot)
source("./Analysis/r_functions.R")


theme_set(theme_bw())


# scenario_folder <- "./Models/v2 model templates"
scenario_folder <- "D:/ISRO landis/"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(!grepl("no_suppression", .))
# scenarios <- scenarios[c(1, 3, 12, 13)]


#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, show_col_types = FALSE) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

get_mgmt <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1) %>%
    strsplit(x = ., split = "[_]") %>%
    pluck(1, 1)
}

get_climate <- function(scenario){
  list.files(scenario, pattern = "NECN_Succession") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            mgmt = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  # mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(browse = ifelse(grepl(pattern = "no pred", run_name), "Low", 
                         ifelse(grepl(pattern = "low pred", run_name), "Medium",
                                "High"))) %>%
  mutate(climate = ifelse(grepl(pattern = "CANESM", run_name), "Hot/Dry (CanESM2 8.5)",
                          ifelse(grepl(pattern = "GFDL", run_name), "Warm (GFDL 4.5)", 
                                 ifelse(grepl(pattern = "MRI", run_name), "Hot/Wet (MRI-CGCM3 8.5)", "Present Climate")))) %>%
  mutate(browse = factor(browse, levels = c("Low", "Medium", "High")))

browse_summaries <- scenario_type %>%
  # filter(scenario_type$browse == "Browse") %>%
  dplyr::select(run_name) %>%
  map(., .f = ~ paste0(scenario_folder, "/", ., "/browse-summary-log.csv"))  %>%
  unlist() %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

browse_summaries2 <- browse_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalPopulation = weighted.mean(TotalPopulation, TotalSites),
            AverageForage =  weighted.mean(MeanForage, TotalSites),
            AverageBiomassKilled = weighted.mean(AverageBiomassKilled, TotalSites),
            AverageBiomassRemoved = weighted.mean(AverageBiomassRemoved, TotalSites),
            BDI = weighted.mean(BDI, TotalSites),
            TotalK = TotalK,
            browse = browse[1],
            climate = climate[1])

# browse_summaries2 <- browse_summaries2[which(browse_summaries2$Time <= 21), ]


area <- browse_summaries$TotalSites[1] * 60 * 60 / 1000000
browse_summaries2$K_density <- browse_summaries2$TotalK/area
browse_summaries2$Pop_density <- browse_summaries2$TotalPopulation/area

historical_moose <- readxl::read_excel("./Models/LANDIS inputs/browse/moose_population_record_isro.xlsx",
                                       col_names = c("Year", "Wolf_pop", "Moose_pop"))

browse_summaries_melt <- pivot_longer(browse_summaries2, cols = c(TotalPopulation:TotalK, K_density, Pop_density),
                                      names_to = "Variable")
browse_summaries_melt


#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             strip.background = element_rect(fill = "white"))



moosepop <- ggplot(data = browse_summaries2, mapping = aes(x = Time+2019, y = TotalPopulation)) + 
  geom_point(aes(colour = browse), alpha = 0.1) + 
  labs(y = "Total Moose Population", x = "Simulation Year", fill = "Predation") +
  geom_smooth(aes(colour = browse)) + 
  geom_point(data = historical_moose, mapping = aes(x = Year, y = Moose_pop)) + 
  geom_line(data = historical_moose, mapping = aes(x = Year, y = Moose_pop)) +
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
moosepop <- tag_facet(moosepop)
moosepop <- shift_legend2(moosepop)
plot(moosepop)
ggsave(file="./Analysis/plots/v2/moosepop.png", plot=moosepop, width=8, height=5)


mooseK <- ggplot(data = browse_summaries_melt[browse_summaries_melt$Variable %in% c("Pop_density","K_density"),], 
                 mapping = aes(x = Time+2019, y = value)) + 
  geom_point(aes(colour = browse, shape = Variable), alpha = 0.1) + 
  labs(title = "Moose Population density and Carrying Capacity density",
       subtitle = "by browse scenario and climate scenario",
       y = "Number of moose per km2", x = "Simulation Year") + 
  geom_smooth(aes(colour = browse, linetype = Variable)) + 
  facet_wrap(facets = ~climate) + 
  guides(colour=guide_legend(title="Predation"),
         linetype = guide_legend(title="")) + 
  scale_fill_discrete(labels = c(""))
mooseK <- tag_facet(mooseK)
mooseK <- shift_legend2(mooseK)
plot(mooseK)
ggsave(file="./Analysis/plots/v2/mooseK.png", plot=mooseK, width=8, height=5)

forage <- ggplot(data = browse_summaries2, mapping = aes(x = Time + 2019, y = AverageForage)) + 
  geom_point(aes(colour = browse), alpha = 0.1) + 
  labs(y = "Average available forage (g m-2)", x = "Simulation year") + 
  guides(colour=guide_legend(title="Predation")) +
  geom_smooth(aes(colour = browse)) + 
  facet_wrap(facets = ~climate)
forage <- tag_facet(forage)
forage <- shift_legend2(forage)
plot(forage)
ggsave(file="./Analysis/plots/forage_avail.svg", plot=forage, width=7, height=5)

browse_kill <- ggplot(data = browse_summaries2, mapping = aes(x = Time + 2020, y = AverageBiomassKilled)) + 
  geom_point(aes(colour = browse, shape = climate), alpha = 0.2) + 
  labs(title = "Average biomass killed by moose",
       subtitle = "by climate scenario",
       y = expression(paste("Biomass killed (g ", m^{-2}, yr^{-1}, ")")), x = "Simulation Year") +
  geom_smooth(aes(linetype = climate, color = browse)) 
plot(browse_kill)
ggsave(file = "browsekill.svg", plot = browse_kill, width = 5, height = 4)

prop_forage_eaten <- ggplot(data = browse_summaries2, mapping = aes(x = Time + 2020, y = BDI)) + 
  geom_point(aes(colour = browse), alpha = 0.2) + 
  labs(y = expression(paste("Proportion of forage eaten (Browse Density Index)")), x = "Simulation Year") +
  geom_smooth(aes( color = browse))  + 
  facet_wrap(facets = ~climate) + 
  guides(colour=guide_legend(title="Predation"),
         linetype = guide_legend(title="")) + 
  scale_fill_discrete(labels = c("")) + 
  ylim(c(0,1))
prop_forage_eaten <- tag_facet(prop_forage_eaten)
prop_forage_eaten <- shift_legend2(prop_forage_eaten)
plot(prop_forage_eaten)
ggsave(file="./Analysis/plots/prop_forage_eaten.svg", plot=prop_forage_eaten, width=7, height=5)

#-------------------------------------------------------------------------------
#Forage maps





