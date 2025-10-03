# look at regeneration patterns
# from regeneration logs
library("tidyverse")
library("terra")
library("cowplot")


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

regen_summaries <- paste0(scenarios, "/NECN-reproduction-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  mutate(TotalCohorts = NumCohortsPlanting + NumCohortsSerotiny + NumCohortsResprout + NumCohortsSeed)


biomass_summaries <- paste0(scenarios, "/spp-biomass-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  filter(EcoName == "eco1") %>%
  pivot_longer(cols =  starts_with("AboveGroundBiomass"),
               names_prefix = "AboveGroundBiomass_",
               names_to = "Species",
               values_to = "Biomass")

browsekill_summaries <- paste0(scenarios, "/browse-event-species-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(regen_summaries, by = c("run_name", "Time", "SpeciesName")) %>%
  mutate(NetRegen = TotalCohorts - TotalCohortsKilled)



spp <- unique(regen_summaries$SpeciesName)
for(sp in spp){
  p1 <- ggplot(data = filter(biomass_summaries, Species == sp), 
              mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
    geom_point(alpha = 0.1) + 
    labs(title = paste(paste0(sp, " aboveground biomass")),
         # subtitle = "by browse scenario and climate scenario",
         y = "Average AGB (Mg/ha)", x = "Timestep") + 
    geom_smooth() +
    facet_wrap(facets = "climate")+
    theme(plot.margin = margin(6, 0, 6, 0)) + 
    guides(colour=guide_legend(title="Predation"))
  # p1 <- shift_legend2(p1)
  
  p2 <- ggplot(data = filter(browsekill_summaries, SpeciesName == sp), 
              mapping = aes(x = Time, y = NetRegen, colour = browse)) +
    geom_point(alpha = 0.1) + 
    labs(title = paste(paste0(sp, " net regeneration")),
         # subtitle = "by browse scenario and climate scenario",
         y = "Number of new cohorts", x = "Timestep") + 
    geom_smooth() +
    facet_wrap(facets = "climate") +
    geom_hline(yintercept = 0)+
    theme(plot.margin = margin(6, 0, 6, 0)) +
    guides(colour=guide_legend(title="Predation"))
  # p2 <- shift_legend2(p2)
  # geom_smooth(aes(linetype = run_name))
 
  c_grid <- cowplot::plot_grid(shift_legend2(p1), shift_legend2(p2), 
                               align = "v", 
                               nrow = 2, ncol = 1,
                               labels = "auto",
                               rel_heights = c(1,1,1.3))
  plot(c_grid)
  ggsave(
    paste0("./Analysis/Plots/v2/biomass_regen_", sp, ".png"),
    plot = c_grid,
    device = "png",
    scale = 1,
    width = 6,
    height = 8,
    units = "in",
    dpi = 300)
}


#---regen log
regen <- read_csv("./Models/Model runs/current - pred1/NECN-reproduction-log.csv")
abba <- filter(regen, SpeciesName == "ABBA")
regen <- read_csv("./Models/Model runs/miroc - pred3/NECN-reproduction-log.csv")
abba2 <- filter(regen, SpeciesName == "ABBA") %>%
  filter(Time %in% abba$Time)

plot(abba$NumCohortsSeed ~ abba$Time)
plot(abba2$NumCohortsSeed ~ abba2$Time)




#---maple disaster
regen <- read_csv("./Models/Model runs/current - pred1/NECN-reproduction-log.csv")
acru <- filter(regen, SpeciesName == "ACRU")
browse <- read_csv("./Models/Model runs/current - pred1/browse-event-species-log.csv")
acru_browse <- filter(browse, SpeciesName == "ACRU")

acru <- left_join(acru, acru_browse)

ggplot(acru) +
  # geom_line(mapping = aes(x = Time, y = NumCohortsSeed)) +
  # geom_line(mapping = aes(x = Time, y = TotalCohortsKilled)) + 
  geom_line(mapping = aes(x = Time, y = NumCohortsSeed - TotalCohortsKilled))
