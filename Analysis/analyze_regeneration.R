# look at regeneration patterns
# from regeneration logs
library("tidyverse")
library("terra")


theme_set(theme_bw())

#what folder do all the runs to be analyzed live in?
# scenario_folder <- "E:/ISRO LANDIS/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
scenario_folder <- "./Models/v2 model templates"
# scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(!(grepl("canesm", .)))
scenarios <- scenarios[c(4)]

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

get_browse <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
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
                            browse = character(length(scenarios)),
                            climate = character(length(scenarios)))


scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(browse = ifelse(grepl(pattern = "pred1", run_name), "Low", 
                         ifelse(grepl(pattern = "pred2", run_name), "Medium",
                                "High"))) %>%
  # mutate(browse = c("Low pred", "Hi pred", "Low pred", "Hi pred")) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "Very Hot (MIROC-ESM-CHEM 8.5)", 
                          ifelse(grepl(pattern = "canesm", run_name), "Hot/Dry (CanESM2 8.5)",
                                 ifelse(grepl(pattern = "ccsm", run_name), "Warm (CCSM4 4.5)", 
                                        ifelse(grepl(pattern = "mri_cgm", run_name), "Hot/Wet (MRI-CGCM3 8.5)", "Present Climate"))))) %>%
  mutate(browse = factor(browse, levels = c("Low", "Medium", "High")),
         climate = factor(climate, levels = unique(climate)[c(3,2,1,4,5)]))

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
              mapping = aes(x = Time, y = Biomass/100)) + #, colour = browse
    geom_point(alpha = 0.1) + 
    labs(title = paste(paste0(sp, " aboveground biomass")),
         # subtitle = "by browse scenario and climate scenario",
         y = "Average AGB (Mg/ha)", x = "Timestep") + 
    geom_smooth() +
    facet_wrap(facets = "run_name")+
    theme(plot.margin = margin(6, 0, 6, 0)) #+ 
    # guides(colour=guide_legend(title="Predation"))
  # p1 <- shift_legend2(p1)
  
  p2 <- ggplot(data = filter(browsekill_summaries, SpeciesName == sp), 
              mapping = aes(x = Time, y = NetRegen)) + #, colour = browse
    geom_point(alpha = 0.1) + 
    labs(title = paste(paste0(sp, " net regeneration")),
         # subtitle = "by browse scenario and climate scenario",
         y = "Number of new cohorts", x = "Timestep") + 
    geom_smooth() +
    facet_wrap(facets = "run_name") +
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
    paste0("./Analysis/Plots/biomass_regen_", sp, ".png"),
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
