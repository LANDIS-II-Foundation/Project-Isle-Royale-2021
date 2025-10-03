# make maps and trajectories for important species


library("tidyverse")
library("terra")


theme_set(theme_bw())

#what folder do all the runs to be analyzed live in?
# scenario_folder <- "E:/ISRO LANDIS/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
# scenario_folder <- "./Models/Model templates"

scenario_folder <- "./Models/v2 model templates"
# scenario_folder <- "./Models/Model runs"

scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(!(grepl("canesm", .)))
scenarios <- scenarios[c(1:4, 6)]

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

biomass_summaries <- paste0(scenarios, "/spp-biomass-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  filter(EcoName == "eco1") %>%
  pivot_longer(cols =  starts_with("AboveGroundBiomass"),
               names_prefix = "AboveGroundBiomass_",
               names_to = "Species",
               values_to = "Biomass")
  

spp <- unique(biomass_summaries$Species)
for(sp in spp){
  
p <- ggplot(data = filter(biomass_summaries, Species == sp), 
         mapping = aes(x = Time, y = Biomass/100)) + 
    geom_point() + 
    labs(title = paste(paste0(sp, " aboveground biomass")),
         subtitle = "by browse scenario and climate scenario",
         y = "Average AGB (Mg/ha)", x = "Timestep") + 
    geom_line() +
  facet_wrap(facets = "run_name")
  # geom_smooth(aes(linetype = run_name))
plot(p)
}


abba_biomass <- ggplot(data = filter(biomass_summaries, Species == "ABBA"), 
                       mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
        geom_point(aes(shape = climate)) + 
        labs(title = paste("Balsam fir aboveground biomass"),
             subtitle = "by browse scenario and climate scenario",
             y = "Average AGB (Mg/ha)", x = "Timestep") + 
        geom_smooth(aes(linetype = climate))
plot(abba_biomass)
# ggsave(file="abba.svg", plot=abba_biomass, width=5, height=4)

pigl_biomass <- ggplot(data = filter(biomass_summaries, Species == "PIGL"), 
                       mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("White spruce aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Timestep") + 
  geom_smooth(aes(linetype = climate))
plot(pigl_biomass)
# ggsave(file="abba.svg", plot=abba_biomass, width=5, height=4)

pima_biomass <- ggplot(data = filter(biomass_summaries, Species == "PIMA"), 
                       mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("Black spruce aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Timestep") + 
  geom_smooth(aes(linetype = climate))
plot(pima_biomass)
ggsave(file="bepa.svg", plot=abba_biomass, width=5, height=4)


potr_biomass <- ggplot(data = filter(biomass_summaries, Species %in% c("POBA2", "POGR4", "POTR5")) %>%
                         group_by(Time, browse, climate) %>%
                         summarise(Biomass = sum(Biomass)) %>%
                         filter(!is.na(browse)), 
                       mapping = aes(x = Time+2020, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("Quaking aspen aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Simulation Year") + 
  geom_smooth(aes(linetype = climate))
plot(potr_biomass)
ggsave(file="potr.svg", plot=potr_biomass, width=8, height=4)


bepa_biomass <- ggplot(data = filter(biomass_summaries, Species == "BEPA"), 
                       mapping = aes(x = Time+2020, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("Paper birch aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Simulation Year") + 
  geom_smooth(aes(linetype = climate))
plot(bepa_biomass)
ggsave(file="potr.svg", plot=potr_biomass, width=8, height=4)





## process rasters

biomass_layers <- list.files(paste0(scenarios, "/", "biomass"), full.names = TRUE)

biomass_df <- data.frame(biomass_layers = biomass_layers,
                         species = basename(biomass_layers) %>%
                           str_extract(., "[^-]+"),
                         years = str_extract_all(basename(biomass_layers),"[0-9]+(?=\\.)") %>% unlist())

species_list <- unique(biomass_df$species)
year_list <- unique(biomass_df$years)
sp <- species_list[1]
for(sp in species_list){
  test <- rast(biomass_df[biomass_df$species == sp, "biomass_layers"])
}
plot(test[[1:4]])
