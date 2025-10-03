# Wrangle the BDA tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library("tidyverse")

scenario_folder <- "D:/ISRO landis/"
# scenario_folder <- "./Models/v2 model templates/"
# scenarios <- list.dirs(scenario_folder, recursive = FALSE) 
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
`[`(!grepl("no_suppression", .))
# scenarios <- scenarios[c(4:9, 16:21, 28:33)]


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
  mutate(browse = factor(browse, levels = c("Low", "Medium", "High"))) %>%
  mutate(fire_model = ifelse(grepl(pattern = "no_suppression", run_name), "No Suppression", "BAU"))

bda_summaries <- paste0(scenarios, "/bda_log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

bda_summaries2 <- bda_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalCohortsKilled = sum(CohortsKilled),
            #TotalBiomassKilled = sum(TotalBiomassMortality),
            TotalSitesAffected = sum(DamagedSites),
            # mgmt = mgmt[1],
            climate = climate[1],
            Time = Time[1]) %>%
  mutate(CumCohortsKilled = cumsum(TotalCohortsKilled))
  # mutate(CumBiomassMortality = cumsum(TotalBiomassKilled))



#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------
bda_summaries$Year <- bda_summaries$Time + 2020
ggplot(data = bda_summaries, 
       mapping = aes(x = Year, y = CohortsKilled, color = AgentName)) + 
  geom_point() +
  labs(title = "Cohorts killed by beetles",
       # subtitle = "by management scenario and climate scenario",
       y = "Number of cohorts killed", x = "Simulation Year") + 
  # geom_smooth() + 
  facet_wrap(~ climate)
# facet_wrap(~ run_name)
#Harvest over time
bda_summaries2$Year <- bda_summaries2$Time + 2020

ggplot(data = bda_summaries2, 
       mapping = aes(x = Year, y = TotalCohortsKilled)) + 
  geom_point(color="steelblue") + 
  labs(title = "Cohorts killed by beetles",
       # subtitle = "by management scenario and climate scenario",
       y = "Number of cohorts killed", x = "Simulation Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate)
# facet_wrap(~ run_name)

ggplot(data = bda_summaries2,
       mapping = aes(x = Time, y = TotalSitesAffected * 0.36)) + 
  geom_point(color="steelblue") + 
  labs(title = "Area affected by beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Sites affected", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)
# facet_wrap(~ mgmt + climate, nrow = 3, ncol = 2)


ggplot(data = bda_summaries2[bda_summaries2$climate == "Historical", ],
       mapping = aes(x = Time, y = CumBiomassMortality)) + 
  geom_point(color="steelblue") + 
  labs(title = "Area affected by beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Sites affected", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)
# facet_wrap(~ mgmt + climate, nrow = 3, ncol = 2)

