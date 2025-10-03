# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library(tidyverse)
library(lemon)
library("cowplot")
library("multcompView")
source("./Analysis/r_functions.R")

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


necn_summaries <- paste0(scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  mutate(TotalC = SOMTC + C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot + C_DeadWood +
           C_DeadCRoot + C_DeadLeaf_Struc + C_DeadLeaf_Meta + C_DeadFRoot_Struc + C_DeadFRoot_Meta,
         TotalNetMineralization = SurfStrucNetMin + SurfMetaNetMin + SoilStrucNetMin + SoilMetaNetMin +
           SOM1surfNetMin + SOM1soilNetMin + SOM2NetMin + SOM3NetMin,
         SimulationYear = Time + 2020) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

necn_summaries2 <- necn_summaries %>%
  group_by(run_name, SimulationYear) %>%
  summarise(across(where(is.numeric), ~weighted.mean(.x, NumSites)),
            browse = browse[1],
            climate = climate[1])

necn_monthly <- paste0(scenarios, "/NECN-succession-monthly-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  mutate(SimulationYear = Time + 2020) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  group_by(run_name, SimulationYear) %>%
  summarise(NPP = sum(AvgTotalNPP_C),
            Rh = sum(AvgHeteroRespiration),
            MAT = mean(AirTemp),
            browse = browse[1],
            climate = climate[1])


#-------------------------------------------------------------------------------
# Compare effects
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#AGB over time

theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             strip.background = element_rect(fill = "white"))

agb_over_time <- ggplot(data = necn_summaries2,
                        mapping = aes(x = SimulationYear, y = AGB/100*0.47, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  # geom_line(aes(group = run_name)) + 
  labs(y = expression(paste("Average aboveground biomass C density (Mg ha"^{-1}, ")")), 
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate")+ 
  guides(colour=guide_legend(title="Predation"))
agb_over_time <- tag_facet(agb_over_time)
agb_over_time <- shift_legend2(agb_over_time)
plot(agb_over_time)
#This actually save the plot in a image
ggsave(file="./Analysis/plots/v2/agb_over_time.png", plot=agb_over_time, width=8, height=5)


#TOtal C
totalc_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = TotalC/100, 
                                                                 colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(y = expression(paste("Ecosystem C density (Mg ha" ^{-1}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") +
  guides(colour=guide_legend(title="Browse"))
totalc_over_time <- tag_facet(totalc_over_time)
totalc_over_time <- shift_legend2(totalc_over_time)
plot(totalc_over_time)
# ggsave(file="./Analysis/plots/totalc_over_time.svg", plot=totalc_over_time, width=7, height=5)

#SOM over time
somtc_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = SOMTC/100, 
                                                                colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(y = expression(paste("Soil organic C density  (Mg ha" ^{-1}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
somtc_over_time <- tag_facet(somtc_over_time)
somtc_over_time <- shift_legend2(somtc_over_time)
plot(somtc_over_time)
ggsave(file="./Analysis/plots/somtc_over_time.svg", plot=somtc_over_time, width=7, height=5)

#SoilN over time
n_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = MineralN, 
                                                            colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(y = expression(paste("Mineral N (g m"^{-2}, ")")), x = "Timestep") + 
  geom_smooth()  + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
n_over_time <- tag_facet(n_over_time)
n_over_time <- shift_legend2(n_over_time)
plot(n_over_time)
ggsave(file="./Analysis/plots/mineral_n_over_time.svg", plot=mineral_n_over_time, width=7, height=5)

#NPP over time
npp_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = AG_NPPC + BG_NPPC, 
                                                                   colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(y = expression(paste("Net primary productivity (g m" ^{-2}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
npp_over_time <- tag_facet(npp_over_time)
npp_over_time <- shift_legend2(npp_over_time)
plot(npp_over_time)
ggsave(file="./Analysis/plots/npp.svg", plot=npp_over_time, width=7, height=5)


#C surf over time
surface_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = C_SOM1surf, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
surface_c_over_time <- tag_facet(surface_c_over_time)
shift_legend2(surface_c_over_time)
plot(surface_c_over_time)
ggsave(file="./Analysis/plots/surfc.svg", plot=surface_c_over_time, width=7, height=5)

#C soil over time
som1soil_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = C_SOM1soil, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Soil SOM1 C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
som1soil_c_over_time <- tag_facet(som1soil_c_over_time)
shift_legend2(som1soil_c_over_time)
plot(som1soil_c_over_time)
ggsave(file="./Analysis/plots/som1soilc.svg", plot=som1soil_c_over_time, width=7, height=5)

#C SOM2 over time
som2_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = C_SOM2, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Soil SOM2 C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
som2_c_over_time <- tag_facet(som2_c_over_time)
shift_legend2(som2_c_over_time)
plot(som2_c_over_time)
ggsave(file="./Analysis/plots/som2c.svg", plot=som2_c_over_time, width=7, height=5)

#C SOM3 over time
som3_c_over_time <- ggplot(data = necn_summaries2, 
                           mapping = aes(x = SimulationYear, y = C_SOM3, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
som3_c_over_time <- tag_facet(som3_c_over_time)
shift_legend2(som3_c_over_time)
plot(som3_c_over_time)
ggsave(file="./Analysis/plots/som3c.svg", plot=som3_c_over_time, width=7, height=5)

#N total over time
total_n_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = TotalN, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Total ecosystem N",
       subtitle = "by browse and climate scenario",
       y = "Total ecosystem N (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
total_n_over_time <- tag_facet(total_n_over_time)
shift_legend2(total_n_over_time)
plot(total_n_over_time)
ggsave(file="./Analysis/plots/totaln.svg", plot=total_n_over_time, width=7, height=5)

#N surf over time
surface_n_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = N_SOM1surf, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Soil surface N",
       subtitle = "by browse and climate scenario",
       y = "Average surface N (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
surface_n_over_time <- tag_facet(surface_n_over_time)
shift_legend2(surface_n_over_time)
plot(surface_n_over_time)
ggsave(file="./Analysis/plots/surfn.svg", plot=surface_n_over_time, width=7, height=5)

#N soil over time
som1soil_n_over_time <- ggplot(data = necn_summaries2, 
                               mapping = aes(x = SimulationYear, y = N_SOM1soil, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Soil surface N",
       subtitle = "by browse and climate scenario",
       y = "Average surface N (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
som1soil_n_over_time <- tag_facet(som1soil_n_over_time)
shift_legend2(som1soil_n_over_time)
plot(som1soil_n_over_time)
ggsave(file="./Analysis/plots/som1soiln.svg", plot=som1soil_n_over_time, width=7, height=5)

#N SOM2 over time
som2_n_over_time <- ggplot(data = necn_summaries2, 
                           mapping = aes(x = SimulationYear, y = N_SOM2, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Slow pool N",
       subtitle = "by browse and climate scenario",
       y = "Average slow N (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Predation"))
som2_n_over_time <- tag_facet(som2_n_over_time)
shift_legend2(som2_n_over_time)
plot(som2_n_over_time)
ggsave(file="./Analysis/plots/som2n.svg", plot=som2_n_over_time, width=7, height=5)

#N SOM3 over time
som3_n_over_time <- ggplot(data = necn_summaries2, 
                           mapping = aes(x = SimulationYear, y = N_SOM3, colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Recalcitrant pool N",
       subtitle = "by browse and climate scenario",
       y = "Average recalcitrant N (g/m2)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
som3_n_over_time <- tag_facet(som3_n_over_time)
shift_legend2(som3_n_over_time)
plot(som3_n_over_time)
ggsave(file="./Analysis/plots/som3n.svg", plot=som3_n_over_time, width=7, height=5)

#N leaching and volatilization
n_loss_over_time <- ggplot(data = necn_summaries2, 
                             mapping = aes(x = SimulationYear, y = LeachedN + Nvol, color = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) +
  labs(title = "N loss (leaching and volatilization)",
       subtitle = "by browse and climate scenario",
       y = "N loss (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
plot(n_loss_over_time)

#N deposition
n_dep_over_time <- ggplot(data = necn_summaries2, 
                           mapping = aes(x = SimulationYear, y = TotalNdep, color = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) +
  labs(title = "N loss (leaching and volatilization)",
       subtitle = "by browse and climate scenario",
       y = "N loss (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
plot(n_dep_over_time)

#NEE over time
nee_over_time<- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = NEEC, 
                                                                            colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(y = expression(paste("Net ecosytem exchange (g m" ^{-2}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation")) +
  geom_hline(yintercept = 0)
nee_over_time <- tag_facet(nee_over_time)
nee_over_time <-shift_legend2(nee_over_time)
plot(nee_over_time)
ggsave(file="./Analysis/plots/nee.svg", plot=npp_over_time, width=7, height=5)


#how does browsing affect NEE?
mean(necn_summaries2[necn_summaries2$climate == "Present Climate" & 
                       necn_summaries2$browse == "High" & necn_summaries2$SimulationYear <2100, ]$NEEC)
mean(necn_summaries2[necn_summaries2$climate == "Present Climate" & 
                       necn_summaries2$browse == "Low"& necn_summaries2$SimulationYear <2100, ]$NEEC)
#approximately 40 g m-2 yr difference, at the beginning; then they get closer later on


c_inputs_over_time <- ggplot(data = necn_summaries2, 
                        mapping = aes(x = SimulationYear, 
                                      y = Litterfall + AgeMortality, 
                                      colour = browse)) + 
  geom_point(alpha = 0.2, stroke = NA) + 
  labs(title = "Detrital inputs",
       subtitle = "by browse and climate scenario",
       y = "Detrital inputs (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation")) +
  geom_hline(yintercept = 0)
c_inputs_over_time <- tag_facet(c_inputs_over_time)
c_inputs_over_time <-shift_legend2(c_inputs_over_time)
plot(c_inputs_over_time)  


#_--------------------

necn_monthly2 <- necn_monthly %>%
  pivot_longer(cols = c("NPP", "Rh", "MAT"))
npp_rh_over_time <- ggplot(data = necn_monthly2 %>% filter(name %in% c("NPP", "Rh")), 
                             mapping = aes(x = SimulationYear, y = value, colour =browse, linetype = name)) + 
  # geom_point(aes(y = Rh)) +
  labs(y = "C flux (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() +
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"),  
         linetype=guide_legend(title=""))+
  scale_linetype_manual(labels = c("NPP", expression(R[H])), values=c("solid", "dotted"))
npp_rh_over_time <- tag_facet(npp_rh_over_time)
npp_rh_over_time <- shift_legend2(npp_rh_over_time)
plot(npp_rh_over_time)
ggsave(file="./Analysis/plots/npp_rh.svg", plot=npp_rh_over_time, width=7, height=5)

npp_over_time <- ggplot(data = necn_monthly,
       mapping = aes(x = SimulationYear, y = NPP, color = browse)) + 
  # geom_point(aes(y = Rh)) +
  labs(y = expression(paste("Net primary productivity (g m"^{-2}," yr"^{-1}, ")")),
       x = "Simulation Year") + 
  geom_smooth() +
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
npp_over_time <- tag_facet(npp_over_time)
npp_over_time <- shift_legend2(npp_over_time)
plot(npp_over_time)

ggplot(data = necn_monthly,
       mapping = aes(x = MAT, y = NPP, color = browse)) +
  geom_point() +
  geom_smooth()
