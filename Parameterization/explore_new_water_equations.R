moisture_1<- 16 #soilwater with maximum gpp
moisture_2<- 30 #maximum or minimum soilwater with any GPP -- changes steepness of decline in gpp
moisture_3<- 0.8 #higher values -- sharper dip down towards zero
moisture_4<- 3 #smaller numbers -- broader curve


#SWC goes from 0 to 1
swc_val<- seq(-20, 40, length.out = 1000)

#Calculate fraction here
fraction_val <-(moisture_2 - swc_val)/(moisture_2 - moisture_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction_val>0, (exp(moisture_3/moisture_4*(1-fraction_val^moisture_4))*(fraction_val^moisture_3)), 0)
plot(swc_val, Landis_Relative_production, lwd=3, ylab="Maximum Relative GPP", xlab="Soil Water Content", type = "l")



Ratio_AvailWaterToPET <- seq(0, 2, length.out = 100)
availWaterCapacity <- 0.6

moisturecurve1 = 0.0 # OtherData.MoistureCurve1; -- hardcoded at 0
moisturecurve2 = 1
moisturecurve3 = 0.5

intcpt = moisturecurve1 + (moisturecurve2 * availWaterCapacity);
slope = 1.0 / (moisturecurve3 - intcpt);

WaterLimit = 1.0 + slope * (Ratio_AvailWaterToPET - moisturecurve3);

WaterLimit <- ifelse(WaterLimit > 1, 1, WaterLimit)
WaterLimit <- ifelse(WaterLimit < 0.01, 0.01, WaterLimit)

plot(WaterLimit ~ Ratio_AvailWaterToPET)

