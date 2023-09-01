# Exploring Team Modules CES 

# 2020 --------------------------------------
DAR_20 <- read_sav("2020_DAR/CES20_DAR_OUTPUT_vv.sav")
DKI_20 <- read_sav("2020_DKI/CCES20_DKI_OUTPUT_vv.sav")
GWU_20 <- read_sav("2020_GWU/CES20_GWU_OUTPUT_vv.sav")
LSU_20 <- read_sav("2020_LSU/CES20_LSU_OUTPUT_vv.sav")
UDE_20 <- read_sav("2020_UDE/CES20_UDE_OUTPUT_vv.sav")
UMV_20 <- read_sav("2020_UMV/CES20_UMV_OUTPUT_vv.sav")

# seeing the distribution of residency across the country 
hist(DAR_20$inputstate)

# subsetting to just latinos / hisp

lat_DAR <- subset(DAR_20, DAR_20$race == 3 | DAR_20$hispanic == 1)
lat_DKI <- subset(DKI_20, DKI_20$race == 3 | DKI_20$hispanic == 1)
lat_GWU <- subset(GWU_20, GWU_20$race == 3 | GWU_20$hispanic == 1)
lat_LSU <- subset(LSU_20, LSU_20$race == 3 | LSU_20$hispanic == 1)
lat_UDE <- subset(UDE_20, UDE_20$race == 3 | UDE_20$hispanic == 1)
lat_UMV <- subset(UMV_20, UMV_20$race == 3 | UMV_20$hispanic == 1)

# subsetting to desired variables