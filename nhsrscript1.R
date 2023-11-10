#Installation ------------------------------------------------------------------
devtools::install_github("PublicHealthEngland/fingertipsR")
install.packages("gridGraphics")
install.packages("usethis")
install.packages("gitcreds")
#Loading libraries -------------------------------------------------------------
library(tidyverse)
library(fingertipsR)
library(RColorBrewer)
library(sf)
library(ggplot2)
library(tmaptools)
library(readxl)
library(sp)
library(raster)
library(geosphere)
library(units)
library(dplyr)
library(cartogram)
library(ggrepel)
library(viridis)
library(biscale)
library(cowplot)
library(gridGraphics)

sf_use_s2(FALSE)

library(usethis)
library(gitcreds)
use_git_config(user.name = "num4yr", user.email = "numayrhabib@aol.com")
usethis::git_sitrep()
usethis::git_vaccinate()
usethis::create_github_token()
gitcreds::gitcreds_set()
#Getting data ------------------------------------------------------------------
NHS_Region_Outline <- st_simplify(st_read(
  "~/NHSr/NHSR_DATA/NHS_England_Regions_April_2021_EN_BGC_2022_5536111515427249197"),dTolerance = 75)
NHS_ICB_Outline <-st_simplify(st_read(
  "~/NHSr/NHSR_DATA/Integrated_Care_Boards_April_2023_EN_BFC_1659257819249669363"),dTolerance = 75)
ICS_characteristics <- read_excel("C:/Users/numay/Downloads/appendix_1_data_table_-_ics_characteristics.xlsx")
diabetesxlsx <- read_excel("NHSR_DATA/diabetesxlsx.xlsx")
ICS_characteristics <- ICS_characteristics %>% rename("ICB23CD" = "ICB22CD","ICB23CDH" = "ICB22CDH","ICB23NM" = "ICB22NM")
#ICB22 -> ICB23

NHS_MAL_Outline <- filter(NHS_Region_Outline, NHSER21NM == "Midlands")
midlands_Region_ID <- list("E54000019", "E54000011", "E54000010", "E54000062", "E54000018", "E54000055", "E54000059", "E54000015", "E54000013", "E54000060", "E54000058")
NHS_ICB_Outline <- filter(NHS_ICB_Outline, NHS_ICB_Outline$ICB23CD %in% midlands_Region_ID)
NHS_ICB_Outline_ICS <- left_join(NHS_ICB_Outline,ICS_characteristics, by="ICB23CD")
NHS_ICB_Outline_ICS <- NHS_ICB_Outline_ICS %>% rename("ICB23NM" = "ICB23NM.x")
NHS_ICB_Outline_ICS <- left_join(NHS_ICB_Outline_ICS,diabetesxlsx, by="ICB23NM")
NHS_ICB_Outline_ICS <- bi_class(NHS_ICB_Outline_ICS, x = `The mean IMD score`, y = Value, style = "quantile", dim = 4)

ICB_by_IMD <- ggplot(NHS_ICB_Outline_ICS) +
  geom_sf(aes(fill = bi_class), colour = NA) +
  ggtitle(label = "Mean IMD score across ICB locations in the Midlands vs. Diabetes: QOF prevalence (17+ yrs)") +
  #scale_fill_continuous(name = "Mean IMD score")+
  bi_scale_fill(pal = "BlueYl", dim = 4)+ 
  
  #geom_point( aes(geometry=geometry, size = Value), colour = "#D8579f", stat= "sf_coordinates")+
  theme_void()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  
IMDdiabetes <- bi_legend(pal = "BlueYl",
                         dim = 4,
                         xlab = "Mean IMD Score",
                         ylab = "Diabetes: QOF Prevalence",
                         size = 8)


IMDdiabetesPlot <- ggdraw() +
  draw_plot(ICB_by_IMD, 0, 0, 1, 1) +
  draw_plot(IMDdiabetes, 0.75, 0.15, 0.2, 0.2)
IMDdiabetesPlot