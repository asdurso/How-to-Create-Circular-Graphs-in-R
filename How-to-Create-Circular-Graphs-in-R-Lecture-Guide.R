###

##

## Load Packages 
library(broom)
library(tidyverse)
library(circlize)

## Load Function 
color_fun <- function(data, variable, color1, color2, color3) {
  #gets summary data for column
  sum_var <- summary(eval(substitute(data$variable))) %>% broom::tidy() 
  #specifies colors for each value, minimum, median, and maximum
  colfun_var <- colorRamp2(c(sum_var$minimum, sum_var$median, sum_var$maximum), c(color1, color2, color3)) 
  #applies `colorRamp2` to get the specific color label 
  colfun_var(eval(substitute(data$variable))) 
}

## Load Data 
pub_dat <- read_csv("pub_dat_cleaned.csv")

## Clean Data 
### Reorder 
pub_dat <- pub_dat %>%
  mutate(state_abbreviation = fct_reorder(state_abbreviation, region, min)) %>%   
  arrange(state_abbreviation)

### Create Regions 
en_central <- pub_dat$state_abbreviation[pub_dat$region =="E.N. Central"] #groups the states in East North Central

es_central <- pub_dat$state_abbreviation[pub_dat$region =="E.S. Central"] #groups the states in East South Central

mid_atlantic <- pub_dat$state_abbreviation[pub_dat$region =="Mid-Atlantic"] #groups the states in the Mid-Atlantic

mountain <- pub_dat$state_abbreviation[pub_dat$region =="Mountain"] #groups the states in Mountain region

new_england <- pub_dat$state_abbreviation[pub_dat$region =="New England"] #groups the states in New England

pacific <- pub_dat$state_abbreviation[pub_dat$region =="Pacific"] #groups the states in the Pacific region

south_atlantic <- pub_dat$state_abbreviation[pub_dat$region =="South Atlantic"] #groups the states in the South Atlantic

wn_central <- pub_dat$state_abbreviation[pub_dat$region =="W.N. Central"] #groups the states in West North Central

ws_central <- pub_dat$state_abbreviation[pub_dat$region =="W.S. Central"] #groups the states in West South Central

# Circular Graphs 
## Initalize

# determines how 'tall' the tracks should be
circos.par(
  "track.height" = 0.075,
  canvas.xlim = c(-1, 1),
  canvas.ylim = c(-1.3, 1.1)
)

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

## Tracks
# adding track information
circos.track(
  pub_dat$state_abbreviation,
  y = pub_dat$population2017,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2),
      niceFacing = T,
      CELL_META$sector.index,
      cex = 1.5
    )
    circos.axis(labels.cex = 0.5,
                labels = F,
                major.tick = F)
  }
)

## Adding Info
# adding background color information
circos.clear()
circos.par(
  "track.height" = 0.075,
  canvas.xlim = c(-1, 1),
  canvas.ylim = c(-1.3, 1.1)
)

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

circos.track(
  pub_dat$state_abbreviation,
  y = pub_dat$population2017,
  bg.border = "grey50",
  ## Specify color here ##
  bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2),
      niceFacing = T,
      CELL_META$sector.index,
      cex = 1.5
    )
    circos.axis(labels.cex = 0.5,
                labels = F,
                major.tick = F)
  }
)

## New Tracks; New Info
circos.track(pub_dat$state_abbreviation, y = pub_dat$urban_perc2010, bg.col = color_fun(pub_dat, urban_perc2010, "lightblue1", "lightblue3", "lightblue4"), bg.border= "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$non_white, bg.col = color_fun(pub_dat, non_white, "pink", "pink2", "pink4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$poverty2017, bg.col = color_fun(pub_dat, poverty2017, "seashell1", "seashell3", "seashell4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$uninsured, bg.col = color_fun(pub_dat, uninsured, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), bg.border = "grey50")

## Regional Groupings: Highlight Sectors 
highlight.sector(col = NULL, sector.index = en_central, border = "black", lwd = 3.5, text = "East North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

# continue adding 
highlight.sector(col = NULL, sector.index = es_central, border = "black", lwd = 3.5, text = "East South Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mid_atlantic, border = "black", lwd = 3.5, text = "Mid-Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mountain, border = "black", lwd = 3.5, text = "Mountain", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = new_england, border = "black", lwd = 3.5, text = "New England", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = pacific, border = "black", lwd = 3.5, text = "Pacific", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = south_atlantic, border = "black", lwd = 3.5, text = "South Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = wn_central, border = "black", lwd = 3.5, text = "West North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = ws_central, border = "black", lwd = 3.5, niceFacing = T, facing = "bending.inside", text = "West South Central", text.vjust = "2.5inches", cex = 2)

## Legends 
borders <- matrix(rep(c(NA, "black", "black", "black"), 5), ncol = 4, byrow = T)

colors <- matrix(c(NA, "burlywood1", "red", "darkred", NA, "lightblue1", "lightblue3", "lightblue4", NA, "pink", "pink2", "pink4", NA, "seashell1", "seashell3", "seashell4", NA, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), ncol = 4, byrow = T)

labels <- matrix(c("Outter Ring:", "Min Population", "Median Population", "Max Population", 
                   "Second Ring:", "Min Urban", "Median Urban", "Max Urban",
                   "Middle Ring:", "Min Non-White", "Median Non-White", "Max Non-White", 
                   "Fourth Ring:", "Min Poverty", "Median Poverty", "Max Poverty", 
                   "Inner Ring:", "Min Uninsured", "Median Uninsured", "Max Uninsured"), ncol = 4, byrow = T)


legend("bottom", legend = labels, fill = colors, border = borders, cex = 1.4, ncol = 4)

# NA's 
circos.clear()
# determines how 'tall' the tracks should be
circos.par(
  "track.height" = 0.075,
  canvas.xlim = c(-1.25, 1.25),
  canvas.ylim = c(-1.5, 1.1)
)

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-1, 1))

# adding track information
circos.track(
  pub_dat$state_abbreviation,
  y = pub_dat$op_prescription100person_2018,
  bg.border = "grey50",
  bg.col = color_fun(
    pub_dat,
    op_prescription100person_2018,
    "burlywood1",
    "red",
    "darkred"
  ),
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2),
      niceFacing = T,
      CELL_META$sector.index,
      cex = 1.5
    )
    circos.axis(labels.cex = 0.5,
                labels = F,
                major.tick = F)
  }
)
## Second Track on Opioid Admissions

circos.track(
  pub_dat$state_abbreviation,
  y = pub_dat$opioid_admissions,
  bg.col = color_fun(
    pub_dat,
    opioid_admissions,
    "mediumpurple1",
    "mediumpurple3",
    "mediumpurple4"
  ),
  bg.border = "grey50"
)

## Dealing with NA's 
admis_col <- color_fun(pub_dat, opioid_admissions, "mediumpurple1", "mediumpurple3", "mediumpurple4")

## Making NA's white
admission_colors <- replace_na(admis_col, "#FFFFFFFF")

admission <- replace_na(pub_dat$opioid_admissions, 0)

circos.track(pub_dat$state_abbreviation, y = admission, bg.col = admission_colors, bg.border = "grey50")

## Labeling NA's 
circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "MD", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "GA", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0,0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "OR", track.index = 2, cex = 1.8, niceFacing = T)