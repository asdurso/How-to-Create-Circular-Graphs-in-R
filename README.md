# How-to-Create-Circular-Graphs-in-R
This tutorial condenses the `circlize` manual for creating circular graphs in R, It provides an easy how-to guide for showing how to display data in a circular format using `circlize`.The data used are publicly available data on health markers from []. This tutorial supplements ["Circular Visualization in R" by Zuguang Gu](https://jokergoo.github.io/circlize_book/book/index.html). 

# Tutorial 

## Terminology 

The main components of circular graphs are *sectors*, *tracks*, and *cells*. 

**Sectors**: Sectors comprise each 'slice' of the circular graph 
**Tracks**: Tracks comprise each 'ring' of the circular graph
**Cells**: Cells are individual components of the circular graph

From the `circlize` manual: *"A circular layout is composed of sectors and tracks. For data in different categories, they are allocated into different sectors, and for multiple measurements on the same category, they are represented as stacked tracks from outside of the circle to the inside. The intersection of a sector and a track is called a cell (or a grid, a panel), which is the basic unit in a circular layout. It is an imaginary plotting region for drawing data points."*

## Set up 

First, we will set up our workspace by loaded any packages, data, and functions we will use.

### Load packages 

Typically, it is advised to load `tidyverse` last, because I want to use the defaults associated with `tidyverse`. However, in the case that there are  incompatibilities with `circlize`, I suggest loading `circlize` last. 

```{r}
library(broom)
library(tidyverse)
library(circlize)
```

Next, we will load our function. The function `color_fun` helps create a scale of color based on the minimum value, maximum value, and median value of the data column. 

```{r}
color_fun <- function(data, variable, color1, color2, color3) {
  sum_var <- summary(eval(substitute(data$variable))) %>% broom::tidy() #gets summary data for column
 colfun_var <- colorRamp2(c(sum_var$minimum, sum_var$median, sum_var$maximum), c(color1, color2, color3)) #specifies colors for each value, minimum, median, and maximum
 colfun_var(eval(substitute(data$variable))) #applies `colorRamp2` to get the specific color label 
}
```

### Data import 

Next, we will import our data. The data are from ___. In this case, I have also appended codes for regions of the United States and the states within those regions. This new variable is called `region`. For simplicity, I have not included the code for `region` in this tutorial. 

```{r}
pub_dat <- read_csv("pub_dat_cleaned.csv")
```

Next, we should arrange our states in a manner that corresponds with our regions. That is, our states will be groups within regions. 

In the example below, I show one way to accomplish this. In this case, I specify the factor order of the states to align with regional groupings.

```{r eval = F}
pub_dat <- pub_dat %>% 
  mutate(state_abbreviation = fct_reorder(state_abbreviation, region, min)) %>%   arrange(state_abbreviation)
```

### Secondary level groupings: Grouping Sectors

We can easily group variables by the smallest unit available in the data and of interest. In this case, we have data at he state level. But we may also wish to group states within regions. That is, we may want to do additional groupings at a larger level than our primary unit. 

In this case, it is easier to create these secondary groupings before creating our circular graph. This is because the circular graph is created using base R, which layers parts of the graph. It is easier to layer using information we have created beforehand. In this case, it is the regional groupings. To do this, we create subsetted versions of the data by region, as shown below.

Once we make this secondary level grouping, we can use the circlize function `highlight.sector` to outline the secondary-level groupings. , which we will see in the following code chunk. Before we can highlight the sectors we want, we must create a string of characters which include the state names we want to group together.

```{r}
en_central <- pub_dat$state_abbreviation[pub_dat$region =="E.N. Central"] #groups the states in East North Central

es_central <- pub_dat$state_abbreviation[pub_dat$region =="E.S. Central"] #groups the states in East South Central

mid_atlantic <- pub_dat$state_abbreviation[pub_dat$region =="Mid-Atlantic"] #groups the states in the Mid-Atlantic

mountain <- pub_dat$state_abbreviation[pub_dat$region =="Mountain"] #groups the states in Mountain region

new_england <- pub_dat$state_abbreviation[pub_dat$region =="New England"] #groups the states in New England

pacific <- pub_dat$state_abbreviation[pub_dat$region =="Pacific"] #groups the states in the Pacific region

south_atlantic <- pub_dat$state_abbreviation[pub_dat$region =="South Atlantic"] #groups the states in the South Atlantic

wn_central <- pub_dat$state_abbreviation[pub_dat$region =="W.N. Central"] #groups the states in West North Central

ws_central <- pub_dat$state_abbreviation[pub_dat$region =="W.S. Central"] #groups the states in West South Central
```

## Initialize circle 

Next, we will initialize our circle plot by specifying how tall our tracks should be and how each section should be divided. In this case, the sectors are divided by each state--our lowest unit of interest in this case.

```{r, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))
```

**But there is nothing there!** That is because we have not specified what information we want. To do this, we have to specify one track at a time. Each time we call a track, it will be plotted on consecutive inner rings.

Next, we will add information onto the track. Here, we specify that we want the circular graph to be divided (or sliced) by the state. That will be our sector indicator. For this first track, we want to look at population in 2017. The code `circos.text` and `circos.axis` allow for modifying the text surrounding the first track, as well as the track axis positioning and features.

```{r, eval=F}
# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, panel.fun = function(x, y) { circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T, CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })
```


```{r, echo=F, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", panel.fun = function(x, y) { circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T, CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })
```
We now see that we have a circular graph with state labels, but it does not have any information about the population. If you look at the `circlize` manual, you'll see there are many options for treating each cell as its own plot. That is, you can plot histograms, line graphs, box plots, etc... in each cell. But when dealing with so many units, such as 50 states, we may want to display information in a simpler way.

In this case, we can specify the background color of each cell. The color can correspond to the intensity of the information we are interested in plotting. To do this, we use the `color_fun` we loaded during the setup in the argument `bg.col` (i.e. background color). 

In the example below, we are going to set up our colors to correspond to population levels. States with lower populations will be a light brown-red color (e.g. `burlywood1`). States with populations near the median will be `red`. States with the highest population are the darkest color (e.g. `darkred`).

```{r eval = F}
# adding background color information

circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })
```

```{r fig.height = 20, fig.width = 20, echo = F}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })
```

As we can see, California is the darkest color, which makes sense.

If we want to add additional information, we must specify new track levels, again using `color_fun` to change the background color. 

Here we can add information about the levels of urban populations, non-white residents, residents living in poverty, and uninsured residents per state. 

```{r fig.height = 20, fig.width = 20, eval = F}
circos.track(pub_dat$state_abbreviation, y = pub_dat$urban_perc2010, bg.col = color_fun(pub_dat, urban_perc2010, "lightblue1", "lightblue3", "lightblue4"), bg.border= "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$non_white, bg.col = color_fun(pub_dat, non_white, "pink", "pink2", "pink4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$poverty2017, bg.col = color_fun(pub_dat, poverty2017, "seashell1", "seashell3", "seashell4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$uninsured, bg.col = color_fun(pub_dat, uninsured, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), bg.border = "grey50")
```


```{r fig.height = 20, fig.width = 20, echo = F}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

circos.track(pub_dat$state_abbreviation, y = pub_dat$urban_perc2010, bg.col = color_fun(pub_dat, urban_perc2010, "lightblue1", "lightblue3", "lightblue4"), bg.border= "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$non_white, bg.col = color_fun(pub_dat, non_white, "pink", "pink2", "pink4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$poverty2017, bg.col = color_fun(pub_dat, poverty2017, "seashell1", "seashell3", "seashell4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$uninsured, bg.col = color_fun(pub_dat, uninsured, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), bg.border = "grey50")
```

Although this contains all of the information we may be interested in, it can be a little overwhelming to interpret. Adding in regional groupings can help orient readers. 

Using `highlight.sector` and the strings of states per region, we can group our graph. It should be noted, however, that you should arrange your states by region. 

```{r fig.height = 20, fig.width = 20, eval = F}
highlight.sector(col = NULL, sector.index = en_central, border = "black", lwd = 3.5, text = "East North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)
```

```{r echo = F, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

circos.track(pub_dat$state_abbreviation, y = pub_dat$urban_perc2010, bg.col = color_fun(pub_dat, urban_perc2010, "lightblue1", "lightblue3", "lightblue4"), bg.border= "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$non_white, bg.col = color_fun(pub_dat, non_white, "pink", "pink2", "pink4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$poverty2017, bg.col = color_fun(pub_dat, poverty2017, "seashell1", "seashell3", "seashell4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$uninsured, bg.col = color_fun(pub_dat, uninsured, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), bg.border = "grey50")

highlight.sector(col = NULL, sector.index = en_central, border = "black", lwd = 3.5, text = "East North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)
```

Within `highlight.sector` there are a few useful arguments. `border` specifies the border color. `lwd` specifies the thickness of the border, `text` is the label name for the section. `niceFacing` rotates the bottom text to face outward, making it easier to read. `text.vjust` allows you to change the vertical justification of the text. And `cex` is the text size. 

We must continue the process for each region to cover the entire graph. 

```{r fig.height = 20, fig.width = 20, eval = F}

highlight.sector(col = NULL, sector.index = es_central, border = "black", lwd = 3.5, text = "East South Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mid_atlantic, border = "black", lwd = 3.5, text = "Mid-Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mountain, border = "black", lwd = 3.5, text = "Mountain", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = new_england, border = "black", lwd = 3.5, text = "New England", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = pacific, border = "black", lwd = 3.5, text = "Pacific", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = south_atlantic, border = "black", lwd = 3.5, text = "South Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = wn_central, border = "black", lwd = 3.5, text = "West North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = ws_central, border = "black", lwd = 3.5, niceFacing = T, facing = "bending.inside", text = "West South Central", text.vjust = "2.5inches", cex = 2)
```

```{r echo = F, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

circos.track(pub_dat$state_abbreviation, y = pub_dat$urban_perc2010, bg.col = color_fun(pub_dat, urban_perc2010, "lightblue1", "lightblue3", "lightblue4"), bg.border= "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$non_white, bg.col = color_fun(pub_dat, non_white, "pink", "pink2", "pink4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$poverty2017, bg.col = color_fun(pub_dat, poverty2017, "seashell1", "seashell3", "seashell4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$uninsured, bg.col = color_fun(pub_dat, uninsured, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), bg.border = "grey50")

highlight.sector(col = NULL, sector.index = en_central, border = "black", lwd = 3.5, text = "East North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = es_central, border = "black", lwd = 3.5, text = "East South Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mid_atlantic, border = "black", lwd = 3.5, text = "Mid-Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mountain, border = "black", lwd = 3.5, text = "Mountain", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = new_england, border = "black", lwd = 3.5, text = "New England", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = pacific, border = "black", lwd = 3.5, text = "Pacific", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = south_atlantic, border = "black", lwd = 3.5, text = "South Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = wn_central, border = "black", lwd = 3.5, text = "West North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = ws_central, border = "black", lwd = 3.5, niceFacing = T, facing = "bending.inside", text = "West South Central", text.vjust = "2.5inches", cex = 2)
```

While it may look like the graph is complete, we must add a legend! 

The best way to do this is to display each ring as a row in the legend. Because of the `legend` arguments, it is best to specify each element as a matrix, arranged by row.

For border, we specify the first element's border as `NA`, because we want to use it as the name of the variable. This is also the case for the `fill`. We repeat this each time for the number of rings. Finally, since we have the name of the variable and three colors for min, max, and median, we want four columns. For the labels, we specify the variable name, as well as labels for each color. 
```{r eval = F}
borders <- matrix(rep(c(NA, "black", "black", "black"), 5), ncol = 4, byrow = T)

colors <- matrix(c(NA, "burlywood1", "red", "darkred", NA, "lightblue1", "lightblue3", "lightblue4", NA, "pink", "pink2", "pink4", NA, "seashell1", "seashell3", "seashell4", NA, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), ncol = 4, byrow = T)

labels <- matrix(c("Outter Ring:", "Min Population", "Median Population", "Max Population", 
            "Second Ring:", "Min Urban", "Median Urban", "Max Urban",
            "Middle Ring:", "Min Non-White", "Median Non-White", "Max Non-White", 
            "Fourth Ring:", "Min Poverty", "Median Poverty", "Max Poverty", 
            "Inner Ring:", "Min Uninsured", "Median Uninsured", "Max Uninsured"), ncol = 4, byrow = T)


legend("bottom", legend = labels, fill = colors, border = borders, cex = 1.4, ncol = 4)
```
### Our Graph: State Population Characteristics
```{r echo = F, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1, 1), canvas.ylim = c(-1.3, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-100, 100))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$population2017, bg.border = "grey50", bg.col = color_fun(pub_dat, population2017, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

circos.track(pub_dat$state_abbreviation, y = pub_dat$urban_perc2010, bg.col = color_fun(pub_dat, urban_perc2010, "lightblue1", "lightblue3", "lightblue4"), bg.border= "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$non_white, bg.col = color_fun(pub_dat, non_white, "pink", "pink2", "pink4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$poverty2017, bg.col = color_fun(pub_dat, poverty2017, "seashell1", "seashell3", "seashell4"), bg.border = "grey50")

circos.track(pub_dat$state_abbreviation, y = pub_dat$uninsured, bg.col = color_fun(pub_dat, uninsured, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), bg.border = "grey50")

highlight.sector(col = NULL, sector.index = en_central, border = "black", lwd = 3.5, text = "East North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = es_central, border = "black", lwd = 3.5, text = "East South Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mid_atlantic, border = "black", lwd = 3.5, text = "Mid-Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = mountain, border = "black", lwd = 3.5, text = "Mountain", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = new_england, border = "black", lwd = 3.5, text = "New England", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = pacific, border = "black", lwd = 3.5, text = "Pacific", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = south_atlantic, border = "black", lwd = 3.5, text = "South Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = wn_central, border = "black", lwd = 3.5, text = "West North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2.5inches", cex = 2)

highlight.sector(col = NULL, sector.index = ws_central, border = "black", lwd = 3.5, niceFacing = T, facing = "bending.inside", text = "West South Central", text.vjust = "2.5inches", cex = 2)

borders <- matrix(rep(c(NA, "black", "black", "black"), 5), ncol = 4, byrow = T)

colors <- matrix(c(NA, "burlywood1", "red", "darkred", NA, "lightblue1", "lightblue3", "lightblue4", NA, "pink", "pink2", "pink4", NA, "seashell1", "seashell3", "seashell4", NA, "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"), ncol = 4, byrow = T)

labels <- matrix(c("Outter Ring:", "Min Population", "Median Population", "Max Population", 
            "Second Ring:", "Min Urban", "Median Urban", "Max Urban",
            "Middle Ring:", "Min Non-White", "Median Non-White", "Max Non-White", 
            "Fourth Ring:", "Min Poverty", "Median Poverty", "Max Poverty", 
            "Inner Ring:", "Min Uninsured", "Median Uninsured", "Max Uninsured"), ncol = 4, byrow = T)


legend("bottom", legend = labels, fill = colors, border = borders, cex = 1.4, ncol = 4)
```

## Dealing with NA's

Sometimes, we will have missing data. This adds a bit of complexity to plotting the circular graph.

```{r error = T, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1.25, 1.25), canvas.ylim = c(-1.5, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-1, 1))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$op_prescription100person_2018, bg.border = "grey50", bg.col = color_fun(pub_dat, op_prescription100person_2018, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

circos.track(pub_dat$state_abbreviation, y = pub_dat$opioid_admissions, bg.col = color_fun(pub_dat, opioid_admissions, "mediumpurple1", "mediumpurple3", "mediumpurple4"), bg.border = "grey50")
```

Here, we see that our second track did not plot Upon looking at the data, we see we have missing values. 

```{r}
summary(pub_dat$opioid_admissions)
```

To deal with this, we must replace the missing values both in our color and our data with something else. For the string of colors using `color_fun`, we replace the missing values with white (e.g."#FFFFFFFF"). Then, we substitute a value that is not in our data for the missing values in our data. In this case, I use 0. It is important to use `color_fun` beforehand, so that the function does not account for the added value in its color scale calculation. 

Finally, we can plot the track with the appropriate scale, leaving out the missing states in white. 

```{r eval = F}

admis_col <- color_fun(pub_dat, opioid_admissions, "mediumpurple1", "mediumpurple3", "mediumpurple4")

admission_colors <- replace_na(admis_col, "#FFFFFFFF")

admission <- replace_na(pub_dat$opioid_admissions, 0)

circos.track(pub_dat$state_abbreviation, y = admission, bg.col = admission_colors, bg.border = "grey50")
```

```{r echo = F, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1.25, 1.25), canvas.ylim = c(-1.5, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-1, 1))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$op_prescription100person_2018, bg.border = "grey50", bg.col = color_fun(pub_dat, op_prescription100person_2018, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

admis_col <- color_fun(pub_dat, opioid_admissions, "mediumpurple1", "mediumpurple3", "mediumpurple4")

admission_colors <- replace_na(admis_col, "#FFFFFFFF")

admission <- replace_na(pub_dat$opioid_admissions, 0)

circos.track(pub_dat$state_abbreviation, y = admission, bg.col = admission_colors, bg.border = "grey50")

```

However, we may want to give our viewers an understanding of what the white cells mean. In this case, we can write 'NA' for our missing values. In this example we are missing data for Maryland, Georgia, and Oregon. In `circos.text`, the first two arguments are the x and y locations of the plot. `adj` is the radial adjustment of the text. In this case, I wanted the text to be centered in the middle of the cell. I want the text to say 'NA', and the color to be `red4`. `track.index` specifies which track we want to add the text in. Since this is the second ring, we specify `track.index = 2`. `sector.index` is the slice we want. In this case, it is the state abbreviation: MD, GA, and OR.

```{r eval = F}
circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "MD", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "GA", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0,0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "OR", track.index = 2, cex = 1.8, niceFacing = T)

```

```{r echo = F, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1.25, 1.25), canvas.ylim = c(-1.5, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-1, 1))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$op_prescription100person_2018, bg.border = "grey50", bg.col = color_fun(pub_dat, op_prescription100person_2018, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

admis_col <- color_fun(pub_dat, opioid_admissions, "mediumpurple1", "mediumpurple3", "mediumpurple4")

admission_colors <- replace_na(admis_col, "#FFFFFFFF")

admission <- replace_na(pub_dat$opioid_admissions, 0)

circos.track(pub_dat$state_abbreviation, y = admission, bg.col = admission_colors, bg.border = "grey50")

circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "MD", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "GA", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0,0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "OR", track.index = 2, cex = 1.8, niceFacing = T)
```

From there, we can continue adding our tracks, regional groupings, and legends as we did above. 

The full code will look something like this: 

### Opioid Use and Mortality

```{r fig3, fig.height = 20, fig.width = 20}
# determines how 'tall' the tracks should be
circos.par("track.height" = 0.075, canvas.xlim = c(-1.25, 1.25), canvas.ylim = c(-1.5, 1.1)) 

# initializes the data, specifies that each state will be it's own category
circos.initialize(pub_dat$state_abbreviation, xlim = c(-1, 1))

# adding track information
circos.track(pub_dat$state_abbreviation, y = pub_dat$op_prescription100person_2018, bg.border = "grey50", bg.col = color_fun(pub_dat, op_prescription100person_2018, "burlywood1", "red", "darkred"),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5) + mm_y(2), niceFacing = T,
                           CELL_META$sector.index, 
                           cex = 1.5)
               circos.axis(labels.cex = 0.5, 
                           labels = F,
                           major.tick = F)
             })

admis_col <- color_fun(pub_dat, opioid_admissions, "mediumpurple1", "mediumpurple3", "mediumpurple4")

admission_colors <- replace_na(admis_col, "#FFFFFFFF")

admission <- replace_na(pub_dat$opioid_admissions, 0)

circos.track(pub_dat$state_abbreviation, y = admission, bg.col = admission_colors, bg.border = "grey50")

circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "MD", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0, 0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "GA", track.index = 2, cex = 1.8, niceFacing = T)

circos.text(0,0, adj = c(0.5, 0), "NA", col = "red4", sector.index = "OR", track.index = 2, cex = 1.8, niceFacing = T)

circos.track(pub_dat$state_abbreviation, y = pub_dat$op_deaths100k2017, bg.col = color_fun(pub_dat, op_deaths100k2017, "darkolivegreen1", "darkolivegreen3", "darkolivegreen4"), bg.border = "grey50")

highlight.sector(col = NULL, sector.index = en_central, border = "black", lwd = 3.5, text = "East North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = es_central, border = "black", lwd = 3.5, text = "East South Central", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = mid_atlantic, border = "black", lwd = 3.5, text = "Mid-Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = mountain, border = "black", lwd = 3.5, text = "Mountain", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = new_england, border = "black", lwd = 3.5, text = "New England", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = pacific, border = "black", lwd = 3.5, text = "Pacific", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = south_atlantic, border = "black", lwd = 3.5, text = "South Atlantic", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = wn_central, border = "black", lwd = 3.5, text = "West North Central", niceFacing = T, facing = "bending.inside", text.vjust = "2inches", cex = 2)

highlight.sector(col = NULL, sector.index = ws_central, border = "black", lwd = 3.5, niceFacing = T, facing = "bending.inside", text = "West South Central", text.vjust = "2inches", cex = 2)

borders <- matrix(rep(c(NA, "black", "black", "black"), 3), ncol = 4, byrow = T)

colors <- matrix(c(NA, "burlywood1", "red", "darkred", NA, "mediumpurple1", "mediumpurple3", "mediumpurple4", NA, "darkolivegreen1", "darkolivegreen3", "darkolivegreen4"), ncol = 4, byrow = T)

labels <- matrix(c("Outter Ring: Opioid Rx (100 People)", "Min ", "Median", "Max", 
            "Middle Ring: Admissions (100k People)", "Min", "Median", "Max", 
            "Inner Ring: Opioid Deaths (100k People)", "Min", "Median", "Max"), ncol = 4, byrow = T)

legend("bottom", legend = labels, fill = colors, border = borders, cex = 1.4, ncol = 4)

```
