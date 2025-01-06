Castor_latitudinal_gradient
================
Pooja Nathan
2025-01-06

\##Latitudinal gradients in species interactions

In this study, we sampled populations of the castor plant, Ricinus
communis, across a large latitudinal range in the Indian subcontinent to
understand if, when the host species is kept constant, there is
variation in mutualistic traits and interactions across latitude as
posited by the biotic interactions hypothesis.

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Warning: package 'ggspatial' was built under R version 4.4.2

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggpubr':
    ## 
    ##     get_legend

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## 
    ## Attaching package: 'patchwork'

    ## The following object is masked from 'package:cowplot':
    ## 
    ##     align_plots

    ## This is lavaan 0.6-18
    ## lavaan is FREE software! Please report any bugs.

    ## Registered S3 method overwritten by 'MuMIn':
    ##   method        from 
    ##   nobs.multinom broom

    ## 
    ##   This is piecewiseSEM version 2.3.0.
    ## 
    ## 
    ##   Questions or bugs can be addressed to <LefcheckJ@si.edu>.

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## Loading required package: sp

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     getData

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE

    ## Linking to ImageMagick 6.9.12.98
    ## Enabled features: cairo, freetype, fftw, ghostscript, heic, lcms, pango, raw, rsvg, webp
    ## Disabled features: fontconfig, x11

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.6-8

    ## terra 1.7.78

    ## 
    ## Attaching package: 'terra'

    ## The following object is masked from 'package:scales':
    ## 
    ##     rescale

    ## The following object is masked from 'package:grid':
    ## 
    ##     depth

    ## The following object is masked from 'package:patchwork':
    ## 
    ##     area

    ## The following object is masked from 'package:ggpubr':
    ## 
    ##     rotate

    ## The following object is masked from 'package:knitr':
    ## 
    ##     spin

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## corrplot 0.94 loaded

    ## Loading required package: viridisLite

    ## 
    ## Attaching package: 'viridis'

    ## The following object is masked from 'package:scales':
    ## 
    ##     viridis_pal

Let’s make a plot of all sites sampled on a map of India

``` r
#Read shapefile
#shapefile_path <- "gadm41_IND_shp/gadm41_IND_0.shp"
shapefile_path <- "India_Country_Boundary.shp"
india <- st_read(shapefile_path)
```

    ## Reading layer `India_Country_Boundary' from data source 
    ##   `D:\PoojaCN\University of Toronto\Academics\Chapter 2 - Mutualism Latitudinal gradient\Data\castor_latitudinal_gradient\India_Country_Boundary.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 253 features and 1 field
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 7583508 ymin: 753607.8 xmax: 10843390 ymax: 4451428
    ## Projected CRS: WGS 84 / Pseudo-Mercator

``` r
#Set latitude and longitude of sites
Site <- c("Ramnagar", "Aligarh", "Gwalior", "Bhopal","Nagpur", "Hyderabad", 
          "Hampi", "Bangalore","Port Blair", "Srirangam", "Kanyakumari" )
Lat <- c(29.40, 27.89, 26.23, 23.22, 21.25, 17.33, 15.34, 13.07, 11.62, 10.86, 8.08)
Lon <- c(79.13, 78.03, 78.26, 77.38, 79.08, 78.44, 76.46, 77.53, 92.73, 78.70, 77.52)

sites <- data.frame(Site, Lat, Lon)

sites_sf <- st_as_sf(sites, coords = c("Lon", "Lat"), crs = 4326)

degree_label <- function(x) {
  paste0(x, "\u00B0")
}
# Transform india and sites_sf to UTM (select appropriate zone for India)
india <- st_transform(india, crs = 32644) # UTM Zone 44N (adjust for your region)
sites_sf <- st_transform(sites_sf, crs = 32644)

# Redraw the map
indmap <- ggplot(data = india) +
  geom_sf() +
  geom_sf(data = sites_sf, aes(color = ifelse(Site == "Port Blair", "turquoise", "olivedrab")), size = 3.5) +
  scale_x_continuous(labels = degree_label) +
  scale_y_continuous(labels = degree_label) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  scale_color_identity() + 
  annotation_scale(location = "bl", width_hint = 0.2) +  # Scale bar in bottom left
  annotation_north_arrow(location = "tl",                # Compass rose in top left
                         which_north = "true",           # True north
                         pad_x = unit(0.5, "cm"), 
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering())
```

\##Data cleaning

Now, let’s import and clean all the trait and interaction data

``` r
#Reading in plant traits dataset
morph <- read.csv("morph_natint_20May24.csv")
names(morph)[names(morph) == "Site"] <- "Location"
names(morph)[names(morph) == "Width.at.10.cm"] <- "width"
names(morph)[names(morph) == "Number.of.fruits"] <- "fruitno"
names(morph)[names(morph) == "Number.of.branches"] <- "branchno"
names(morph)[names(morph) == "Plant ID"] <- "Plant.ID"
morph$Latitude <- as.numeric(morph$Latitude)
```

    ## Warning: NAs introduced by coercion

``` r
morph$Phenology <- as.factor(morph$Phenology)
morph$width <- as.numeric(morph$width)
morph$branchno <- as.numeric(morph$branchno)
morph$fruitno <- as.numeric(morph$fruitno)


#reading in nectary size dataset
efnarea <- read.csv("Nectary_areas.csv")
efnarea$Location <- as.factor(efnarea$Location)
efnarea$Nec_type <- as.factor(efnarea$Nec_type)
efnarea$Nec_type <- as.factor(efnarea$Nec_type)
names(efnarea)[names(efnarea) == "PlantID"] <- "Plant.ID"

#Clean up, average when multiple measurements are available for the same individual/type of EFN
efnarea <- efnarea %>%
  group_by(Plant.ID, Nec_type) %>%
  summarize(
    Area = mean(Area, na.rm = TRUE),
    Location = first(Location)
  )
```

    ## `summarise()` has grouped output by 'Plant.ID'. You can override using the
    ## `.groups` argument.

``` r
#converting to wideform

efnarea <- efnarea %>%
  pivot_wider(names_from = Nec_type, values_from = Area, 
              names_prefix = "Area_") %>%
  dplyr::select(Plant.ID, Area_Leafbase, Area_Petiole, Location)

#Reading in nectar volume dataset
volume <- read.csv("Nectary_volume.csv")

#volume <- merge(volume, lat, by = "Location")
names(volume)[names(volume) == "Drop.Volume..mm.3."] <- "dropvol"
names(volume)[names(volume) == "Location.on.Plant"] <- "Type"


volume <- volume %>% 
  group_by(Plant.ID, Type) %>% 
  summarise(meanvol = mean(dropvol, na.rm = TRUE),
    Location = first(Location)) 
```

    ## `summarise()` has grouped output by 'Plant.ID'. You can override using the
    ## `.groups` argument.

``` r
#converting to wideform
volume <- volume %>%
  pivot_wider(names_from = Type, values_from = meanvol, 
              names_prefix = "Vol_") %>%
  dplyr::select(Plant.ID, Vol_Leafbase, Vol_Petiole, Location)

#Reading in seed dataset
seeds <- read.csv("Seed and elaiosome weights.csv")
names(seeds)[names(seeds) == "Site"] <- "Location"
names(seeds)[names(seeds) == "Seed.weight..g."] <- "seedweight"
names(seeds)[names(seeds) == "Elaiosome.seed"] <- "ratio"
#seeds <- merge(seeds, lat, by = "Location")

#Reading in average ant abundance by plant
#Calculated in the ant abundance analysis
#Different R script
ant_ab <- read.csv("ant_ab_by_plant.csv")

morphseed <- left_join(morph, seeds, by = "Plant.ID")
morphants <- left_join(morphseed, ant_ab, by = "Plant.ID")
morphants$average_abundance[is.na(morphants$average_abundance)] <- 0

#combining all data into one unified dataset
all_comb <- left_join(morphants, efnarea, by = "Plant.ID")
all_comb <- left_join(all_comb, volume, by = "Plant.ID")

#removing unneeded columns
data <- all_comb[, c(1:4, 6:27, 30:35, 37, 40, 41, 43, 44)]                         
names(data)[names(data) == "Location.x"] <- "Location"                      
```

## Plots and statistical models

\#Correlations among variables

``` r
png("efn_correlations.png")
corrplot::corrplot(cor(data[, c("EFNLB", "EFNS", "EFNL", "Area_Leafbase", "Area_Petiole", "Vol_Leafbase", "Vol_Petiole")], use="pairwise.complete.obs"))
dev.off()
```

    ## png 
    ##   2

``` r
#EFN volumes are correlated to EFN areas
#Calculate R^2 and p-value for Area_Leafbase vs Vol_Leafbase
model1 <- lm(log10(Vol_Leafbase) ~ log10(Area_Leafbase), data = data)
r_squared1 <- summary(model1)$r.squared
p_value1 <- summary(model1)$coefficients[2, 4]

area_volume_cor1 <- ggplot(data=data, aes(x=Area_Leafbase, y=Vol_Leafbase)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Leaf base EFNs (area, mm2)") +
  ylab("Leaf base EFNs (volume, mm3)") +
  geom_smooth(method="lm") +
  theme_cowplot() +
  annotate("text", x=2, y=2, label=paste("R^2 =", round(r_squared1, 3), "\nP =", signif(p_value1, 3)), hjust=0)

#Calculate R^2 and p-value for Area_Petiole vs Vol_Petiole
model2 <- lm(log10(Vol_Petiole) ~ log10(Area_Petiole), data = data)
r_squared2 <- summary(model2)$r.squared
p_value2 <- summary(model2)$coefficients[2, 4]

area_volume_cor2 <- ggplot(data=data, aes(x=Area_Petiole, y=Vol_Petiole)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Petiole EFNs (area, mm2)") +
  ylab("Petiole EFNs (volume, mm3)") +
  geom_smooth(method="lm") +
  theme_cowplot() +
  annotate("text", x=2, y=2, label=paste("R^2 =", round(r_squared2, 3), "\nP =", signif(p_value2, 3)), hjust=0)

area_volume <- plot_grid(area_volume_cor1, area_volume_cor2, labels=c("A) Leaf base EFNs", "B) Petiole EFNs"))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 156 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 156 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 157 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 157 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
ggsave(filename = "efn_area_volume_correlation.png", plot = area_volume, width = 12, height = 7, dpi = 300)

#Number of plants at each site
data$Location <- as.factor(data$Location)

plantnos <- data %>%
  group_by(Location) %>%
  summarise(Number_of_Plants = n_distinct(Plant.ID))
```

\##Statistical modelling

We will now use linear models and multivariate linear models where
appropriate

    ## Response EFNLB :
    ## 
    ## Call:
    ## lm(formula = EFNLB ~ Latitude + width + Type, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.03998 -0.51021  0.01978  0.39736  1.50166 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.929037   0.263410   7.323 1.29e-11 ***
    ## Latitude     -0.012125   0.006012  -2.017   0.0455 *  
    ## width        -0.019191   0.011211  -1.712   0.0890 .  
    ## TypeMainland  0.071918   0.242697   0.296   0.7674    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5218 on 153 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.03867,    Adjusted R-squared:  0.01982 
    ## F-statistic: 2.052 on 3 and 153 DF,  p-value: 0.109
    ## 
    ## 
    ## Response EFNS :
    ## 
    ## Call:
    ## lm(formula = EFNS ~ Latitude + width + Type, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6647 -1.2172 -0.1322  0.8726  5.0147 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   2.95035    0.81146   3.636 0.000378 ***
    ## Latitude      0.02717    0.01852   1.467 0.144449    
    ## width        -0.06075    0.03454  -1.759 0.080557 .  
    ## TypeMainland -0.60338    0.74765  -0.807 0.420896    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.608 on 153 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.04129,    Adjusted R-squared:  0.0225 
    ## F-statistic: 2.197 on 3 and 153 DF,  p-value: 0.09073

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##          Df test stat approx F num Df den Df   Pr(>F)   
    ## Latitude  1  0.082698   6.8517      2    152 0.001416 **
    ## width     1  0.024678   1.9230      2    152 0.149714   
    ## Type      1  0.009438   0.7241      2    152 0.486428   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Response Area_Leafbase :
    ## 
    ## Call:
    ## lm(formula = Area_Leafbase ~ Latitude + width + Type, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8743 -0.9918  0.2593  0.8616  4.0138 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   5.05809    1.89242   2.673  0.01112 * 
    ## Latitude     -0.12980    0.03926  -3.306  0.00211 **
    ## width         0.04621    0.06898   0.670  0.50711   
    ## TypeMainland  1.67779    1.81944   0.922  0.36243   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.777 on 37 degrees of freedom
    ##   (123 observations deleted due to missingness)
    ## Multiple R-squared:  0.239,  Adjusted R-squared:  0.1773 
    ## F-statistic: 3.873 on 3 and 37 DF,  p-value: 0.01665
    ## 
    ## 
    ## Response Area_Petiole :
    ## 
    ## Call:
    ## lm(formula = Area_Petiole ~ Latitude + width + Type, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3923 -0.8582 -0.0857  0.5904  3.7341 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   4.83484    1.39476   3.466  0.00135 **
    ## Latitude     -0.09848    0.02894  -3.403  0.00161 **
    ## width         0.02034    0.05084   0.400  0.69133   
    ## TypeMainland  0.02490    1.34097   0.019  0.98529   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.31 on 37 degrees of freedom
    ##   (123 observations deleted due to missingness)
    ## Multiple R-squared:  0.2389, Adjusted R-squared:  0.1772 
    ## F-statistic: 3.871 on 3 and 37 DF,  p-value: 0.01669

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##          Df test stat approx F num Df den Df   Pr(>F)   
    ## Latitude  1  0.262340   6.4015      2     36 0.004182 **
    ## width     1  0.012300   0.2242      2     36 0.800299   
    ## Type      1  0.043378   0.8162      2     36 0.450115   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Call:
    ## lm(formula = Vol_Leafbase ~ Area_Leafbase, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2198 -0.3190 -0.1468  0.4092  1.5613 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -0.09201    0.32330  -0.285 0.778487    
    ## Area_Leafbase  0.28008    0.06598   4.245 0.000306 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6604 on 23 degrees of freedom
    ##   (139 observations deleted due to missingness)
    ## Multiple R-squared:  0.4393, Adjusted R-squared:  0.4149 
    ## F-statistic: 18.02 on 1 and 23 DF,  p-value: 0.0003058

    ## 
    ## Call:
    ## lm(formula = Vol_Petiole ~ Area_Petiole, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6383 -0.2824 -0.1430  0.4352  0.7321 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.009992   0.266461  -0.037 0.970426    
    ## Area_Petiole  0.288871   0.074575   3.874 0.000821 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4251 on 22 degrees of freedom
    ##   (140 observations deleted due to missingness)
    ## Multiple R-squared:  0.4055, Adjusted R-squared:  0.3785 
    ## F-statistic:    15 on 1 and 22 DF,  p-value: 0.0008205

    ## 
    ## Call:
    ## lm(formula = pherb ~ Latitude + width + average_abundance + Type, 
    ##     data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.61561 -0.15879  0.02668  0.18434  0.41062 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        0.260988   0.119894   2.177 0.031037 *  
    ## Latitude           0.008994   0.002862   3.143 0.002010 ** 
    ## width             -0.006493   0.005021  -1.293 0.197930    
    ## average_abundance -0.003603   0.002345  -1.536 0.126563    
    ## TypeMainland       0.368920   0.109140   3.380 0.000921 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2331 on 152 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.1943, Adjusted R-squared:  0.1731 
    ## F-statistic: 9.166 on 4 and 152 DF,  p-value: 1.164e-06

    ## 
    ## Call:
    ## lm(formula = average_abundance ~ Latitude + width + Type, data = data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.828 -4.025 -2.128  1.232 51.574 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   9.81496    4.05615   2.420   0.0167 *  
    ## Latitude     -0.42130    0.09258  -4.551 1.08e-05 ***
    ## width        -0.15280    0.17264  -0.885   0.3775    
    ## TypeMainland  5.36891    3.73719   1.437   0.1529    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.035 on 153 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.1208, Adjusted R-squared:  0.1036 
    ## F-statistic: 7.009 on 3 and 153 DF,  p-value: 0.0001898

    ## Response seedweight :
    ## 
    ## Call:
    ## lm(formula = seedweight ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.051548 -0.029625 -0.000222  0.009889  0.198798 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.1404646  0.0209103   6.717 2.42e-08 ***
    ## Latitude    -0.0009393  0.0008607  -1.091    0.281    
    ## width        0.0018359  0.0013171   1.394    0.170    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04081 on 46 degrees of freedom
    ##   (115 observations deleted due to missingness)
    ## Multiple R-squared:  0.0696, Adjusted R-squared:  0.02915 
    ## F-statistic: 1.721 on 2 and 46 DF,  p-value: 0.1903
    ## 
    ## 
    ## Response elaiosome.weight :
    ## 
    ## Call:
    ## lm(formula = elaiosome.weight ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.010144 -0.005369 -0.002233  0.001132  0.039879 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  1.065e-02  5.130e-03   2.075   0.0436 *
    ## Latitude    -3.329e-06  2.112e-04  -0.016   0.9875  
    ## width       -5.300e-05  3.232e-04  -0.164   0.8704  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01001 on 46 degrees of freedom
    ##   (115 observations deleted due to missingness)
    ## Multiple R-squared:  0.0005844,  Adjusted R-squared:  -0.04287 
    ## F-statistic: 0.01345 on 2 and 46 DF,  p-value: 0.9866

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##          Df test stat approx F num Df den Df Pr(>F)
    ## Latitude  1  0.025889  0.59799      2     45 0.5542
    ## width     1  0.043940  1.03409      2     45 0.3638

    ## 
    ## Call:
    ## lm(formula = ratio ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.069348 -0.035873 -0.021111  0.005259  0.282975 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.0759202  0.0364100   2.085   0.0426 *
    ## Latitude     0.0003347  0.0014987   0.223   0.8242  
    ## width       -0.0010309  0.0022934  -0.450   0.6552  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07106 on 46 degrees of freedom
    ##   (115 observations deleted due to missingness)
    ## Multiple R-squared:  0.005891,   Adjusted R-squared:  -0.03733 
    ## F-statistic: 0.1363 on 2 and 46 DF,  p-value: 0.8729

    ## 
    ## Call:
    ## lm(formula = fruitno ~ Latitude + width + branchno + pherb + 
    ##     Type, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -57.436 -20.313  -3.798  12.851 218.886 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   34.2044    18.1172   1.888 0.061017 .  
    ## Latitude      -1.6655     0.4321  -3.854 0.000174 ***
    ## width          3.2556     0.8317   3.914 0.000139 ***
    ## branchno       2.4383     0.5698   4.279 3.38e-05 ***
    ## pherb          0.2862    12.3003   0.023 0.981466    
    ## TypeMainland -24.6424    17.0326  -1.447 0.150103    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 35.41 on 146 degrees of freedom
    ##   (12 observations deleted due to missingness)
    ## Multiple R-squared:  0.3584, Adjusted R-squared:  0.3364 
    ## F-statistic: 16.31 on 5 and 146 DF,  p-value: 9.145e-13

\##Visualizing the data

\#Extrafloral nectaries

Data visualization for extrafloral nectary traits

``` r
base_size <- 10

#Extracting coefficients and p-values from MLMs
intercept_efnlb <- coef(mlm1)["(Intercept)", "EFNLB"]
slope_latitude_efnlb <- coef(mlm1)["Latitude", "EFNLB"]
p_value_latitude_efnlb <- summary(mlm1)$`Response EFNLB`$coefficients["Latitude", "Pr(>|t|)"] 

data$Type <- factor(data$Type, levels = c("Island", "Mainland"))

#Leafbase EFNs
p_efn_lb_1 <- ggplot(data = data, aes(x = Latitude, y = EFNLB, color = Type)) +  
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 5) +
  xlab("Latitude") +
  ylab("Leaf base EFNs (no./leaf)") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  #scale_y_continuous(limits = c(min(morph$EFNLB), max(morph$EFNLB))) +
  scale_color_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.position = "none",  # Remove the legend
        plot.margin = margin(10, 10, 10, 10)) +
  geom_abline(intercept = intercept_efnlb, slope = slope_latitude_efnlb, color = "slategrey", size = 2) +
  annotate("text", x = 19, y = 1.8, label = "*", size = 12, color = "black")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
p_inset <- ggplot(data = data, aes(x = Type, y = EFNLB, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  scale_x_discrete(labels = c("Island" = "I", "Mainland" = "M")) +  
  theme_classic() +
  theme(
    legend.position = "none",            
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 12), 
    axis.ticks.x = element_line(),       
    axis.text.y = element_text(size = 12)
  )

combined_plot_efnlb <- ggdraw() +
  draw_plot(p_efn_lb_1) +
  draw_plot(p_inset, x = 0.65, y = 0.65, width = 0.35, height = 0.35)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#ggsave(filename = "efnlb.png", plot = combined_plot_efnlb, width = 10, height = 7, dpi = 300)

#Stem EFNs - non-significant


p_efn_s_1 <- ggplot(data = data, aes(x = Latitude, y = EFNS, color = Type)) +  
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 5) +
  xlab("Latitude") +
  ylab("Petiole EFNs (no./leaf)") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  #scale_y_continuous(limits = c(min(morph$EFNS), max(morph$EFNS))) +
  scale_color_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) 


p_inset <- ggplot(data = data, aes(x = Type, y = EFNS, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  scale_x_discrete(labels = c("Island" = "I", "Mainland" = "M")) +  
  theme_classic() +
  theme(
    legend.position = "none",            
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 12), 
    axis.ticks.x = element_line(),       
    axis.text.y = element_text(size = 12)
  )

combined_plot_efns <- ggdraw() +
  draw_plot(p_efn_s_1) +
  draw_plot(p_inset, x = 0.65, y = 0.65, width = 0.35, height = 0.35)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#ggsave(filename = "efns.png", plot = combined_plot_efns, width = 10, height = 7, dpi = 300)



#EFN areas


#Leaf base EFNs

intercept_aefnlb <- coef(mlm2)["(Intercept)", "Area_Leafbase"]
slope_latitude_aefnlb <- coef(mlm2)["Latitude", "Area_Leafbase"]

p_area_lb <- ggplot(data = data, aes(x = Latitude, y = Area_Leafbase, color = Type)) +
  geom_point(position = position_jitter(width = 0.3), alpha = 0.6, size = 5) +  
  xlab("Latitude") +
  ylab("Leaf base EFN area (mm2)") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  scale_color_manual(values = c(
    "Mainland" = "olivedrab",  # Shade of green
    "Island" = "#40e0d0"     # Light turquoise
  )) +
  labs(color = "Type") +  # Update legend title
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) + 
    geom_abline(intercept = intercept_aefnlb, slope = slope_latitude_aefnlb, color = "slategrey", size = 2) +
  annotate("text", x = 19, y = 4, label = "*", size = 12, color = "black")

p_inset <- ggplot(data = data, aes(x = Type, y = Area_Leafbase, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  scale_x_discrete(labels = c("Island" = "I", "Mainland" = "M")) +  
  theme_classic() +
  theme(
    legend.position = "none",            
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 12), 
    axis.ticks.x = element_line(),       
    axis.text.y = element_text(size = 12)
  )

combined_plot_arealb <- ggdraw() +
  draw_plot(p_area_lb) +
  draw_plot(p_inset, x = 0.65, y = 0.65, width = 0.35, height = 0.35)
```

    ## Warning: Removed 103 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 102 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
#ggsave(filename = "arealb.png", plot = combined_plot_arealb, width = 10, height = 7, dpi = 300)

#Petiole EFNs

intercept_aefns <- coef(mlm2)["(Intercept)", "Area_Petiole"]
slope_latitude_aefns <- coef(mlm2)["Latitude", "Area_Petiole"]

p_area_s <- ggplot(data = data, aes(x = Latitude, y = Area_Petiole, color = Type)) +
  geom_point(position = position_jitter(width = 0.3), alpha = 0.6, size = 5) +  
  xlab("Latitude") +
  ylab("Petiole EFN area (mm2)") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  scale_color_manual(values = c(
    "Mainland" = "olivedrab",  # Shade of green
    "Island" = "#40e0d0"     # Light turquoise
  )) +
  labs(color = "Type") +  # Update legend title
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) + 
    geom_abline(intercept = intercept_aefns, slope = slope_latitude_aefns, color = "slategrey", size = 2) +
  annotate("text", x = 19, y = 4, label = "*", size = 12, color = "black")

p_inset <- ggplot(data = data, aes(x = Type, y = Area_Petiole, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  scale_x_discrete(labels = c("Island" = "I", "Mainland" = "M")) +  
  theme_classic() +
  theme(
    legend.position = "none",            
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 12), 
    axis.ticks.x = element_line(),       
    axis.text.y = element_text(size = 12)
  )

combined_plot_areas <- ggdraw() +
  draw_plot(p_area_s) +
  draw_plot(p_inset, x = 0.65, y = 0.65, width = 0.35, height = 0.35)
```

    ## Warning: Removed 112 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 111 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
#ggsave(filename = "areas.png", plot = combined_plot_areas, width = 10, height = 7, dpi = 300)

#EFN volumes

#Leaf base EFNs

p_volume_lb <- ggplot(data = data, aes(x = Latitude, y = Vol_Leafbase, color = Type)) +
  geom_point(position = position_jitter(width = 0.3), alpha = 0.6, size = 5) +  
  geom_smooth(method = "lm", se = TRUE, color = "grey") +
  xlab("Latitude") +
  ylab("Leaf base EFN volume") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  scale_color_manual(values = c(
    "Mainland" = "olivedrab",  # Shade of green
    "Island" = "#40e0d0"     # Light turquoise
  )) +
  labs(color = "Type") +  # Update legend title
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.3),
        legend.title = element_text(size = base_size * 1.5),
        plot.margin = margin(10, 10, 10, 10))

p_inset <- ggplot(data = data, aes(x = Type, y = Vol_Leafbase, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c( "Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = base_size * 0.7),
        axis.text.y = element_text(size = base_size * 0.7))

combined_plot_vollb <- ggdraw() +
  draw_plot(p_volume_lb) +
  draw_plot(p_inset, x = 0.75, y = 0.7, width = 0.25, height = 0.25)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 140 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 140 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 139 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
#ggsave(filename = "vollb.png", plot = combined_plot_vollb, width = 10, height = 7, dpi = 300)

#Petiolar EFNs

p_volume_s <- ggplot(data = data, aes(x = Latitude, y = Vol_Petiole, color = Type)) +
  geom_point(position = position_jitter(width = 0.3), alpha = 0.6, size = 5) +  
  geom_smooth(method = "lm", se = TRUE, color = "grey") +
  xlab("Latitude") +
  ylab("Petiole EFN volume") +
  scale_x_continuous(limits = c(min(morph$Latitude), 40)) +
  scale_color_manual(values = c(
    "Mainland" = "olivedrab",  # Shade of green
    "Island" = "#40e0d0"     # Light turquoise
  )) +
  labs(color = "Type") +  # Update legend title
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.3),
        legend.title = element_text(size = base_size * 1.5),
        plot.margin = margin(10, 10, 10, 10))

p_inset <- ggplot(data = data, aes(x = Type, y = Vol_Petiole, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c( "Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_void() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = base_size * 0.7),
        axis.text.y = element_text(size = base_size * 0.7))

combined_plot_vols <- ggdraw() +
  draw_plot(p_volume_s) +
  draw_plot(p_inset, x = 0.75, y = 0.7, width = 0.25, height = 0.25)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 140 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 140 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 139 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
#ggsave(filename = "volumes.png", plot = combined_plot_vols, width = 10, height = 7, dpi = 300)
```

``` r
#Herbivory

intercept_herb <- coef(lm8)["(Intercept)"]
slope_latitude_herb <- coef(lm8)["Latitude"]

p_herb1 <- ggplot(data = data, aes(x = Latitude, y = pherb, color = Type)) +
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 5) +
  xlab("Latitude") +
  ylab("Prop. of leaves with herbivory") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  scale_color_manual(values = c( "Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) + 
    geom_abline(intercept = intercept_herb, slope = slope_latitude_herb, color = "slategrey", size = 2) +
  annotate("text", x = 19, y = 0.5, label = "*", size = 12, color = "black")

p_inset <- ggplot(data = data, aes(x = Type, y = pherb, fill = Type)) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  scale_x_discrete(labels = c("Island" = "I", "Mainland" = "M")) +  
  theme_classic() +
  theme(
    legend.position = "none",            
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 12), 
    axis.ticks.x = element_line(),       
    axis.text.y = element_text(size = 12)
  ) +
  annotate("text", x = 1.5, y = 0.8, label = "*", size = 8, color = "black")

#Combine the plots using cowplot
combined_plot_herb <- ggdraw() +
  draw_plot(p_herb1) +
  draw_plot(p_inset, x = 0.65, y = 0.65, width = 0.35, height = 0.35)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#ggsave(filename = "herb.png", plot = combined_plot_herb, width = 12, height = 7, dpi = 300)


#ant visitation rates

intercept_ant <- coef(lm9)["(Intercept)"]
slope_latitude_ant <- coef(lm9)["Latitude"]


p_ants1 <- ggplot(data = data, aes(x = Latitude, y = average_abundance, color = Type)) +
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 5) +
  xlab("Latitude") +
  ylab("Average ant visits (no./leaf)") +
  scale_x_continuous(limits = c(min(morph$Latitude), 43)) +
  scale_color_manual(values = c( "Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = base_size * 1.7),
        axis.title = element_text(size = base_size * 1.4),
        axis.text = element_text(size = base_size * 1.3),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) + 
    geom_abline(intercept = intercept_ant, slope = slope_latitude_ant, color = "slategrey", size = 2) +
  annotate("text", x = 19, y = 9, label = "*", size = 12, color = "black")

p_inset <- ggplot(data = data, aes(x = Type, y = average_abundance, fill = Type)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  scale_x_discrete(labels = c("Island" = "I", "Mainland" = "M")) +  
  theme_classic() +
  theme(
    legend.position = "none",            
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 12), 
    axis.ticks.x = element_line(),       
    axis.text.y = element_text(size = 12)
  )

#Combine the plots using cowplot
combined_plot_ants <- ggdraw() +
  draw_plot(p_ants1) +
  draw_plot(p_inset, x = 0.65, y = 0.65, width = 0.35, height = 0.35)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#ggsave(filename = "combined_plot_ants.png", plot = combined_plot_ants, width = 12, height = 7, dpi = 300)
```

``` r
#Figure 1

#Create a legend
legend_plot <- ggplot(data = data.frame(Type = c("Island", "Mainland"), x = 1:2, y = 1:2), 
                      aes(x, y, color = Type)) +
  geom_point(size = 5) + 
  scale_color_manual(values = c("Island" = "turquoise", "Mainland" = "olivedrab")) +
  theme_void() + 
  guides(color = guide_legend(override.aes = list(shape = 16)))  

common_legend <- get_legend(legend_plot)
```

    ## Warning in get_plot_component(plot, "guide-box"): Multiple components found;
    ## returning the first one. To return all, use `return_all = TRUE`.

``` r
#Add it to the map
indmap <- plot_grid(indmap, common_legend, nrow = 2, rel_heights = c(1, 0.2))

ggsave("Indiamap.png", indmap, width = 8, height = 6)


###Figure 2


#Combine the second row with 4 plots
second_row <- (combined_plot_efnlb + combined_plot_efns) +
  plot_layout(ncol = 2)

#Combine the third row with 2 plots
third_row <- (combined_plot_arealb + combined_plot_areas) +
  plot_layout(ncol = 2)

fourth_row <- (combined_plot_herb + combined_plot_ants) +
  plot_layout(ncol = 2)

#Combine the three rows into one layout
combined_plot <-  second_row / third_row / fourth_row

#Annotate with labels for subplots (a-i)
final_plot <- combined_plot +
  plot_annotation(tag_levels = 'a')  # Labels from a onwards


# Display the final plot
print(final_plot)
```

![](README_files/figure-gfm/Making%20Fig.%201%20and%202-1.png)<!-- -->

``` r
# Save the final plot to a file
ggsave("figure2.png", final_plot, width = 11.5, height = 12)
```

\#How does seed size and elaiosome/seed mass vary across latitude?

``` r
#How does seed size and elaiosome weight vary across latitude?

#Seeds were placed on scale and total weight was divided by number of seeds
#For each source plant

#Seed weight
p_seedwt <- ggplot(data = data, aes(x = Latitude, y = seedweight)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "saddlebrown") +
  xlab("Latitude") +
  ylab("Seed weight") +
  ggtitle("Variation in seed weight across latitude") 

#Seed to elaiosome weight ratio
p_seedrat <- ggplot(data = data, aes(x = Latitude, y = ratio)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "saddlebrown") +
  xlab("Latitude") +
  ylab("Elaiosome weight to seed weight ratio") +
  ggtitle("Variation in e/s ratio across latitude") 
```

\##Visualizing plant traits

``` r
#How do plant traits vary across latitude?


#Stem width
p_width <- ggplot(data = data, aes(x = Latitude, y = width)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  stat_summary(fun.data = "median_hilow", geom = "errorbar", width = 0.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlab("Latitude") +
  ylab("Stem width at 10 cm") +
  ggtitle("Variation in stem width with latitude") 


#Branches
p_branch <- ggplot(data = data, aes(x = Latitude, y = as.numeric(branchno))) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlab("Latitude") +
  ylab("Number of branches") +
  ggtitle("Variation in no. of branches with latitude") 

#Heights
p_height <- ggplot(data = data, aes(x = Latitude, y = as.numeric(Height))) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlab("Latitude") +
  ylab("Plant height") +
  ggtitle("Variation in plant height with latitude") 

#leaf size
p_leaf <- ggplot(data = data, aes(x = Latitude, y = areaavg)) +  
  geom_point( alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "olivedrab") +
  xlab("Latitude") +
  ylab("Proxy of leaf area") +
  ggtitle("Variation in leaf size across latitude") +
  scale_y_continuous(limits = c(min(morph$areaavg), max(morph$areaavg)))


#fruitsize
p_fruit <- ggplot(data = data, aes(x = Latitude, y = Fruit_size_avg)) +  
  geom_point( alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "olivedrab") +
  xlab("Latitude") +
  ylab("Radius of fruits") +
  ggtitle("Variation in fruit size across latitude") +
  scale_y_continuous(limits = c(min(morph$Fruit_size_avg), max(morph$Fruit_size_avg)))


#number of fruits
p_fit <- ggplot(data = data, aes(x = Latitude, y = fruitno)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "slateblue") +
  xlab("Latitude") +
  ylab("Number of fruits") 
```

\##Ant community analysis

Here, we will generate some plots to understand how scan number and and
number of days scanned affect ant abundance data trends. Inferences from
these plots will be used to control for sampling effort in the main data
analysis of this study.

``` r
ants <- read.csv("Ant_abundance_all_natint_20May24.csv") #read in the abundance dataset
ants$Site <- as.factor(ants$Site) 

#Making sure formats are correct
ants$number1 <- as.numeric(as.character(ants$number1))
```

    ## Warning: NAs introduced by coercion

``` r
ants$number2 <- as.numeric(as.character(ants$number2))
ants$number3 <- as.numeric(as.character(ants$number3))
ants$number4 <- as.numeric(as.character(ants$number4))

#Removing Kenya points
ants <- ants %>%
  filter(!Site %in% c("Rumuruti1", "Rumuruti2"))


#Convering to long form
ants_long_data <- ants%>%
  pivot_longer(
    cols = starts_with("species") | starts_with("number") | starts_with("feeding"),
    names_to = c(".value", "group"),
    names_pattern = "(species|number|feeding)(\\d)"
  ) %>% drop_na(species)

#Calculating cumulative species counts
cumulative_species_count <- ants_long_data %>%
  group_by(Plant.ID, scan_no) %>%
distinct(species, .keep_all = TRUE) %>%
  ungroup() %>%
arrange(Plant.ID, scan_no) %>%
  group_by(Plant.ID) %>%
mutate(cumulative_species = cumsum(!duplicated(species))) %>%
  ungroup()

stabilization_data <- cumulative_species_count %>%
  group_by(Site, scan_no) %>%  # Group by both Site and scan_no
  summarise(
    mean_species = mean(cumulative_species, na.rm = TRUE),
    sd_species = sd(cumulative_species, na.rm = TRUE),
    .groups = "drop"
  )


p <- ggplot() +
  # Site-level mean cumulative species line
  geom_line(
    data = stabilization_data, 
    aes(x = scan_no, y = mean_species, color = Site), 
    size = 1
  ) +
  # Ribbon for standard deviation
  geom_ribbon(
    data = stabilization_data, 
    aes(x = scan_no, ymin = mean_species - sd_species, ymax = mean_species + sd_species, fill = Site), 
    alpha = 0.2
  ) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "red") +  # Highlight scan 6
  facet_wrap(~ Site, scales = "free_y") +  # Facet by site
  theme_minimal() +
  labs(
    title = "Average cumulative number of ant species by number of scans",
    x = "Number of Scans",
    y = "Mean Cumulative Number of Unique Species"
  ) +
  theme(legend.position = "none")

#6 scans

#Choosing how many days to include

species_per_plant <- ants_long_data %>%
  group_by(Site, Plant.ID, scan_day) %>%
  summarise(species_list = list(unique(species)), .groups = 'drop') %>%
  arrange(Site, Plant.ID, scan_day) %>%
  group_by(Site, Plant.ID) %>%
  mutate(
    cumulative_species_list = lapply(seq_along(species_list), function(x) unique(unlist(species_list[1:x]))),
    cumulative_species_count = sapply(cumulative_species_list, length)
  ) %>%
  ungroup()

# Summarize cumulative species counts across plants for each site
summary_species_per_day <- species_per_plant %>%
  group_by(Site, scan_day) %>%
  summarise(
    mean_cumulative_species = mean(cumulative_species_count, na.rm = TRUE),
    sd_cumulative_species = sd(cumulative_species_count, na.rm = TRUE),
    .groups = 'drop'
  )

# Plot individual plant curves and site-level summary
p_days_plants <- ggplot() +
  # Site-level mean cumulative species line
  geom_line(data = summary_species_per_day, 
            aes(x = scan_day, y = mean_cumulative_species, color = Site), 
            size = 1) +
  # Ribbon for standard deviation
  geom_ribbon(data = summary_species_per_day, 
              aes(x = scan_day, ymin = mean_cumulative_species - sd_cumulative_species, 
                  ymax = mean_cumulative_species + sd_cumulative_species, fill = Site), 
              alpha = 0.2) +
  facet_wrap(~ Site, scales = "free_x") +  # Facet by site
  theme_minimal() +
  labs(
    title = "Average cumulative ant species across scan days",
    x = "Scan Day",
    y = "Cumulative unique species count"
  ) +
  theme(legend.position = "none")


scans_days_plot <-  p / p_days_plants

#Annotate with labels for subplots (a-i)
scans_days_plot <- scans_days_plot +
  plot_annotation(tag_levels = 'A')

ggsave("figureS2.png", scans_days_plot, width = 7, height = 10)
```

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

``` r
#Generate a list of ant species seen at each site
ant_species_summary <- ants_long_data %>%
  group_by(Site) %>%
  summarise(Ant_Species = paste(unique(species), collapse = ", ")) %>%
  arrange(Site)

#View the summarized data
print(ant_species_summary)
```

    ## # A tibble: 11 × 2
    ##    Site        Ant_Species                                                 
    ##    <fct>       <chr>                                                       
    ##  1 Aligarh     CaC, Crem, Tap                                              
    ##  2 Bengaluru   Par, CaC, Tap, CaI, Myrm, CaP, Mon, Sol, Crem, Ano, Pol     
    ##  3 Bhopal      CaP, Tap, CaC, Par                                          
    ##  4 Gwalior     CaC                                                         
    ##  5 Hampi       Tap, CaS, Mon, Par, ParB, Pol, Sol, Tec                     
    ##  6 Hyderabad   Par, Crem, Tap, Lep, CaS, Mon, Sol, Cas1, CaP               
    ##  7 Kanyakumari Tap, CaS, Par-, Crem, Lep, CaP, Par                         
    ##  8 Koradi      Lep, CaC, CaI, Par, Tap, Mon                                
    ##  9 Portblair   Sol, Crem, Tap, CaC, Par, Lep                               
    ## 10 Ramnagar    Tec, CaC, CaP, CaI, Pol, ParB, Lep, Tap, Mon, Crem, Sol, Par
    ## 11 Srirangam   Tap, CaR, Mer, Cas1, Par, Eco, Mon, CaC

``` r
unique_ant_species <- ants_long_data %>%
  dplyr::select(species) %>%
  distinct() %>%
  pull(species)

# Count the total number of unique species
total_ant_species <- length(unique_ant_species)


#Save the output to a CSV file
#write.csv(ant_species_summary, "ant_species_summary.csv", row.names = FALSE)
```

Using the vegan package to investigate how ant communities vary across
latitude

    ## Warning: NAs introduced by coercion

![](README_files/figure-gfm/community%20characteristics-1.png)<!-- -->![](README_files/figure-gfm/community%20characteristics-2.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Shannon ~ Latitude, data = diversity_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.15062 -0.13808 -0.06947  0.30550  0.69914 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  1.78458    0.43088   4.142  0.00252 **
    ## Latitude    -0.02417    0.02180  -1.109  0.29633   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5074 on 9 degrees of freedom
    ## Multiple R-squared:  0.1202, Adjusted R-squared:  0.0224 
    ## F-statistic: 1.229 on 1 and 9 DF,  p-value: 0.2963

    ## 
    ## Call:
    ## lm(formula = Richness ~ Latitude, data = diversity_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7232 -2.3188 -0.3095  1.1113  6.6419 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   8.8126     2.8454   3.097   0.0128 *
    ## Latitude     -0.1178     0.1440  -0.818   0.4344  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.35 on 9 degrees of freedom
    ## Multiple R-squared:  0.06922,    Adjusted R-squared:  -0.0342 
    ## F-statistic: 0.6693 on 1 and 9 DF,  p-value: 0.4344

    ## 
    ## Call:
    ## lm(formula = Shannon ~ Latitude, data = diversity_data1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.90169 -0.18561  0.04651  0.26177  0.54475 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.06693    0.40970   5.045 0.000995 ***
    ## Latitude    -0.04443    0.02210  -2.010 0.079263 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4486 on 8 degrees of freedom
    ## Multiple R-squared:  0.3356, Adjusted R-squared:  0.2525 
    ## F-statistic: 4.041 on 1 and 8 DF,  p-value: 0.07926

    ## 
    ## Call:
    ## lm(formula = Richness ~ Latitude, data = diversity_data1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8869 -1.6406  0.0166  1.2216  3.5585 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  11.4949     1.9706   5.833  0.00039 ***
    ## Latitude     -0.3102     0.1063  -2.918  0.01934 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.158 on 8 degrees of freedom
    ## Multiple R-squared:  0.5156, Adjusted R-squared:  0.4551 
    ## F-statistic: 8.516 on 1 and 8 DF,  p-value: 0.01934

    ## 
    ## Call:
    ## lm(formula = Shannon ~ Latitude, data = diversity_data2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1500 -0.1535 -0.0364  0.3453  0.7030 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  1.81199    0.49528   3.658  0.00642 **
    ## Latitude    -0.02524    0.02428  -1.039  0.32897   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5375 on 8 degrees of freedom
    ## Multiple R-squared:  0.119,  Adjusted R-squared:  0.008868 
    ## F-statistic: 1.081 on 1 and 8 DF,  p-value: 0.329

    ## 
    ## Call:
    ## lm(formula = Richness ~ Latitude, data = diversity_data2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7022 -2.1233 -0.2821  0.8384  6.7793 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   9.7761     3.1551   3.099   0.0147 *
    ## Latitude     -0.1553     0.1547  -1.004   0.3447  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.424 on 8 degrees of freedom
    ## Multiple R-squared:  0.112,  Adjusted R-squared:  0.0009466 
    ## F-statistic: 1.009 on 1 and 8 DF,  p-value: 0.3447

    ## 
    ## Call:
    ## lm(formula = Shannon ~ Latitude, data = diversity_data3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8983 -0.2359  0.1596  0.2687  0.5382 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  2.11560    0.47178   4.484  0.00285 **
    ## Latitude    -0.04641    0.02463  -1.884  0.10157   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4771 on 7 degrees of freedom
    ## Multiple R-squared:  0.3364, Adjusted R-squared:  0.2417 
    ## F-statistic: 3.549 on 1 and 7 DF,  p-value: 0.1016

    ## 
    ## Call:
    ## lm(formula = Richness ~ Latitude, data = diversity_data3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7978 -0.7991  0.3234  0.9353  2.9932 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.7037     1.9447   6.532 0.000324 ***
    ## Latitude     -0.3595     0.1015  -3.540 0.009469 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.967 on 7 degrees of freedom
    ## Multiple R-squared:  0.6416, Adjusted R-squared:  0.5904 
    ## F-statistic: 12.53 on 1 and 7 DF,  p-value: 0.009469

![](README_files/figure-gfm/community%20characteristics-3.png)<!-- -->![](README_files/figure-gfm/community%20characteristics-4.png)<!-- -->

    ##  [1] 39.256574 56.382812 18.645370 41.210283 50.440556 50.412604 19.916609
    ##  [8]  6.416667  1.000000  5.026667 65.561667

    ## Wisconsin double standardization
    ## Run 0 stress 0.1444013 
    ## Run 1 stress 0.1499244 
    ## Run 2 stress 0.2397507 
    ## Run 3 stress 0.1499244 
    ## Run 4 stress 0.1398569 
    ## ... New best solution
    ## ... Procrustes: rmse 0.2003312  max resid 0.4118767 
    ## Run 5 stress 0.1338938 
    ## ... New best solution
    ## ... Procrustes: rmse 0.18658  max resid 0.3910242 
    ## Run 6 stress 0.1338938 
    ## ... New best solution
    ## ... Procrustes: rmse 2.687674e-05  max resid 5.2794e-05 
    ## ... Similar to previous best
    ## Run 7 stress 0.1372117 
    ## Run 8 stress 0.1398571 
    ## Run 9 stress 0.1372117 
    ## Run 10 stress 0.139857 
    ## Run 11 stress 0.1402498 
    ## Run 12 stress 0.1338938 
    ## ... Procrustes: rmse 3.406726e-05  max resid 7.859538e-05 
    ## ... Similar to previous best
    ## Run 13 stress 0.1394493 
    ## Run 14 stress 0.1488845 
    ## Run 15 stress 0.1338939 
    ## ... Procrustes: rmse 0.0002549255  max resid 0.0005936822 
    ## ... Similar to previous best
    ## Run 16 stress 0.1338939 
    ## ... Procrustes: rmse 0.0001746664  max resid 0.0004061532 
    ## ... Similar to previous best
    ## Run 17 stress 0.1424101 
    ## Run 18 stress 0.1398572 
    ## Run 19 stress 0.1453734 
    ## Run 20 stress 0.1338939 
    ## ... Procrustes: rmse 0.0001731091  max resid 0.0004001521 
    ## ... Similar to previous best
    ## *** Best solution repeated 5 times

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = bray_curtis ~ sp_arranged$Lat, data = sp_arranged, permutations = 999)
    ##          Df SumOfSqs      R2      F Pr(>F)  
    ## Model     1  0.53239 0.18741 2.0756  0.044 *
    ## Residual  9  2.30847 0.81259                
    ## Total    10  2.84086 1.00000                
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = bray_curtis1 ~ sp_arranged1$Lat, data = sp_arranged1, permutations = 999)
    ##          Df SumOfSqs      R2      F Pr(>F)  
    ## Model     1  0.67501 0.27145 2.9807   0.02 *
    ## Residual  8  1.81167 0.72855                
    ## Total     9  2.48669 1.00000                
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = bray_curtis2 ~ sp_arranged2$Lat, data = sp_arranged2, permutations = 999)
    ##          Df SumOfSqs      R2      F Pr(>F)  
    ## Model     1  0.60006 0.22603 2.3363  0.034 *
    ## Residual  8  2.05471 0.77397                
    ## Total     9  2.65476 1.00000                
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

\##Structural equation modelling

We implemented a structural equation model using the lavaan package to
further understand relationships between variables.

``` r
#Subsetting dataset to contain relevant variables

semdata <- data[, c("Plant.ID", "Latitude", "width", "EFNLB", "EFNS", "pherb", "average_abundance", "fruitno", "Area_Leafbase", "Area_Petiole", "Vol_Leafbase", "Vol_Petiole")]

semdata_standardized <- as.data.frame(scale(semdata[, -1]))  #Scaling all except Plant.ID
semdata_standardized$Plant.ID <- semdata$Plant.ID  #Add back the Plant.ID

# Inspect the standardized data
summary(semdata_standardized)
```

    ##     Latitude           width             EFNLB             EFNS         
    ##  Min.   :-1.4997   Min.   :-1.5763   Min.   :-2.170   Min.   :-1.44699  
    ##  1st Qu.:-0.8023   1st Qu.:-0.6734   1st Qu.:-1.127   1st Qu.:-0.82368  
    ##  Median :-0.2054   Median :-0.1424   Median : 0.137   Median :-0.06013  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.000   Mean   : 0.00000  
    ##  3rd Qu.: 1.0401   3rd Qu.: 0.5215   3rd Qu.: 0.769   3rd Qu.: 0.44890  
    ##  Max.   : 1.4780   Max.   : 4.1861   Max.   : 2.665   Max.   : 3.09425  
    ##  NA's   :1         NA's   :6                                            
    ##      pherb         average_abundance    fruitno        Area_Leafbase     
    ##  Min.   :-1.9218   Min.   :-0.6559   Min.   :-0.5151   Min.   :-2.20981  
    ##  1st Qu.:-0.7659   1st Qu.:-0.5356   1st Qu.:-0.5151   1st Qu.:-0.66717  
    ##  Median : 0.1973   Median :-0.3908   Median :-0.5151   Median :-0.07306  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000  
    ##  3rd Qu.: 0.8716   3rd Qu.: 0.1860   3rd Qu.: 0.1582   3rd Qu.: 0.59766  
    ##  Max.   : 1.1606   Max.   : 5.7583   Max.   : 6.2178   Max.   : 2.21229  
    ##                                      NA's   :1         NA's   :102       
    ##   Area_Petiole      Vol_Leafbase      Vol_Petiole        Plant.ID        
    ##  Min.   :-1.6804   Min.   :-1.2000   Min.   :-1.4766   Length:164        
    ##  1st Qu.:-0.6680   1st Qu.:-0.6499   1st Qu.:-0.8222   Class :character  
    ##  Median :-0.1546   Median :-0.4784   Median :-0.2107   Mode  :character  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000                     
    ##  3rd Qu.: 0.6297   3rd Qu.: 0.7288   3rd Qu.: 0.5929                     
    ##  Max.   : 3.1571   Max.   : 2.3398   Max.   : 2.3889                     
    ##  NA's   :111       NA's   :139       NA's   :139

``` r
#model specification
model12 <- '
  average_abundance ~ Latitude + EFNLB + EFNS
  pherb ~ Latitude + average_abundance + EFNLB + EFNS
  EFNLB ~ Latitude + width + pherb 
  EFNS ~ Latitude + width + pherb
  fruitno ~ pherb + average_abundance + Latitude + EFNLB + EFNS +  width 
 '

rev_fit <- sem(model12, data = semdata_standardized, std.lv = TRUE)

#Evaluate the model
revfit_summary <- summary(rev_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#pherb and Area_Leafbase have -ve R2 values
#Suggests they are misspecified, the variables/models don't sufficiently 
#explain variation in these variables

#25 November 2024 - trying another model without EFN predicted by herbivory
#To remove circularity

model121 <- '
  average_abundance ~ Latitude + EFNLB + EFNS
  pherb ~ Latitude + average_abundance + EFNLB + EFNS
  EFNLB ~ Latitude + width  
  EFNS ~ Latitude + width 
  fruitno ~ pherb + average_abundance + Latitude + EFNLB + EFNS +  width 
 '
rev_fit1 <- sem(model121, data = semdata_standardized, std.lv = TRUE)

#Evaluate the model
revfit_summary1 <- summary(rev_fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Negative R-squared goes away when circular dependencies are removed
#Much poorer fit than model12

#Checking for collinearity between variables

#Subset predictors of an endogenous variable
predictors <- semdata_standardized[, c("Latitude", "width", "EFNLB", "EFNS", "pherb", "average_abundance", "fruitno")]

#Compute correlation matrix
cor_matrix <- cor(predictors, use = "complete.obs")

#Visualize
corrplot(cor_matrix, method = "circle")
```

![](README_files/figure-gfm/structural%20equation%20modelling-1.png)<!-- -->

``` r
#Nothing concerning

#Inspect VIF values

vif_ab <- lm(average_abundance ~ Latitude + EFNLB + EFNS, data = semdata_standardized)
vif_herb <- lm(pherb ~  Latitude + average_abundance + EFNLB + EFNS, data = semdata_standardized)
vif_lb <- lm( EFNLB ~ Latitude + width, data = semdata_standardized)
vif_s <- lm(EFNS ~ Latitude + width, data = semdata_standardized )
vif_fru <- lm(fruitno ~ pherb + average_abundance + Latitude + EFNLB + EFNS +  width, data = semdata_standardized)

#Compute VIF
vif(vif_ab)
```

    ## Latitude    EFNLB     EFNS 
    ## 1.084315 1.498019 1.486488

``` r
vif(vif_herb)
```

    ##          Latitude average_abundance             EFNLB              EFNS 
    ##          1.241201          1.179277          1.515174          1.578498

``` r
vif(vif_lb)
```

    ## Latitude    width 
    ## 1.017577 1.017577

``` r
vif(vif_s)
```

    ## Latitude    width 
    ## 1.017577 1.017577

``` r
vif(vif_fru)
```

    ##             pherb average_abundance          Latitude             EFNLB 
    ##          1.172385          1.207469          1.382929          1.518952 
    ##              EFNS             width 
    ##          1.598683          1.048953

``` r
#collinearity not an issue

#Adding correlated errors

model122 <- '
  average_abundance ~ Latitude + EFNLB + EFNS
  pherb ~ Latitude + average_abundance + EFNLB + EFNS
  EFNLB ~ Latitude + width  
  EFNS ~ Latitude + width 
  fruitno ~ pherb + average_abundance + Latitude + EFNLB + EFNS +  width 
  EFNS ~~ EFNLB
 '
rev_fit2 <- sem(model122, data = semdata_standardized, std.lv = TRUE)

#Evaluate the model
revfit_summary2 <- summary(rev_fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#This model meets specifications and has positive R-squared values

#Simplified model in which directly non-explainable links for fruit number are removed

model123 <- '
  average_abundance ~ Latitude + EFNLB + EFNS
  pherb ~ Latitude + average_abundance + EFNLB + EFNS
  EFNLB ~ Latitude + width  
  EFNS ~ Latitude + width 
  fruitno ~ pherb + average_abundance + Latitude + width 
  EFNS ~~ EFNLB
 '
rev_fit3 <- sem(model123, data = semdata_standardized, std.lv = TRUE)

#Evaluate the model
revfit_summary3 <- summary(rev_fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Good fit, best model so far but goodness of fit indices lower than model122

#Same model as above, but with indirect effects of EFN on fruits

model124 <- '
  # Direct effects
  average_abundance ~ c1*Latitude + c9*EFNLB + c2*EFNS
  pherb ~ c3*Latitude + c4*average_abundance + c10*EFNLB + c5*EFNS
  fruitno ~ c6*pherb + c7*average_abundance + c8*Latitude + width
  EFNLB ~ Latitude + width
  EFNS ~ Latitude + width
  EFNS ~~ EFNLB
  
  # Indirect effects of EFNs on fruit number
  ind_efnlb := c9 * c4 * c6  # EFNLB → average_abundance → pherb → fruitno
  ind_efns := c2 * c4 * c6   # EFNS → average_abundance → pherb → fruitno
'

rev_fit4 <- sem(model124, data = semdata_standardized, std.lv = TRUE)

#Evaluate the model
revfit_summary4 <- summary(rev_fit4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#model124 has good fit but no evidence of indirect effects

#Use direct AND indirect effects - what if there is some genetics?

model125 <- '
  # Direct effects
  average_abundance ~ c1*Latitude + c9*EFNLB + c2*EFNS
  pherb ~ c3*Latitude + c4*average_abundance + c10*EFNLB + c5*EFNS
  fruitno ~ c6*pherb + c7*average_abundance + c8*Latitude + EFNLB + EFNS + width
  EFNLB ~ Latitude + width
  EFNS ~ Latitude + width
  EFNS ~~ EFNLB
  
  # Indirect effects of EFNs on fruit number
  ind_efnlb := c9 * c4 * c6  # EFNLB → average_abundance → pherb → fruitno
  ind_efns := c2 * c4 * c6   # EFNS → average_abundance → pherb → fruitno
'

rev_fit5 <- sem(model125, data = semdata_standardized, std.lv = TRUE)

#Evaluate the model
revfit_summary5 <- summary(rev_fit5, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Best model

#Final model

###Make Figure 4
sem_apriori <- ggdraw() + draw_image("Slide2.PNG")
sem_sig <- ggdraw() + draw_image("Slide3.PNG")

sem_plot <- (sem_apriori / sem_sig)  + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 25))  

ggsave(filename = "figure4.png", plot = sem_plot, width = 12, height = 12, dpi = 300)
```
