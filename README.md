Castor_latitudinal_gradient
================
Pooja Nathan
2025-05-27

\##Latitudinal gradients in species interactions

In this study, we sampled populations of the castor plant, Ricinus
communis, across a large latitudinal range in the Indian subcontinent to
understand if, when the host species is kept constant, there is
variation in mutualistic traits and interactions across latitude as
posited by the biotic interactions hypothesis.

    ## Warning: package 'dplyr' was built under R version 4.4.3

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

    ## Warning: package 'terra' was built under R version 4.4.3

    ## terra 1.8.42

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

    ## Warning: package 'geodata' was built under R version 4.4.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:terra':
    ## 
    ##     intersect, union

    ## The following objects are masked from 'package:raster':
    ## 
    ##     intersect, union

    ## The following object is masked from 'package:cowplot':
    ## 
    ##     stamp

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Warning: package 'DHARMa' was built under R version 4.4.3

    ## This is DHARMa 0.4.7. For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')

    ## Warning: package 'glmmTMB' was built under R version 4.4.3

    ## Warning: package 'lmerTest' was built under R version 4.4.3

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## Warning: package 'MASS' was built under R version 4.4.2

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:terra':
    ## 
    ##     area

    ## The following objects are masked from 'package:raster':
    ## 
    ##     area, select

    ## The following object is masked from 'package:patchwork':
    ## 
    ##     area

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

\##Data cleaning

Now, let’s import and clean all the trait and interaction data

``` r
#Reading in plant traits dataset
morph <- read.csv("morph_natint_20May24.csv")

#rename columns as needed
morph <- morph %>%
  rename(
    Location         = Site,
    width            = Width.at.10.cm,
    fruitno          = Number.of.fruits,
    branchno         = Number.of.branches,
    EFNLB_raw        = `EFN...LB`,
    EFNS_raw         = `EFN...S`,
    EFNL_raw         = `EFN...L`,
    leaves_in_branch = Number.of.leaves.in.EFN.branch
  )

morph$Latitude <- as.numeric(morph$Latitude)
```

    ## Warning: NAs introduced by coercion

``` r
morph$Longitude <- as.numeric(morph$Longitude)
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

\##Statistical models

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

#correlations between branch number and width
model3 <- lm(width ~ branchno, data = data)
r_squared3 <- summary(model3)$r.squared
p_value3 <- summary(model3)$coefficients[2, 4]

width_branchno_cor <- ggplot(data=data, aes(x=branchno, y=width)) +
  geom_point() +
  xlab("Number of branches") +
  ylab("Stem width (plant age, cm)") +
  geom_smooth(method="lm") +
  theme_cowplot() +
  annotate("text", x=2, y=2, label=paste("R^2 =", round(r_squared3, 3), "\nP =", signif(p_value3, 3)), hjust=0)


ggsave(filename = "width_branchno.png", plot = width_branchno_cor, width = 7, height = 7, dpi = 300)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 15 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#Does width vary with latitude?
lm_width <- lm(width ~ Latitude, data = data)
r_squared4 <- summary(lm_width)$r.squared
p_value4 <- summary(lm_width)$coefficients[2, 4]

#marginally significant
width_lat_cor <- ggplot(data=data, aes(x=Latitude, y=width)) +
  geom_point() +
  xlab("Latitude") +
  ylab("Stem width (cm)") +
  geom_smooth(method="lm") +
  theme_cowplot() +
  annotate("text", x=2, y=2, label=paste("R^2 =", round(r_squared4, 3), "\nP =", signif(p_value4, 3)), hjust=0)

ggsave(filename = "width_lat.png", plot = width_lat_cor, width = 7, height = 7, dpi = 300)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 12 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#Number of plants at each site
data$Location <- as.factor(data$Location)

plantnos <- data %>%
  group_by(Location) %>%
  summarise(Number_of_Plants = n_distinct(Plant.ID))
```

Statistical modelling

We will now use linear models and multivariate linear models where
appropriate

``` r
#removing Kenya datapoints and island site
data <- data[!data$Location %in% c("Rumuruti1", "Rumuruti2", "Portblair"), ]

#Calculating a measure of urbanization for each site
#first calculate the centroid for each site
dat_no_latNAs <- data %>% 
  filter(!is.na(Longitude) & !is.na(Latitude))

pts <- st_as_sf(dat_no_latNAs,
                coords = c("Longitude", "Latitude"),
                crs    = 4326,               # WGS‑84
                remove = FALSE) 

pts_eqarea <- st_transform(pts, 8857) #India has more than one UTM zone

centroids_proj <- pts_eqarea %>% 
  group_by(Location) %>%                    
  summarise(geometry = st_centroid(st_union(geometry)),
            .groups  = "drop")

centroids_ll <- centroids_proj %>% 
  st_transform(4326) %>%                    # back to WGS‑84
  mutate(long_centroid = st_coordinates(.)[, 1],
         lat_centroid  = st_coordinates(.)[, 2]) %>% 
  st_drop_geometry() 


#add urbanization scores from software 
centroids_ll$urb <- c(3.11499,0.6754, 2.89652, -0.2998, -1.1211, -0.6686, -2.815, -0.5959, -1.16525, -0.02087)

#now merge with main dataset
data <- data %>% 
  left_join(centroids_ll, by = "Location") 

p_urb <- ggplot(centroids_ll, aes(x = lat_centroid, y = urb)) +
  geom_point(size = 4, color = "olivedrab") +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linewidth = 1.5) +
  xlab("Latitude") +
  ylab("Urbanization score") +
  theme_classic(base_size = 14)

ggsave(filename = "urb_lat.png", plot = p_urb, width = 5, height = 4, dpi = 300)
```

    ## `geom_smooth()` using formula = 'y ~ x'

``` r
lm_urb_lat <- lm(urb ~ lat_centroid, data = centroids_ll)
#No effect of latitude on urbanization

#Classifying as native, mainland and island sites
#data$Type <- ifelse(data$Location %in% c("Rumuruti1", "Rumuruti2"), "Native", 
                   #ifelse(data$Location == "Portblair", "Island", "Mainland"))

#write.csv(data, "data_20May.csv")
```

``` r
#data <- read.csv("data_20May.csv")

#Linear models

#Stem width at 10 cm used as a proxy for plant age

###EFN counts

#Leaf base EFN

#Using the standardized counts per leaf
lm_efnlb <- lm(EFNLB ~ Latitude + width, data = data)
sim_res <- simulateResiduals(fittedModel = lm_efnlb, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-1.png)<!-- -->

``` r
summary(lm_efnlb)
```

    ## 
    ## Call:
    ## lm(formula = EFNLB ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.04887 -0.51012  0.01997  0.40217  1.48607 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.974698   0.175976  11.221   <2e-16 ***
    ## Latitude    -0.011938   0.006033  -1.979   0.0497 *  
    ## width       -0.016829   0.011348  -1.483   0.1402    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5235 on 149 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.03472,    Adjusted R-squared:  0.02176 
    ## F-statistic: 2.679 on 2 and 149 DF,  p-value: 0.07191

``` r
#assumptions violated

#Using poisson family distribution on raw EFN counts
lm_efnlb1 <- glm(EFNLB_raw ~ Latitude + width +  offset(log(leaves_in_branch)),
                   data = data,
                   family = poisson)

summary(lm_efnlb1)
```

    ## 
    ## Call:
    ## glm(formula = EFNLB_raw ~ Latitude + width + offset(log(leaves_in_branch)), 
    ##     family = poisson, data = data)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.807699   0.117593   6.869 6.48e-12 ***
    ## Latitude    -0.013613   0.004396  -3.097  0.00196 ** 
    ## width       -0.015618   0.008340  -1.873  0.06111 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 133.38  on 151  degrees of freedom
    ## Residual deviance: 120.42  on 149  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 679.83
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
sim_res <- simulateResiduals(fittedModel = lm_efnlb1, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-2.png)<!-- -->

``` r
#assumptions violated

lm_efnlb2 <- glm.nb(EFNLB_raw ~ Latitude + width +  offset(log(leaves_in_branch)),
                   data = data)

summary(lm_efnlb2)
```

    ## 
    ## Call:
    ## glm.nb(formula = EFNLB_raw ~ Latitude + width + offset(log(leaves_in_branch)), 
    ##     data = data, init.theta = 144.0030351, link = log)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.795263   0.121455   6.548 5.84e-11 ***
    ## Latitude    -0.013235   0.004520  -2.928  0.00341 ** 
    ## width       -0.014774   0.008557  -1.726  0.08427 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(144.003) family taken to be 1)
    ## 
    ##     Null deviance: 124.31  on 151  degrees of freedom
    ## Residual deviance: 112.92  on 149  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 681.46
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  144 
    ##           Std. Err.:  245 
    ## 
    ##  2 x log-likelihood:  -673.46

``` r
sim_res <- simulateResiduals(fittedModel = lm_efnlb2, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-3.png)<!-- -->

``` r
#Final model

#Stem EFNs
lm_efns <- glm(EFNS ~ Latitude + width,
                   data = data)
summary(lm_efns)
```

    ## 
    ## Call:
    ## glm(formula = EFNS ~ Latitude + width, data = data)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.33543    0.54707   4.269 3.48e-05 ***
    ## Latitude     0.02725    0.01875   1.453   0.1483    
    ## width       -0.05972    0.03528  -1.693   0.0926 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 2.648676)
    ## 
    ##     Null deviance: 410.12  on 151  degrees of freedom
    ## Residual deviance: 394.65  on 149  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 584.38
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
sim_res <- simulateResiduals(fittedModel = lm_efns, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-4.png)<!-- -->

``` r
#This is not bad

#Trying model structure as in leaf base EFNs for consistency
lm_efns1 <- glm.nb(EFNS_raw ~ Latitude + width +  offset(log(leaves_in_branch)),
                   data = data)

summary(lm_efns1)
```

    ## 
    ## Call:
    ## glm.nb(formula = EFNS_raw ~ Latitude + width + offset(log(leaves_in_branch)), 
    ##     data = data, init.theta = 1.976304972, link = log)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.904678   0.263359   3.435 0.000592 ***
    ## Latitude     0.009837   0.009135   1.077 0.281534    
    ## width       -0.031389   0.017463  -1.797 0.072274 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(1.9763) family taken to be 1)
    ## 
    ##     Null deviance: 170.21  on 151  degrees of freedom
    ## Residual deviance: 165.94  on 149  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 970.76
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  1.976 
    ##           Std. Err.:  0.280 
    ## 
    ##  2 x log-likelihood:  -962.759

``` r
sim_res <- simulateResiduals(fittedModel = lm_efns1, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-5.png)<!-- -->

``` r
#Better than poisson model but worse than EFNS used directly

###EFN areas

#Leaf base EFNs
lm_efna_lb <- lm(Area_Leafbase ~ Latitude + width, data = data)
summary(lm_efna_lb)
```

    ## 
    ## Call:
    ## lm(formula = Area_Leafbase ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1403 -1.2360  0.1126  1.0547  4.1588 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.30721    0.85703   8.526 1.41e-11 ***
    ## Latitude    -0.14249    0.03413  -4.176 0.000109 ***
    ## width        0.01401    0.06200   0.226 0.822044    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.837 on 54 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.2441, Adjusted R-squared:  0.2161 
    ## F-statistic: 8.721 on 2 and 54 DF,  p-value: 0.0005223

``` r
sim_res <- simulateResiduals(fittedModel = lm_efna_lb, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-6.png)<!-- -->

``` r
#final model

lm_efna_s <- lm(Area_Petiole ~ Latitude + width, data = data)
summary(lm_efna_s)
```

    ## 
    ## Call:
    ## lm(formula = Area_Petiole ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3457 -0.8359 -0.1162  0.5997  3.7358 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.69994    0.64661   7.269 4.08e-09 ***
    ## Latitude    -0.09434    0.02530  -3.729 0.000537 ***
    ## width        0.03347    0.04584   0.730 0.469152    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.261 on 45 degrees of freedom
    ##   (106 observations deleted due to missingness)
    ## Multiple R-squared:  0.2395, Adjusted R-squared:  0.2057 
    ## F-statistic: 7.087 on 2 and 45 DF,  p-value: 0.00211

``` r
sim_res <- simulateResiduals(fittedModel = lm_efna_s, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-7.png)<!-- -->

``` r
#final model

#Examining correlations between EFN areas and volumes
lm6 <- lm(Vol_Leafbase ~ Area_Leafbase, data = data) 
summary(lm6)
```

    ## 
    ## Call:
    ## lm(formula = Vol_Leafbase ~ Area_Leafbase, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1324 -0.3238 -0.1556  0.4958  1.1248 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -0.03661    0.28497  -0.128 0.898949    
    ## Area_Leafbase  0.25273    0.05884   4.295 0.000294 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5807 on 22 degrees of freedom
    ##   (130 observations deleted due to missingness)
    ## Multiple R-squared:  0.4561, Adjusted R-squared:  0.4314 
    ## F-statistic: 18.45 on 1 and 22 DF,  p-value: 0.0002935

``` r
lm7 <- lm(Vol_Petiole ~ Area_Petiole, data = data)
summary(lm7)
```

    ## 
    ## Call:
    ## lm(formula = Vol_Petiole ~ Area_Petiole, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.50658 -0.25509 -0.09306  0.15540  0.76014 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   0.30503    0.24545   1.243   0.2299  
    ## Area_Petiole  0.16755    0.07229   2.318   0.0324 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3564 on 18 degrees of freedom
    ##   (134 observations deleted due to missingness)
    ## Multiple R-squared:  0.2299, Adjusted R-squared:  0.1871 
    ## F-statistic: 5.373 on 1 and 18 DF,  p-value: 0.03243

``` r
#EFN areas and volumes are highly correlated
#Because we had only a small number of volume measurements 
#And because they are correlated, we used areas as a proxy for investment

#Herbivory
lm_herb <- lm(pherb ~ Latitude + width + average_abundance, 
          data = data)
sim_res <- simulateResiduals(fittedModel = lm_herb, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-8.png)<!-- -->

``` r
summary(lm_herb)
```

    ## 
    ## Call:
    ## lm(formula = pherb ~ Latitude + width + average_abundance, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.61549 -0.16354  0.02742  0.19130  0.40837 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        0.630807   0.087041   7.247 2.18e-11 ***
    ## Latitude           0.009005   0.002892   3.114  0.00222 ** 
    ## width             -0.006641   0.005117  -1.298  0.19637    
    ## average_abundance -0.003547   0.002375  -1.494  0.13734    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2354 on 148 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1212, Adjusted R-squared:  0.1034 
    ## F-statistic: 6.805 on 3 and 148 DF,  p-value: 0.0002501

``` r
#final model

#ant abundance
lm_ants <- lm(average_abundance ~ Latitude + width, 
          data = data)

sim_res <- simulateResiduals(fittedModel = lm_ants, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-9.png)<!-- -->

``` r
summary(lm_ants)
```

    ## 
    ## Call:
    ## lm(formula = average_abundance ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.867 -4.041 -2.219  1.135 51.561 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 15.26800    2.72994   5.593 1.04e-07 ***
    ## Latitude    -0.42190    0.09359  -4.508 1.32e-05 ***
    ## width       -0.16036    0.17604  -0.911    0.364    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.121 on 149 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1203, Adjusted R-squared:  0.1085 
    ## F-statistic: 10.19 on 2 and 149 DF,  p-value: 7.111e-05

``` r
#many model assumptions violated

#log transformed ant abundance
lm_ants1 <- lm(log(average_abundance+1) ~ Latitude + width, data = data)

sim_res <- simulateResiduals(fittedModel = lm_ants1, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/linear%20models-10.png)<!-- -->

``` r
summary(lm_ants1)
```

    ## 
    ## Call:
    ## lm(formula = log(average_abundance + 1) ~ Latitude + width, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9912 -0.7697 -0.0420  0.5489  3.3692 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.86307    0.30673   9.334  < 2e-16 ***
    ## Latitude    -0.07216    0.01052  -6.863  1.7e-10 ***
    ## width       -0.01726    0.01978  -0.873    0.384    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9125 on 149 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.2403, Adjusted R-squared:  0.2301 
    ## F-statistic: 23.56 on 2 and 149 DF,  p-value: 1.287e-09

``` r
#Better but can be improved
#final model


#Seed mass, elaiosome mass

#mlm3 <- lm(cbind(seedweight, elaiosome.weight) ~ Latitude + width, data = data)
#summary(mlm3)
#Anova(mlm3)

#lm12 <- lm(ratio ~ Latitude + width, data = data)
#summary(lm12)

#Fitness
#lm13 <- lm(fruitno ~ Latitude + width + branchno + pherb + Type, data = data)
#summary(lm13)
```

``` r
#I don't think this is necessary
data_clean <- data %>% 
  mutate(Timestamp = dmy_hms(Timestamp, tz = "UTC"))       

#Creating a SpatVector
pts <- vect(data_clean,
            geom = c("Longitude", "Latitude"),
            crs  = "EPSG:4326")    # WGS‑84 lat/long

#Download worldclim data
dl_dir <- file.path(getwd(), "worldclim_cache")
dir.create(dl_dir, showWarnings = FALSE)
bio  <- worldclim_global(var = "bio",  res = 2.5, path = dl_dir)   
tmax <- worldclim_global(var = "tmax", res = 2.5, path = dl_dir)
tmin <- worldclim_global(var = "tmin", res = 2.5, path = dl_dir)
prec <- worldclim_global(var = "prec", res = 2.5, path = dl_dir)

#Summarize monthly stacks
ann_tmax <- app(tmax, fun = max)     # hottest month
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
ann_tmin <- app(tmin, fun = min)     # coldest month
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
ann_prec <- app(prec, fun = sum)     # annual precip (mm)
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
names(ann_tmax) <- "ann_tmax"        # hottest‑month Tmax (°C × 10)
names(ann_tmin) <- "ann_tmin"        # coldest‑month Tmin (°C × 10)
names(ann_prec) <- "annual_precip"   # yearly precip (mm)

#extract for my coordinates
bio_vals <- extract(bio, pts) %>% as.data.frame() %>% dplyr::select(-ID)
tmax_vals <- extract(ann_tmax, pts) %>% as.data.frame() %>% dplyr::select(-ID)
tmin_vals <- extract(ann_tmin, pts) %>% as.data.frame() %>% dplyr::select(-ID)
prec_vals <- extract(ann_prec, pts) %>% as.data.frame() %>% dplyr::select(-ID)

df_clim <- bind_cols(data_clean, bio_vals, tmax_vals, tmin_vals, prec_vals) %>%
  mutate(across(c(ann_tmax, ann_tmin, starts_with("wc2.1_2.5m_bio_")),
                ~ .x / 10)) #woldclim shows x10 values

#Visualizing correlations between climate variables and latitude

png("efn_correlations.png")
corrplot::corrplot(cor(df_clim[, c("ann_tmax", "ann_tmin", "annual_precip", "Latitude" )], use="pairwise.complete.obs"))
dev.off()
```

    ## png 
    ##   2

``` r
#ann_tmax, ann_tmin and latitude are correlated
#ann_precip seems relatively uncorrelated so can be added to existing models
#So we will run one set of models with latitude + precip 
#And another with just max or min temp

#Leaf base EFN

#Using poisson family distribution on raw EFN counts
lm_efnlb3 <- glm(EFNLB_raw ~ Latitude + width + annual_precip + offset(log(leaves_in_branch)),
                   data = df_clim,
                   family = poisson)

summary(lm_efnlb3)
```

    ## 
    ## Call:
    ## glm(formula = EFNLB_raw ~ Latitude + width + annual_precip + 
    ##     offset(log(leaves_in_branch)), family = poisson, data = df_clim)
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    1.0243561  0.1596765   6.415 1.41e-10 ***
    ## Latitude      -0.0100392  0.0047642  -2.107   0.0351 *  
    ## width         -0.0157916  0.0083459  -1.892   0.0585 .  
    ## annual_precip -0.0002997  0.0001498  -2.001   0.0453 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 133.38  on 151  degrees of freedom
    ## Residual deviance: 116.36  on 148  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 677.77
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
sim_res <- simulateResiduals(fittedModel = lm_efnlb3, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-1.png)<!-- -->

``` r
#final model for precipitation

#For temperature and precipitation
#Using tmin because plants are more sensitive to cold temperatures

lm_efnlb4 <- glm(EFNLB_raw ~  width + ann_tmin + annual_precip + 
                      offset(log(leaves_in_branch)), 
                 data = df_clim, family = poisson)

summary(lm_efnlb4)
```

    ## 
    ## Call:
    ## glm(formula = EFNLB_raw ~ width + ann_tmin + annual_precip + 
    ##     offset(log(leaves_in_branch)), family = poisson, data = df_clim)
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)    0.6307295  0.2005460   3.145  0.00166 **
    ## width         -0.0154992  0.0083126  -1.865  0.06225 . 
    ## ann_tmin       0.1537512  0.0612118   2.512  0.01201 * 
    ## annual_precip -0.0003140  0.0001447  -2.170  0.02998 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 133.38  on 151  degrees of freedom
    ## Residual deviance: 114.51  on 148  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 675.92
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
sim_res <- simulateResiduals(fittedModel = lm_efnlb4, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-2.png)<!-- -->

``` r
#final model

#Stem EFNs

#precipitation
lm_efns2 <- lm(EFNS ~ Latitude + width + annual_precip, data = df_clim)

summary(lm_efns2)
```

    ## 
    ## Call:
    ## lm(formula = EFNS ~ Latitude + width + annual_precip, data = df_clim)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -2.517 -1.295 -0.009  0.883  5.169 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)    1.8419462  0.6470536   2.847  0.00505 **
    ## Latitude       0.0110206  0.0219249   0.503  0.61595   
    ## width         -0.0546259  0.0353419  -1.546  0.12433   
    ## annual_precip  0.0007736  0.0005462   1.416  0.15877   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.622 on 148 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.05059,    Adjusted R-squared:  0.03135 
    ## F-statistic: 2.629 on 3 and 148 DF,  p-value: 0.05242

``` r
sim_res <- simulateResiduals(fittedModel = lm_efns2, n = 1000)
plot(sim_res)
```

    ## Warning in newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,
    ## : Fitting terminated with step failure - check results carefully

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-3.png)<!-- -->

``` r
#diagnostic plots best
#final model

#temperature + precipitation

lm_efns3 <- glm(EFNS ~  width + ann_tmin + annual_precip, data = df_clim)
summary(lm_efns3)
```

    ## 
    ## Call:
    ## glm(formula = EFNS ~ width + ann_tmin + annual_precip, data = df_clim)
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)    1.9100013  0.8572800   2.228   0.0274 *
    ## width         -0.0559895  0.0353418  -1.584   0.1153  
    ## ann_tmin       0.0081153  0.2884281   0.028   0.9776  
    ## annual_precip  0.0009231  0.0005137   1.797   0.0744 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 2.63539)
    ## 
    ##     Null deviance: 410.12  on 151  degrees of freedom
    ## Residual deviance: 390.04  on 148  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 584.6
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
sim_res <- simulateResiduals(fittedModel = lm_efns3, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-4.png)<!-- -->

``` r
#final model

#EFN areas

#Leaf base EFNs

#precipitation

lm_efna_lb1 <- lm(Area_Leafbase ~ Latitude + width + annual_precip, data = df_clim)
summary(lm_efna_lb1)
```

    ## 
    ## Call:
    ## lm(formula = Area_Leafbase ~ Latitude + width + annual_precip, 
    ##     data = df_clim)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6010 -1.0925  0.0134  1.1955  4.5875 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)    3.984575   1.909941   2.086  0.04179 * 
    ## Latitude      -0.120857   0.035117  -3.442  0.00114 **
    ## width          0.016019   0.060489   0.265  0.79217   
    ## annual_precip  0.003266   0.001688   1.935  0.05835 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.792 on 53 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.294,  Adjusted R-squared:  0.254 
    ## F-statistic: 7.357 on 3 and 53 DF,  p-value: 0.0003276

``` r
sim_res <- simulateResiduals(fittedModel = lm_efna_lb1, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-5.png)<!-- -->

``` r
#final model

#temperature + precipitation
lm_efna_lb2 <- lm(Area_Leafbase ~  width + ann_tmin + annual_precip, data = df_clim)
summary(lm_efna_lb2)
```

    ## 
    ## Call:
    ## lm(formula = Area_Leafbase ~ width + ann_tmin + annual_precip, 
    ##     data = df_clim)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7973 -1.0909  0.1695  1.3057  4.4787 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   0.108346   1.657935   0.065  0.94814   
    ## width         0.020269   0.061801   0.328  0.74422   
    ## ann_tmin      1.526955   0.497940   3.067  0.00341 **
    ## annual_precip 0.002726   0.001808   1.508  0.13747   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.827 on 53 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.2664, Adjusted R-squared:  0.2249 
    ## F-statistic: 6.415 on 3 and 53 DF,  p-value: 0.0008668

``` r
sim_res <- simulateResiduals(fittedModel = lm_efna_lb2, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-6.png)<!-- -->

``` r
#final model

#petiole EFNs

#precipitation

lm_efna_s1 <- lm(Area_Petiole ~ Latitude + width + annual_precip, data = df_clim)
summary(lm_efna_s1)
```

    ## 
    ## Call:
    ## lm(formula = Area_Petiole ~ Latitude + width + annual_precip, 
    ##     data = df_clim)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2008 -0.6978 -0.2456  0.4723  3.4605 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)    2.748407   1.325099   2.074  0.04395 * 
    ## Latitude      -0.080917   0.026066  -3.104  0.00333 **
    ## width          0.028188   0.045055   0.626  0.53479   
    ## annual_precip  0.002000   0.001193   1.677  0.10061   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.236 on 44 degrees of freedom
    ##   (106 observations deleted due to missingness)
    ## Multiple R-squared:  0.2852, Adjusted R-squared:  0.2365 
    ## F-statistic: 5.853 on 3 and 44 DF,  p-value: 0.001872

``` r
sim_res <- simulateResiduals(fittedModel = lm_efna_s1, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-7.png)<!-- -->

``` r
#final model

#temperature + precipitation
lm_efna_s2 <- lm(Area_Petiole ~  width + ann_tmin + annual_precip, data = df_clim)
summary(lm_efna_s2)
```

    ## 
    ## Call:
    ## lm(formula = Area_Petiole ~ width + ann_tmin + annual_precip, 
    ##     data = df_clim)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2276 -0.6669 -0.0906  0.5351  3.4632 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   0.122555   1.148358   0.107  0.91549   
    ## width         0.031661   0.045969   0.689  0.49459   
    ## ann_tmin      1.037400   0.370405   2.801  0.00755 **
    ## annual_precip 0.001634   0.001273   1.284  0.20600   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.258 on 44 degrees of freedom
    ##   (106 observations deleted due to missingness)
    ## Multiple R-squared:  0.2605, Adjusted R-squared:  0.2101 
    ## F-statistic: 5.167 on 3 and 44 DF,  p-value: 0.003803

``` r
sim_res <- simulateResiduals(fittedModel = lm_efna_s2, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-8.png)<!-- -->

``` r
#final model

#Herbivory

#precipitation
lm_herb1 <- lm(pherb ~ Latitude + width + average_abundance + annual_precip, 
          data = df_clim)
sim_res <- simulateResiduals(fittedModel = lm_herb1, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-9.png)<!-- -->

``` r
summary(lm_herb1)
```

    ## 
    ## Call:
    ## lm(formula = pherb ~ Latitude + width + average_abundance + annual_precip, 
    ##     data = df_clim)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.63082 -0.16200  0.02422  0.18482  0.39425 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        6.545e-01  9.878e-02   6.626 6.09e-10 ***
    ## Latitude           9.936e-03  3.421e-03   2.904  0.00425 ** 
    ## width             -6.885e-03  5.152e-03  -1.336  0.18347    
    ## average_abundance -3.385e-03  2.402e-03  -1.409  0.16087    
    ## annual_precip     -4.107e-05  8.017e-05  -0.512  0.60917    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.236 on 147 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1228, Adjusted R-squared:  0.09891 
    ## F-statistic: 5.144 on 4 and 147 DF,  p-value: 0.00066

``` r
#final model

#temperature + precipitation
lm_herb2 <- lm(pherb ~  width + average_abundance + ann_tmin + annual_precip, 
          data = df_clim)
sim_res <- simulateResiduals(fittedModel = lm_herb2, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-10.png)<!-- -->

``` r
summary(lm_herb2)
```

    ## 
    ## Call:
    ## lm(formula = pherb ~ width + average_abundance + ann_tmin + annual_precip, 
    ##     data = df_clim)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.59537 -0.16928  0.03264  0.20061  0.41728 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        9.222e-01  1.275e-01   7.235 2.38e-11 ***
    ## width             -7.420e-03  5.223e-03  -1.421   0.1575    
    ## average_abundance -3.982e-03  2.471e-03  -1.611   0.1093    
    ## ann_tmin          -9.167e-02  4.627e-02  -1.981   0.0494 *  
    ## annual_precip      1.824e-05  7.620e-05   0.239   0.8111    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2395 on 147 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.09657,    Adjusted R-squared:  0.07199 
    ## F-statistic: 3.928 on 4 and 147 DF,  p-value: 0.004641

``` r
#final model

#ant abundance

#precipitation

#log transformed ant abundance
lm_ants2 <- lm(log(average_abundance+1) ~ Latitude + width + annual_precip, 
          data = df_clim)

sim_res <- simulateResiduals(fittedModel = lm_ants2, n = 1000)
plot(sim_res)
```

    ## qu = 0.25, log(sigma) = -2.721532 : outer Newton did not converge fully.

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-11.png)<!-- -->

``` r
summary(lm_ants2)
```

    ## 
    ## Call:
    ## lm(formula = log(average_abundance + 1) ~ Latitude + width + 
    ##     annual_precip, data = df_clim)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.94981 -0.67093 -0.08369  0.54339  3.11584 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2.3859239  0.3579056   6.666 4.86e-10 ***
    ## Latitude      -0.0878568  0.0121273  -7.245 2.21e-11 ***
    ## width         -0.0123417  0.0195487  -0.631   0.5288    
    ## annual_precip  0.0007479  0.0003021   2.476   0.0144 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8972 on 148 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.2705, Adjusted R-squared:  0.2557 
    ## F-statistic: 18.29 on 3 and 148 DF,  p-value: 3.784e-10

``` r
#final model

#temperature + precipitation
lm_ants3 <- lm(log(average_abundance+1) ~  width + ann_tmin + annual_precip, 
          data = df_clim)

sim_res <- simulateResiduals(fittedModel = lm_ants3, n = 1000)
plot(sim_res)
```

![](README_files/figure-gfm/rerunning%20the%20models%20using%20worldclim%20data-12.png)<!-- -->

``` r
summary(lm_ants3)
```

    ## 
    ## Call:
    ## lm(formula = log(average_abundance + 1) ~ width + ann_tmin + 
    ##     annual_precip, data = df_clim)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.08214 -0.53139 -0.09432  0.51112  3.06093 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -0.7050954  0.4687432  -1.504   0.1347    
    ## width         -0.0107947  0.0193242  -0.559   0.5773    
    ## ann_tmin       1.1887736  0.1577066   7.538 4.42e-12 ***
    ## annual_precip  0.0004947  0.0002809   1.761   0.0803 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8876 on 148 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.2859, Adjusted R-squared:  0.2714 
    ## F-statistic: 19.75 on 3 and 148 DF,  p-value: 7.976e-11

``` r
#final model
```

\##Visualizing the data

Extrafloral nectaries

``` r
base_size <- 10

#Extracting coefficients and p-values from linear models
intercept_efnlb <- coef(lm_efnlb2)["(Intercept)"]
slope_latitude_efnlb <- coef(lm_efnlb2)["Latitude"]
summary_efnlb <- summary(lm_efnlb2)
p_value_latitude_efnlb <- summary_efnlb$coefficients["Latitude", "Pr(>|z|)"]

#Leafbase EFNs
p_efn_lb_1 <- ggplot(data, aes(x = Latitude, y = log(EFNLB+1))) +  
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 3.5, color = "olivedrab") +
  xlab("Latitude") +
  ylab("Log(Leaf base EFNs (no./leaf) +1)") +
  scale_x_continuous(limits = c(min(data$Latitude), 30)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size * 1.7),
    axis.title = element_text(size = base_size * 1.4),
    axis.text = element_text(size = base_size * 1.3),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_abline(intercept = intercept_efnlb, slope = slope_latitude_efnlb,
              color = "slategrey", linewidth = 2)+
  annotate("text", x = 19, 
           y = max(1.35, na.rm = TRUE),
           label = "*", 
           size = 12, 
           color = "black")


#ggsave(filename = "efnlb.png", plot = combined_plot_efnlb, width = 10, height = 7, dpi = 300)

#Stem EFNs - non-significant
p_efn_s_1 <- ggplot(data, aes(x = Latitude, y = log(EFNS+1))) +  
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 3.5, color = "olivedrab") +
  xlab("Latitude") +
  ylab("Log(Petiole EFNs (no./leaf) + 1)") +
  scale_x_continuous(limits = c(min(morph$Latitude), 30)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size * 1.7),
    axis.title = element_text(size = base_size * 1.4),
    axis.text = element_text(size = base_size * 1.3),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) 

#ggsave(filename = "efns.png", plot = combined_plot_efns, width = 10, height = 7, dpi = 300)

#EFN areas

#Leaf base EFNs
intercept_aefnlb <- coef(lm_efna_lb)["(Intercept)"]
slope_latitude_aefnlb <- coef(lm_efna_lb)["Latitude"]

p_area_lb <- ggplot(data = data, aes(x = Latitude, y = Area_Leafbase)) +  
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 3.5, color = "olivedrab") +
  xlab("Latitude") +
  ylab("Leaf base EFN area (mm²)") +
  scale_x_continuous(limits = c(min(data$Latitude, na.rm = TRUE), 30)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size * 1.7),
    axis.title = element_text(size = base_size * 1.4),
    axis.text = element_text(size = base_size * 1.3),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_abline(
    intercept = intercept_aefnlb,
    slope = slope_latitude_aefnlb,
    color = "slategrey",
    size = 2
  ) +
  annotate(
    "text",
    x = 19,
    y = max(9.5, na.rm = TRUE),
    label = "*",
    size = 12,
    color = "black"
  )
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
#ggsave(filename = "arealb.png", plot = combined_plot_arealb, width = 10, height = 7, dpi = 300)

#Petiole EFNs
intercept_aefns <- coef(lm_efna_s)["(Intercept)"]
slope_latitude_aefns <- coef(lm_efna_s)["Latitude"]

p_area_s <- ggplot(data = data, aes(x = Latitude, y = Area_Petiole)) +  
  geom_jitter(alpha = 0.7, width = 0.1, height = 0.1, size = 3.5, color = "olivedrab") +
  xlab("Latitude") +
  ylab("Petiole EFN area (mm²)") +
  scale_x_continuous(limits = c(min(data$Latitude, na.rm = TRUE), 30)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size * 1.7),
    axis.title = element_text(size = base_size * 1.4),
    axis.text = element_text(size = base_size * 1.3),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_abline(
    intercept = intercept_aefns,
    slope = slope_latitude_aefns,
    color = "slategrey",
    size = 2
  ) +
  annotate(
    "text",
    x = 19,
    y = max(7.5, na.rm = TRUE),
    label = "*",
    size = 12,
    color = "black"
  )

#ggsave(filename = "areas.png", plot = combined_plot_areas, width = 10, height = 7, dpi = 300)
```

``` r
#Herbivory

intercept_herb <- coef(lm_herb)["(Intercept)"]
slope_latitude_herb <- coef(lm_herb)["Latitude"]

p_herb1 <- ggplot(data = data, aes(x = Latitude, y = pherb)) +
  geom_jitter(
    alpha = 0.7,
    width = 0.1,
    height = 0.1,
    size = 3.5,
    color = "olivedrab"
  ) +
  xlab("Latitude") +
  ylab("Proportion of leaves with herbivory") +
  scale_x_continuous(limits = c(min(data$Latitude, na.rm = TRUE), 30)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size * 1.7),
    axis.title = element_text(size = base_size * 1.4),
    axis.text = element_text(size = base_size * 1.3),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_abline(
    intercept = intercept_herb,
    slope = slope_latitude_herb,
    color = "slategrey",
    size = 2
  ) +
  annotate(
    "text",
    x = 19,
    y = max(data$pherb, na.rm = TRUE),
    label = "*",
    size = 12,
    color = "black"
  )

#ggsave(filename = "herb.png", plot = combined_plot_herb, width = 12, height = 7, dpi = 300)


#ant visitation rates
intercept_ant <- coef(lm_ants1)["(Intercept)"]
slope_latitude_ant <- coef(lm_ants1)["Latitude"]

p_ants1 <- ggplot(data = data, aes(x = Latitude, y = log(average_abundance+1))) +
  geom_jitter(
    alpha = 0.7,
    width = 0.1,
    height = 0.1,
    size = 3.5,
    color = "olivedrab"
  ) +
  xlab("Latitude") +
  ylab("Log(Average ant visits (no./leaf) +1)") +
  scale_x_continuous(limits = c(min(data$Latitude, na.rm = TRUE), 30)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size * 1.7),
    axis.title = element_text(size = base_size * 1.4),
    axis.text = element_text(size = base_size * 1.3),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_abline(
    intercept = intercept_ant,
    slope = slope_latitude_ant,
    color = "slategrey",
    size = 2
  ) +
  annotate(
    "text",
    x = 19,
    y = max(3.5, na.rm = TRUE),
    label = "*",
    size = 12,
    color = "black"
  )

#ggsave(filename = "combined_plot_ants.png", plot = combined_plot_ants, width = 12, height = 7, dpi = 300)
```

``` r
#Figure 1
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
          "Hampi", "Bangalore", "Srirangam", "Kanyakumari" )
Lat <- c(29.40, 27.89, 26.23, 23.22, 21.25, 17.33, 15.34, 13.07, 10.86, 8.08)
Lon <- c(79.13, 78.03, 78.26, 77.38, 79.08, 78.44, 76.46, 77.53, 78.70, 77.52)

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
  geom_sf(data = sites_sf, aes(color = "olivedrab"), size = 3.5) +
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


#Add it to the map

ggsave("Indiamap.png", indmap, width = 8, height = 6)


###Figure 2
#Combine the second row with 4 plots
second_row <- (p_efn_lb_1 + p_efn_s_1) +
  plot_layout(ncol = 2)

#Combine the third row with 2 plots
third_row <- (p_area_lb + p_area_s) +
  plot_layout(ncol = 2)

fourth_row <- (p_herb1 + p_ants1) +
  plot_layout(ncol = 2)

#Combine the three rows into one layout
combined_plot <-  second_row / third_row / fourth_row

#Annotate with labels for subplots (a-i)
final_plot <- combined_plot +
  plot_annotation(tag_levels = 'a')  # Labels from a onwards


# Save the final plot to a file
ggsave("figure2.png", final_plot, width = 11.5, height = 12)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 103 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 110 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

How does seed size and elaiosome/seed mass vary across latitude?

Not in paper

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

Visualizing plant traits

Not in paper

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

![](README_files/figure-gfm/Community%20characteristics-1.png)<!-- -->![](README_files/figure-gfm/Community%20characteristics-2.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Shannon ~ Latitude, data = diversity_data)
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
    ## lm(formula = Richness ~ Latitude, data = diversity_data)
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
    ## lm(formula = Shannon ~ Latitude, data = diversity_data1)
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
    ## lm(formula = Richness ~ Latitude, data = diversity_data1)
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

![](README_files/figure-gfm/Community%20characteristics-3.png)<!-- -->![](README_files/figure-gfm/Community%20characteristics-4.png)<!-- -->

    ##  [1] 39.256574 56.382812 41.210283 50.440556 50.412604 19.916609  6.416667
    ##  [8]  1.000000  5.026667 65.561667

    ## Wisconsin double standardization
    ## Run 0 stress 0.09286488 
    ## Run 1 stress 0.2588647 
    ## Run 2 stress 0.09286486 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0008352369  max resid 0.001839115 
    ## ... Similar to previous best
    ## Run 3 stress 0.09286477 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0006919014  max resid 0.001610502 
    ## ... Similar to previous best
    ## Run 4 stress 0.09286515 
    ## ... Procrustes: rmse 0.0009241218  max resid 0.002050274 
    ## ... Similar to previous best
    ## Run 5 stress 0.1150362 
    ## Run 6 stress 0.1047489 
    ## Run 7 stress 0.09286492 
    ## ... Procrustes: rmse 0.0006270422  max resid 0.001378805 
    ## ... Similar to previous best
    ## Run 8 stress 0.09286505 
    ## ... Procrustes: rmse 0.0003008102  max resid 0.0007498599 
    ## ... Similar to previous best
    ## Run 9 stress 0.1047489 
    ## Run 10 stress 0.09286502 
    ## ... Procrustes: rmse 0.0008168265  max resid 0.002012111 
    ## ... Similar to previous best
    ## Run 11 stress 0.1047492 
    ## Run 12 stress 0.09286479 
    ## ... Procrustes: rmse 2.914143e-05  max resid 7.290828e-05 
    ## ... Similar to previous best
    ## Run 13 stress 0.1047489 
    ## Run 14 stress 0.1157047 
    ## Run 15 stress 0.09286514 
    ## ... Procrustes: rmse 0.0003235503  max resid 0.0008001435 
    ## ... Similar to previous best
    ## Run 16 stress 0.09286489 
    ## ... Procrustes: rmse 0.0001585076  max resid 0.0003959273 
    ## ... Similar to previous best
    ## Run 17 stress 0.09286489 
    ## ... Procrustes: rmse 8.867248e-05  max resid 0.0001943733 
    ## ... Similar to previous best
    ## Run 18 stress 0.104749 
    ## Run 19 stress 0.09286483 
    ## ... Procrustes: rmse 0.0001128293  max resid 0.0001921338 
    ## ... Similar to previous best
    ## Run 20 stress 0.1081466 
    ## *** Best solution repeated 10 times

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = bray_curtis ~ sp_arranged$Lat, data = sp_arranged, permutations = 999)
    ##          Df SumOfSqs      R2      F Pr(>F)  
    ## Model     1  0.60006 0.22603 2.3363  0.025 *
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

    ##     Latitude           width             EFNLB              EFNS         
    ##  Min.   :-1.5697   Min.   :-1.5812   Min.   :-2.1563   Min.   :-1.39855  
    ##  1st Qu.:-0.8704   1st Qu.:-0.6828   1st Qu.:-1.1145   1st Qu.:-0.79104  
    ##  Median :-0.2718   Median :-0.1544   Median : 0.1195   Median :-0.06203  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000  
    ##  3rd Qu.: 0.9769   3rd Qu.: 0.5062   3rd Qu.: 0.7796   3rd Qu.: 0.49992  
    ##  Max.   : 1.4159   Max.   : 4.1525   Max.   : 2.6737   Max.   : 3.02760  
    ##  NA's   :1         NA's   :1                                             
    ##      pherb         average_abundance    fruitno        Area_Leafbase     
    ##  Min.   :-2.1035   Min.   :-0.6485   Min.   :-0.4943   Min.   :-2.17501  
    ##  1st Qu.:-0.8948   1st Qu.:-0.5316   1st Qu.:-0.4943   1st Qu.:-0.64602  
    ##  Median : 0.3140   Median :-0.4123   Median :-0.4943   Median :-0.07068  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000  
    ##  3rd Qu.: 1.1198   3rd Qu.: 0.2373   3rd Qu.: 0.1980   3rd Qu.: 0.58987  
    ##  Max.   : 1.1198   Max.   : 5.5867   Max.   : 6.6670   Max.   : 2.18001  
    ##                                      NA's   :1         NA's   :96        
    ##   Area_Petiole      Vol_Leafbase      Vol_Petiole         Plant.ID        
    ##  Min.   :-1.6333   Min.   :-1.2361   Min.   :-1.69798   Length:154        
    ##  1st Qu.:-0.6278   1st Qu.:-0.6481   1st Qu.:-0.81560   Class :character  
    ##  Median :-0.1307   Median :-0.4355   Median :-0.08295   Mode  :character  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000                     
    ##  3rd Qu.: 0.6560   3rd Qu.: 0.7335   3rd Qu.: 0.85789                     
    ##  Max.   : 3.1713   Max.   : 2.2681   Max.   : 1.72800                     
    ##  NA's   :105       NA's   :130       NA's   :133

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
    ## 1.095089 1.505422 1.502401

``` r
vif(vif_herb)
```

    ##          Latitude average_abundance             EFNLB              EFNS 
    ##          1.287716          1.214362          1.525902          1.613568

``` r
vif(vif_lb)
```

    ## Latitude    width 
    ## 1.022763 1.022763

``` r
vif(vif_s)
```

    ## Latitude    width 
    ## 1.022763 1.022763

``` r
vif(vif_fru)
```

    ##             pherb average_abundance          Latitude             EFNLB 
    ##          1.162990          1.246089          1.381370          1.529849 
    ##              EFNS             width 
    ##          1.638424          1.055897

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
sem_apriori <- ggdraw() + draw_image("Slide2_cropped.PNG")
sem_sig <- ggdraw() + draw_image("Slide3_cropped.PNG")

sem_plot <- (sem_apriori / sem_sig)  + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 25))  

ggsave(filename = "figure4.png", plot = sem_plot, width = 12, height = 12, dpi = 300)
```

\#Common garden results

``` r
df <- read.csv("common_garden_data.csv")

#adding NAs in blank cells
df$leaf_touch <- na_if(df$leaf_touch, "")

#subsetting to only round 2
df <- df[df$round== 2,]

#removing NAs in the ant exclusion column
df <- df %>% 
  filter(!is.na(ant_exc))

#Plot herbivory in round 2 for excluded (non touching), excluded plants

#removing excluded rows for which leaves were touching 
df <- df %>% 
  filter(!leaf_touch == "Yes")

#Herbivory

#calculate sample sizes

df_n <- df %>%
  group_by(ant_exc) %>%
  summarize(n = n())

p4 <- ggplot(df, aes(x = ant_exc, y = prop_herb, fill = ant_exc)) +
  geom_boxplot() +
  labs(x = "Ant exclusion", 
       y = "Herbivory (proportion of leaves)")+
  scale_fill_manual(values = c("Yes" = "green", "No" = "yellow"), 
                    labels = c("Yes" = "Excluded", "No" = "Control")) +
  
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "none"
  ) +
  annotate("text", x = 1.5, y = 0.95, label = "*", size = 13)

#Add sample sizes
p4_n <- p4 + 
  geom_text(
    data = df_n,
    aes(
      x = ant_exc, 
      y = 0.08,   # Adjust to position text where you want it
      label = paste0("n=", n)
    ),
    vjust = -1,   # Nudges text upward/downward
    size = 5
  )

ggsave(filename = "ant_exc_herb.png", plot = p4_n, width = 6, height = 6, dpi = 300)
```

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

``` r
m1 <- lm(prop_herb ~ ant_exc + ant_ab_all, data = df)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = prop_herb ~ ant_exc + ant_ab_all, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5807 -0.1355  0.0160  0.1587  0.3791 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.641303   0.012402  51.709  < 2e-16 ***
    ## ant_excYes   0.093929   0.020522   4.577 6.05e-06 ***
    ## ant_ab_all  -0.002914   0.002214  -1.316    0.189    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2057 on 469 degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## Multiple R-squared:  0.04918,    Adjusted R-squared:  0.04512 
    ## F-statistic: 12.13 on 2 and 469 DF,  p-value: 7.318e-06

``` r
anova(m1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: prop_herb
    ##             Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## ant_exc      1  0.9526 0.95262 22.5247 2.759e-06 ***
    ## ant_ab_all   1  0.0733 0.07326  1.7323    0.1888    
    ## Residuals  469 19.8350 0.04229                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
