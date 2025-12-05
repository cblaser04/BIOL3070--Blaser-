Firefly Abundance Northern vs Southern Utah
================
Caden Blaser
2025-12-05

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS and RESULTS](#methods-and-results)
  - [1st analysis of violin plot](#1st-analysis-of-violin-plot)
  - [2nd analysis of Shapiro-Wilk
    test](#2nd-analysis-of-shapiro-wilk-test)
  - [3rd analysis of Wilcoxon test](#3rd-analysis-of-wilcoxon-test)
  - [4th analysis of median](#4th-analysis-of-median)
- [DISCUSSION](#discussion)
  - [Interpretation of 1st analysis (e.g. violin
    plot)](#interpretation-of-1st-analysis-eg-violin-plot)
  - [Interpretation of 2nd analysis (e.g. Shapiro-Wilk
    test)](#interpretation-of-2nd-analysis-eg-shapiro-wilk-test)
  - [Interpretation of 3rd analysis (e.g. Wilcoxon
    test)](#interpretation-of-3rd-analysis-eg-wilcoxon-test)
  - [Interpretation of 4th analysis
    (e.g. median)](#interpretation-of-4th-analysis-eg-median)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Firefly abundance can be influenced by environmental conditions such as
temperature, moisture, and the habitat characteristics, which will vary
between regions. This study investigated whether the abundance of
fireflies differs between northern and southern Utah. Observation data
was collected at many sites across Utah, by recording the number of
fireflies at each of the locations. Data was then polished and analyzed
using a visualization plot and statistical tests, that included the
Shapiro-Wilk test for normality and the Wilcoxon test to compare the
abundance per region.

The results showed that firefly counts were highly skewed in both
northern and southern Utah, while also not following a normal
distribution. Although the median firefly abundance was very similar
between northern and southern sites, the Wilcoxon test suggested a
tendency for southern Utah to have higher ranked firefly counts, but the
difference was not statistically significant. This shows the difference
in distribution and frequency of the higher count observations, but
clearly not a shift in overall abundance. All in all, these findings
suggest that the geographic location doesn’t have an influence on
firefly abundance in Utah based on the data that was analyzed.

# BACKGROUND

Fireflies are bioluminescent beetles that are observed in temperate and
tropical regions. The abundance and activity of fireflies are influenced
by environmental conditions like temperature, humidity, soil moisture,
and the availability of light. Since the larval and adult stages are
both sensitive to changes in climate, firefly populations will end up
varying across geographic gradients. Specifically, regional differences
in precipitation and temperature can strongly influence the fireflies
breeding success as well as the adult emergence rates (Lewis, 2016).

Geographic location plays a big role in determining the firefly
abundance. The northern regions typically experience cooler temperatures
and a higher amount of moisture, which promotes a higher larval survival
and an increase in the overall abundance. On the other hand, southern
areas typically experience higher temperature and more drier conditions
that lead to a decrease in firefly activity. This mirrors material that
was covered in class, where environmental gradients impact the
population patterns, where the differences in the climate across the
regions can create somewhat of a predictable biological variation.

To be able to explore these kinds of patterns, our study asks: Does the
abundance of fireflies vary between northern and southern regions in
Utah. We hypothesize that northern populations will have a higher
abundance than southern populations because of the cooler temperatures
and an increase in moisture levels which promotes larval survival and
overall firefly activity (Lewis, 2016). Based off of this hypothesis, we
predicted that the northern regions of Utah will show a higher mean
abundance of fireflies compared to the southern regions, which we will
be able to visualize using a violin plot, and test statistics such as a
Shapiro–Wilk test and a Wilcoxon test.

# STUDY QUESTION and HYPOTHESIS

## Questions

Does the location (Northern or Southern) of the fireflies affect the
level of abundance.

## Hypothesis

The geographic location of fireflies (Northern vs. Southern populations)
will affect the abundance, with northern populations having a higher
abundance than southern populations due to the climate.

## Prediction

If we find that geographic location affects firefly abundance, then
northern populations will have a higher abundance of fireflies that
southern populations.

# METHODS and RESULTS

Observational data was collected to estimate firefly abundance in
northern and southern regions of Utah, with each of the records
documenting the number of fireflies that were observed at a certain
site. The dataset was then cleaned by removing the incomplete records
and by standardizing the regions that were north and south in Utah. Once
the cleaning was finished, the data was then put into their respective
regions. To visualize the differences, a split violin plot was used to
show the variation and distribution of firefly counts in each of the
regions. This plot was able to highlight the density in counts,
mid-range values, and the outliers in each data set. The violin plot
showed that the northern sites had a higher concentration of mid-range
counts, while the southern sites had a higher concentration of higher
counts. This shows the variability within each of the regions but it
doesn’t show any obvious differences in the average abundance.

In order to assess whether the data followed a normal distribution, a
Shapiro-Wilk test was conducted. The results showed that the counts in
both northern and southern Utah were non-normal and highly skewed
(south: W = 0.229, p \< 0.001; north: W = 0.177, p \< 0.001). Then a
Wilcoxon test was used to compare firefly abundance between the regions.
This test had a W statistic of 13,812.5 and a p-value of 0.279. This
shows that there is no strong evidence that either the northern or
southern sites had a higher abundance of fireflies. This aligns with our
observation that the median counts were the same between the regions.
Overall, these results suggest that the geographic location doesn’t
strongly effect firefly abundance, although there could be differences
in the distribution and variability that could effect firefly abundance.

``` r
# Split Violin Plot (y-axis limited to 50)

library(ggplot2)
library(gghalves)
library(stringi)

# Read and clean your data
fireflies <- read.csv("Copy of firefliesUtah - Usable Data.csv", stringsAsFactors = FALSE)
colnames(fireflies) <- c("firefly_count", "region")
fireflies <- na.omit(fireflies)

fireflies$region[fireflies$region == ""] <- NA
fireflies$region <- stri_trans_general(fireflies$region, "NFKC")
fireflies$region <- stri_replace_all_regex(fireflies$region, "\\p{C}", "")
fireflies$region <- gsub("\u00A0", " ", fireflies$region)
fireflies$region <- trimws(tolower(fireflies$region))
fireflies$region[fireflies$region %in% c("n", "nrth", "noth")] <- "north"
fireflies$region[fireflies$region %in% c("s", "sth", "soth")] <- "south"
fireflies$region <- factor(fireflies$region, levels = c("north", "south"))
fireflies_clean <- droplevels(subset(fireflies, !is.na(region)))

# Split violin plot
ggplot() +
geom_half_violin(
data = subset(fireflies_clean, region == "north"),
aes(x = factor(1), y = firefly_count, fill = region),
side = "l", trim = TRUE, color = "black", alpha = 0.7
) +
geom_half_violin(
data = subset(fireflies_clean, region == "south"),
aes(x = factor(1), y = firefly_count, fill = region),
side = "r", trim = TRUE, color = "black", alpha = 0.7
) +
scale_fill_manual(values = c("north" = "#8EC9E8", "south" = "#F4A261")) +
coord_cartesian(ylim = c(0, 50)) + # y-axis capped at 50
labs(
title = "Firefly Abundance: North vs South",
x = NULL,
y = "Firefly Count"
) +
theme_minimal(base_size = 13) +
theme(
legend.position = "bottom",
plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
axis.text.x = element_blank(),
axis.ticks.x = element_blank()
)
```

![](Fireflies_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## 1st analysis of violin plot

We compared the firefly abundance between the northern and southern
regions in Utah to determine if the overall firefly numbers differed
between sites. A violin plot was used because it shows the distribution
as well as the density of the counts in both of the regions. This allows
us to see where the majority of the observations were as well as where
the extreme values were observed. The violin plot showed that both of
the regions have a somewhat similar shape, but the north has a higher
abundance of mid-range counts, while the south has a higher abundance of
higher counts. The higher concentration of mid-range counts in the north
could reflect more stable, consistent microhabitat conditions that
support moderate firefly activity. This infers that firefly abundance
varies within each of the regions, but it doesn’t show a big difference
between them.

``` r
#shaprio test to determine strength of fit to normal curve
shapiro.test(
fireflies$firefly_count[fireflies$region == "south"]
)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fireflies$firefly_count[fireflies$region == "south"]
    ## W = 0.22918, p-value = 3.044e-16

``` r
shapiro.test(
fireflies$firefly_count[fireflies$region == "north"])
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fireflies$firefly_count[fireflies$region == "north"]
    ## W = 0.17659, p-value < 2.2e-16

|         |
|:--------|
| Results |

data: fireflies$firefly_count[fireflies$region == “south”\] W = 0.22918,
p-value = 3.044e-16 data: fireflies$firefly_count[fireflies$region ==
“north”\] W = 0.17659, p-value \< 2.2e-16

## 2nd analysis of Shapiro-Wilk test

We conducted a Shapiro-Wilk test which is able to assess whether firefly
counts in the northern and southern regions of Utah follow a normal
distribution. The test results show that neither northern or southern
Utah had normally distributed data (south: W = 0.229, p \< 0.001; north:
W = 0.177, p \< 0.001). With these values being exceedingly small it
indicates that the firefly count data is not normally distributed.
Instead of being normally distributed, the data is highly skewed and
contains extreme values.

Since the data shows a non-normal pattern, normal statistical methods
rely on normal distribution meaning that we can’t use normal statistical
methods. With our data not being normally distributed, it justified the
use of non-parametric analyses such as the Wilcoxon test. Overall, the
Shapiro-Wilk test shows that the firefly counts in both the regions have
irregular distributions, which impacts how the data is compared and
therefore interpreted.

``` r
wilcox.test(firefly_count ~ region, data = fireflies,
alternative = "greater")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  firefly_count by region
    ## W = 13812, p-value = 0.2794
    ## alternative hypothesis: true location shift is greater than 0

## 3rd analysis of Wilcoxon test

Since the firefly count data was not normally distributed, a Wilcoxon
test was used to compare the abundance between the northern and southern
regions. The test produced a W statistic of 13,812.5 and a p-value of
0.279, which test whether southern sites have higher firefly counts than
northern sites. These results show that there is no strong evidence that
either of the sites have a higher or lower firefly abundance. This
aligns with the observation below that the median counts are the same
between the regions. Although this test does account for the skewed
nature of the data, it suggests that the geographic location doesn’t
have a clear effect on firefly abundance, even though there could be
differences in the distribution or the variability of the counts of
fireflies.

``` r
tapply(fireflies$firefly_count, fireflies$region, median)
```

    ## north south 
    ##     4     4

## 4th analysis of median

As seen above the medians were the same for firefly abundance in
northern and southern Utah. This shows that there is no difference or
preference in firefly populations across Utah. This means that firefly
populations don’t appear to have a higher abundance in one region
compared to the other.

# DISCUSSION

Overall, the results of this experiment suggest that firefly abundance
doesn’t differ significantly between northern and southern Utah, which
doesn’t support the original hypothesis that the firefly abundance would
be higher in the northern regions of Utah due to the cooler temperatures
and the increase in moisture. Alternatively, the distributional patterns
and statistical results seen indicate that both region have similar
levels of firefly abundance. The very slight difference seen in the
spread of values in the violin plot reflects habitat variability. With
the abnormality of the data highlighting how firefly abundance varies
unpredictably across many sites, due to the microclimates, land use, or
the observer differences.

The real uncertainty in the study is the nature of the observational
data. There could be environmental variables like soil moisture,
vegetation, or pollution that were not measured, yet all of these could
influence firefly abundance. Another thing is that the sampling effort
and timing could have been different between the regions, which could
also affect the counts. Although these are limitations, the analyses
consistently show that geographic region by itself doesn’t explain
differences in the firefly abundance across the sites that were sampled.

## Interpretation of 1st analysis (e.g. violin plot)

The violin plot shows that firefly abundance is around the same for both
northern and southern Utah, while both of them have higher abundance in
certain ranges. The southern sites show a higher concentration of higher
counts, while northern sites have a higher concentration of lower to
moderate counts with greater variability among sites. The variability
seen between sites could be due to differences in microhabitats, or the
local environmental conditions. Also, differences in the vegetation,
moisture in the soil or the temperature changes could also contribute to
the patterns seen.

## Interpretation of 2nd analysis (e.g. Shapiro-Wilk test)

The Shapiro-Wilk test results show that firefly counts in both northern
and southern Utah are very abnormal, with both having very low W-values
and small p-values. This means that the data is skewed and it includes
outliers, rather than being normally distributed around a mean. The
result of this is that you can’t assume normality which means analyses
that assume normality cannot be used with this kind of data. The
irregular distribution spotlights that firefly abundance varies greatly
across sites within each region in Utah. As a result, distribution-free
methods, such as the Wilcoxon test, are there to properly assess the
differences between northern and southern Utah. Overall, the
Shapiro-Wilk test confirms that the structure of the data influences the
choice of the statistical test used, and how the data of firefly
abundance can be explained.

## Interpretation of 3rd analysis (e.g. Wilcoxon test)

The Wilcoxon test shows that there is absolutely no strong evidence of
differences in firefly abundance between northern and southern Utah.
This aligns with observations that the median counts are the same for
both of the regions. With this, it suggests that the geographic location
doesn’t have a strong effect on the firefly abundance. On the other
hand, the test only compares the ranks of counts, but doesn’t get all
the distribution, so the differences in the variability of the high
counts could somehow still exist between the two regions. Overall,
although location may not be a measure of firefly abundance, regional
factors could still affect the distribution and the patterns seen in the
abundance in ways that more data would need to be collected.

## Interpretation of 4th analysis (e.g. median)

With the median firefly abundance for northern and southern Utah being
identical, this indicates that the typical abundance of fireflies
between the two regions is very similar. Because the median represents
the central tendency of the data its given, it doesn’t factor in skewed
distributions, which is why both of the regions had the same firefly
abundance median. Based on the median abundance alone, there is no
difference in firefly abundance between northern and southern Utah.

# CONCLUSION

The findings that we got from our tests indicate that firefly abundance
in Utah doesn’t differ between northern and southern regions. The violin
plot, Shapiro-Wilk test, Wilcoxon test, and the median comparison all
show that both of the regions have very similar distributions of firefly
counts. This does not support the hypothesis that northern Utah would
have a higher firefly abundance than southern Utah. A limitation in this
experiment is the influence of unmeasured environmental factors, which
can sometimes override geographic trends. In the future, studies should
incorporate the habitat variables while also using a standardized
sampling condition which would help clarify what actually drives firefly
abundance differences in the state of Utah.

# REFERENCES

1.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-12-05.
2.  Lewis, S. M. (2016). Silent sparks: The wondrous world of fireflies.
    Princeton: Princeton University Press.
