NYC COVID-19 Test Positivity by Modified Zip Code Tabulation Area
(modZCTA)
================
Sarah Forrest
01-11-2023

# Create Dataset

Read in and tidy `caserate-by-modzcta.csv`, `testrate-by-modzcta.csv`,
`percentpositive-by-modzcta.csv`, and
`populationestimates-by-modzcta.csv`:

``` r
caserate_df = 
  read_csv("data/raw/caserate-by-modzcta.csv") %>%
  janitor::clean_names() %>%
  select(week_ending, caserate_10001:caserate_11697) %>%
  pivot_longer(
    starts_with("caserate"),
    names_to = "modzcta",
    names_prefix = "caserate_", 
    values_to = "caserate_100k"
  )

testrate_df = 
  read_csv("data/raw/testrate-by-modzcta.csv") %>%
  janitor::clean_names() %>%
  select(week_ending, testrate_10001:testrate_11697) %>%
  pivot_longer(
    starts_with("testrate"),
    names_to = "modzcta",
    names_prefix = "testrate_", 
    values_to = "testrate_100k"
  )

pctpos_df =
read_csv("data/raw/percentpositive-by-modzcta.csv") %>%
  janitor::clean_names() %>%
  select(week_ending, pctpos_10001:pctpos_11697) %>%
  pivot_longer(
    starts_with("pctpos"),
    names_to = "modzcta",
    names_prefix = "pctpos_", 
    values_to = "pctpos"
  )

modzcta_pop_df = 
  read_csv("data/raw/populationestimates-by-modzcta.csv") %>% 
  janitor::clean_names() %>%
  select(modzcta, pop_est) %>%
  slice(1:(n() - 1))
```

Merge `caserate-by-modzcta.csv`, `testrate-by-modzcta.csv`, and
`percentpositive-by-modzcta.csv` by `modzcta` and `week_ending`:

``` r
merged_df = 
  list(caserate_df, testrate_df, pctpos_df) %>% 
  reduce(full_join, by = c("week_ending", "modzcta")) %>% 
  mutate(
    modzcta = as.numeric(modzcta),
    week_ending = as.Date(week_ending, format = "%m/%d/%Y"))
```

Merge modZCTA population estimates to the dataframe by `modzcta`:

``` r
merged_df = full_join(merged_df, modzcta_pop_df)
```

Restrict study period to the the weeks ending August 08, 2020 to October
10, 2020:

``` r
merged_df = 
  merged_df %>%
  filter(week_ending <= '2020-10-10')
```

Calculate `casecount`, `testcount`, and `testpos` (test positivity) and
reorder variables:

``` r
merged_df = 
  merged_df %>% 
  mutate(
    casecount = caserate_100k * pop_est / 100000,
    testcount = testrate_100k * pop_est / 100000,
    testpos = casecount / testcount * 100) %>%
  select(week_ending, modzcta, pop_est, casecount, caserate_100k, testcount, testrate_100k, pctpos, testpos)
```

Export dataframe as a CSV file:

``` r
final_df =
  merged_df %>%
  select(-pctpos)

# write.csv(final_df,"data/casecount-testcount-testpos-by-modzcta.csv", row.names = TRUE)
```

# Create Files for SaTScan Analysis

Case File:

``` r
case_file = 
  final_df %>% 
  select(modzcta, casecount, week_ending) %>% 
  mutate(casecount = round(casecount, digits = 0))

write.csv(case_file,"data/satscan/case_file.csv", row.names = TRUE)
```

Population File:

``` r
pop_file = 
  final_df %>% 
  select(modzcta, week_ending, testcount) %>% 
  mutate(testcount = round(testcount, digits = 0))

write.csv(pop_file,"data/satscan/population_file.csv", row.names = TRUE)
```

# Create Files for ArcGIS Emerging Hotspot Analysis

``` r
ehsa_data = 
  final_df %>% 
  select(modzcta, week_ending, testpos, casecount, testcount) %>% 
  mutate(
    casecount = round(casecount, digits = 0),
    testcount = round(testcount, digits = 0))

write.csv(pop_file,"data/arcgis/ehsa_data.csv", row.names = TRUE)
```

# Exploratory Data Analysis

Average Percent Test Positivity across all ModZCTAs over Study Period:

``` r
final_df %>% 
  group_by(week_ending) %>%
  summarize(mean_testpos = mean(testpos)) %>%
  ggplot(aes(x = week_ending, y = mean_testpos)) + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + geom_line(show.legend = FALSE) +
  ggtitle("Avg NYC COVID-19 Test % Positivity by Week From 8/2/2020 to 10/10/2020") + 
  labs(
    x = "week ending date",
    y = "mean test % positivity")
```

![](thesis_data_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Percent Test Positivity for by ModZCTA over Study Period:

``` r
final_df %>% 
  mutate(modzcta = as.character(modzcta)) %>% 
  ggplot(aes(x = week_ending, y = testpos, group = modzcta, color = modzcta)) + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + geom_line(show.legend = FALSE) +
  ggtitle("COVID-19 Test % Positivity by ModZCTA by Week From 8/2/2020 to 10/10/2020") + 
  labs(
    x = "week ending date",
    y = "mean test % positivity")
```

![](thesis_data_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Percent Test Positivity for Each ModZCTA over Study Period

Create dataframe for use in plots:

``` r
plot_df = 
  merged_df %>% 
  rename(
    pctpos_reported = pctpos,
    pctpos_calculated = testpos) %>% 
  pivot_longer(
    starts_with("pctpos"),
    names_to = "group",
    names_prefix = "pctpos_", 
    values_to = "value")
```

Create function to plot the % positive value (from
`percentpositive-by-modzcta.csv`) vs. my calculated
casecount/testcount\*100 value for each modZCTA over the study period:

``` r
plot_modzcta = function(modzcta_entry) {

plot_df %>% 
  filter(modzcta == modzcta_entry) %>% 
  ggplot(aes(x = week_ending, y = value, group = group, color = group)) + 
  geom_line(alpha = 0.7) +
  ylim(0, 9) +
  labs(
    title = modzcta_entry,
    x = "week ending date",
    y = "test % positivity") +
  scale_color_hue(labels = c("calculated" = "casecount/testcount*100", "reported" = "% positive"))
  
}

modzcta_list = modzcta_pop_df$modzcta

for (i in 1:177) {
  
  plot = plot_modzcta(modzcta_list[[i]])
  print(plot)
  
}
```

![](thesis_data_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-8.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-9.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-10.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-12.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-13.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-14.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-15.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-16.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-17.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-18.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-19.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-20.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-21.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-22.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-23.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-24.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-25.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-26.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-27.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-28.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-29.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-30.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-31.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-32.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-33.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-34.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-35.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-36.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-37.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-38.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-39.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-40.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-41.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-42.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-43.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-44.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-45.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-46.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-47.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-48.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-49.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-50.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-51.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-52.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-53.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-54.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-55.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-56.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-57.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-58.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-59.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-60.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-61.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-62.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-63.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-64.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-65.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-66.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-67.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-68.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-69.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-70.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-71.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-72.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-73.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-74.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-75.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-76.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-77.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-78.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-79.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-80.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-81.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-82.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-83.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-84.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-85.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-86.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-87.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-88.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-89.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-90.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-91.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-92.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-93.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-94.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-95.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-96.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-97.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-98.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-99.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-100.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-101.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-102.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-103.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-104.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-105.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-106.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-107.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-108.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-109.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-110.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-111.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-112.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-113.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-114.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-115.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-116.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-117.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-118.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-119.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-120.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-121.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-122.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-123.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-124.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-125.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-126.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-127.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-128.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-129.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-130.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-131.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-132.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-133.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-134.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-135.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-136.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-137.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-138.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-139.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-140.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-141.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-142.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-143.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-144.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-145.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-146.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-147.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-148.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-149.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-150.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-151.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-152.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-153.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-154.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-155.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-156.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-157.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-158.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-159.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-160.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-161.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-162.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-163.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-164.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-165.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-166.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-167.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-168.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-169.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-170.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-171.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-172.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-173.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-174.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-175.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-176.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-13-177.png)<!-- -->
