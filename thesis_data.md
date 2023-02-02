NYC COVID-19 Test Positivity by Modified Zip Code Tabulation Area
(modZCTA)
================
Sarah Forrest
02-02-2023

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

location_df = 
  read_csv("data/raw/data-by-modzcta.csv") %>% 
  janitor::clean_names() %>%
  select(modified_zcta, lat, lon) %>%
  rename(modzcta = modified_zcta)
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

Coordinate File:

``` r
# write.csv(location_df,"data/satscan/coordinate_file.csv", row.names = TRUE)
```

## Full Study Period: 8/2/2020 - 10/10/2020

Case File:

``` r
case_file = 
  final_df %>% 
  select(modzcta, casecount, week_ending) %>% 
  mutate(casecount = round(casecount, digits = 0))

# write.csv(case_file,"data/satscan/case_file.csv", row.names = TRUE)
```

Population File:

``` r
pop_file = 
  final_df %>% 
  select(modzcta, week_ending, testcount) %>% 
  mutate(testcount = round(testcount, digits = 0))

# write.csv(pop_file,"data/satscan/population_file.csv", row.names = TRUE)
```

## Retrospective Analysis

Case File:

``` r
case_file_retro = 
  case_file %>% 
  filter(week_ending <= '2020-08-22')

# write.csv(case_file_retro,"data/satscan/case_file_retro.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_retro = 
  pop_file %>% 
  filter(week_ending <= '2020-08-22')

# write.csv(pop_file_retro,"data/satscan/pop_file_retro.csv", row.names = TRUE)
```

# Create Files for ArcGIS Emerging Hotspot Analysis

``` r
ehsa_data = 
  final_df %>% 
  select(modzcta, week_ending, testpos, casecount, testcount)

# write.csv(ehsa_data,"data/arcgis/ehsa_data.csv", row.names = TRUE)
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

![](thesis_data_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

![](thesis_data_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

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

![](thesis_data_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-5.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-6.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-7.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-8.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-9.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-10.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-11.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-12.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-13.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-14.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-15.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-16.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-17.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-18.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-19.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-20.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-21.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-22.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-23.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-24.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-25.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-26.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-27.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-28.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-29.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-30.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-31.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-32.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-33.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-34.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-35.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-36.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-37.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-38.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-39.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-40.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-41.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-42.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-43.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-44.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-45.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-46.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-47.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-48.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-49.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-50.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-51.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-52.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-53.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-54.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-55.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-56.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-57.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-58.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-59.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-60.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-61.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-62.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-63.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-64.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-65.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-66.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-67.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-68.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-69.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-70.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-71.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-72.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-73.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-74.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-75.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-76.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-77.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-78.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-79.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-80.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-81.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-82.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-83.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-84.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-85.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-86.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-87.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-88.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-89.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-90.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-91.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-92.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-93.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-94.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-95.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-96.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-97.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-98.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-99.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-100.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-101.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-102.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-103.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-104.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-105.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-106.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-107.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-108.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-109.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-110.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-111.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-112.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-113.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-114.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-115.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-116.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-117.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-118.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-119.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-120.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-121.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-122.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-123.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-124.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-125.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-126.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-127.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-128.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-129.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-130.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-131.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-132.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-133.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-134.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-135.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-136.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-137.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-138.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-139.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-140.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-141.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-142.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-143.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-144.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-145.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-146.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-147.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-148.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-149.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-150.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-151.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-152.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-153.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-154.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-155.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-156.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-157.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-158.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-159.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-160.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-161.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-162.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-163.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-164.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-165.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-166.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-167.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-168.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-169.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-170.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-171.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-172.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-173.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-174.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-175.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-176.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-16-177.png)<!-- -->
