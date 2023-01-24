NYC COVID-19 Test Positivity by Modified Zip Code Tabulation Area
(modZCTA)
================
Sarah Forrest
01-23-2023

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

## Five-Week Study Period: 8/2/2020 - 9/5/2020

Case File:

``` r
case_file_5wk = 
  case_file %>% 
  filter(week_ending <= '2020-09-05')

# write.csv(case_file_5wk,"data/satscan/case_file_5wk.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_5wk = 
  pop_file %>% 
  filter(week_ending <= '2020-09-05')

# write.csv(pop_file_5wk,"data/satscan/population_file_5wk.csv", row.names = TRUE)
```

## Six-Week Study Period: 8/2/2020 - 9/12/2020

Case File:

``` r
case_file_6wk = 
  case_file %>% 
  filter(week_ending <= '2020-09-12')

# write.csv(case_file_6wk,"data/satscan/case_file_6wk.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_6wk = 
  pop_file %>% 
  filter(week_ending <= '2020-09-12')

# write.csv(pop_file_6wk,"data/satscan/population_file_6wk.csv", row.names = TRUE)
```

## Seven-Week Study Period: 8/2/2020 - 9/19/2020

Case File:

``` r
case_file_7wk = 
  case_file %>% 
  filter(week_ending <= '2020-09-19')

# write.csv(case_file_7wk,"data/satscan/case_file_7wk.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_7wk = 
  pop_file %>% 
  filter(week_ending <= '2020-09-19')

# write.csv(pop_file_7wk,"data/satscan/population_file_7wk.csv", row.names = TRUE)
```

## Eight-Week Study Period: 8/2/2020 - 9/26/2020

Case File:

``` r
case_file_8wk = 
  case_file %>% 
  filter(week_ending <= '2020-09-26')

# write.csv(case_file_8wk,"data/satscan/case_file_8wk.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_8wk = 
  pop_file %>% 
  filter(week_ending <= '2020-09-26')

# write.csv(pop_file_8wk,"data/satscan/population_file_8wk.csv", row.names = TRUE)
```

## Nine-Week Study Period I: 8/2/2020 - 10/3/2020

Case File:

``` r
case_file_9wki = 
  case_file %>% 
  filter(week_ending <= '2020-10-03')

# write.csv(case_file_9wki,"data/satscan/case_file_9wki.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_9wki = 
  pop_file %>% 
  filter(week_ending <= '2020-10-03')

# write.csv(pop_file_9wki,"data/satscan/population_file_9wki.csv", row.names = TRUE)
```

## Nine-Week Study Period II: 8/9/2020 - 10/10/2020

Case File:

``` r
case_file_9wkii = 
  case_file %>% 
  filter(week_ending <= '2020-10-10') %>% 
  filter(week_ending >= '2020-08-09')

# write.csv(case_file_9wkii,"data/satscan/case_file_9wkii.csv", row.names = TRUE)
```

Population File:

``` r
pop_file_9wkii = 
  pop_file %>% 
  filter(week_ending <= '2020-10-10') %>% 
  filter(week_ending >= '2020-08-09')

# write.csv(pop_file_9wkii,"data/satscan/population_file_9wkii.csv", row.names = TRUE)
```

# Create Files for ArcGIS Emerging Hotspot Analysis

``` r
ehsa_data = 
  final_df %>% 
  select(modzcta, week_ending, testpos, casecount, testcount) %>% 
  mutate(
    casecount = round(casecount, digits = 0),
    testcount = round(testcount, digits = 0))

# write.csv(pop_file,"data/arcgis/ehsa_data.csv", row.names = TRUE)
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

![](thesis_data_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

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

![](thesis_data_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

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

![](thesis_data_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-3.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-4.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-5.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-6.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-7.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-8.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-9.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-10.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-11.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-12.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-13.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-14.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-15.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-16.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-17.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-18.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-19.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-20.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-21.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-22.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-23.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-24.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-25.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-26.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-27.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-28.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-29.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-30.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-31.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-32.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-33.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-34.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-35.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-36.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-37.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-38.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-39.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-40.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-41.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-42.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-43.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-44.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-45.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-46.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-47.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-48.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-49.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-50.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-51.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-52.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-53.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-54.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-55.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-56.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-57.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-58.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-59.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-60.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-61.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-62.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-63.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-64.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-65.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-66.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-67.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-68.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-69.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-70.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-71.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-72.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-73.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-74.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-75.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-76.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-77.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-78.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-79.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-80.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-81.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-82.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-83.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-84.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-85.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-86.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-87.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-88.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-89.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-90.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-91.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-92.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-93.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-94.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-95.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-96.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-97.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-98.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-99.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-100.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-101.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-102.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-103.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-104.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-105.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-106.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-107.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-108.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-109.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-110.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-111.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-112.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-113.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-114.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-115.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-116.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-117.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-118.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-119.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-120.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-121.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-122.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-123.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-124.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-125.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-126.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-127.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-128.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-129.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-130.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-131.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-132.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-133.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-134.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-135.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-136.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-137.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-138.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-139.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-140.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-141.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-142.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-143.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-144.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-145.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-146.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-147.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-148.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-149.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-150.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-151.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-152.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-153.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-154.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-155.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-156.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-157.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-158.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-159.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-160.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-161.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-162.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-163.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-164.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-165.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-166.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-167.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-168.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-169.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-170.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-171.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-172.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-173.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-174.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-175.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-176.png)<!-- -->![](thesis_data_files/figure-gfm/unnamed-chunk-26-177.png)<!-- -->
