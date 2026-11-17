
If we want to do this over and over again have to do the 5 boroughs

Fix this with iteration
```{r}
nyc_airbnb |> 
  nest(datasets = -borough)

airbnb_lm = function(data) {
  
  lm(price ~ stars + room_type, data = data)
}
```


```{r}
nyc_airbnb |> 
  nest(datasets = -borough) |> 
  mutate(fits = map(datasets, airbnb_lm))
```
by bourgh fits linear models to data

```{r}
nyc_airbnb |> 
  nest(datasets = -borough) |> 
  mutate(
    fits = map(datasets, airbnb_lm),
    results = map(fits, broom::tidy)
  ) |> 
  pull(results)

nyc_airbnb |> 
  nest(datasets = -borough) |> 
  mutate(
    fits = map(datasets, airbnb_lm),
    results = map(fits, broom::tidy)
  ) |> 
  select(-datasets, -fits) |> 
  unnest(results) |> 
  select(borough, term, estimate) |> 
  pivot_wider(
    names_from = term,
    values_from = estimate
  )

```

Before create one function and plugs in input where it's supposed to go, happens often enough that 
  -create anonymous function 
  -puts argument and 

```{r}
nyc_airbnb |> 
  nest(datasets = -borough) |> 
  mutate(
    fits = map(datasets, lm(price ~ stars + room_type, data = data)),
    results = map(fits, broom::tidy)
  ) |> 
  pull(results)

nyc_airbnb |> 
  nest(datasets = -borough) |> 
  mutate(
    fits = map(datasets, airbnb_lm),
    results = map(fits, broom::tidy)
  ) |> 
  select(-datasets, -fits) |> 
  unnest(results) |> 
  select(borough, term, estimate) |> 
  pivot_wider(
    names_from = term,
    values_from = estimate
  )

```

```{r}
nyc_airbnb |> 
  filteR(borough == "linear_models.Rmdanhattan") |> 
  nest(datasets = -neighborhood) |> 
  mutate(
    fits = map(datasets, \(df) lm(price ~ stars + room_type, data =df)),
    results = map(fits, broom::tidy)
  ) |> 
  select(neighborhood, results) |> 
  unnest(resutls) |> 
  filter(term == "room_typePrivate room") |> 
  ggplot(aes(x = neighborhood, y = estimate)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))
```
nest data according to neighborhood, tidy up with broom tidy, look at results: is room vs entire apartment different in Manhattan

working the way I'm excpecting to: data sets fits column and results column
-one factor could be dropping between different neighborhoods

## Logistic regression

```{r}
baltimore_df = 
  read_csv("data/homicide-data.csv") |> 
  filter(city == "Baltimore") |> 
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"),
    victim_age = as.numeric(victim_age)
  )
baltimore_df
```


Fit the logistic regression

```{r}
fit_logistic = 
  baltimore_df |> 
  mutate(victim_race = fct_infreq(victim_race)) |> 
  glm(resolved ~ victim_age + victim_race, data = _, family = binomial())



fit_logistic |> 
  broom::tidy() |> 
  mutate(OR = exp(estimate)) |> 
  select(term, OR, estimate, p.value) |> 
  knitr::kable(digits = 3)
```

Binomial just goes with logistic regression