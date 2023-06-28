# confint visualisations package

## and_candles_for_all

Builds candles which reflect percentiles, outliers and pass/fail ratio.

#### Example

![](images/clipboard-3874127004.png)

#### Data

Input the file with the following columns:

```         
timeStamp,elapsed,label,success
2024/02/19 14:07:28.070,1605,Login,TRUE
2024/02/19 14:08:28.070,1034,GetTime,TRUE
2024/02/19 14:09:28.070,93,Logout,FALSE
```

Or use built-in data generation.

#### Idea

This one is enhance boxplot with whiskers chart to produce more accessible visualisation of changing response time.

Splits the entire input time interval to the defined quantity of candles and visualises data as the candle of:

-   defined lower percentile (`default: min value or 0p`)

-   defined higher percentile (`default: 95p`)

-   ratio of passed and failed requests during each interval (success = TRUE / FALSE)

-   all outliers higher than higher percentile

    -   main chart contains all the data within multiplier of max of observed 95p value (`default: 1.5`)

    -   additional squeezed chart contains all the data above main chart

    -   outliers are colorized in the same way as success = TRUE / FALSE

-   Legend is added.

#### Usage

Call `generate_candles_chart(your_data, name_of_your_label)`.

#### Settings

Use `####CHART SETTINGS####`
