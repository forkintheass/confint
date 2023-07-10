# confint visualisations package

Set of data visualisation ideas created in R.

R is just an instrument, any other tool can be used, if necessary.

Each separate file is one type of the graph. Follow the description of each one.

------------------------------------------------------------------------

## and_candles_for_all.R

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

...the columns can have different names. But you have to specify them in your code then.

...or use built-in data generation.

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

------------------------------------------------------------------------

## distributions.R

Builds distributions which reflects quantity of responses within the specified interval and over it.

#### Examples

![](images/distrib_single.png) ![](images/distrib_compar.png)

#### Data

Input the file with the following columns:

```         
response,group
1605,Login
1034,GetTime
93,Logout
```

Or use built-in data generation.

#### Idea

This is an enhanced distribution diagram.

The diagram divides data into intervals within a specified limit (for example, a specified SLA time) and also shows data exceeding this limit.

Comparing these distributions can be challenging. Overlaying semi-transparent layers makes it difficult to distinguish values between data samples. Pattern-applied bars are suitable for this purpose, though not all tools support them.

Additionally, the diagram includes statistics for the median value (which can be replaced with other statistics, such as the 95th percentile) and indicates when the median is outside the main chart's bounds.

#### Usage

For single distribution: `distrib_single(data_original, your_request_type_to_filter, your_sla))`.

For comparison distribution: `distrib_single(data_original, data_original_name_for_legend, data_comparison, data_comparison_name_for_legend, your_request_type_to_filter, your_sla))`.

#### Settings

Use `####CHART SETTINGS####` section to specify look and feel.
