# using api key (general example)
library(fredr)
library(tidyverse)
library(lubridate)

df <- some_api_function("FRED_series_we_want", key = Sys.getenv("FRED_API_Key"))

df <- fredr(
        series_id = "GNPCA",
        observation_start = as.Date("1948-01-01"), 
        observation_end = as.Date("2020-01-01")
)

ggplot(df, aes(x = date, y = value)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x="Date", y="2012 USD (Billions)",
    title="US Real Gross National Product", caption="Source: FRED"
  )

# now data.gov api
library(rscorecard)
# set key
sc_key(Sys.getenv("USGOV_API_KEY"))

# download some data on education
df <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm, stabbr, ugds) %>% 
  sc_year("latest") %>% 
  sc_get()
