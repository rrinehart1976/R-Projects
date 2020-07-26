# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART First Sales Analysis ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)


# 2.0 Importing Files ----
bikes_tbl <- read_excel(path="00_data/bike_sales/data_raw/bikes.xlsx")
bikeshops_tbl <- read_excel(path="00_data/bike_sales/data_raw/bikeshops.xlsx")
orderlines_tbl <- read_excel(path="00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining Data ----
bikes_tbl
glimpse(orderlines_tbl)


# 4.0 Joining Data ----
inner_join(orderlines_tbl, bikes_tbl, by=c("product.id" = "bike.id"))

bike_orderlines_joined <- orderlines_tbl %>% 
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))    
glimpse(bike_orderlines_joined)


# 5.0 Wrangling Data ----

bike_orderlines_wrangled <- bike_orderlines_joined %>% 
    separate(description, into = c("category.1", "category.2", "frame.material"), sep = " - ", remove = TRUE) %>% 
    separate(location, into = c("city", "state"), sep = ", ", remove = FALSE) %>% 
    mutate(total.price = price * quantity) %>% 
    select(-"...1", -location) %>% 
    select(-ends_with(".id")) %>% 
    bind_cols(bike_orderlines_joined %>% select(order.id)) %>% 
    select(contains("date"), contains("id"), contains("order"), quantity, price, total.price, everything()) %>% 
    rename(order_date = order.date) %>% 
    set_names(names(.) %>% str_replace_all("\\.", "_")) 


glimpse(bike_orderlines_wrangled)

# 6.0 Business Insights ----

# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year <- bike_orderlines_wrangled %>% 
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    mutate(sales_text = scales::dollar(sales))


# Step 2 - Visualize
sales_by_year %>% 
    ggplot(aes(year, sales)) +
    geom_col(fill = "#2c3e50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward trend",
        x = "",
        y = "Revenue"
    )


# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat2 <- bike_orderlines_wrangled %>% 
    select(order_date, total_price, category_2) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year, category_2) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    mutate(sales_text = scales::dollar(sales))


# Step 2 - Visualize
sales_by_year_cat2 %>% 
    ggplot(aes(year, sales, fill = category_2)) +
    geom_col() +
    facet_wrap( ~ category_2, ncol = 3, scales = "free_y") +
    geom_smooth(method = "lm", se = FALSE) +
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue",
        fill = "Product Secondary Category"
    )




# 7.0 Writing Files ----
fs::dir_create("00_data/bike_sales/data_wrangled_student/")

# 7.1 Excel ----
bike_orderlines_wrangled %>% 
    write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")


# 7.2 CSV ----
bike_orderlines_wrangled %>% 
    write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled %>% 
    write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")

