# Census Highcharts Code

plots = list()

# Population ageing -------------------------------------------------------

plots[["POPULATION_ageing"]] <- highchart() %>%
  hc_plotOptions(
    line = list(marker = list(enabled = T,
                              radius = 6),
                lineWidth = 5)) %>%
  hc_add_theme(hc_theme) %>% 
  hc_add_series(
    round(
      subset(datasets[["POPULATION_ageing"]], age_group == "0 to 14")$percentage,
      digits = 2),
    type = "line",
    lineWidth = 4,
    name = "Under 15",
    color = gss_palette[[1]],
    group = subset(datasets[["POPULATION_ageing"]], age_group == "0 to 14")$year) %>%
  hc_add_series(
    round(
      subset(datasets[["POPULATION_ageing"]], age_group == "65 and over")$percentage,
      digits = 2),
    type = "line",
    lineWidth = 4,
    name = "65 and over",
    color = gss_palette[[4]],
    group = subset(datasets[["POPULATION_ageing"]], age_group == "65 and over")$year)%>%
  hc_xAxis(categories = unique(datasets[["POPULATION_ageing"]]$year)
  ) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           min = 0,
           max = 40) %>%
  hc_title(text = "Figure 2: Scotland's population is ageing",
           align = "left",
           style = list(fontSize = "20px")) %>%
  hc_subtitle(text = "Age groups as a proportion of Scotland's population, 1971 - 2022, Scotland",
              align = "left",
              style = list(fontSize = "20px")
  ) %>%
  hc_legend(itemStyle = list(fontSize = "20px",
                             fontFamily = "Roboto",
                             fontWeight = "light")) %>% 
  hc_tooltip(
    style = list(color = "white"),
    backgroundColor = "black",
    formatter = JS(
      "function () {
            return '<b>' 
            + this.point.category
            + '</b> <br/>'
            + this.series.name
            + '<br/>Population proportion: <b>' 
            + Highcharts.numberFormat(this.point.y, 1) 
            + '%<br/>';}"
    )
  )  %>%
  hc_exporting(
    enabled = TRUE,
    filename = "natural_change_net_migration_projections",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )



# 4 POP MAP  --------------------------------------------------------------

plots[["POPULATION_map"]] <- highchart() %>%
  hc_add_theme(my_theme) %>% 
  hc_add_series_map(
    datasets[["mapjson"]],
    datasets[["POPULATION_map"]],
    value = "percentage_change",
    joinBy = "la_code",
    name = "Population change from 2011",
    borderColor = col_grey_dark,
    borderWidth = 0.5,
    tooltip = list(
      pointFormat = "{point.la_name}: {point.value}",
      valueDecimals = 1,
      valueSuffix = "%"
    )
  ) %>%
  hc_tooltip(
    style = list(color = "white"),
    backgroundColor = "black") %>% 
  hc_colorAxis(min = -5,
               max = 15,
               stops = color_stops(
                 n = 5,
                 colors = c(gss_palette[[4]],
                            "white",
                            "#a6cef0",
                            "#2078c3",
                            gss_palette[[1]])
               )) %>%
  hc_title(text = "Figure 4: Population decreases were concentrated in the west and south west ",
           align = "left",
           style = list(fontFamily = "Roboto",
                        fontSize = "20px")) %>%
  hc_subtitle(text = "Population change, 2011 - 2022, council areas in Scotland",
              align = "left",
              style = list(fontFamily = "Roboto",
                           fontSize = "20px")) %>%
  hc_legend(
    title = list(
      text = "%",
      style = list(fontWeight = "light",
                   fontSize = "20px")),
    align = "left"
  ) %>% 
  hc_mapNavigation(., enabled = TRUE)


# census records --------------------------------------------------------------

plots[["census_records"]] <- highchart() %>%
  hc_add_theme(my_theme) %>% 
  hc_add_series(
    datasets$census_population$persons,
    type = "column",
    name = "People",
    color = gss_palette[[1]]) %>%
  hc_yAxis(
    min = 0,
    title = list(style = list(fontSize = "20px"), 
                 text = "People (millions)"),
    labels = list(
      formatter =  JS("function () {
        return this.value/1000000;
      }")),
    tickPositions = c(0:6*1000000)) %>%
  hc_xAxis(
    categories = c(datasets$census_population$year),
    labels = list(style = list(fontSize = "20px"),
                  rotation = 0),
    plotLines = list(
      list(
        width = 2,
        value = 14,
        zIndex = 5,
        dashStyle = "dash",
        label = list(
          style = list(fontSize = "16px"),
          text = "No census<br>during WWII",
          verticalAlign = "top",
          rotation = 0,
          align = "right",
          y = 15,
          x = -10,
          clip = F
        )
      )
    ),
    tickPositions = c(0, 7, 14, 22)) %>%
  hc_title(text = "Figure 1: Scotland's population continued to increase",
           align = "left",
           style = list(fontSize = "20px")) %>%
  hc_subtitle(text = "Census day population estimates, 1801 - 2022, Scotland",
              align = "left",
              style = list(fontSize = "20px")) %>%
  hc_legend(enabled = F) %>% 
  hc_tooltip(
    style = list(color = "white"),
    backgroundColor = "black",
    formatter = JS(
      "function () {return '<b>'
      + this.point.category
      + '</b> <br/>'
      + 'People: '
      + '<b>'
      + Highcharts.numberFormat(this.point.y, 0)
      + '<br/>';}")) 

# Population Pyramid -------------------------------

plots[["pop_pyr_1921"]] <- highchart() %>%
  hc_plotOptions(bar = list(pointWidth = 15,
                            stacking = "normal"),
                 line = list(marker = list(enabled = F))) %>%
  hc_add_theme(my_theme) %>% 
  hc_add_series(
    datasets$pop_age_5_yr$females_1921,
    type = "line",
    name = "Female, 1921",
    color = gss_palette[[4]],
    lineWidth = 4,
    zIndex = 7
  ) %>%
  hc_add_series(
    datasets$pop_age_5_yr$females_2022,
    type = "bar",
    name = "Female, 2022",
    color = gss_palette[[1]]
  ) %>%
  hc_add_series(
    datasets$pop_age_5_yr$males_2022,
    type = "bar",
    name = "Male, 2022",
    color = gss_palette[[2]]
  ) %>%
  hc_add_series(
    datasets$pop_age_5_yr$males_1921,
    type = "line",
    name = "Male, 1921",
    color = gss_palette[[3]],
    lineWidth = 4
  ) %>%
  hc_xAxis(
    categories = sort(datasets$pop_age_5_yr$age_group),
    reversed = F,
    lineWidth = 0,
    labels = list(align = "right",
                  style = list(fontFamily = "Roboto"))
  ) %>%
  hc_yAxis(
    labels = list(
      formatter = JS("function() {return Highcharts.numberFormat(Math.abs(this.value),0);}"),
      reversed = T,
      style = list(fontSize = "20px",
                   fontFamily = "Roboto")
    ),
    tickPositions = c(-250000,-100000, 0, 100000, 250000),
    reversed = T,
    gridLineWidth = 0,
    title = list(style = list(fontSize = "20px"), 
                 text = "People")
  ) %>%
  hc_title(text = "Figure 3: The structure of Scotland's population is changing ",
           align = "left",
           style = list(fontSize = "20px",
                        fontFamily = "Roboto")) %>%
  hc_subtitle(text = "Population by age group and sex, 1921 - 2022, Scotland ",
              align = "left",
              style = list(fontSize = "20px",
                           fontFamily = "Roboto")) %>%
  hc_legend(itemStyle = list(fontSize = "20px",
                             fontFamily = "Roboto",
                             fontWeight = "light")) %>% 
  hc_tooltip(
    style = list(color = "white"),
    backgroundColor = "black",
    formatter = JS(
      "function () {
            return '<b>'
            + this.series.name
            + '</b> <br/>'
            + 'Age group: '
            + this.point.category
            + '<br/>'
            + 'People: '
            + '<b>'
            + Highcharts.numberFormat(Math.abs(this.point.y), 0)
            + '<br/>';}"
    )
  ) %>% hc_add_dependency(name = "modules/sonification.js")
