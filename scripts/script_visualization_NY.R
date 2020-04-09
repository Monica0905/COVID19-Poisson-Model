
library(plotly)

# unemp data
df <- read.csv(choose.files())
df <- subset(df, select = c(county_name,state_name,current_total))

df$county_name <- tolower(df$county_name)

ny.cases <- df %>%
  filter(state_name == 'NY')

  
# county data
ny <- map_data("county") %>%
  filter(region == 'new york')
# merge enemp and county data
ny_cases <- merge(ny, ny.cases, by.x = "subregion", by.y = "county_name")

ny_cases$rate_cat <- cut(ny_cases$current_total, breaks = c(0,100,500,1000,5000,10000,20000), 
                              labels=c("0-100","100-500","500-1000","1000-5000","5000-10000"
                                       ,">10000"))
#check whether the county name is matched
unique(ny$subregion)==ny.cases$county_name

# layout style
blank_layer <- list(
  title = "",
  showgrid = F,
  showticklabels = F,
  zeroline = F)

# plotly object
p <- ny_cases %>%
  group_by(group) %>%
  plot_ly(x = ~long, 
          y = ~lat, 
          color = ~rate_cat, 
          colors = "Blues") %>%
  add_polygons(line = list(width = 0.4)) %>%
  add_polygons(
    fillcolor = 'transparent',
    line = list(color = 'black', width = 0.5),
    showlegend = FALSE
  ) %>%
  layout(
    title = "ny cases",
    title = list(font = 10),
    hovermode = 'x',
    xaxis = blank_layer,
    yaxis = blank_layer
  )

p <- style(p, hoverinfo = 'none', traces = c(6:10)) # remove state boundary hoverinfo
p
