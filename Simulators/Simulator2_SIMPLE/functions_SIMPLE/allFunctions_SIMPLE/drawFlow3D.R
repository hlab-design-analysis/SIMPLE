library(plotly)
a <- lapply(1:length(ltb), function(x){
  ts <- x
  cbind(
  ltb[[x]] %>% 
    melt() %>% 
    filter(Var3 == 3) %>% 
    rename(
      Weight = value
    ), 
  ltb[[x]] %>% 
    melt() %>% 
    filter(Var3 == 2) %>% 
    rename(
      Species = value
    ) %>% 
    select(
      Species
    )
  ) %>% 
    mutate(
      timeStep = as.numeric(ts)
      )
  })
a <- do.call(rbind, a)
a


a$Species = ifelse(a$Species == 1, "herring", ifelse(a$Species == "2", "sprat", "empty"))

a <- a %>% filter(timeStep != 1)

p <- plot_ly(a, x = ~Var1, 
        y = ~Var2, 
        z = ~Weight, 
        color = ~Species, 
        frame = ~timeStep, 
        colors = c("herring" = "blue", 
                   "sprat" = "red",
                   "empty" = NA),
        type = "scatter3d", 
        mode = "markers") %>% 
  add_markers() %>%
  layout(
    xaxis = list(
      range=c(0,100)
    )
  ) 


animation_opts(
  p, 
    frame = 500,
    easing = "linear",
    redraw = TRUE,
    mode = "immediate"
  )

    