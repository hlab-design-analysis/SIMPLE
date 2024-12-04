## Estimate variance SRS
## Credits code: Sara Freyland

## for ref 
#a <- read.table("~/Personal_Eros_locale/varianceImprovement/data/resa1.txt", header=T)
#a
# names(a)
# names(data[[1]])

data <- finalDf_wide_SRS_multiExtraction 
data <- split(data, data$Replica)                              
l <- length(data)

y <- (data %>% map(~.x[,9]-(sum(.x[,9])/sum(.x[,8]))*.x[,8]))[1:l]
n = map(y,~length(.x)) %>% unlist() #sample size, number of buckets
N = (map(data,~.x[1,18]) %>% unlist())[1:l] #total number of buckets
f = n/N #sampling fraction

ym <- y %>% map(~mean(.x)) %>% unlist()
s2 <- Map(function(x, y, z) relist((unlist(x)-y)^2/(z-1), skeleton = x), 
          y, ym, n) %>%
  map(~sum(.x)) %>%
  unlist()
v1 <- (1-f)*s2/n

v2 <- (1-f)*(1/n)*Map(function(x, y) relist(unlist(x)^2/(2*(y-1)), skeleton = x), 
                      y %>% map(~diff(.x)), n) %>%
  map(~sum(.x)) %>%
  unlist()

v3 <- (1-f)*(1/n)*Map(function(x, y) relist((unlist(x))^2/y, skeleton = x), 
                      y %>% map(~.x[seq(3,length(.x),2)] - 
                                  .x[seq(2,length(.x)-1,2)]), n) %>%
  map(~sum(.x)) %>%
  unlist()

v4 <- (1-f)*(1/n)*Map(function(x, y) relist((unlist(x))^2/(6*(y-2)), skeleton = x), 
                      y %>% map(~.x[seq(3,length(.x),1)] - 
                                  2*(.x[seq(2,length(.x)-1,1)]) + 
                                  .x[seq(1,length(.x)-2,1)]), n) %>%
  map(~sum(.x)) %>%
  unlist()

v5 <- (1-f)*(1/n)*Map(function(x, y) relist(unlist(x)^2/(3.5*(y-4)), skeleton = x), 
                      y %>% map(~.x[seq(5,length(.x),1)]/2 - 
                                  .x[seq(4,length(.x)-1,1)] + 
                                  .x[seq(3,length(.x)-2,1)] - 
                                  .x[seq(2,length(.x)-3,1)] + 
                                  .x[seq(1,length(.x)-4,1)]/2), n) %>%
  map(~sum(.x)) %>%
  unlist()

v6 <- (1-f)*(1/n)*Map(function(x, y) relist(unlist(x)^2/(7.5*(y-8)), skeleton = x), 
                      y %>% map(~.x[seq(9,length(.x),1)]/2 -
                                  .x[seq(8,length(.x)-1,1)] +
                                  .x[seq(7,length(.x)-2,1)] -
                                  .x[seq(6,length(.x)-3,1)] +
                                  .x[seq(5,length(.x)-4,1)] - 
                                  .x[seq(4,length(.x)-5,1)] + 
                                  .x[seq(3,length(.x)-6,1)] - 
                                  .x[seq(2,length(.x)-7,1)] + 
                                  .x[seq(1,length(.x)-8,1)]/2), n) %>%
  map(~sum(.x)) %>%
  unlist()

p = 4

#Remove sample for ordered lists
v7 <- (1-f)*(1/(p*(p-1)))*Map(function(x,y) relist((mean(unlist(x))-y)^2, skeleton = x), 
                              Map(function(x, y) relist(split(unlist(x), 
                                                              ceiling(seq_along(x)/(y/p))), 
                                                        skeleton = x), y, n) %>% 
                                unlist(recursive = FALSE), 
                              rep(ym, each=p)) %>%
  unlist() %>%
  matrix(nrow=p) %>%
  colSums()

rho_k <- Map(function(x, y, z, w) relist(((unlist(x)[-1]-y)*
                                            (head(unlist(x),-1)-y))/((z-1)*w), 
                                         skeleton = x), y, ym, n, s2) %>%
  map(~sum(.x)) %>%
  unlist()
v8 <- Map(function(x, y, z, w) relist(if(unlist(x) > 0){
  (1-w)*(y/z)*(1 + 2/log(unlist(x)) + 
                 2/((1/unlist(x)) - 1))
} else {
  (1-w)*y/z #v1
}, skeleton = x), rho_k, s2, n, f) %>% 
  unlist()

df_varianceComparison_SRS_multiExtraction <- data.frame((N^2/((N/n)*((data %>% map(~sum(.x[,8])))[1:l] %>% unlist()))^2) 
                                                       *rbind(v1,v2,v3,v4,v5,v6,v7,v8)) 
colnames(df_varianceComparison_SRS_multiExtraction) = paste0("Replica_", 1:l)

#table with highlighted min-values (columnwise)
reactable(df_varianceComparison_SRS_multiExtraction,
          defaultColDef = colDef(
            style = highlight_min(df_varianceComparison_SRS_multiExtraction)
          )
)

## Summarize the results
# Extract the summary of the variance results
df_varianceComparison_SRS_multiExtraction_summary <- df_varianceComparison_SRS_multiExtraction %>% 
  t() %>% 
  as.data.frame() %>% 
  rowwise() %>% 
  mutate(
    scheme = "SRS", 
    meanVariance = mean(c_across(all_of(2:nVarEstimators))),
    sdVariance = sd(c_across(all_of(2:nVarEstimators))),
    minVariance = min(c_across(all_of(2:nVarEstimators))), 
    maxVariance = max(c_across(all_of(2:nVarEstimators)))
    )

# Extract the name of the estimators having minimum value at a certain replica
df_varianceComparison_SRS_multiExtraction_summary <- df_varianceComparison_SRS_multiExtraction_summary %>% 
  as.data.frame() %>% 
  mutate(minimumVarianceBelongsTo = apply(.[,2:nVarEstimators], 1, function(x) names(x)[which.min(x)]))  %>%  # Credits: https://stackoverflow.com/questions/37196854/r-dplyr-get-name-of-which-min-rowwise
  tibble::rownames_to_column(var = "Replica") %>% 
  relocate(scheme, .before = 1)

# Select only relevant columns 
# df_varianceComparison_SRS_multiExtraction_summary <- df_varianceComparison_SRS_multiExtraction_summary %>% 
#   dplyr::select(
#      Replica, meanVariance, sdVariance, minVariance, maxVariance, minimumVarianceBelongsTo
#   )

# Save as image
table_df_varianceComparison_SRS_multiExtraction_summary <- kable(df_varianceComparison_SRS_multiExtraction_summary) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = TRUE) %>%
  as_image(file = paste0("results_SIMPLE/Simulation", simName, "/results_VarianceComparisonSchemes_", simName,"_multiExtraction_SRS", ".jpg"))

# Create a plot reporting the score of variance estimators 
df_varianceComparison_SRS_multiExtraction_summary %>% 
  group_by(minimumVarianceBelongsTo) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(
    scheme = "SRS"
  )
  




