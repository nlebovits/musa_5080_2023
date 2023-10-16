### tmap theme------------------------------------

tmap_theme <- function(tm_obj, main_title){
  tm_obj +
    tm_layout(frame = FALSE,
              main.title = main_title,
              legend.outside = TRUE
    ) +
    tm_credits("Philadelphia, PA", fontface = "italic", align = "right") +
    tm_credits("Figure 3", fontface = "bold", align = "right")
}