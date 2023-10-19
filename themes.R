### tmap theme------------------------------------

tmap_theme <- function(tm_obj, main_title){
  tm_obj +
    tm_layout(frame = FALSE,
              main.title = main_title,
              legend.outside = TRUE,
              inner.margins = c(0.06, 0.10, 0.10, 0.08)
    ) +
    tm_scale_bar(color.dark = "gray60",
                   position = c("right", "bottom")) + 
    tm_compass(type = "4star", size = 2.5, text.size = 0.5,
               color.dark = "gray60", text.color = "gray60",
               position = c("left", "top")) +
    tm_credits("Philadelphia, PA", fontface = "italic", align = "right")
}