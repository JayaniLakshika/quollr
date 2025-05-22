#Import necessary packages
library(hexSticker)
library(here)
library(showtext)

# Loading Google fonts
font_add_google("Rubik Maze", "rubik", db_cache = FALSE)
#Automatically use showtext to render text for future device
showtext_auto()
#img <- system.file("leaf.png", package  = "MedLEA")
sticker(here("hexsticker/quoll_bkg.png"), package = "quollr",
        p_size = 20, p_y = 1.5, s_x = 1, s_y =.88, s_width = .6,
        s_height = 0.3, h_fill = "#8c510a", p_color = "#f7f7f7", h_color = "#dfc27d",
        p_family = "rubik", filename = here("hexsticker/sticker.png"))

