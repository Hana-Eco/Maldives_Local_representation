# =============================================================================== ~
#        ~ Timeline of Marine Research and Education in the Maldives ~
# =============================================================================== ~
#
# Timeline extends nearly two centuries from 1834 to 2026
#
# ~> Load packages ####
library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)
#
# ===============================================================================
#                     #### ~ Set up timeline data ~ ####
# =============================================================================== ~
#
timeline<-read.csv("MDV_Hist.csv", stringsAsFactors = TRUE)%>%
  clean_names()%>%
  mutate(
    # check which events start and end in the same year
    point = end == start,
    # adjust ordering so events come in timeline order and not alphabetical order
    event = fct_reorder(event, start, .desc = TRUE)
  )

#
# ===============================================================================
#                     #### ~ Plot timeline v1 ~ ####
# =============================================================================== ~
#
#### ===> 1. compressed year axis <=== ####

knots <- c(1830, 1900, 1980, 2000, 2004, 2013, 2026)
w     <- c(0.25, 0.30, 1.00, 0.60, 1.60, 3.00)
pos_k <- c(0, cumsum(diff(knots) * w))

yr2pos <- function(y) approx(knots, pos_k, y, rule = 2)$y
pos2yr <- function(p) approx(pos_k, knots, p, rule = 2)$y

squish_rev <- new_transform(
  "squish_rev",
  transform = function(y) -yr2pos(y),
  inverse   = function(p) pos2yr(-p)
)

#### ===> 2. column layout: one allocator per type <==== ####

gap  <- 8
step <- 1

tiers   <- c(National = 3, International = 4, Joint = 4)
col_dir <- c(National = -1, International = 1, Joint = 1)
col_off <- c(National = 0, International = 0, Joint = 11)
wrap    <- c(National = 32, International = 38, Joint = 32)

outer_col <- "Joint"

assign_lanes <- function(pos, n_lanes, gap) {
  occ  <- rep(-Inf, n_lanes)
  lane <- integer(length(pos))
  for (i in seq_along(pos)) {
    free    <- which(occ + gap < pos[i])
    lane[i] <- if (length(free)) min(free) else which.min(occ)
    occ[lane[i]] <- pos[i]
  }
  lane
}

timeline <- timeline |>
  mutate(type = as.character(type), pos = yr2pos(start)) |>
  arrange(type, pos) |>
  group_by(type) |>
  mutate(lane = assign_lanes(pos, tiers[type[1]], gap)) |>
  ungroup() |>
  mutate(
    x     = col_dir[type] * lane * step + col_off[type],
    hj    = if_else(col_dir[type] == 1, 0, 1),
    stem0 = if_else(type == outer_col, col_off[[outer_col]] - 1.5, 0),
    label = map2_chr(as.character(event), unname(wrap[type]), str_wrap)
  )

x_lo <- -tiers[["National"]] - 4
x_hi <- col_off[[outer_col]] + tiers[[outer_col]] + 5

#### ===> 3. plot <=== ####

plot.time<-ggplot(timeline, aes(y = start)) +
  geom_hline(yintercept = 1980, colour = "grey75",
             linewidth = .3, linetype = 2) +
  annotate("text", x = x_hi, y = 1980, label = "scale change",
           hjust = 1, vjust = -0.6, size = 2, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_segment(aes(x = stem0, xend = x, yend = start),
               colour = "grey85", linewidth = .25) +
  geom_point(aes(x = x, colour = type), size = 2) +
  geom_text_repel(
    aes(x = x + col_dir[type] * .35, label = label, hjust = hj),
    colour = "grey20",
    direction = "y", segment.size = .2, segment.colour = "grey80",
    min.segment.length = 0, box.padding = .15, max.overlaps = Inf,
    size = 2.3, lineheight = .9
  ) +
  scale_y_continuous(
    transform = squish_rev,
    breaks = c(1830, 1880, 1930, seq(1980, 2026, 5)),
    expand = expansion(mult = .02)
  ) +
  scale_x_continuous(limits = c(x_lo, x_hi)) +
  labs(x = NULL, y = NULL, colour = NULL,
       caption = "Year axis compressed before 1980") +
  scale_color_manual(values = c("#0072B2","#e69f00","#009E73"))+
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 3, colour = "grey90"),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size = 6, colour = "grey50")
  )
plot.time

### ==> save plot
ggsave(plot.time, 
       filename = "pub_outputs/MDV_timeline.pdf",
       dpi = 600, width = 183, height = 247, units = "mm", scale = 1)

#span <- diff(range(pos_k))
#ggsave("timeline.pdf", p,
#       width = 13, height = span / gap * 0.30 + 1.5, limitsize = FALSE)


#
# ===============================================================================
#                     #### ~ Plot timeline v2 ~ ####
# =============================================================================== ~
#

#### ===> 1. compressed year axis <=== ####

knots <- c(1830, 1900, 1980, 2000, 2005, 2015, 2026) #(1830, 1900, 1960, 1995, 2030)
w     <- c(0.1,  0.20, 1.00, 0.80, 1.8, 3.00)      # vertical room per year, by era, OG (0.30, 0.55, 1.00, 2.40)  
pos_k <- c(0, cumsum(diff(knots) * w))

yr2pos <- function(y) approx(knots, pos_k, y, rule = 2)$y
pos2yr <- function(p) approx(pos_k, knots, p, rule = 2)$y

squish_rev <- new_transform(            # scales < 1.3: use trans_new()
  "squish_rev",
  transform = function(y) -yr2pos(y),
  inverse   = function(p) pos2yr(-p)
)

#### ===>2. lane assignment, one allocator per side <=== ####

gap   <- 8                              # label spacing, in position units
tiers <- c(left = 2, right = 4)         # lanes per side
step  <- 1

assign_lanes <- function(pos, n_lanes, gap) {
  occ  <- rep(-Inf, n_lanes)
  lane <- integer(length(pos))
  for (i in seq_along(pos)) {
    free    <- which(occ + gap < pos[i])
    lane[i] <- if (length(free)) min(free) else which.min(occ)
    occ[lane[i]] <- pos[i]
  }
  lane
}

timeline <- timeline |>
  mutate(
    pos  = yr2pos(start),
    side = if_else(type == "National", -1, 1)
  ) |>
  arrange(side, pos) |>
  group_by(side) |>
  mutate(
    lane = assign_lanes(pos, if (side[1] == -1) tiers[["left"]] else tiers[["right"]], gap),
    x    = side * lane * step
  ) |>
  ungroup() |>
  mutate(label = str_wrap(as.character(event), 26))

#### ===> 3. plot <=== ####

plot.time2<-ggplot(timeline, aes(y = start)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_segment(aes(x = 0, xend = x, yend = start),
               colour = "grey80", linewidth = .3) +
  geom_point(aes(x = x, colour = type), size = 2) +
  geom_text_repel(
    aes(x = x + side * .12, label = label,
        hjust = if_else(side == 1, 0, 1), colour = type),
    direction = "y", segment.size = .2, min.segment.length = 0,
    box.padding = .15, max.overlaps = Inf,
    size = 2.3, lineheight = .9, show.legend = FALSE
  ) +
  scale_y_continuous(
    transform = squish_rev,
    breaks = c(seq(1830, 1970, 25), seq(1980, 2026, 5)),
    expand = expansion(mult = .02)
  ) +
  scale_x_continuous(
    limits = c(-tiers[["left"]] - 3.5, tiers[["right"]] + 3.5)
  ) +
  labs(x = NULL, y = NULL, colour = NULL) +
  scale_color_manual(values = c("#0072B2","#e69f00","#009E73"))+
  theme_minimal(base_size = 8) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 3, colour = "grey90"),
    panel.grid.minor.y = element_blank(),
    axis.text.x  = element_blank(),
    legend.position = "bottom"
  )
plot.time2
#### ===> 4. save <=== ####

#span <- diff(range(pos_k))
#ggsave("pub_outputs/MDV_timeline_v2.pdf", 
#       plot.time2,
#       width = 9, height = span / gap * 0.30 + 1.5, limitsize = FALSE, dpi = 600)

ggsave(plot.time2, 
       filename = "pub_outputs/MDV_timeline_v2.pdf",
       dpi = 600, width = 183, height = 247, units = "mm", scale = 1)

#
# ===============================================================================
#                     #### ~ Plot timeline v3 ~ ####
# =============================================================================== ~
#
#### ===> 1. compressed year axis <=== ####

knots <- c(1830, 1900, 1980, 2000, 2004, 2013, 2026)
w     <- c(0.25, 0.30, 1.00, 0.60, 1.60, 3.00)
pos_k <- c(0, cumsum(diff(knots) * w))

yr2pos <- function(y) approx(knots, pos_k, y, rule = 2)$y
pos2yr <- function(p) approx(pos_k, knots, p, rule = 2)$y

squish_rev <- new_transform(            # scales < 1.3: use trans_new()
  "squish_rev",
  transform = function(y) -yr2pos(y),
  inverse   = function(p) pos2yr(-p)
)

#### ===> 2. layout parameters <=== ####

gap    <- 2        # numbers need far less room than labels
step_n <- 1.1      # x-distance between lanes

tiers   <- c(National = 3, International = 3, Joint = 3)
col_dir <- c(National = -1, International = 1, Joint = 1)
col_off <- c(National = 0, International = 0, Joint = 4.5)

pal <- c(International = "#D62728", Joint = "#2CA02C", National = "#1F77B4")

assign_lanes <- function(pos, n_lanes, gap) {
  occ  <- rep(-Inf, n_lanes)
  lane <- integer(length(pos))
  for (i in seq_along(pos)) {
    free    <- which(occ + gap < pos[i])
    lane[i] <- if (length(free)) min(free) else which.min(occ)
    occ[lane[i]] <- pos[i]
  }
  lane
}

timeline <- timeline |>
  mutate(type = as.character(type)) |>
  arrange(start) |>
  mutate(n = row_number(), pos = yr2pos(start)) |>
  arrange(type, pos) |>
  group_by(type) |>
  mutate(lane = assign_lanes(pos, tiers[type[1]], gap)) |>
  ungroup() |>
  mutate(x = col_dir[type] * lane * step_n + col_off[type])

x_lo <- -tiers[["National"]] * step_n - 1
x_hi <- col_off[["Joint"]] + tiers[["Joint"]] * step_n + 1

#### ===> 3. timeline panel <=== ####

p_time <- ggplot(timeline, aes(y = start)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_segment(aes(x = 0, xend = x, yend = start),
               colour = "grey88", linewidth = .2) +
  geom_point(aes(x = x, colour = type), size = 3.2) +
  geom_text(aes(x = x, label = n), size = 1.7, colour = "white") +
  scale_colour_manual(values = pal) +
  scale_y_continuous(
    transform = squish_rev,
    breaks = c(1830, 1880, 1930, seq(1980, 2026, 5)),
    expand = expansion(mult = .02)
  ) +
  scale_x_continuous(limits = c(x_lo, x_hi)) +
  labs(x = NULL, y = NULL, colour = NULL,
       caption = "Year axis compressed before 1980") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 3, colour = "grey90"),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size = 6, colour = "grey50")
  )
p_time

#### ===> 4. key panel <=== ####

n_cols  <- 3
per_col <- ceiling(nrow(timeline) / n_cols)

key <- timeline |>
  arrange(n) |>
  mutate(
    row  = (n - 1) %% per_col,
    kcol = (n - 1) %/% per_col,
    text = paste0(n, ". ", event, " (", start, ")")
  )

p_key <- ggplot(key, aes(x = kcol, y = -row)) +
  geom_text(aes(label = text, colour = type),
            hjust = 0, size = 2.1, show.legend = FALSE) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(limits = c(-0.05, n_cols)) +
  theme_void() +
  theme(plot.margin = margin(5, 5, 5, 5))

#### ===> 5. combine and save <=== ####

library(cowplot)
out <- plot_grid(p_time, p_key, ncol = 2, rel_widths = c(1, 1.4))
out

span <- diff(range(pos_k))
ggsave("timeline_numbered.pdf", out,
       width = 16,
       height = max(span / gap * 0.12, 0.14 * per_col) + 1,
       limitsize = FALSE)

