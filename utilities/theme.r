# theme for the plot :)

theme_new <- function (legend.pos = "right", style = c("white", "light", "dark_blue", 
    "dark_gray"), base_size = 18, base_family = "", base_line_size = base_size/22, 
    base_rect_size = base_size/22) {
    text_and_line_color <- switch(style[1], white = "#34495e", 
        light = "#34495e", dark_blue = "#ecf0f1", dark_gray = "#ecf0f1", 
        light_dark = "#34495e")
    bg_color <- switch(style[1], white = "#FFFFFF", light = "#ecf0f1", 
        dark_blue = "#34495e", dark_gray = "#3c3c3c", light_dark = "#3c3c3c")
    all_bg <- switch(style[1], white = "#FFFFFF", light = "#ecf0f1", 
        dark_blue = "#34495e", dark_gray = "#3c3c3c", light_dark = "#dddddd")
    grid_color <- switch(style[1], white = "#e8e8e8", light = "#bdc3c7", 
        dark_blue = "#46627f", dark_gray = "#777777", light_dark = "#777777")
    plot <- theme_minimal(base_size = base_size, base_family = base_family, 
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
        ggplot2::theme(line = element_line(colour = text_and_line_color, 
            linewidth = 0.5, linetype = 1, lineend = "butt", 
            arrow = FALSE), text = element_text(colour = text_and_line_color, 
            face = "plain", family = base_family, size = 11, 
            hjust = 0.5, vjust = 0.5, lineheight = 1, margin = margin(5, 
                5, 5, 5, "pt"), angle = 0, debug = FALSE), rect = element_rect(colour = text_and_line_color, 
            fill = bg_color, linewidth = 0.5, linetype = 1), 
            plot.background = element_rect(fill = all_bg, colour = all_bg), 
            plot.title = element_text(face = "bold", family = base_family, 
                margin = margin(10, 5, 10, 5, "pt"), hjust = 0, 
                size = 14), panel.background = element_rect(fill = bg_color, 
                colour = bg_color, linewidth = 0), axis.title = element_text(face = "bold", 
                family = base_family), legend.title = element_text(face = "bold", 
                family = base_family), strip.text = element_text(face = "bold", 
                family = base_family), legend.key.width = ggplot2::unit(1.5, 
                "lines"), legend.key = element_rect(colour = bg_color), 
            panel.grid.major = element_line(linetype = "longdash", 
                colour = grid_color), panel.grid.minor = element_blank(), 
            panel.spacing.x = ggplot2::unit(2, "lines"), axis.text.x = element_text(colour = text_and_line_color, 
                family = base_family), axis.text.y = element_text(colour = text_and_line_color, 
                family = base_family), plot.caption.position = "plot", 
            complete = TRUE)
    if (legend.pos == "topleft") {
        plot <- plot + ggplot2::theme(legend.position = c(0.05, 
            0.95), legend.justification = c(0.05, 0.95))
    }
    else if (legend.pos == "topright") {
        plot <- plot + ggplot2::theme(legend.position = c(0.95, 
            0.95), legend.justification = c(0.95, 0.95))
    }
    else if (legend.pos == "topmiddle") {
        plot <- plot + ggplot2::theme(legend.position = c(0.5, 
            0.95), legend.justification = c(0.5, 0.95))
    }
    else if (legend.pos == "bottomleft") {
        plot <- plot + ggplot2::theme(legend.position = c(0.05, 
            0.05), legend.justification = c(0.05, 0.05))
    }
    else if (legend.pos == "bottomright") {
        plot <- plot + ggplot2::theme(legend.position = c(0.95, 
            0.05), legend.justification = c(0.95, 0.05))
    }
    else if (legend.pos == "bottommiddle") {
        plot <- plot + ggplot2::theme(legend.position = c(0.5, 
            0.05), legend.justification = c(0.5, 0.05))
    }
    else if (legend.pos == "none") {
        plot <- plot + ggplot2::theme(legend.position = "none")
    }
    else {
        plot <- plot + ggplot2::theme(legend.position = legend.pos)
    }
    return(plot)
}
