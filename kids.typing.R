suppressMessages(library(tidyverse))

add_raw <- function(row, names, shifts, widths, spaces) {
  a <- tibble(y = row, name = names)
  if (missing(shifts)) {
    a <- a %>% mutate(shift = "")
  } else {
    a <- a %>% cbind(shift = shifts)
  }
  if (missing(widths)) {
    a <- a %>% mutate(w = 1)
  } else {
    a <- a %>%
      left_join(tibble(name = names(widths), w = widths), by = "name") %>%
      mutate(w = ifelse(is.na(w), 1, w))
  }
  if (missing(spaces)) {
    a <- a %>% mutate(space = 0)
  } else {
    a <- a %>%
      left_join(tibble(name = names(spaces), space = spaces), by = "name") %>%
      mutate(space = ifelse(is.na(space), 0, space))
  }
  a <- a %>% mutate(x = head(c(0, cumsum(w + space)), n()))
  return(a)
}

build_keyboard_layout <- function() {
  a <- rbind(add_raw(row = 6,
                     names = c("Esc", paste0("F", 1:12)),
                     widths = c("Esc" = 1.5),
                     spaces = c("Esc" = .5, "F4" = .5, "F8" = .5)),
             add_raw(row = 5,
                     names = c("`", 1:9, 0, "-", "=", "Backspace"),
                     shifts = c("~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "+", ""),
                     widths = c("Backspace" = 2)),
             add_raw(row = 4,
                     names = c("Tab", "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "[", "]", "\\"),
                     widths = c("Tab" = 1.5, "\\" = 1.5)),
             add_raw(row = 3,
                     names = c("CapsLock", "A", "S", "D", "F", "G", "H", "J", "K", "L", ";", "\'", "Enter"),
                     widths = c("CapsLock" = 2, "Enter" = 2)),
             add_raw(row = 2,
                     names = c("Shift", "Z", "X", "C", "V", "B", "N", "M", ",", ".", "/", "Shift"),
                     widths = c("Shift" = 2.5)),
             add_raw(row = 1,
                     names = c("Ctrl", "Alt", " ", "Alt", "Ctrl"),
                     widths = c("Ctrl" = 1.5, "Alt" = 1.5, " " = 7),
                     spaces = c("Ctrl" = .5, "Alt" = .5, " " = .5)))
  return(a)
}

draw_keyboard <- function(d, pressed = "", message = "", target = "", typed = "", newpage = FALSE) {
  g <- d %>%
    left_join(tibble(name = pressed, pressed = TRUE), by = "name") %>%
    mutate(pressed = !is.na(pressed)) %>%
    ggplot() +
    geom_rect(aes(xmin = x, ymin = y, xmax = x + w - .2, ymax = y + .8, fill = pressed),
              color = "#333333") +
    geom_text(aes(x = x + .2, y = y + .2 + ifelse(shift == "", .2, 0), label = name),
              hjust = 0, vjust = .5) +
    geom_text(aes(x = x + .2, y = y + .6, label = shift),
              hjust = 0, vjust = .5) +
    scale_fill_manual(values = c("#ccffcc", "#ccccff")) +
    guides(fill = FALSE) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              data = tibble(xmin = min(d$x) - 1, xmax = max(d$x) + 1,
                            ymin = max(d$y) + 1, ymax = max(d$y) + 3),
              fill = "white") +
    geom_text(aes(x = x, y = y, label = label),
              data = tibble(x = sum(range(d$x)) / 2, y = max(d$y) + 2, label = message),
              fontface = "bold", size = 12, color = "red") +
    geom_text(aes(x = x, y = y, label = label),
              data = tibble(x = 0, y = max(d$y) + 2.2, label = target),
              fontface = "bold", size = 10, color = "black", hjust = 0, vjust = .5) +
    geom_text(aes(x = x, y = y, label = label),
              data = tibble(x = 0, y = max(d$y) + 1.5, label = typed),
              fontface = "bold", size = 10, color = "blue", hjust = 0, vjust = .5) +
    theme_void()
  print(g, newpage = newpage)
  if (newpage) {
    setGraphicsEventHandlers(onKeybd = function(x) {
      print(x)
      mapping <- c("\033" = "Esc",
          "\r" = "Enter",
          "\t" = "Tab",
          "\b" = "Backspace")
      if (x %in% names(mapping)) x = mapping[x]
      return(x)
    })
  }
}

demo <- function() {
  kbd <- build_keyboard_layout()
  message <- "Press <Esc> twice to exit"
  draw_keyboard(kbd, "", message, newpage = TRUE)
  esc_pressed = FALSE
  while (TRUE) {
    v <- getGraphicsEvent()
    if (is.null(v)) break
    if (v == "Esc") {
      if (esc_pressed) break
      esc_pressed = TRUE
      message <- "Press <Esc> again to exit"
    } else {
      esc_pressed = FALSE
      message <- "Press <Esc> twice to exit"
    }
    if (v %in% letters) {
      v <- str_to_upper(v)
    }
    draw_keyboard(kbd, v, message)
  }
  dev.off()
}

play <- function() {
  kbd <- build_keyboard_layout()
  target <- paste(LETTERS, collapse = "")
  to_exit <- FALSE
  while (!to_exit) {
    typed <- ""
    draw_keyboard(kbd, "", "", target, typed, newpage = TRUE)
    esc_pressed = FALSE
    while (!to_exit) {
      v <- getGraphicsEvent()
      if (is.null(v)) { to_exit = TRUE; break }
      if (v == "Esc") {
        if (esc_pressed) { to_exit = TRUE; break }
        esc_pressed = TRUE
        message <- "Press <Esc> again to exit"
      } else {
        esc_pressed = FALSE
        message <- "Press <Esc> twice to exit"
      }
      if (v %in% letters) {
        v <- str_to_upper(v)
      }
      if (v %in% LETTERS) {
        offset <- nchar(typed) + 1
        if (substr(target, offset, offset) == v) {
          typed <- paste0(typed, v)
        }
      }
      if (typed == target) break
      draw_keyboard(kbd, v, "", target, typed)
    }
    if (to_exit) break
    draw_keyboard(kbd, "", "Congratuations!", newpage = TRUE)
    Sys.sleep(3)
    target <- paste(sample(LETTERS, 10, replace = TRUE), collapse = "")
  }
  dev.off()
}
