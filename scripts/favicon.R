c1 <- terra::vect(
  data.frame(
    x = c(.5, 1, .5, 0, .5),
    y = c(0, .5, 1, .5, 0)
  ),
  geom = c("x", "y")
) |>
  terra::as.lines() |>
  terra::as.polygons()

l <- sqrt(2) / 2 / 2

c2 <- terra::vect(
  data.frame(
    x = c(.5 + l, .5 + l, .5 - l, .5 - l, .5 + l),
    y = c(.5 - l, .5 + l, .5 + l, .5 - l, .5 - l)
  ),
  geom = c("x", "y")
) |>
  terra::as.lines() |>
  terra::as.polygons()

c3 <- terra::vect(
  data.frame(x = .5, y = .5),
  geom = c("x", "y")
) |>
  terra::buffer(width = .2, quadsegs = 200)


s <- terra::union(c1, c2) |>
  terra::aggregate() |>
  terra::symdif(c3)

png(
  filename = "extras/favicon.png",
  width = 15,
  height = 15,
  units = "cm",
  bg = "transparent",
  res = 300
)
terra::plot(s, col = "#00AF50", border = "transparent", axes = FALSE)
dev.off()
