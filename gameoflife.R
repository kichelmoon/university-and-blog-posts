count_neighbors <- function(grid) {
  n <- nrow(grid)
  m <- ncol(grid)
  neighbors <- matrix(0, nrow = n, ncol = m)
  
  for (r in 1:n) {
    for (c in 1:m) {
      # With wrapping
      r_indices <- ((r + c(-1, 0, 1) - 1) %% n) + 1
      c_indices <- ((c + c(-1, 0, 1) - 1) %% m) + 1
      
      sub_grid <- grid[r_indices, c_indices]
      neighbors[r, c] <- sum(sub_grid) - grid[r, c]
    }
  }
  return(neighbors)
}

step_game <- function(grid) {
  neighbors <- count_neighbors(grid)
  new_grid <- grid
  
  new_grid[grid == 1 & (neighbors < 2 | neighbors > 3)] <- 0
  new_grid[grid == 0 & neighbors == 3] <- 1
  
  return(new_grid)
}

plot_grid <- function(grid, title) {
  n <- nrow(grid)
  image(1:n, 1:n, t(grid[n:1, ]), 
        col = c("white", "black"), 
        axes = FALSE, 
        xlab = "", ylab = "", 
        main = title)
  box()
  grid(nx = n, ny = n, col = "gray", lty = 1)
}
grid_size <- 10
grid <- matrix(0, nrow = grid_size, ncol = grid_size)

# Different starting objects, choose one in live_coords
square <- matrix(c(3,3,3,4,4,3,4,4), ncol = 2, byrow = TRUE)
blinker <- matrix(c(4,4,5,4,6,4), ncol = 2, byrow = TRUE)
complex <- matrix(c(8,2,7,4,8,4, 5,3,5,4,5,5,7,2,7,3,7,4,8,3), ncol = 2, byrow = TRUE)
live_coords <- square
grid[live_coords] <- 1

history <- list()
history[[1]] <- grid

for (round in 2:10) {
  grid <- step_game(grid)
  history[[round]] <- grid
}

par(mfrow = c(1, 3), mar = c(2, 2, 3, 1))
plot_grid(history[[1]],  "Round 1")
plot_grid(history[[2]],  "Round 2")
plot_grid(history[[10]], "Round 10")
par(mfrow = c(1, 1))