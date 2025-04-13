dependencies <- c(
    'fourPNO',
    'dplyr'
)

for (d in dependencies) {
  if (!require(d)) {
    install.packages(d)
  }
}