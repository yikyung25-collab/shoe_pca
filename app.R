# ============================================================
# app.R : Explain PCA (Step 1~6 + cumulative variance)
# engine.R 만 source 한다 (build_pca.R 은 서버에서 돌리지 않음)
# 같은 폴더에 필요한 것: engine.R, pca.rds, shoes_meta.csv, selfcheck/ (labels.csv 포함)
# ============================================================
library(shiny)

source("engine.R")   # library(magick), pca, meta, read_shoe, show_shoe, photo_score, rebuild, search_shoe

# ---------- 시작할 때 한 번만 계산 ----------
ve <- pca$sdev^2 / sum(pca$sdev^2)
cv <- cumsum(ve)
k80 <- which(cv >= 0.80)[1]
k90 <- which(cv >= 0.90)[1]
k95 <- which(cv >= 0.95)[1]

# 음수 값이 있는 그림(Step 2, 4)용: 0을 기준으로 대칭 색
show_diff <- function(v) {
  m <- max(abs(v))
  image(matrix(v, 64, 64)[, 64:1], col = hcl.colors(101, "Blue-Red 3"),
        zlim = c(-m, m), axes = FALSE, asp = 1)
}

# 정사각형 + 밝기 고정(0~255)
show_shoe_fixed <- function(x, zlim = c(0, 255)) {
  x <- pmin(pmax(x, zlim[1]), zlim[2])
  image(matrix(x, 64, 64)[, 64:1], col = gray(0:255 / 255),
        zlim = zlim, axes = FALSE, asp = 1)
}

# K=1에서도 에러가 나지 않는 복원 함수
rebuild_safe <- function(z, K) {
  V <- pca$rotation[, 1:K, drop = FALSE]
  as.numeric(pca$center + V %*% z[1:K])
}

sc_files <- list.files("selfcheck", pattern = "\\.(png|jpg|jpeg)$", ignore.case = TRUE)

# ---------- UI ----------
ui <- navbarPage("Explain PCA",
  tabPanel("Explain PCA",
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons("src", "Photo", c("Self-check", "Upload")),
        conditionalPanel("input.src == 'Self-check'",
          selectInput("sc_file", "Self-check photo", sc_files)),
        conditionalPanel("input.src == 'Upload'",
          fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png"))),
        sliderInput("K", "Number of PCs (K)", 1, 100, 20)
      ),
      mainPanel(width = 9,
        p("For the chosen photo, show how PCA finds the match."),
        h4("Step 1. Average shoe"),
        p("x̅, the mean of the 2,000 photos."),
        plotOutput("p1", height = 280, width = 280),
        h4("Step 2. Photo − average"),
        p("x − x̅, where the photo is brighter or darker than the average shoe."),
        plotOutput("p2", height = 280, width = 280),
        h4("Step 3. Score map"),
        p("All catalogue photos on PC1 and PC2, colored by category, with your photo marked (star)."),
        plotOutput("p3", height = 420),
        h4("Step 4. Eigenshoes"),
        p("PC1 to PC8 as 64 × 64 images, with the share of variance each explains."),
        plotOutput("p4", height = 360),
        h4("Step 5. Scores"),
        p("The photo's first K PC scores, the numbers used to find the match."),
        plotOutput("p5", height = 260),
        div(style = "max-height:300px; overflow-y:auto;", tableOutput("t5")),
        h4("Step 6. Rebuild"),
        p("The photo rebuilt from the average shoe and the first K eigenshoes, for several K."),
        plotOutput("p6k", height = 300, width = 600),
        p("Fixed comparison (K = 1, 5, and the K needed for 80%, 90%, 95% of the variance):"),
        plotOutput("p6", height = 240),
        h4("More: cumulative explained variance"),
        p("How many PCs are needed to keep 80%, 90% and 95% of the variance."),
        plotOutput("p7", height = 360)
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  photo_path <- reactive({
    if (input$src == "Upload") {
      req(input$photo)
      input$photo$datapath
    } else {
      req(input$sc_file)
      file.path("selfcheck", input$sc_file)
    }
  })

  x <- reactive(read_shoe(photo_path()))
  z <- reactive(photo_score(photo_path()))

  output$p1 <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    show_shoe(pca$center)
    title("Average shoe")
  })

  output$p2 <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    show_diff(x() - pca$center)
    title("Photo - average")
  })

  output$p3 <- renderPlot({
    cat_f <- as.factor(meta$category)
    plot(pca$x[, 1], pca$x[, 2], col = cat_f, pch = 16,
         xlab = "PC1", ylab = "PC2", main = "Score map")
    points(z()[1], z()[2], pch = 8, cex = 2, lwd = 2)
    legend("topright", legend = levels(cat_f), col = 1:nlevels(cat_f), pch = 16)
  })

  output$p4 <- renderPlot({
    par(mfrow = c(2, 4), mar = c(1, 1, 3, 1))
    for (k in 1:8) {
      show_diff(pca$rotation[, k])
      title(sprintf("PC%d (%.1f%%)", k, 100 * ve[k]))
    }
  })

  output$p5 <- renderPlot({
    n <- min(input$K, length(z()))
    barplot(z()[1:n], names.arg = 1:n, xlab = "PC number", ylab = "PC score",
            main = sprintf("First %d PC scores of the photo", n),
            col = "grey60", border = NA)
  })

  output$t5 <- renderTable({
    n <- min(input$K, length(z()))
    data.frame(`PC number` = paste0("PC", 1:n),
               `PC score`  = round(z()[1:n], 2),
               check.names = FALSE)
  })

  output$p6k <- renderPlot({
    par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
    show_shoe_fixed(x())
    title("Original photo")
    show_shoe_fixed(rebuild_safe(z(), input$K))
    title(paste("Rebuilt with K =", input$K))
  })

  output$p6 <- renderPlot({
    ks <- sort(unique(c(1, 5, k80, k90, k95)))
    par(mfrow = c(1, length(ks)), mar = c(1, 1, 3, 1))
    for (k in ks) {
      show_shoe_fixed(rebuild_safe(z(), k))
      title(paste("K =", k))
    }
  })

  output$p7 <- renderPlot({
    n <- min(200, length(cv))
    plot(cv[1:n], type = "l", xlab = "K", ylab = "Cumulative variance",
         main = "Cumulative explained variance")
    abline(h = c(0.8, 0.9, 0.95), lty = 2)
  })
}

shinyApp(ui, server)
