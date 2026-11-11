# ============================================================
# engine.R : PCA 신발 검색 엔진
#            (Shoe Search / Explain PCA / Our Shoes 공용)
# ------------------------------------------------------------
# 준비물 (전부 같은 폴더에)
#   · pca.rds          ← build_pca.R 을 한 번 돌리면 생김
#   · shoes_meta.csv   ← 카탈로그 id, category
#   · shoes/           ← 카탈로그 사진 (Best match 사진 보여줄 때 필요)
#   · selfcheck/       ← self-check 사진 + labels.csv
#
# 사용법
#   source("engine.R")   # 콘솔에서든 app.R 맨 위에서든 이거 실행 한 번
#
# pca 내용물 네 가지
#   pca$center         평균 신발 x̄ (숫자 4096개)
#   pca$rotation[, k]  k번째 eigenshoe v_k (숫자 4096개, loading)
#   pca$x[i, ]         i번 카탈로그 사진의 PC score (숫자 2000개)
#   pca$sdev         각 PC의 표준편차
# ============================================================

library(magick)

pca  <- readRDS("pca.rds")
meta <- read.csv("shoes_meta.csv")             # meta$category[i] = i번 신발 카테고리
selfcheck_labels <- read.csv("selfcheck/labels.csv")


# ------------------------------------------------------------
# 1) read_shoe(path) : [input] 사진 파일 -> [output] 픽셀 정보 숫자 4096개
#    카탈로그 사진, self-check 사진, 업로드/폰 사진 모두 사용 가능
#    · 흑백으로 바꾸고
#    · 비율 유지하며 64x64 안에 맞게 줄이고
#    · 남는 칸은 흰색으로 채워 64x64 정사각형으로
#       (pc score를 계산할 때 원소가 4096개인 eigenshoe와 곱이 가능해야 해서)
# ------------------------------------------------------------
read_shoe <- function(path) {
  img <- image_read(path)
  img <- image_convert(img, colorspace = "gray")
  img <- image_resize(img, "64x64")
  img <- image_extent(img, "64x64", gravity = "center", color = "white")
  x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))
  x
}


# ------------------------------------------------------------
# 2) show_shoe(x) : [input] 픽셀 숫자 4096개 -> [output] 64x64 그림
#    사진, 평균 신발, eigenshoe, 복원 사진 모두 이걸로 그림
#    [, 64:1] : image()는 그래프처럼 아래부터 그리므로 위아래를 뒤집어 똑바로 세움
# ------------------------------------------------------------
show_shoe <- function(x) {
  image(matrix(x, 64, 64)[, 64:1], col = gray(0:255 / 255))
}


# ------------------------------------------------------------
# 3) photo_score(path) : [input] 새 사진 → [output] PC score (숫자 2000개 벡터)
#    카탈로그 평균을 빼고 카탈로그 eigenshoe로 projection
#    z = (x - x̄)ᵀ V   ← predict()가 이 계산을 함
#    score <- as.numeric((x - pca$center) %*% pca$rotation)
#    predict() 결과는 1 x 2000 행렬이라 as.numeric()으로 벡터로 바꿈
#    (2000장의 카탈로그 사진은 이 함수가 필요 없고 이미 계산돼 있으니 pca$x[id, ] 를 쓰면 됨)
# ------------------------------------------------------------
photo_score <- function(path) {
  x <- read_shoe(path)
  score <- as.numeric(predict(pca, newdata = matrix(x, nrow = 1)))
  score
}


# ------------------------------------------------------------
# 4) rebuild(z, K) : [input] score → [output] K개 PC로 복원한 사진의 픽셀 정보 4096개
#    x̂ = x̄ + z_1 v_1 + ... + z_K v_K = x̄ + V z
# ------------------------------------------------------------
rebuild <- function(z, K) {
  V <- pca$rotation[, 1:K]
  z <- z[1:K]
  x_hat <- pca$center + V %*% z
  as.numeric(x_hat)
}


# ------------------------------------------------------------
# 5) search_shoe(zq, K, exclude_id) : 비슷한 신발 Top 5 + 카테고리 예측
#    zq         : 찾고 싶은 사진의 score (pca$x[id, ] 또는 photo_score(path))
#    K          : 거리 잴 때 쓸 PC 개수
#    exclude_id : 카탈로그 사진으로 검색할 때 그 id (자기 자신 제외)
#                 self-check / 업로드 사진이면 NULL (기본값)
#    거리       : d_i = sqrt( sum_k (zq_k - z_ik)^2 )   (앞 K개 score끼리)
#    결과       : list(top5 = id 5개, dist = 거리 5개,
#                      category = 카테고리 5개, pred = 다수결 예측)
# ------------------------------------------------------------
search_shoe <- function(zq, K, exclude_id = NULL) {
  S  <- pca$x[, 1:K]
  zq <- zq[1:K]
  
  d <- numeric(nrow(S))
  for (i in 1:nrow(S)) {
    d[i] <- sqrt(sum((S[i, ] - zq)^2)) # i번째 사진과 검색 대상 사진의 pc score 거리
  }
  if (!is.null(exclude_id)) { # 카탈로그 사진을 검색할 때를 대비
    d[exclude_id] <- Inf     # 자기 자신은 거리를 무한대로 보냄
  }
  
  top5     <- order(d)[1:5]
  top5_cat <- meta$category[top5]
  pred     <- names(which.max(table(top5_cat)))
  
  list(top5 = top5, dist = d[top5], category = top5_cat, pred = pred)
}


# ============================================================
# 사용 예시 (source() 할 때는 실행 안 되게 if (FALSE) 로 막아둠)
# 콘솔에서 한 줄씩 드래그해서 돌려보면 됨
# ============================================================
if (FALSE) {
  
  # (a) 카탈로그 3번으로 검색
  res <- search_shoe(pca$x[3, ], K = 50, exclude_id = 3)
  res$top5; res$dist; res$pred
  res$pred == meta$category[3]                 # 맞았는지
  
  # (b) self-check 사진으로 검색
  res <- search_shoe(photo_score("selfcheck/selfcheck_01.png"), K = 50)
  res$pred == selfcheck_labels$category[1]
  
  # (c) 업로드 / 폰 사진으로 검색
  res <- search_shoe(photo_score("test_shoe.jpg"), K = 50)
  res$pred
  
  # (d) 그림
  show_shoe(pca$center)                                   # 평균 신발
  show_shoe(pca$rotation[, 1])                            # PC1 eigenshoe
  show_shoe(rebuild(pca$x[3, ], 50))                      # 3번을 PC 50개로 복원
  show_shoe(read_shoe(sprintf("shoes/%04d.png", res$top5[1])))   # Best match 사진
  
  # (e) PC별 분산 설명 비율
  ve <- pca$sdev^2 / sum(pca$sdev^2)
  round(ve[1:10], 3)            # PC1~PC10 각각
  round(cumsum(ve)[c(10, 20, 50, 100)], 3)   # 누적 비율 (참고용)
}