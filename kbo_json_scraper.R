library(tidyverse)
library(jsonlite)
library(httr)

save_dir <- "kbo_raw_all"
dir.create(save_dir, showWarnings = FALSE)

# 실제 운영 시: 어제 날짜 / 테스트 시: "2025-03-22"
kst_now <- Sys.time() + (9 * 60 * 60)
target_date <- format(as.Date(kst_now) - 1, "%Y-%m-%d")

url_main <- sprintf('https://api-gw.sports.naver.com/schedule/games?fields=basic&upperCategoryId=kbaseball&categoryId=kbo&fromDate=%s&toDate=%s&size=100', 
                   target_date, target_date)

# 1. 경기 일정 호출
raw_res <- fromJSON(url_main)

# [핵심] 경기가 없거나 리스트가 비어있으면 에러 없이 종료
if (is.null(raw_res$result$games) || length(raw_res$result$games) == 0) {
  message(paste0(target_date, ": 경기가 없는 날입니다. 작업을 종료합니다."))
  quit(save = "no", status = 0) # status 0은 '정상 종료'를 의미하여 YAML이 에러로 인식하지 않음
}

# 2. 취소되지 않은 경기 ID 추출
kbo_game_ids <- raw_res$result$games %>% 
  filter(cancel == FALSE) %>% 
  pull(gameId)

# [핵심] 모든 경기가 우천 취소 등으로 ID가 없는 경우 대응
if (length(kbo_game_ids) == 0) {
  message("진행된 경기가 없습니다.")
  quit(save = "no", status = 0)
}

# 3. 저장 함수 및 실행 (1~11회)
download_kbo_json <- function(g_id, inning) {
  relay_url <- sprintf('https://api-gw.sports.naver.com/schedule/games/%s/relay?inning=%s', g_id, inning)
  file_name <- sprintf("%s_%02d.json", g_id, inning)
  file_path <- file.path(save_dir, file_name)
  
  if (file.exists(file_path)) return(NULL)
  
  tryCatch({
    res <- GET(relay_url)
    if (status_code(res) == 200) {
      writeLines(content(res, as = "text", encoding = "UTF-8"), file_path)
    }
  }, error = function(e) NULL)
}

walk2(rep(kbo_game_ids, each = 11), rep(1:11, times = length(kbo_game_ids)), ~download_kbo_json(.x, .y))
