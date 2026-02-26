library(tidyverse)
library(jsonlite)
library(httr)

# 1. 통합 저장 폴더 생성 (데이터를 한곳에 모아 합치기 편하게)
save_dir <- "kbo_raw_all"
dir.create(save_dir, showWarnings = FALSE)

# 2. 날짜 설정 (KST 기준 어제 날짜)
kst_now <- Sys.time() + (9 * 60 * 60)
target_date <- format(as.Date(kst_now) - 1, "%Y-%m-%d")

# 3. 경기 일정 호출
url_main <- sprintf('https://api-gw.sports.naver.com/schedule/games?fields=basic&upperCategoryId=kbaseball&categoryId=kbo&fromDate=%s&toDate=%s&size=100', 
                   target_date, target_date)

raw_res <- fromJSON(url_main)
if (is.null(raw_res$result$games) || length(raw_res$result$games) == 0) {
  message(paste0(target_date, ": 경기가 없는 날입니다."))
  quit(save = "no", status = 0)
}

# 취소되지 않은 경기 ID 추출
kbo_game_ids <- raw_res$result$games %>% 
  filter(cancel == FALSE) %>% 
  pull(gameId)

# 4. JSON 저장 함수 (파일명: 게임코드_이닝.json)
download_kbo_json <- function(g_id, inning) {
  relay_url <- sprintf('https://api-gw.sports.naver.com/schedule/games/%s/relay?inning=%s', g_id, inning)
  
  # 파일명 예: 20260322SSOB0_11.json (요청하신 대로 11회까지)
  file_name <- sprintf("%s_%02d.json", g_id, inning)
  file_path <- file.path(save_dir, file_name)
  
  # 이미 수집한 파일은 건너뛰기
  if (file.exists(file_path)) return(NULL)
  
  tryCatch({
    res <- GET(relay_url)
    if (status_code(res) == 200) {
      # 원본 JSON 텍스트 그대로 저장
      writeLines(content(res, as = "text", encoding = "UTF-8"), file_path)
    }
  }, error = function(e) message(paste0("Error: ", g_id, " (", inning, "회)")))
}

# 5. 실행 (모든 경기 x 1~11회 순회)
# 요청하신 대로 11회까지만 루프를 돌립니다.
walk2(
  rep(kbo_game_ids, each = 11), 
  rep(1:11, times = length(kbo_game_ids)), 
  ~download_kbo_json(.x, .y)
)

message(paste0(target_date, " 작업 완료. 총 파일 수: ", length(list.files(save_dir))))
