library(tidyverse)
library(jsonlite)
library(httr)

# 1. 통합 저장 폴더 생성
save_dir <- "kbo_raw_all"
dir.create(save_dir, showWarnings = FALSE)

# 2. 테스트 날짜 고정 (2025년 개막일)
target_date <- "2025-03-22"

# 3. 경기 일정 호출 (KBO 전용 API)
# 2025-03-22의 경기 정보를 가져옵니다.
url_main <- sprintf('https://api-gw.sports.naver.com/schedule/games?fields=basic&upperCategoryId=kbaseball&categoryId=kbo&fromDate=%s&toDate=%s&size=100', 
                   target_date, target_date)

raw_res <- fromJSON(url_main)

# 데이터 유무 확인
if (is.null(raw_res$result$games) || length(raw_res$result$games) == 0) {
  stop(paste0(target_date, ": 해당 날짜에 경기 데이터가 없습니다."))
}

# 취소되지 않은 경기 ID 추출 (예: 20250322SSOB0 등)
kbo_game_ids <- raw_res$result$games %>% 
  filter(cancel == FALSE) %>% 
  pull(gameId)

message(paste0("발견된 경기 수: ", length(kbo_game_ids)))

# 4. JSON 저장 함수 (게임코드_이닝.json)
download_kbo_json <- function(g_id, inning) {
  relay_url <- sprintf('https://api-gw.sports.naver.com/schedule/games/%s/relay?inning=%s', g_id, inning)
  
  # 파일명 예: 20250322SSOB0_01.json
  file_name <- sprintf("%s_%02d.json", g_id, inning)
  file_path <- file.path(save_dir, file_name)
  
  # 중복 다운로드 방지
  if (file.exists(file_path)) return(NULL)
  
  tryCatch({
    res <- GET(relay_url)
    if (status_code(res) == 200) {
      # 원본 텍스트 그대로 저장
      writeLines(content(res, as = "text", encoding = "UTF-8"), file_path)
    }
  }, error = function(e) message(paste0("에러: ", g_id, " (", inning, "회)")))
}

# 5. 실행 (모든 경기 x 1~11회 순회)
# 11회까지만 수집하도록 루프 설정
walk2(
  rep(kbo_game_ids, each = 11), 
  rep(1:11, times = length(kbo_game_ids)), 
  ~download_kbo_json(.x, .y)
)

message(paste0("테스트 완료. '", save_dir, "' 폴더 내 파일 수: ", length(list.files(save_dir))))
