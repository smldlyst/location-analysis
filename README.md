# location-analysis
- 학원업을 대상으로 대상고객 수립을 위한 입지정보 시각화 서비스 개발(경기도 구리시, 영수사관학원)

1. Open API 을 통한 아파트 상세정보 불러오기 (**apt-total-info.csv"**)
(국토교통부 공동주택 - '단지목록 제공서비스', '기본(상세)정보 제공서비스')
   - 시군구코드를 통하여 시군구 정보에 따른 단지정보 추출
   - 단지정보에 따른 아파트 기본,상세 정보 추출
 ※ 학원 및 학교 정보는 검색(**ex. 경기도 구리시 학교(학원 현황)**) 하여 파일데이터 다운로드
1. KaKao API을 이용하여 아파트, 학원에 대한 주소정보를 통해 위경도 추출
1. 학원으로부터 반경 선택 기능 및 기대이익 계산 기능을 가지고 있는 Shiny App 구현 

아이디어 도움을 주신 이 : 임채오(https://blog.naver.com/chaeohyim)
