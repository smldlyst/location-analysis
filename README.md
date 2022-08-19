# location-analysis
- 학원 입지정보 시각화 서비스 개발(경기도 구리시, 영수사관학원)

1. Open API 을 이용하여 아파트 데이터 불러오기 (**apt-total-info.csv**)(국토교통부 공동주택 - 단지목록, 기본정보)
   - 시군구코드를 통하여 시군구 정보에 따른 단지정보 추출
   - 단지정보에 따른 아파트 기본,상세 정보 추출
   - **※ 학교 및 학원은 '경기도 구리시 ○○ 현황' 검색 후 파일데이터 다운로드**
1. KaKao API을 이용하여 아파트, 학원에 대한 주소정보를 통해 위경도 추출하기
1. 학원으로부터 반경 선택 기능 및 기대이익 계산 기능을 가지고 있는 Shiny App 구현 

![loation-analysis-shiny](https://user-images.githubusercontent.com/79900437/185560053-1d7a1e41-d237-4b98-bf9f-11c0749d4d24.gif)

도움을 주신 이 : 임채오 님. (https://blog.naver.com/chaeohyim)

