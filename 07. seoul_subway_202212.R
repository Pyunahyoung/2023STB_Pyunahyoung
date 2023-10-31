#실습에필요한packages를설치
install.packages("dplyr")
install.packages("ggplot2")

#실습에필요한packages를라이브러리에등록
library(dplyr)
library(ggplot2)

#CSV형식의파일불러와서subway객체에입력하고구조확인
str(subway)

#변수의이상치와결측치확인하고처리
summary(subway)
좌측하단의console창에NA결측지가없다.
최소값이0이면내린사람이없다는의미입니다.

#지하철역의하루평균승차/하차승객수
#역별일평균전체승객수상위10개역을막대그래프로작성// 역명(station)
#일별전체승객분석//총승하차승객수(total_passenger)
#특정line 분석(1호선) // 총승하차승객수(total_passenger)
#노선별전체승객비율비교지하철전체승객비율막대그래프그리기
#일별전체승객선그래프그리기// 일자데이터(day), 총승하차승객수(total_passenger)

#파생변수1.정수형day변수
subway$day<-substr(subway$Date,7,8)
class(subway$day)
subway$day<-as.integer(subway$day)

#파생변수1.정수형day변수
subway$day<-substr(subway$Date,7,8)
class(subway$day)
subway$day<-as.integer(subway$day)

#파생변수2.line변수
table(subway$Line)

#9호선과9호선2~3단계가구분되어있어통합해주는명령어실행.
subway$Line<-ifelse(subway$Line=="9호선2~3단계","9호선",subway$Line)

#파생변수3.station변수
table(subway$Station)

#파생변수4.총승하차승객수total_passenger
subway$total_passenger<-subway$on_board+subway$getting_off

#분석데이터최종확인
str(subway)

#1.지하철역의하루평균승차/하차승객수
subway%>%  
  summarise(on_m=mean(on_board),off_m=mean(getting_off))

#2.승차승객이가장많았던역을찾아보기
#2-1 solutions. 
max(subway$on_board)
subway%>% 
  filter(on_board==94732)%>%  
  select(Date, Line, Station, on_board)

#3.역별 하루평균 전체승객수 분석
passenger10 <-subway %>%
  group_by(Station)%>%
  summarise(m=mean(total_passenger))%>%
  arrange(desc(m))%>%
  head(10)

head(passenger10, 3)

#4.역별 일평균 전체승객수 상위 10개 역을 막대그래프로 작성
ggplot(data=passenger10, aes(x=reorder(Station, m), y=m))+
  geom_col()+
  coord_flip()
coord_flip()함수를이용해서역이름이y축에가도록막대를90도회전했습니다.

#5.일별 전체승객수 분석
subway %>%
  group_by(Date) %>%
  summarise(total=sum(total_passenger)) %>%
  arrange(desc(total)) %>%
  head(3)

#6.특정라인분석(1호선)
subway %>%
  filter(Line=="1호선") %>%
  filter(total_passenger==max(total_passenger)) %>%
  select(Date, Station, on_board, getting_off, 
total_passenger)

#7.노선별 전체 승객 비율 비교
line_pct<-subway %>%
  group_by(Line) %>%
  summarise(total=sum(total_passenger)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct%>%
  arrange(desc(pct)) %>%
  head(3)

#8.지하철전체승객비율막대그래프그리기
line_pct10 <-line_pct%>%filter(Line%in%c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선","9호선","분당선" ))

ggplot(data = line_pct10, aes(x=reorder(Line,pct),y=pct))+geom_col()+coord_flip()+ggtitle("수도권지하철노선별이용비율")+xlab("노선")+ylab("이용비율")

#9.일별전체승객선그래프그리기
line_graph<-subway %>%group_by(day) %>%summarise(s=sum(total_passenger))

ggplot(data = line_graph, aes(x=day, y=s, group=1))+geom_line()+ggtitle("수도권지하철일별이용승객수")+xlab("일")+ylab("이용승객")

