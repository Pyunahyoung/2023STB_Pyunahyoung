# table()함수/구분 1개의 인자를 가지고 도수분포표 작성
table(KOTRA2023 $진출대륙명)

# table()함수/2개의 인자를 가지고 교차표를 작성
table(KOTRA2023 $진출대륙명, KOTRA2023 $진출형태)

#  상대도수 계산
ECN <- table(KOTRA2023 $진출대륙명)
prop.table(ECN)    

#막대그래프
barplot(table(KOTRA2023 $진출대륙명))

entry <- table(KOTRA2023 $진출대륙명, KOTRA2023 $진출형태)
barplot(entry, legend = TRUE)

#파이차트
pie(table(KOTRA2023 $진출대륙명))
pie(table(KOTRA2023 $투자형태))

colors <-c("red", "orange", "yellow", "green", "blue")
pie(table(KOTRA2023 $투자형태), col=colors, main="해외진출기업의투자형태")

#무지개색
pie(table(KOTRA2023 $투자형태), col=rainbow(12), main="해외진출기업의투자형태")

#적색과황색에치우친색
pie(table(KOTRA2023 $투자형태), col=heat.colors(12), main="해외진출기업의투자형태")

#지구지형색
pie(table(KOTRA2023 $투자형태), col=terrain.colors(12), main="해외진출기업의투자형태")

#앞에서조금더청색에가까운색
pie(table(KOTRA2023 $투자형태), col=topo.colors(12), main="해외진출기업의투자형태")

#가로막대그래프
barplot(table(KOTRA2023 $진출대륙명),col=pal1, xlab= "진출대륙명", ylab= "진출기업수", xlim=c(0,10000), horiz=TRUE)

#히스토그램
hist(finedust$`3_ultrafine dust`, main="서울시서대문구2020년1월초미세먼지측정분포", col=terrain.colors(12))

#확률밀도선
lines(density(finedust$`3_ultrafine dust`), lwd=2)

#박스플롯
boxplot(finedust$`3_fine dust`, main="야식업의2020년1월미세먼지발생현황", col="yellow")

#2개 업종의 박스 플롯 비교
boxplot(finedust$`3_fine dust`, finedust$`7_fine dust`, main="업종별2020년1월미세먼지발생현황", col="yellow", names = c("야식업","중식"))

#산점도
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와초미세먼지의변화")

#산점도 편집
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와초미세먼지의변화", pch=24, col="red", bg="yellow", cex=1.5)

#산점도 그래프의 Type따라 변화
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와초미세먼지의변화", type = "h")

#X2023_STB_survey Gender 도수분포표
table(X2023_STB_survey $Gender)

#X2023_STB_survey Gender 상대도수분포표
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN) 

#X2023_STB_survey Gender, Grade 교차표
entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE)

#X2023_STB_survey Nationality 막대그래프
barplot(table(X2023_STB_survey $Nationality))

#X2023_STB_survey Residential Area 가로 막대그래프
barplot(table(X2023_STB_survey $`residential area`), xlab= "residentail area", ylab= "numbers", xlim=c(0,10000), horiz=TRUE)

#X2023_STB_survey Gender, Grade 막대그래프
entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE)

#X2023_STB_survey Grade 파이차트
pie(table(X2023_STB_survey $Grade))

#X2023_STB_survey Age 히스토그램
hist(X2023_STB_survey$`Age`, main="age", col=terrain.colors(12))

#X2023_STB_survey Age, Grade 박스플롯 비교
hist(X2023_STB_survey$`Age`, main="age", col=terrain.colors(12))
boxplot(X2023_STB_survey$`Age`, X2023_STB_survey$`Grade`, main="age, grade 비교", col="yellow", names = c("age","grade"))

#X2023_STB_survey Y=Age, X=Grade 산점도
plot(x=X2023_STB_survey$`Grade`, y=X2023_STB_survey$`Age`, xlab="Grade", ylab="Age", main="Grade, Age")
