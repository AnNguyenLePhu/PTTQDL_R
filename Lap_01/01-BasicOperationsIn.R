#Bước 1: Khai báo các thông số
dai <- 20
rong <- 10
don_vi <- "met"

#Bước 2: Tính toán chu vi và diện tích
chu_vi <- (dai+rong)*2
dien_tich <- dai*rong

#Bước 3: sử dụng toán so sánh + logic
# Kiểm tra nếu diện tích > 150 và dài hơn rộng hay không
check <- (dien_tich>150) & (dai>rong)
check

#Bước 4: In kết quả
print(paste("Chu vi là: ", chu_vi, don_vi))
ket_qua <- paste("Diện tích là: ", dien_tich, don_vi)
ket_qua

#Bước 5: Kiểm tra kiểu dữ liệu
class(don_vi)
class(check)

#class() : kiểm tra kiểu dữ liệu

#is.numeric() : kiểm tra dữ liệu có phải numeric không
#is.integer() : kiểm tra dữ liệu có phải intergr không
#is.logical() : kiểm tra dữ liệu có phải logical không
#is.character() : kiểm tra dữ liệu có phải character không

#Chuyển đổi kiểu
#as.numeric()
#as.integer()
#as.logical()
#as.character()
#as.Date()

a<-1.5
class(a)

b<-10
class(b)

is.integer(a)
is.integer(b)
is.numeric(a)
is.numeric(b)

b<-6.89
class(b)
b<-as.integer(b)
class(b)
b

#Bổ sung
help(print)
help(class)

class(check) <- "character"
check
