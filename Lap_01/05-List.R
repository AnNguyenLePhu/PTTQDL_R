# List trong R là một cấu trúc dữ liệu linh hoạt có thể chứa:
# Nhiều kiểu dữ liệu khác nhau
# Các đối tượng có độ dài khác nhau
# Các cấu trúc dữ liệu phức tạp (vector, matrix, dataframe, thậm chí list khác)

# Tạo một list đơn giản
student <- list(
  name = "An",
  age = 21,
  grades = c(8.5, 9.0, 7.5),
  passed = TRUE
)

# Truy cập dữ liệu
# Cách 1: Dùng [ ] ==> trả về list con
student[1]
class(student[1])

# Cách 2: Dùng [[]] ==> trả về giá trị
student[[1]]
class(student[[1]])

student[[3]][c(1,2)]

# Cách 3: Dùng $ ==> truy cập theo tên
student$grades

# Thêm giá trị vào
student$id <- '2386400763'
student

#cập nhật giá trị 
student$grades <- c(9,8,9)
student
