-- Fibo thứ n 
fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

-- Nhận vào một list int trả về tổng của các số chẳn
sumEven :: [Int] -> Int
sumEven [] = 0
sumEven xs = if (mod x 2 == 0) then x+y else y
                where
                    x = head xs
                    y = sumEven (tail xs)

-- Nhận vào một chuỗi trả về chuỗi đảo ngược
reverseString :: String -> String
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]

-- Trị tuyệt đối của một mảng
absoluteFunct :: [Int] -> [Int]
absoluteFunct [] = []
absoluteFunct xs = [abs(x)| x<-xs]

-- Chứa các phần tử trùng nhau
--concatList :: [Int] -> [Int] -> [Int] 
--concatList xs [] = xs
--concatList [] ys = ys
--concatList xs ys = 

-- Tích các phần tử trong list Int
productList :: [Int] -> Int
productList xs = if (length xs) == 1 then x else x*y
                    where 
                        x = head xs
                        y = productList(tail xs)

-- Trả về list Int chỉ chứa các phần tử chẳn
listEven :: [Int] -> [Int] 
listEven [] = []
listEven xs = [x| x<-xs, mod x 2 == 0]