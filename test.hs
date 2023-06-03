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