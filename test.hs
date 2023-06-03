-- Nhận vào một chuỗi trả về chuỗi đảo ngược
reverseString :: String -> String
reverseString "" = ""
reverseString (x:xs) = reverseString xs ++ [x]