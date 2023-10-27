mode :: (Eq a) => [a] -> a
mode = fst . biggest . frequency

frequency :: (Eq a) => [a] -> [(a, Integer)]
frequency [] = []
frequency (x : xs) = joinf x ys
    where
    ys = frequency xs
--배열을 입력값으로 받는다.
--입력값의 원소와 그의 빈도수를 가지는 튜플에 대한 배열을 반납한다.
--입력배열에 원소가 없을 떄 빈 배열을 출력한다.
--tail recursion이용

joinf :: (Eq a) => a -> [(a, Integer)] -> [(a, Integer)]
joinf x [] = [(x,1)]
joinf x ((y,numYs) : rest)
    | x == y    = (y, numYs + 1) : rest
    | otherwise = (y, numYs) : joinf x rest
--원소와 (원소, 빈도)수 튜플을 입력으로 받아 빈도수 계산된 튜플을 반환한다.
--해당 원소에 대한 빈도수 튜플이 없을 때는 아직 처음으로 배열에서 나타난 경우이므로 빈도수가 1인 튜플로 추가한다.
--해당 원소가 나타났을 떄 빈도수에 가산 해준다.
--그렇지 않을때 다음 원소에 대한 빈도수 계산을 한다.

biggest :: [(a, Integer)] -> (a, Integer)
biggest [] = undefined
biggest ((x, numXs) : []) = (x, numXs)
biggest ((x, numXs) : rest)
    | numXs >= numYs    = (x, numXs)
    | otherwise         = (y, numYs)
    where
    (y, numYs) = biggest rest