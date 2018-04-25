quarters:: Int -> Int -> Maybe Int
quarters qRemaining 0 = Just qRemaining
quarters qRemaining nDay
    | (qRemaining `mod` 6) > 0  = Nothing
    | otherwise = quarters qPrior (nDay-1)
                  where qPrior = nDay + 7*qRemaining `quot` 6

solutions = [(nDays, qs) | nDays <-[4..10^7],
                           let qs = quarters 0 nDays,
                           qs /= Nothing]

main = putStrLn.unwords $ map show solutions
