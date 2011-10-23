-- I believe this works, however it seems to take a really long time on my box
-- so I can't confirm it. It does compile though
findlcm target (curr:rest) 
    | 0 == sum [mod curr x | x <- target] = curr
    | otherwise                           = findlcm target rest
answer = findlcm [1..20] [1..]
