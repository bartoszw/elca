module BWLib
where

-- =======================================================
-- Some important and useful functions for general purpose
-- =======================================================
-- Function composition - in case of more than 1 argument
comp4 = (.) . (.) . (.) . (.)
comp5 = (.) . (.) . (.) . (.) . (.)

fstOf3  (a,b,c) = a
fst2Of3 (a,b,c) = (a,b)
sndOf3  (a,b,c) = b
trdOf3  (a,b,c) = c
fstOf5  (a,b,c,d,e) = a
sndOf5  (a,b,c,d,e) = b
trdOf5  (a,b,c,d,e) = c
frthOf5  (a,b,c,d,e) = d
fvthOf5  (a,b,c,d,e) = e

-- Checks if all elements of the list are equall
allEq :: (Eq a) => [a] -> Bool
allEq ls = all ((head ls) ==) ls

--zip4 []     _      _      _      = []
--zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d):zip4 as bs cs ds

--zip5 []     _      _      _      _      = []
--zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e):zip5 as bs cs ds es

--zip8 [] _ _ _ _ _ _ _ = []
--zip8 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
--     (a,b,c,d,e,f,g,h):zip8 as bs cs ds es fs gs hs

zip9 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) =
     (a,b,c,d,e,f,g,h,i) : zip9 as bs cs ds es fs gs hs is
zip9 _ _ _ _ _ _ _ _ _ = []

unzip9          =  foldr (\(a,b,c,d,e,f,g,h,i) ~(as,bs,cs,ds,es,fs,gs,hs,is) ->
                                (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs,h:hs,i:is))
                         ([],[],[],[],[],[],[],[],[])

