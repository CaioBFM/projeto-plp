-- TESTES A SEREM IMPLEMENTADOS --

menorDeDois :: (Ord t) => t -> t -> t
menorDeDois a b
    | a < b     = a
    | otherwise = b

-- questao 1
ultimo :: [t] -> t
ultimo [u] = u
ultimo (c:r) = ultimo r

-- questao 4
maiores_que :: (Ord t) => t -> [t] -> [t]
maiores_que v (c:r)
    | v < c     = c : maiores_que v r
    | otherwise = maiores_que v r
maiores_que _ [] = []