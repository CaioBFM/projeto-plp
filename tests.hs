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

-- questao 7
remover_ultimo :: [t] -> [t]
remover_ultimo [c]   = []
remover_ultimo (c:r) = c : remover_ultimo r
---------------------funçoes auxiliares-------------------------------------------------
pertence :: (Eq t) => t -> [t] -> Bool 
pertence e (c:r) 
    | e == c    = True
    | otherwise = pertence e r
pertence _ _= False

-- estilo merge sort
pares :: (a -> a -> a) -> [a] -> [a]
pares fun (a:b:r) = (fun a b) : (pares fun r)
pares _ lista     = lista

-- acumulação em árvore
accumArv :: (t -> t -> t) -> [t] -> t
accumArv _ [x] = x
accumArv fun l = accumArv fun (pares fun l)

-- merge sort
merge l1@(c1:r1) l2@(c2:r2)
    | c2 < c1   = c2:(merge l1 r2)
    | otherwise = c1:(merge r1 l2)
merge l [] = l
merge [] l = l

mergesort l = accumArv merge (map (\a -> [a]) l)

pegaNMaiores:: Int -> [t] -> [t]
pegaNMaiores n l = pegaNMaiores' n l
    where
        pegaNMaiores' 0 _     = []
        pegaNMaiores' _ []    = []
        pegaNMaiores' k (c:r) = c : pegaNMaiores' (k-1) r
------------------------------------------------------------------
-- questao 10
maiores :: (Ord t, Eq t) => Int -> [t] -> [t]
maiores n l = filter (\x -> pertence x maioresN) l
    where
        maioresN = pegaNMaiores n (reverse(mergesort l))

-- questao 13
divide :: Int -> [t] -> ([t], [t])
divide 0 l     = ([], l)
divide _ []    = ([], [])
divide n (c:r) = (c : e1, e2)
  where (e1, e2) = divide (n-1) r

-- questao 16
intercala :: [t] -> [t] -> [t]
intercala [] l = l
intercala l [] = l 
intercala (c1:r1) (c2:r2) = c1 : c2 : (intercala r1 r2)

-- questao 19
mesmos_elementos :: (Eq t) => [t] -> [t] -> Bool
mesmos_elementos l1 l2 =
    let s1 = remove_duplicatas l1
        s2 = remove_duplicatas l2
    in todos_pertencem s1 s2 && todos_pertencem s2 s1

remove_duplicatas :: (Eq t) => [t] -> [t]
remove_duplicatas [] = []
remove_duplicatas (c:r)
    | pertence c r = remove_duplicatas r
    | otherwise     = c : remove_duplicatas r

-- verifica se todos os elementos de a estão em b
todos_pertencem :: (Eq t) => [t] -> [t] -> Bool
todos_pertencem [] _ = True
todos_pertencem (c:r) l = pertence c l && todos_pertencem r l

-- Questao 22 bolsonaro
rodar_esquerda :: Int -> [t] -> [t]
rodar_esquerda n l = fim ++ inicio
  where (inicio, fim) = divide n l

-- Questao 28
primeira_maiusculas :: String -> String
primeira_maiusculas = aux True
  where
    aux _ [] = []
    aux True (c:r)  -- início de palavra
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32) : aux False r
      | otherwise            = c : aux (c == ' ') r -- sempre que encontra um espaço, booleano fica true e entende que esta dentro de uma palavra
    aux False (c:r) -- dentro da palavra
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32) : aux (c == ' ') r
      | otherwise            = c : aux (c == ' ') r
    
                            