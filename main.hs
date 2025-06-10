-- ===============================================================================
--    Trabalho de Programação Funcional 2025/1  -  PLP
----------------------------------------------------------------------------------
--    Descrição : Implementação de várias funções em Haskell
--    Alunos    : Caio Bueno Finocchio Martins  -  202410377
--                Lana da Silva Miranda         -  202410364
--    Grupo 1   : 1, 4, 7, 10 ,13, 16, 19, 22, 25, 28, 31, 34 e 37
--    Professor : Bruno de Oliveira Schneider
-- ===============================================================================

-- Questão 1:
ultimo :: [t] -> t
ultimo [u]   = u
ultimo (c:r) = ultimo r

-- Questão 4:
maiores_que :: (Ord t) => t -> [t] -> [t]
maiores_que v (c:r)
    | v < c     = c : maiores_que v r
    | otherwise = maiores_que v r
maiores_que _ [] = []

-- Questão 7:
remover_ultimo :: [t] -> [t]
remover_ultimo [c]   = []
remover_ultimo (c:r) = c : remover_ultimo r

-- Questão 10:

-- Questão 13:
divide :: Int -> [t] -> ([t], [t])
divide 0 l     = ([], l)
divide _ []    = ([], [])
divide n (c:r) = (c : e1, e2)
    where (e1, e2) = divide (n-1) r

-- Questão 16:
intercala :: [t] -> [t] -> [t]
intercala [] l = l
intercala l [] = l 
intercala (c1:r1) (c2:r2) = c1 : c2 : (intercala r1 r2)

-- Questão 19:

-- Questão 22:
ordena :: (Ord t) => [t] -> [t]
ordena [] = []
ordena l  = accumArv merge (map (\a -> [a]) l) -- merge sort

pares :: (a -> a -> a) -> [a] -> [a]
pares fun (a:b:r) = (fun a b) : (pares fun r)
pares _ lista     = lista

accumArv :: (t -> t -> t) -> [t] -> t
accumArv _ [x] = x
accumArv fun l = accumArv fun (pares fun l)

merge l1@(c1:r1) l2@(c2:r2)
    | c2 < c1   = c2:(merge l1 r2)
    | otherwise = c1:(merge r1 l2)
merge l [] = l
merge [] l = l

-- Questão 25:
rodar_esquerda :: Int -> [t] -> [t]
rodar_esquerda n l = fim ++ inicio
  where (inicio, fim) = divide n l

-- Questão 28:

