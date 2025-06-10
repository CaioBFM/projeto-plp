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
