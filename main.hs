-- ==========================================================================================================
--    Trabalho de Programação Funcional 2025/1  -  PLP
-------------------------------------------------------------------------------------------------------------
--    Descrição : Implementação de várias funções em Haskell
--    Alunos    : Caio Bueno Finocchio Martins  -  202410377
--                Lana da Silva Miranda         -  202410364
--    Grupo 1   : 1, 4, 7, 10 ,13, 16, 19, 22, 25, 28, 31, 34 e 37
--    Professor : Bruno de Oliveira Schneider
--    Link do repositório remoto do projeto: https://github.com/CaioBFM/projeto-plp (testes, readme, etc)
-- ==========================================================================================================

--Questão 1: recebe uma lista e retorna o último elemento da lista.

ultimo :: [t] -> t
ultimo [u]   = u
ultimo (c:r) = ultimo r

-------------------------------------------------------------------------------------------------------------
-- Questão 4: recebe um valor e uma lista de coisas ordenáveis, retorna uma lista com os valores que são
--            maiores que o fornecido.

maiores_que :: (Ord t) => t -> [t] -> [t]
maiores_que v (c:r)
    | v < c     = c : maiores_que v r -- Adiciona a cabeça na lista retornada pela 
--                                       chamada recursiva de maiores_que v no resto
    | otherwise = maiores_que v r
maiores_que _ [] = []

-------------------------------------------------------------------------------------------------------------
-- Questão 7: recebe uma lista e retorna a lista sem o último elemento.
--            Obs.: lista vazia não retorna último elemento.

remover_ultimo :: [t] -> [t]
remover_ultimo [c]   = []
remover_ultimo (c:r) = c : remover_ultimo r
remover_ultimo []    = []

-- Função auxiliar:
pertence :: (Eq t) => t -> [t] -> Bool 
pertence e (c:r) 
    | e == c    = True
    | otherwise = pertence e r
pertence _ _= False

-------------------------------------------------------------------------------------------------------------
-- Questão 10: recebe um número natural n e uma lista, retorna uma lista com os n maiores elementos sem
--             alterar a ordem entre os elementos.

maiores :: (Ord t, Eq t) => Int -> [t] -> [t]
maiores n l = filter (\x -> pertence x maioresN) l      -- Filtra os n maiores mantendo a ordem original
    where
        maioresN = pegaNMaiores n (reverse (ordena l))  -- Pega os n maiores da lista ordenada decrescente

-- Função auxiliar:
pegaNMaiores:: Int -> [t] -> [t]
pegaNMaiores n l = pegaNMaiores' n l
    where
        pegaNMaiores' 0 _     = []                            -- Caso base: n acabou
        pegaNMaiores' _ []    = []                            -- Caso base: lista acabou
        pegaNMaiores' k (c:r) = c : pegaNMaiores' (k-1) r     -- Pega elemento e continua

-- Função auxiliar: remove a primeira ocorrência de um elemento
removePrimeiro :: (Eq t) => t -> [t] -> [t]
removePrimeiro _ [] = []
removePrimeiro m (c:r)
    | m == c    = r
    | otherwise = c : removePrimeiro m r

-------------------------------------------------------------------------------------------------------------
-- Questão 13: recebe uma lista e um número natural n, retorna o par (lista com os n primeiros números da
--             lista original, lista com o resto dos elementos da lista original).

divide :: [t] -> Int -> ([t], [t])
divide l 0     = ([], l)
divide [] _    = ([], [])
divide (c:r) n = (c : e1, e2)       -- Coloca o elemento atual na primeira lista até n acabar
    where (e1, e2) = divide r (n-1) -- Chama recursivamente diminuindo n, separando os n primeiros elementos

-------------------------------------------------------------------------------------------------------------
-- Questão 16: recebe duas listas e retorna outra lista com os elementos das listas originais intercalados.

intercala :: [t] -> [t] -> [t]
intercala [] l = l                          -- Se a primeira lista acabar ou já for vazia, retorna a segunda
intercala l [] = l                          -- Mesma lógica da linha anterior
intercala (c1:r1) (c2:r2) = c1 : c2 : (intercala r1 r2) -- Alterna elementos das duas listas

-------------------------------------------------------------------------------------------------------------
-- Questão 19: recebe duas listas e verifica se elas tem os mesmos elementos
--             Obs.: elementos repetidos são o mesmo elemento. 

mesmos_elementos :: (Eq t) => [t] -> [t] -> Bool
mesmos_elementos l1 l2 =
    let s1 = remove_duplicatas l1
        s2 = remove_duplicatas l2
    in todos_pertencem s1 s2 && todos_pertencem s2 s1 -- Verifica se todos os elementos estão em ambas

-- Função auxiliar:
remove_duplicatas :: (Eq t) => [t] -> [t]
remove_duplicatas [] = []
remove_duplicatas (c:r)
    | pertence c r = remove_duplicatas r            -- Se já existe no resto, ignora
    | otherwise     = c : remove_duplicatas r       -- Senão, mantém o elemento

-- Função auxiliar: verifica se todos os elementos de a estão em b
todos_pertencem :: (Eq t) => [t] -> [t] -> Bool
todos_pertencem [] _ = True
todos_pertencem (c:r) l = pertence c l && todos_pertencem r l

-------------------------------------------------------------------------------------------------------------
-- Questão 22: recebe uma lista e retorna outra lista com seus itens ordenados.

ordena :: (Ord t) => [t] -> [t]
ordena [] = []
ordena l  = accumArv merge (map (\a -> [a]) l)     -- Merge sort

-- Função auxiliar:
pares :: (a -> a -> a) -> [a] -> [a]
pares fun (a:b:r) = (fun a b) : (pares fun r)      -- Aplica a função aos pares de elementos
pares _ lista     = lista                          -- Se sobrou um elemento ou lista vazia, retorna

-- Função auxiliar: árvore de acumulação
accumArv :: (t -> t -> t) -> [t] -> t
accumArv _ [x] = x 
accumArv fun l = accumArv fun (pares fun l)        -- Aplica pares recursivamente até sobrar um

-- Função auxiliar: merge sort
merge l1@(c1:r1) l2@(c2:r2)
    | c2 < c1   = c2:(merge l1 r2)
    | otherwise = c1:(merge r1 l2)
merge l [] = l
merge [] l = l

-------------------------------------------------------------------------------------------------------------
-- Questão 25: recebe um número natural, uma lista e retorna uma nova lista onde a posição dos elementos
--             mudou como se eles tivessem sido “rodados”.

rodar_esquerda :: Int -> [t] -> [t]
rodar_esquerda n l = fim ++ inicio
  where
    tam = length l
    n'
      | tam == 0  = 0             -- Se a lista é vazia, não rotaciona
      | otherwise = mod n tam     -- Garante que n não ultrapasse o tamanho
    (inicio, fim) = divide l n'   -- Divide a lista em duas partes e inverte a ordem

-------------------------------------------------------------------------------------------------------------
-- Questão 28: recebe uma string qualquer e retorna outra string onde somente as
--             iniciais são maiúsculas.

primeiras_maiusculas :: String -> String
primeiras_maiusculas = aux True
  where
    aux _ [] = []
    aux True (c:r)  -- Início de palavra
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32) : aux False r
      | otherwise            = c : aux (c == ' ') r -- Sempre que encontra um espaço, booleano fica
--                                                     true e entende que esta dentro de uma palavra
    aux False (c:r) -- Dentro da palavra
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32) : aux (c == ' ') r
      | otherwise            = c : aux (c == ' ') r

-------------------------------------------------------------------------------------------------------------
-- Questão 31: calcula a mediana de uma lista de números racionais.

mediana :: [Rational] -> Double
mediana l
  | odd n     = fromRational (pegarElemento meioIdx lOrd) -- Se ímpar, pega o elemento do meio
  | otherwise = fromRational ((pegarElemento (meioIdx - 1) lOrd + pegarElemento meioIdx lOrd) / 2)
  where
    lOrd = ordena l
    n = length lOrd
    meioIdx = div n 2

    pegarElemento :: Int -> [a] -> a
    pegarElemento 0 (c:_)  = c                      -- Retorna o elemento na posição
    pegarElemento k (_:r) = pegarElemento (k-1) r   -- Avança até o índice

-------------------------------------------------------------------------------------------------------------
-- Questão 34: recebe uma lista e verifica se ela é palíndromo ou não.

palindromo :: (Eq t) => [t] -> Bool
palindromo l
    | l == (reverse l) = True
    | otherwise        = False

-------------------------------------------------------------------------------------------------------------
-- Questão 37: recebe uma lista e retorna a lista ordenada, pelo método da bolha.
--             Obs.: versão do método em que não se verifica se houve alguma troca para parar mais cedo.

bolha :: (Ord t) => [t] -> [t]
bolha l = bolhaAux (length l) l
  where
    bolhaAux 0 l = l
    bolhaAux n l = bolhaAux (n-1) (bolhaIteracao l) -- Realiza n-1 passagens pela lista

    -- Uma iteracao do bubble sort: compara pares e troca se necessário
    bolhaIteracao (a:b:r)
      | a > b     = b : bolhaIteracao (a:r)
      | otherwise = a : bolhaIteracao (b:r)
    bolhaIteracao r = r

-------------------------------------------------------------------------------------------------------------