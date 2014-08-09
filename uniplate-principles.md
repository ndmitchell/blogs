# Uniplate Principles


    data Tree a
        = Value a
        | List [Tree a]
        | Label String (Tree a)
        | Literal Int
        | Pair (Tree a) (Tree a)

    walk :: Tree a -> Either a ([Tree a], [Tree b] -> Tree b)
    walk (Value x) = Left x
    walk (List xs) = Right (xs, List)
    walk (Label x y) = Right ([y], \[y] -> Label x y)
    walk (Literal x) = Right ([], const $ Literal x)
    walk (Pair x y) = Right ([x,y], \[x,y] -> Pair x y)

    instance Functor Tree where
        fmap f x = case walk x of
            Left a -> Extra $ f a
            Right (cs, gen) -> gen $ map (fmap f) cs

    instance Applicative Tree where
        pure = Extra
        f <*> x = case walk f of
            Left f -> fmap f x
            Right (cs, gen) -> gen $ map (<*> x) cs

    instance Monad Tree where
        return = Extra
        x >>= f = case walk x of
            Left a -> f a
            Right (cs, gen) -> gen $ map (>>= f) cs

    instance Foldable ?
    instance Traversable ?
    
    childrenTree :: Tree a -> [Tree a]
    childrenTree = either (const []) fst . walk
    
    universeTree :: Tree a -> [Tree a]
    universeTree x = x : concatMap universeTree (childrenTree x)

Useful to write:

    join $ fmap (\(Value x) -> Literal x)
