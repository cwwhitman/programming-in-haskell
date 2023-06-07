import Data.Foldable

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--    -- mempty :: (a, b)
--    mempty = (mempty, mempty)

--    -- mappend :: (a,b) -> (a,b) -> (a,b)
--    (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- instance Monoid b => Monoid ((->) a b) where
--    mempty :: a -> b
--    mempty = const mempty

--    mappend :: (a -> b) -> (a -> b) -> (a -> b) 
--    mappend f g x = f x `mappend` g x

data Erm a = Ig a | Nope

instance Functor Erm where
   fmap :: (a -> b) -> Erm a -> Erm b
   fmap _ Nope   = Nope
   fmap f (Ig x) = Ig (f x)

instance Foldable Erm where
   fold :: Monoid m => Erm m -> m
   fold Nope = mempty
   fold (Ig x) = x

   foldMap :: Monoid m => (a -> m) -> Erm a -> m
   foldMap _ Nope   = mempty
   foldMap f (Ig x) = f x

   foldr :: (a -> b -> b) -> b -> Erm a -> b
   foldr _ v Nope   = v
   foldr f v (Ig x) = f x v

   foldl :: (b -> a -> b) -> b -> Erm a -> b
   foldl _ v Nope   = v
   foldl f v (Ig x) = f v x

instance Traversable Erm where
   traverse :: Applicative f => (a -> f b) -> Erm a -> f (Erm b)
   traverse _ Nope   = pure Nope
   traverse f (Ig x) = Ig <$> f x

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
   fold :: Monoid m => Tree m -> m
   fold Leaf         = mempty
   fold (Node l x r) = fold l `mappend` x `mappend` fold r

   foldMap :: Monoid m => (a -> m) -> Tree a -> m
   foldMap _ Leaf         = mempty
   foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

   foldr :: (a -> b -> b) -> b -> Tree a -> b
   foldr _ v Leaf         = v
   foldr f v (Node l x r) = foldr f (f x (foldr f v r)) l

   foldl :: (b -> a -> b) -> b -> Tree a -> b
   foldl _ v Leaf         = v
   foldl f v (Node l x r) = foldl f (f (foldl f v l) x) r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF g = foldMap (\x -> [x | g x])

