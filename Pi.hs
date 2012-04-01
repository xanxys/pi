module Pi where

-- synchronous, polyadic
data Proc
    =New Name Proc
    |Recv Name [Name] Proc
    |Send Name [Name] Proc
    |Par Proc Proc
    |Many Proc
    |Null
    -- this is hidden from user. only 'bind' knows about it.
    |Prim String
    deriving(Show)

type Name=String

toMonadicAsync=toAsync . toMonadic

-- sync poly to sync mono
toMonadic :: Proc -> Proc
toMonadic (Par p q)=Par (toMonadic p) (toMonadic q)
toMonadic (New n p)=New n (toMonadic p)
toMonadic (Recv n ns p)
    |length ns<=1 = Recv n ns (toMonadic p)
    |otherwise = Recv n ["#t"] $ foldr (\a e->Recv "#t" [a] e) (toMonadic p) ns
toMonadic (Send n ns p)
    |length ns<=1 = Send n ns (toMonadic p)
    |otherwise = Par (Send n ["#t"] Null) (foldr (\a e->Send "#t" [a] e) (toMonadic p) ns)
toMonadic (Many p)=Many (toMonadic p)
toMonadic Null=Null
toMonadic p=p -- ?

-- sync mono to async mono
toAsync :: Proc -> Proc
toAsync (Send n ns p)=
    New "#s" $ Par
        (Send n ["#s"] Null)
        (Recv "#s" ["#t"] $ Par
            (Send "#t" ns Null)
            (toAsync p))
toAsync (Recv n ns p)=
    Recv n ["#s"] $ Par
        (Send "#s" ["#t"] Null)
        (Recv "#t" ns (toAsync p))
toAsync (New n p)=New n (toAsync p)
toAsync (Par p q)=Par (toAsync p) (toAsync q)
toAsync (Many p)=Many (toAsync p)
toAsync Null=Null
toAsync p=p -- ?


