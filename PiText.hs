-- | Pretty printing & parsing of indented pi notation
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module PiText where
import Control.Monad
import Data.Tree
import Data.Maybe
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import qualified Data.Digest.MD5 as C
import System.Random
import Text.Printf
import Text.Peggy

-- | Asynchronous, polyadic, weak-replication pi calculus with 128 bit channel
-- 0<=#argument<16
data Process
    =Input !BID ![BID] !Process
    |InputR !BID ![BID] !Process
    |New !BID !Process
    |Output !BID ![BID]
    |Par [Process]
    |Null
    deriving(Show)

data BID=BID !Word64 !Word64 deriving(Eq,Ord)




data PError=PError Int String deriving(Show)

parseProcesses :: String -> Either [PError] Process
parseProcesses filecont
    |any isLeft lines_a = Left $ map fromLeft $ filter isLeft lines_a
    |otherwise = Right $ constructM $ parseIndentForest cont
    where
        construct (Node h@(HNew r) cs)=New r $ constructM cs
        construct (Node h@(HInput c as) cs)=Input c as $ constructM cs
        construct (Node h@(HInputR c as) cs)=InputR c as $ constructM cs
        construct (Node h@(HOutput c as) cs)=case constructM cs of
            Null -> Output c as
            _ -> error "output cannot take continuation"
        
        constructM []=Null
        constructM [x]=construct x
        constructM xs=Par $ map construct xs
        
        -- preprocess
        cont=map fromRight $ lines_a
        
        lines_a :: [Either PError (Int,PrHeader)]
        lines_a=map proc_a $ filter (not . null . snd) $ zip [1..] (map removeComment $ lines filecont)
        
        proc_a :: (Int,String) -> Either PError (Int,PrHeader)
        proc_a (a,xs)=case proc xs of
            Left err -> Left $ PError a err
            Right c -> Right $ c
        
        proc xs=
            parseLine =<< separateTabs xs
        
        removeComment xs=
            reverse $ dropWhile (==' ') $ reverse $ takeWhile (/='#') xs
        
        separateTabs xs
            |ni `mod` 4/=0 = Left "illegal indent"
            |otherwise = Right (ni `div` 4,content)
            where
                ni=length indent
                (indent,content)=span (==' ') xs
        
        parseLine (tabs,cont)=case parseString prHeaderTop "" cont of
            Left err -> Left $ show err
            Right ph -> Right (tabs,ph)


pprintProcess :: Process -> String
pprintProcess p=unlines $ map f $ flattenIndentForest $ toForest p
    where
        f (i,x)=replicate (i*4) ' '++show x
        
        toForest (Input c as x)=[Node (HInput c as) $ toForest x]
        toForest (InputR c as x)=[Node (HInputR c as) $ toForest x]
        toForest (Output c as)=[Node (HOutput c as) []]
        toForest (New c x)=[Node (HNew c) $ toForest x]
        toForest (Par ps)=concatMap toForest ps
        toForest Null=[]
        


flattenIndentForest ::  Forest a -> [(Int,a)]
flattenIndentForest=aux 0
    where
        aux n ts=concatMap (aux1 n) ts
        aux1 n (Node x cs)=(n,x):aux (n+1) cs


parseIndentForest :: [(Int,a)] -> Forest a
parseIndentForest=fst . aux 0
    where
        aux _ []=([],[])
        aux n rs0@((i,x):rs)
            |i<n = ([],rs0)
            |i==n = let (cs,rs')=aux (n+1) rs; (ds,rs'')=aux n rs' in (Node x cs:ds,rs'')
            |otherwise = error "unexpected indent level"


isLeft (Left _)=True
isLeft _=False

fromLeft (Left x)=x
fromLeft _=undefined

fromRight (Right x)=x
fromRight _=undefined

data PrHeader
    =HInput BID [BID]
    |HInputR BID [BID]
    |HOutput BID [BID]
    |HNew BID
    deriving(Show)

[peggy|

prHeaderTop :: PrHeader
    = prHeader !.

prHeader :: PrHeader
    = "!" channel ">"  "(" (channel , ",") ")" {HInputR $1 $2}
    / "new" "(" channel ")" {HNew $1}
    / channel prHPartial { $2 $1 }

prHPartial :: BID -> PrHeader
    = ">"  "(" (channel , ",") ")" {flip HInput $1}
    / "<"   "(" (channel , ",") ")" {flip HOutput $1}


channel :: BID
    = [_A-Za-z][_A-Za-z0-9]* {hash ($1:$2)}
    / "[" [0-9]* "]" {toU32 $ read $1}
    / uint128 {bid $1}

uint128 :: String
    = uint32 uint32 uint32 uint32 { $1 ++ $2 ++ $3 ++ $4 }

uint32 :: String
    = byte byte byte byte { $1 ++ $2 ++ $3 ++ $4 }

byte :: String
    = [a-f0-9] [a-f0-9] { [ $1 ,  $2 ]}

|]



hash :: String -> BID
hash "_arith32_add"=bid "12a7dd977d635c3c1c36cdbe40b7d99b"
hash "_arith32_not"=bid "fe058d8716b9f8ff498f1adfc1e8c0b6"
hash "_arith32_and"=bid "5481a53338613416250422c03aa5ff6b"
hash "_arith32_mul"=bid "a52d225780383881ac7c246e75b8805b"
hash "_arith32_div"=bid "da3e4dc5790428ad70f3b1b5a1017139"
hash "_mirror_encode"=bid "eec4b16005331d2fe9dc5aaf6f9f1eb6"
hash "_mirror_decode"=bid "76c24ddb17c0fb6d66ae713d03ef7511"
hash "_stdio_putbyte"=bid "6c1d0fba4c7e97a7b3252dfe1f23ab3a"
hash "_stdio_getbyte"=bid "9d3e378ac6d4b6a151380ad49adb6cf9"
hash str=BID (packBytes x) (packBytes y)
    where
        packBytes=foldl1' (\s v->(s `shiftL` 8) .|. v)
        (x,y)=splitAt 8 $ (map fromIntegral d :: [Word64])
        d=C.hash $ map (fromIntegral . ord) str

bid :: String -> BID
bid str=BID (read $ "0x"++a) (read $ "0x"++b)
    where
        (a,b)=splitAt 16 str




instance Show BID where
    show (BID x y)=if True then printf "%016x%016x" x y else printf "%016x" x

instance Random BID where
    random g=(BID x y,g'')
        where
            (x,g')=random g
            (y,g'')=random g'
    randomR=undefined

instance Random Word64 where
    random g=(fromIntegral x,g')
        where (x,g')=randomR (0,2^64-1 :: Integer) g
    randomR=undefined



toU32 :: Word32 -> BID
toU32 x=BID 0xc356154e282ade9b (0xd11b4e1200000000 .|. fromIntegral x)

asU32 :: BID -> Maybe Word32
asU32 (BID x y)
    |x==0xc356154e282ade9b && (y `shiftR` 32)==0xd11b4e12 = Just $ fromIntegral (y .&. 0xffffffff)
    |otherwise = Nothing


