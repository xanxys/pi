{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Data.Map as M
import Control.Exception
import Data.Tree
import System.IO
import Data.Bits
import Data.Char
import Data.List
import Data.IORef
import Data.Word
import qualified Data.Digest.MD5 as C
import System.Random
import System.IO
import System.Directory
import Text.Printf
import Text.Peggy
import qualified Pi

main=shell $ VM [] M.empty

shell vm=do
    putStr "> "
    hFlush stdout
    x<-getLine
    
    case x of
        "q" -> return ()
        "e" -> do
            putStrLn "emptying VM"
            shell $ VM [] M.empty
        "r" -> do
            putStrLn "running VM (Ctrl-C to suspend)" 
            vm'<-runWithInterrupt vm
            putStrLn "suspended"
            shell vm'
        "s" -> do
            showVM vm
            shell vm
        ('f':'i':path) -> do
            ex<-doesFileExist path
            if ex
                then do
                    putStrLn $ "filing in from "++path
                    x<-readFile path
                    case parseProcesses x of
                        Left errs -> mapM_ (\(PError n s)->printf "at line %d: %s\n" n s) errs >> shell vm
                        Right ps_delta -> do
                            printf "inserting processes\n"
                            let VM ps cs=vm
                            shell $ VM (ps_delta:ps) cs
                else do
                    putStrLn $ "file not found "++path
                    shell vm
        ('f':'o':path) -> do 
            putStrLn $ "saving to "++path
            shell vm
        {-
        ('i':ws) -> do
            case parseString pr "<user input>" ws of
                Left err -> print err >> shell vm
                Right p -> do
                    putStrLn "inserting process: "
                    print p
                    let VM ps cs=vm
                    shell $ VM (p:ps) cs
        -}
        ""-> shell vm
        _ -> do
            putStrLn $ "unknown command "++x
            shell vm

data PError=PError Int String deriving(Show)

parseProcesses :: String -> Either [PError] Process
parseProcesses filecont
    |any isLeft lines_a = Left $ map fromLeft $ filter isLeft lines_a
    |otherwise = Right $ constructM $ parseIndentTree cont
    where
        construct (Node h@(HNew r) cs)=New r $ constructM cs
        construct (Node h@(HInput c as) cs)=Input c as $ constructM cs
        construct (Node h@(HInputR c as) cs)=InputR c as $ constructM cs
        construct (Node h@(HOutput c as) [])=Output c as
        construct (Node h@(HOutput _ _) _)=error "output cannot take continuation"
        
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



parseIndentTree :: [(Int,a)] -> Forest a
parseIndentTree=fst . aux 0
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

showVM (VM ps cs)=do
    printf "VM stat: proc=%d channel=%d\n" (length ps) (M.size cs)
    putStrLn "processes"
    mapM_ print ps
    putStrLn ""
    putStrLn "channels"
    mapM_ print $ M.assocs cs

runWithInterrupt :: VM -> IO VM
runWithInterrupt vm=do
    (tid,cstrm)<-charStream
    current<-newIORef vm
    let step=readIORef current >>= stepVM cstrm >>= writeIORef current
    forever step `Control.Exception.catch`
        \e -> killThread tid >> if e==UserInterrupt then return () else throw e
    readIORef current

charStream :: IO (ThreadId,TChan Char)
charStream=do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    tc<-newTChanIO
    tid<-forkIO $ forever $ do
        ch<-hGetChar stdin
        atomically $ writeTChan tc ch
    return (tid,tc)

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

data Lambda
    =Fun String Lambda
    |App Lambda Lambda
    |Ref String


-- call-by-value (strict)
{-
lambdaToPi at (Ref n)=Output (hash n) [at]
lambdaToPi at (App f x)=New q $ Par
    (lambdaToPi q f)
    (Input q [hash "v"] $ New (hash "r") $ Par
        (lambdaToPi x $ hash "r")
        (Input (hash "r") [hash "w"] $ New (hash "x") $ 
    where
        q=hash "f"
-}

-- async, input-replication only 
convertPi :: Pi.Proc -> Process
convertPi (Pi.Send c ns Pi.Null)=Output (hash c) (map hash ns)
convertPi (Pi.Recv c ns p)=Input (hash c) (map hash ns) (convertPi p)
convertPi (Pi.Many (Pi.Recv c ns p))=InputR (hash c) (map hash ns) (convertPi p)
convertPi (Pi.New v p)=New (hash v) (convertPi p)
convertPi (Pi.Par p q)=Par [convertPi p,convertPi q]
convertPi (Pi.Null)=Output (BID 0 0) [] -- hackish
convertPi p=error $ "cannot process: "++show p



data BID=BID !Word64 !Word64 deriving(Eq,Ord)

instance Show BID where
    show (BID x y)=printf "%016x" x -- printf "%016x%016x" x y

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

data Process
    =Input !BID ![BID] !Process
    |InputR !BID ![BID] !Process
    |New !BID !Process -- is this really necessary?
    |Output !BID ![BID]
    |Par [Process]
    |Null
    deriving(Show)


data VM=VM [Process] (M.Map BID [BID]) deriving(Show)

stepVM :: TChan Char -> VM -> IO VM
stepVM cstrm (VM ps cs)=do    
    -- calcCycle
    g<-newStdGen
    let (ps',(cs',_))=runState (calcCycle ps) (cs,g)
    -- primCycle
    cs''<-primCycle cstrm cs'
    return $ VM ps' cs''

calcCycle :: [Process] -> State (M.Map BID [BID],StdGen) [Process]
calcCycle=liftM concat . mapM stepProcess

primCycle :: TChan Char -> M.Map BID [BID] -> IO (M.Map BID [BID])
primCycle cstrm cs=do
    pairs<-mapM (stepPrim cstrm) $ M.assocs cs
    return $ M.fromList $ concat pairs

stepPrim :: TChan Char -> (BID,[BID]) -> IO [(BID,[BID])]
-- artih32:add
stepPrim _ (BID 0x12a7dd977d635c3c 0x1c36cdbe40b7d99b,args)=return $ 
    case args of
        [x,y,c] -> case liftM2 (+) (asU32 x) (asU32 y) of
            Just v -> [(c,[toU32 v])]
            Nothing -> []
        _ -> []
-- mirror
stepPrim _ (BID 0xeec4b16005331d2f 0xe9dc5aaf6f9f1eb6,args)=return $
    case args of
        [a,b,c,d,k] -> case liftM4 compose (asU32 a) (asU32 b) (asU32 c) (asU32 d) of
            Just v -> [(c,[v])]
            Nothing -> []
        _ -> []
    where
        compose a b c d=BID
            (fromIntegral a `shiftL` 32 .|. fromIntegral b)
            (fromIntegral c `shiftL` 32 .|. fromIntegral d)
stepPrim _ (BID 0x76c24ddb17c0fb6d 0x66ae713d03ef7511,args)=return $
    case args of
        [c,k] -> [(k,decompose c)]
        _ -> []
    where
        decompose (BID x y)=map (toU32 . fromIntegral) [x `shiftR` 32,x,y `shiftR` 32,y]
-- stdio
stepPrim _ (BID 0x6c1d0fba4c7e97a7 0xb3252dfe1f23ab3a,args)=do
    case args of
        [x,k] -> case asU32 x of
            Just v -> hPutChar stdout (chr $ fromIntegral v) >> hFlush stdout >> return [(k,[])]
            Nothing -> return []
        _ -> return []
stepPrim strm proc@(BID 0x9d3e378ac6d4b6a1 0x51380ad49adb6cf9,args)=
    case args of
        [k] -> do
            c<-tryReadTChan strm
            case c of
                Nothing -> return [proc]
                Just ch -> return [(k,[toU32 $ fromIntegral $ ord ch])]
        _ -> return []
-- others
stepPrim _ p=return [p]



tryReadTChan :: TChan a -> IO (Maybe a)
tryReadTChan tc=atomically $
    liftM Just (readTChan tc) `orElse` return Nothing

toU32 :: Word32 -> BID
toU32 x=BID 0xc356154e282ade9b (0xd11b4e1200000000 .|. fromIntegral x)

asU32 :: BID -> Maybe Word32
asU32 (BID x y)
    |x==0xc356154e282ade9b && (y `shiftR` 32)==0xd11b4e12 = Just $ fromIntegral (y .&. 0xffffffff)
    |otherwise = Nothing



stepProcess p@(Input ch var q)=do
    (cs,r)<-get
    case M.lookup ch cs of
        Nothing -> return [p]
        Just val -> do
            put (M.delete ch cs,r)
            return [foldl' (\q (fr,to)->replaceProcess fr to q) q $ zip var val]

stepProcess p@(InputR ch var q)=do
    (cs,r)<-get
    case M.lookup ch cs of
        Nothing -> return [p]
        Just val -> do
            put (M.delete ch cs,r)
            return [foldl' (\q (fr,to)->replaceProcess fr to q) q $ zip var val, p]
            -- ",p" is different from Input

stepProcess p@(Output ch var)=do
    (cs,r)<-get    
    case M.lookup ch cs of
        Just _ -> return [p]
        Nothing -> do
            put (M.insert ch var cs,r)
            return []

stepProcess (New var p)=do
    (cs,r)<-get
    let (var',r')=random r
    put (cs,r')
    return [replaceProcess var var' p]

stepProcess (Par ps)=return ps

stepProcess Null=return []

replaceProcess :: BID -> BID -> Process -> Process
replaceProcess from to (Input x ys p)=
    Input (repl from to x) (map (repl from to) ys) (replaceProcess from to p)
replaceProcess from to (InputR x ys p)=
    InputR (repl from to x) (map (repl from to) ys) (replaceProcess from to p)
replaceProcess from to (New x p)=New (repl from to x) (replaceProcess from to p)
replaceProcess from to (Output x ys)=Output (repl from to x) (map (repl from to) ys)
replaceProcess from to (Par ps)=Par (map (replaceProcess from to) ps)
replaceProcess _ _ Null=Null

repl from to x
    |x==from = to
    |otherwise = x
