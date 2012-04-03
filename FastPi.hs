-- | parallel pi interpreter
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import qualified Data.Map as M
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Exception
import System.IO
import Data.Maybe
import Data.Bits
import Data.Char
import Data.List
import Data.IORef
import Data.Word
import System.Random
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Environment
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding(get,Socket)
import Text.Printf
import Network.Socket hiding(send,sendTo,recv,recvFrom)
import Network.Socket.ByteString

import PiText
import qualified Pi

main=do
    tc<-parseArgs >>= createIncoming
    sk<-genSendSocket
    shell ShellState{sendSocket=sk,recvChannel=tc,neighbors=[],current=VM [] M.empty}


drawChannel :: TChan ((Int,Int),(Double,Double,Double))
drawChannel=unsafePerformIO $ do
    tc<-newTChanIO
    
    forkIO $ do
        initGUI
        
        window<-windowNew
        area<- drawingAreaNew
        set window [containerChild := area]
        widgetShowAll window
        dw<-widgetGetDrawWindow area
        
        img<-newMVar M.empty
        
        -- apply new value to img map
        forkIO $ forever $ do
            x<-tryReadTChan tc
            case x of
                Nothing -> threadDelay $ 10*100
                Just (pos,col) -> void $ modifyMVar_ img $ return . M.insert pos col
        
        -- trigger redraw
        forkIO $ forever $ do
            drawWindowInvalidateRect dw (Rectangle 0 0 100 100) True
            threadDelay $ 30*1000
        
        -- redraw handler
        area `on` exposeEvent $ tryEvent $ liftIO $ do
            (w,h) <-widgetGetSize area
            mm<-readMVar img
            print ("drawing",M.size mm)
            renderWithDrawable dw $ mapM_ (\((px,py),(r,g,b))->
                setSourceRGB r g b >>
                rectangle (fromIntegral px) (fromIntegral py) 1 1 >>
                fill) $ M.assocs mm

        
        mainGUI
    
    return tc

        
    

-- | fetch port no. to receive processes
parseArgs :: IO Int
parseArgs=do
    args<-getArgs
    case args of
        [] -> return 4423
        [p] -> return $ read p


-- generate random socket for sending processes
genSendSocket=do
    from_addrs<-getAddrInfo (Just defaultHints{addrFlags=[AI_PASSIVE,AI_ADDRCONFIG],addrSocketType=Datagram}) (Just "::") Nothing
    let addr=head from_addrs
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)


createIncoming :: Int -> IO (TChan Process)
createIncoming port=do
    as<-getAddrInfo (Just defaultHints{addrFlags=[AI_PASSIVE,AI_ADDRCONFIG],addrSocketType=Datagram}) (Just "::") (Just $ show port)
    let addr=head as
    sock<-socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bindSocket sock $ addrAddress addr
    printf "waiting on port %d\n" port
    
    tc<-newTChanIO
    forkIO $ forever $ do
        packet<-liftM fst $ recvFrom sock 257
        let flag=head $ BS.unpack packet; body=BS.drop 1 packet
--        printf "data incoming flag=0x%02x len=%d\n" flag (BS.length packet)
        case flag .&. 0xf0 of
            0x20 -> do
                let n=fromIntegral $ flag .&. 0x0f
                let (c:as)=Bin.runGet (replicateM (n+1) Bin.get) $ stol body
   --             putStrLn "ok"
                atomically $ writeTChan tc $ Output c as
            _ -> return ()
    
    return tc


-- it's assumed that underlying link exposes underlying physical property as closely as possible.
-- In other terms, it's optimized for latency.
-- providing reliability / security etc. is up to application (and library)
-- (communication as program)
data OtherVM
    =IPLink String Int -- ^ ip address & port (UDP)
    deriving(Show)

-- output
diffuse :: Socket -> [OtherVM] -> [(BID,[BID])] -> IO [(BID,[BID])]
diffuse sk ns cs=do
    cs'<-mapM select cs
    return $ catMaybes cs'
    where
        n=length ns
        select c=do
            ix<-randomRIO (0,n)
            if ix==n
                then return $ Just c
                else Main.sendTo sk (ns!!ix) c >> return Nothing
            
        

sendTo :: Socket -> OtherVM -> (BID,[BID]) -> IO ()
sendTo sock (IPLink n port) (c,as)=do
    -- generate recipent addr
    to_addrs<-getAddrInfo (Just defaultHints{addrFlags=[AI_PASSIVE,AI_ADDRCONFIG],addrSocketType=Datagram}) (Just n) (Just $ show port)
    let addr=head to_addrs
    
    -- send process through it
    sendAllTo sock (ltos $ Bin.encode (Output c as)) (addrAddress addr)


stol=BSL.pack . BS.unpack
ltos=BS.pack . BSL.unpack

data ShellState=ShellState
    {sendSocket :: Socket
    ,recvChannel :: TChan Process
    ,neighbors :: [OtherVM]
    ,current :: VM}


shell :: ShellState -> IO ()
shell st=do
    putStr "> "
    hFlush stdout
    x<-getLine
    
    case x of
        "q" -> return ()
        "e" -> do
            putStrLn "emptying VM"
            shell st{current=VM [] M.empty}
        "r" -> do
            putStrLn "running VM (Ctrl-C to suspend)" 
            vm'<-runWithInterrupt (sendSocket st) (recvChannel st) (current st) (neighbors st)
            putStrLn "suspended"
            shell st{current=vm'}
        "s" -> do
            x<-tryReadTChan (recvChannel st)
            print x
            case x of
                Just y -> atomically $ writeTChan  (recvChannel st) y
                Nothing -> return ()
            printf "= %d links =\n" (length $ neighbors st)
            mapM_ print $ neighbors st
            showVM $ current st
            shell st
        ('f':'i':path) -> do
            ex<-doesFileExist path
            if ex
                then do
                    putStrLn $ "filing in from "++path
                    x<-readFile path
                    case parseProcesses x of
                        Left errs -> mapM_ (\(PError n s)->printf "at line %d: %s\n" n s) errs >> shell st
                        Right ps_delta -> do
                            printf "inserting process\n"
                            putStr $ pprintProcess ps_delta
                            let VM ps cs=current st
                            shell st{current=VM (ps_delta:ps) cs}
                else do
                    putStrLn $ "file not found "++path
                    shell st
        ('f':'o':path) -> do 
            putStrLn $ "saving to "++path
            shell st
        ('c':node) -> do
            let [h,p]=words node; port=read p
            printf "linking to %s : %d\n" h port
            shell st{neighbors=IPLink h port:neighbors st}
        "d" -> do
            printf "severing all links\n"
            shell st{neighbors=[]}
        ""-> shell st
        _ -> do
            putStrLn $ "unknown command "++x
            shell st


showVM (VM ps cs)=do
    printf "VM stat: proc=%d channel=%d\n" (length ps) (M.size cs)
    putStrLn "processes"
    mapM_ print ps
    putStrLn ""
    putStrLn "channels"
    mapM_ print $ M.assocs cs

runWithInterrupt :: Socket -> TChan Process -> VM -> [OtherVM] -> IO VM
runWithInterrupt sk inc vm links=do
    (tid,cstrm)<-charStream
    current<-newIORef vm
    let step=readIORef current >>= infuseVM inc >>= stepVM cstrm >>= diffuseVM sk links >>= writeIORef current
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




instance Bin.Binary BID where
    put (BID x y)=Bin.put x >> Bin.put y
    get=liftM2 BID Bin.get Bin.get

instance Bin.Binary Process where
    put (Input c as p)=Bin.putWord8 (0x00 .|. (fromIntegral $ length as)) >> Bin.put c >> mapM_ Bin.put as >> Bin.put p
    put (InputR c as p)=Bin.putWord8 (0x10 .|. (fromIntegral $ length as)) >> Bin.put c >> mapM_ Bin.put as >> Bin.put p
    put (Output c as)=Bin.putWord8 (0x20 .|. (fromIntegral $ length as)) >> Bin.put c >> mapM_ Bin.put as
    put (New c p)=Bin.putWord8 0x30 >> Bin.put c >> Bin.put p
    put (Par ps)=mapM_ Bin.put ps
    put Null=return ()
    
    get=do
        w<-Bin.getWord8
        let code=w .&. 0xf0; len=fromIntegral (w .&. 0x0f)
        case code of
            0x00 -> liftM3 Input Bin.get (replicateM len Bin.get) Bin.get
            0x10 -> liftM3 InputR Bin.get (replicateM len Bin.get) Bin.get
            0x20 -> liftM2 Output Bin.get (replicateM len Bin.get)
            0x30 -> liftM2 New Bin.get Bin.get
            _ -> return Null

data VM=VM [Process] (M.Map BID [BID]) deriving(Show)

infuseVM :: TChan Process -> VM -> IO VM
infuseVM tc vm@(VM ps cs)=do
    x<-tryReadTChan tc
    case x of
        Nothing -> return vm
        Just p ->return $ VM (p:ps) cs
    
diffuseVM :: Socket -> [OtherVM] -> VM -> IO VM
diffuseVM sk links (VM ps cs)=do
    xs<-diffuse sk links $ M.assocs cs
    return $ VM ps $ M.fromList xs

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
            Just v -> [(k,[v])]
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
-- raster2

stepPrim strm proc@(BID 0xb94e960c1b828a55 0xcb213d5747d37e98,args)=
    case args of
        [k] -> print "here" >> return [(k,[packPos (0,0)])]
        _ -> return []

stepPrim strm proc@(BID 0x2f5323a5235e8b6b 0xb97469b18623ad36,args)=
    case args of
        [p,r,u,l,d] -> do
            let Just (x,y)=unpackPos p
            return
                [(r, [packPos (x+1,y)])
                ,(u, [packPos (x,y-1)])
                ,(l, [packPos (x-1,y)])
                ,(d, [packPos (x,y+1)])]
        _ -> return []

stepPrim strm proc@(BID 0xb202935f7db468ef 0xba228f67e38501ea,args)=
    case args of
        [pos,colR] -> do
            let Just (x,y)=unpackPos pos; Just col=unpackCol colR
            atomically $ writeTChan drawChannel ((x,y),col)
            return []
        _ -> return []

-- others
stepPrim _ p=return [p]

-- VM dependent impl.
packPos ::  (Int,Int) -> BID
packPos (x,y)=toU32 $
    fromIntegral (x+2^15) `shiftL` 16 .|. fromIntegral (y+2^15)

unpackPos :: BID -> Maybe (Int,Int)
unpackPos bid=do
    w<-asU32 bid
    return (fromIntegral (w `shiftR` 16)-2^15, fromIntegral (w .&. 0xffff)-2^15)

unpackCol :: BID -> Maybe (Double,Double,Double)
unpackCol bid=do
    w<-asU32 bid
    let f n=fromIntegral ((w `shiftR` n) .&. 0xff)/256
    return (f 16,f 8,f 0)
    

tryReadTChan :: TChan a -> IO (Maybe a)
tryReadTChan tc=atomically $
    liftM Just (readTChan tc) `orElse` return Nothing



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

