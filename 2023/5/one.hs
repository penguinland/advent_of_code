main :: IO()
main = do
    stdin <- getContents
    print "hello"
    print stdin
