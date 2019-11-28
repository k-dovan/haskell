{- SPOP. Lab 2. (2 pkt.) -}

{- Obiekt w systemie plików jest plikiem o określonej nazwie, lub
katalogiem o określonej nazwie, zawierającym inne obiekty (które są
plikami lub katalogami): -}

data FSObject = File String | Directory String [FSObject] deriving (Eq, Show)

{- Przykładowa wartość: -}

fs :: FSObject
fs = Directory "Root" [(File "root.txt"),
                       (Directory "Tmp" [(File "tmp.txt"),
                                         (File "tmp.exe")
                                        ]),
                       (File "root.exe"),
                       (File "tmp.txt")
                      ]
fs1 :: FSObject
fs1 = (File "test.txt")

{- Zadanie 1. Napisz funkcję, która sprawdza, czy plik o podanej nazwie
istnieje w danym systemie plików. -}

is_file (File _) = True
is_file (Directory _ _) = False

find :: String -> FSObject -> Bool
find str (File f) = if str == f then True else False
find _ (Directory _ []) = False
find str (Directory dir (x:xs)) = find str x || find str (Directory dir xs)

{- Zadanie 2. Napisz funkcję, która szuka pliku o podanej nazwie w danym
systemu plików. Jeśli taki plik istnieje, funkcja zwraca ścieżkę do
tego pliku postaci "nazwa katalogu/nazwa katalogu/.../nazwa pliku". -}

search :: String -> FSObject -> Maybe String
search str (File f) = if str == f then Just str else Nothing
search str (Directory dir (x:xs)) = 
        Just (sear str [] (Directory dir (x:xs))) where
                sear str path (File f) = if str == f then (path ++ "/" ++ str ++ "|") else []
                sear str path (Directory y []) = []
                sear str path (Directory dir (x:xs)) = sear str (path ++ "/" ++ dir) x ++ sear str path (Directory dir xs)
                
                                            
                                       





                                       
                                            
                                            