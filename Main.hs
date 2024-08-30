-- Función principal
main :: IO ()
main = do
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"
    menu

-- Función para mostrar el menú y manejar la elección del usuario
menu :: IO ()
menu = do
    putStrLn "Selecciona una de las opciones:"
    putStrLn "1. Registremos la entrada del artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Contar todos los artículos"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> registrarArticulo >> menu
        "2" -> buscarPorCategoria >> menu
        "3" -> listarArticulos >> menu
        "4" -> contarArticulos >> menu
        "5" -> putStrLn "Saliendo..."
        _   -> putStrLn "Opción no válida. Por favor, intente de nuevo." >> menu

-- Función para registrar un nuevo artículo
registrarArticulo :: IO ()
registrarArticulo = do
    putStrLn "Ingrese el nombre del artículo:"
    nombre <- getLine
    putStrLn "Ingrese la categoría del artículo:"
    categoria <- getLine
    appendFile "inventario.txt" (nombre ++ "," ++ categoria ++ "\n")
    putStrLn "Tu artículo se registró exitosamente"

-- Función para buscar artículos por categoría
buscarPorCategoria :: IO ()
buscarPorCategoria = do
    putStrLn "Ingrese la categoría a buscar:"
    categoria <- getLine
    contenido <- readFile "inventario.txt"
    let articulos = lines contenido
        filtrados = filter (esDeCategoria categoria) articulos
    if null filtrados
        then putStrLn "La categoría que solicitas no se encuentra registrada, intenta nuevamente."
        else do
            putStrLn "Artículos encontrados:"
            mapM_ putStrLn filtrados

esDeCategoria :: String -> String -> Bool
esDeCategoria categoria line =
    let partes = wordsWhen (==',') line
    in length partes > 1 && categoria == partes !! 1

-- Función para listar todos los artículos
listarArticulos :: IO ()
listarArticulos = do
    contenido <- readFile "inventario.txt"
    if null contenido
        then putStrLn "Aún no hay artículos registrados"
        else do
            putStrLn "Lista de todos los artículos:"
            putStrLn contenido

-- Función para contar todos los artículos
contarArticulos :: IO ()
contarArticulos = do
    contenido <- readFile "inventario.txt"
    let numArticulos = length (lines contenido)
    putStrLn $ "Número total de artículos: " ++ show numArticulos

-- Función auxiliar para dividir cadenas de texto
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

